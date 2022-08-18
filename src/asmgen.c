#include "asmgen.h"

#include "assert.h"
#include "astree.h"
#include "attributes.h"
#include "badalist.h"
#include "debug.h"
#include "lyutils.h"
#include "state.h"
#include "symtable.h"

#define MAX_OPCODE_LENGTH 8
#define MAX_OPERAND_LENGTH 32
#define MAX_LABEL_LENGTH 32

/* macros used to generate string constants for OPCODES */
#define FOREACH_OPCODE(GENERATOR) \
  /* arithmetic */                \
  GENERATOR(ADD)                  \
  GENERATOR(SUB)                  \
  GENERATOR(MUL)                  \
  GENERATOR(DIV)                  \
  GENERATOR(INC)                  \
  GENERATOR(DEC)                  \
  GENERATOR(NEG)                  \
  GENERATOR(IMUL)                 \
  GENERATOR(IDIV)                 \
  /* compare and test */          \
  GENERATOR(TEST)                 \
  GENERATOR(CMP)                  \
  GENERATOR(SETE)                 \
  GENERATOR(SETNE)                \
  GENERATOR(SETG)                 \
  GENERATOR(SETGE)                \
  GENERATOR(SETL)                 \
  GENERATOR(SETLE)                \
  GENERATOR(SETA)                 \
  GENERATOR(SETAE)                \
  GENERATOR(SETB)                 \
  GENERATOR(SETBE)                \
  GENERATOR(SETZ)                 \
  GENERATOR(SETNZ)                \
  /* jump */                      \
  GENERATOR(JMP)                  \
  GENERATOR(JE)                   \
  GENERATOR(JNE)                  \
  GENERATOR(JG)                   \
  GENERATOR(JGE)                  \
  GENERATOR(JL)                   \
  GENERATOR(JLE)                  \
  GENERATOR(JA)                   \
  GENERATOR(JAE)                  \
  GENERATOR(JB)                   \
  GENERATOR(JBE)                  \
  GENERATOR(JZ)                   \
  GENERATOR(JNZ)                  \
  /* logical and bitwise */       \
  GENERATOR(NOT)                  \
  GENERATOR(OR)                   \
  GENERATOR(AND)                  \
  GENERATOR(LEA)                  \
  GENERATOR(XOR)                  \
  /* shifts */                    \
  GENERATOR(SHL)                  \
  GENERATOR(SHR)                  \
  GENERATOR(SAR)                  \
  /* code movement */             \
  GENERATOR(MOV)                  \
  GENERATOR(MOVZX)                \
  GENERATOR(MOVSX)                \
  GENERATOR(PUSH)                 \
  GENERATOR(POP)                  \
  GENERATOR(CALL)                 \
  GENERATOR(LEAVE)                \
  GENERATOR(RET)                  \
  GENERATOR(NOP)                  \
  /* data section definitions */  \
  GENERATOR(DB)                   \
  GENERATOR(DW)                   \
  GENERATOR(DD)                   \
  GENERATOR(DQ)                   \
  /* bss section definitions */   \
  GENERATOR(RESB)                 \
  GENERATOR(RESW)                 \
  GENERATOR(RESD)                 \
  GENERATOR(RESQ)

#define GENERATE_ENUM(ENUM) OP_##ENUM,
#define GENERATE_STRING(STRING) #STRING,

typedef enum opcode { FOREACH_OPCODE(GENERATE_ENUM) } Opcode;

typedef enum address_mode {
  MODE_NONE,
  MODE_IMMEDIATE,
  MODE_DIRECT,
  MODE_INDIRECT,
  MODE_SCALE_1,
  MODE_SCALE_2,
  MODE_SCALE_4,
  MODE_SCALE_8, /* decimal 7, binary 111 */
  MODE_REG_Q = 0,
  MODE_REG_D = 1 << 3, /* decimal 8, binary 1000 */
  MODE_REG_W = 1 << 4, /* decimal 16, binary 10000 */
  MODE_REG_B = 3 << 3, /* decimal 24, binary 11000 */
  MODE_BASE_Q = 0,
  MODE_BASE_D = 1 << 3,
  MODE_BASE_W = 1 << 4,
  MODE_BASE_B = 3 << 3,
  MODE_INDEX_Q = 0,
  MODE_INDEX_D = 1 << 5, /* decimal 32, binary 100000 */
  MODE_INDEX_W = 1 << 6, /* decimal 64, binary 1000000 */
  MODE_INDEX_B = 3 << 5  /* decimal 96, binary 1100000 */
} AddressMode;

typedef union operand {
  struct opall {
    AddressMode mode;
  } all;
  struct opimm {
    AddressMode mode;
    uintmax_t val;
  } imm;
  struct opdir {
    AddressMode mode;
    intmax_t disp;
    const char *lab;
  } dir;
  struct opind {
    AddressMode mode;
    intmax_t disp;
    size_t num;
  } ind;
  struct opsca {
    AddressMode mode;
    intmax_t disp;
    size_t base;
    size_t index;
  } sca;
} Operand;
typedef struct opall Opall;

typedef struct instruction_data {
  Opcode opcode;
  unsigned int flags;
  Operand dest;
  Operand src;
} InstructionData;

/* rbx, rsp, rbp, r12-r15 are preserved across function calls in the System V
 * ABI; Microsoft's ABI additionally preserves rdi and rsi
 */
enum instruction_flag {
  NO_INSTR_FLAGS = 0,
  USE_REG = 1 << 1,  /* result must be placed in a register */
  WANT_ADDR = 1 << 2 /* result should be object address, not value */
};

const char OPCODES[][MAX_OPCODE_LENGTH] = {FOREACH_OPCODE(GENERATE_STRING)};

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and offset
 * is a signed 32-bit integer. Having unnecessary offsets and scales shouldn't
 * affect the validity of the code.
 *
 * stars indicate that field width is an argument
 * TODO(Robert): make field width an argument
 */
static const char INDEX_FMT[] = "[%s+%s*%lu+%i]";
static const char OFFSET_FMT[] = "[%s+%i]";
static const char INDIRECT_FMT[] = "[%s]";
static const char VREG_FMT[] = "vr%lu%c";
static const char BINOP_FMT[] = "%8s%8s %16s, %s\n";
static const char UNOP_FMT[] = "%8s%8s %16s\n";
static const char NULLOP_FMT[] = "%8s%8s\n";
static const char LABEL_FMT[] = "%s: \n";
static const char COND_FMT[] = ".C%lu";
static const char END_FMT[] = ".E%lu";
static const char STMT_FMT[] = ".S%lu";
static const char LOOP_FMT[] = ".L%lu";
static const char BOOL_FMT[] = ".B%lu";
static const char SECTION_FMT[] = ".section %s\n";
static const char EMPTY_FMT[] = "";

/* Microsoft x64 calling convention flips these two numbers */
static const size_t VOLATILE_COUNT = 9;
static const size_t NONVOLATILE_COUNT = 7;
static const size_t VOLATILE_START = 0;
static const size_t NONVOLATILE_START = VOLATILE_COUNT;
static const size_t STACK_POINTER_VREG = VOLATILE_COUNT;
static const size_t RETURN_VREG = 0;
static const char STACK_POINTER_STRING[] = "vr9q";

static size_t branch_count = 0;
static size_t vreg_count = 0;
static size_t stack_window = 0;
static char current_label[MAX_LABEL_LENGTH];

static LinkedList *text_section;
static LinkedList *data_section;
static LinkedList *bss_section;

/* the function that mallocs the InstructionData is also responsible
 * for pushing the InstructionData onto its respective stack
 *
 * if the function called to populate the InstructionData fails, it should not
 * be pushed onto the stack, and ideally should not modify the InstructionData
 * until it cannot/should not fail
 */
static char *translate_stride(ASTree *index, ASTree *memblock);
static char *translate_reg_type(ASTree *);
static char *translate_type(void *type, int flags);
static int translate_stmt(ASTree *stmt, CompilerState *state);
static int translate_block(ASTree *block, CompilerState *state);
static int translate_expr(ASTree *tree, CompilerState *state,
                          InstructionData *data, unsigned int flags);

/* assigns space for a symbol given an existing offset. inserts padding as
 * needed and returns the sum of the previous offset, the padding and the width
 * of the symbol
 */
size_t assign_space(SymbolValue *symval, size_t offset) {
  size_t alignment = typespec_get_alignment(&symval->type);
  size_t width = typespec_get_width(&symval->type);
  size_t padding = alignment - (offset % alignment);
  if (padding != alignment) offset += padding;
  symval->offset = offset;
  offset += width;
  return offset;
}

/* TODO(Robert): use flags to assign specific numbers for opcodes which
 * use specific registers as their source or destination to make register
 * allocation easier
 */
void assign_vreg(TypeSpec *type, Operand *operand, size_t vreg_num) {
  assert(type->base != TYPE_VOID);
  size_t width = typespec_get_width(type);
  assert(width == X64_SIZEOF_LONG || width == X64_SIZEOF_INT ||
         width == X64_SIZEOF_SHORT || width == X64_SIZEOF_CHAR);
  operand->ind.num = vreg_num;
  switch (type->width) {
    case X64_SIZEOF_LONG:
      return;
    case X64_SIZEOF_INT:
      operand->ind.mode |= MODE_REG_D;
      return;
    case X64_SIZEOF_SHORT:
      operand->ind.mode |= MODE_REG_W;
      return;
    case X64_SIZEOF_CHAR:
      operand->ind.mode |= MODE_REG_B;
      return;
  }
}

void resolve_object(CompilerState *state, Operand *operand, const char *ident) {
  SymbolValue *symval = NULL;
  state_get_symbol(state, ident, strlen(ident), &symval);
  assert(symval != NULL);
  if (symval->reg == 0) {
    operand->dir.mode = MODE_DIRECT;
    operand->dir.lab = ident;
    operand->dir.disp = symval->offset;
  } else {
    operand->ind.mode = MODE_INDIRECT;
    operand->ind.num = symval->reg;
    operand->ind.disp = symval->offset;
  }
}

void save_registers(size_t start, size_t count) {
  size_t i;
  for (i = 0; i < count; ++i) {
    DEBUGS('g', "Saving register %lu to stack", start + i);
    InstructionData *data = calloc(1, sizeof(InstructionData));
    assert(data != NULL);
    data->opcode = OP_PUSH;
    assign_vreg((TypeSpec *)&SPEC_ULONG, &data->dest, start + i);
    assert(!llist_push_back(text_section, data));
  }
}

void restore_registers(size_t start, size_t count) {
  size_t i;
  for (i = 1; i <= count; ++i) {
    DEBUGS('g', "Restoring register %lu from stack", start + (count - i));
    InstructionData *data = calloc(1, sizeof(InstructionData));
    assert(data != NULL);
    data->opcode = OP_POP;
    assign_vreg((TypeSpec *)&SPEC_ULONG, &data->dest, start + (count - i));
    assert(!llist_push_back(text_section, data));
  }
}

int translate_ident(ASTree *ident, CompilerState *state, InstructionData *data,
                    unsigned int flags) {
  int status =
      resolve_object(state, ident->lexinfo, data->src_operand, INDIRECT_FMT);
  if (status) return status;
  const TypeSpec *ident_type = ident->type;
  AuxSpec *ident_aux = llist_back(&ident_type->auxspecs);
  if (flags & WANT_ADDR || (ident_aux && (ident_aux->aux == AUX_FUNCTION ||
                                          ident_aux->aux == AUX_ARRAY))) {
    data->opcode = OPCODES[OP_LEA];
    /* TODO(Robert): define pointer type constant */
    int status = assign_vreg(&SPEC_ULONG, data->dest_operand, vreg_count++);
    if (status) return status;
  } else {
    data->opcode = OPCODES[OP_MOV];
    int status = assign_vreg(ident->type, data->dest_operand, vreg_count++);
    if (status) return status;
  }
  return 0;
}

/* NOTE: on x64, most operations that write to the lower 32 bits of a
 * register will zero the upper 32 bits. However, I will be zero- and
 * sign-extensions whenever the conversion requires it, just to be safe.
 *
 * any signed int -> any wider unsigned int: movzx
 * any signed int -> any wider signed int: movsx
 * any unsigned int -> any wider int: movzx
 * any int -> any narrower int: simple mov
 * any int -> any int of same width: nop
 */
int translate_conversion(ASTree *operator, CompilerState * state,
                         InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating conversion");

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  ASTree *converted_expr = NULL;
  if (astree_count(operator) == 1) {
    converted_expr = astree_get(operator, 0);
  } else if (astree_count(operator) == 2) {
    converted_expr = astree_get(operator, 1);
  } else if (astree_count(operator) == 3) {
    converted_expr = astree_get(operator, 2);
  } else {
    fprintf(
        stderr,
        "ERROR: unable to perform conversion on node which has %lu children\n",
        astree_count(operator));
    return -1;
  }

  int status = translate_expr(converted_expr, state, src_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);
  status = assign_vreg(operator->type, data->dest_operand, vreg_count++);
  if (status) return status;

  const TypeSpec *target_type = operator->type;
  const TypeSpec *source_type = converted_expr->type;
  AuxSpec *source_aux = llist_back(&source_type->auxspecs);

  if (source_aux &&
      (source_aux->aux == AUX_ARRAY || source_aux->aux == AUX_FUNCTION)) {
    /* functions and arrays have special conversion rules */
    data->opcode = OPCODES[OP_MOV];
  } else if (source_type->width > target_type->width) {
    data->opcode = OPCODES[OP_MOV];
  } else if (source_type->width == target_type->width) {
    data->opcode = OPCODES[OP_NOP];
  } else if (source_type->base == TYPE_SIGNED) {
    if (target_type->base == TYPE_SIGNED) {
      data->opcode = OPCODES[OP_MOVSX];
    } else if (target_type->base == TYPE_UNSIGNED) {
      data->opcode = OPCODES[OP_MOVZX];
    }
  } else if (source_type->base == TYPE_UNSIGNED) {
    data->opcode = OPCODES[OP_MOVZX];
  } else {
    fprintf(stderr, "ERROR: unable to determine conversion\n");
    return -1;
  }

  return 0;
}

int translate_intcon(ASTree *constant, InstructionData *data,
                     unsigned int flags) {
  DEBUGS('g', "Translating integer constant");
  if (flags & USE_REG) {
    int status = assign_vreg(constant->type, data->dest_operand, vreg_count++);
    if (status) return status;
    strcpy(data->src_operand, constant->lexinfo);
    data->opcode = OPCODES[OP_MOV];
  } else {
    /* result does not need to be in a register */
    strcpy(data->dest_operand, constant->lexinfo);
  }
  return 0;
}

/* Two classes of operators whose result is a boolean:
 * - comparison: >, <, >=, <=, ==, !=
 * - logical: &&, ||, !
 *
 * logical NOT does the same thing as the conversion from an arbitrary value
 * to a boolean except instead of using SETNZ it does SETZ
 */
int translate_logical_not(ASTree *tree, CompilerState *state,
                          InstructionData *data) {
  /* TODO(Robert): add attribute indicating when the result of an expression
   * is boolean so that we can skip doing all of this if we don't need to, or
   * maybe even add a whole boolean type, though that may be overkill
   */

  /* evaluate operand */
  InstructionData *tree_data = calloc(1, sizeof(InstructionData));
  int status = translate_expr(tree, state, tree_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, tree_data);

  /* TEST operand with itself */
  InstructionData *test_data = calloc(1, sizeof(InstructionData));
  test_data->opcode = OPCODES[OP_TEST];
  strcpy(test_data->dest_operand, tree_data->dest_operand);
  strcpy(test_data->src_operand, tree_data->dest_operand);
  llist_push_back(text_section, test_data);

  data->opcode = OPCODES[OP_SETZ];
  return assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
}

int translate_logical(ASTree *operator, CompilerState * state,
                      InstructionData *data, InstructionEnum num,
                      unsigned int flags) {
  /* create label used to skip second operand */
  InstructionData *label_data = calloc(1, sizeof(InstructionData));
  sprintf(label_data->label, BOOL_FMT, branch_count++);

  /* test first operand; jump on false for && and true for || */
  InstructionData *first_data = calloc(1, sizeof(InstructionData));
  int status =
      translate_expr(astree_get(operator, 0), state, first_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, first_data);

  InstructionData *test_first_data = calloc(1, sizeof(InstructionData));
  test_first_data->opcode = OPCODES[OP_TEST];
  strcpy(test_first_data->dest_operand, first_data->dest_operand);
  strcpy(test_first_data->src_operand, first_data->dest_operand);
  llist_push_back(text_section, test_first_data);

  InstructionData *jmp_first_data = calloc(1, sizeof(InstructionData));
  if (operator->symbol == TOK_AND)
    jmp_first_data->opcode = OPCODES[OP_JZ];
  else if (operator->symbol == TOK_OR)
    jmp_first_data->opcode = OPCODES[OP_JNZ];
  strcpy(jmp_first_data->dest_operand, label_data->label);
  llist_push_back(text_section, jmp_first_data);

  /* test second operand; no need to jump afterwards */
  InstructionData *second_data = calloc(1, sizeof(InstructionData));
  status = translate_expr(astree_get(operator, 1), state, second_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, second_data);

  InstructionData *test_second_data = calloc(1, sizeof(InstructionData));
  test_second_data->opcode = OPCODES[OP_TEST];
  strcpy(test_second_data->dest_operand, second_data->dest_operand);
  strcpy(test_second_data->src_operand, second_data->dest_operand);
  llist_push_back(text_section, test_second_data);

  /* write label used to skip second operand */
  llist_push_back(text_section, label_data);

  /* result will always be the truth value of the last evaluated expression */
  data->opcode = OPCODES[OP_SETNZ];
  return assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
}

/* TODO(Robert): check location of result of first subexpression, and require
 * second subexpression to place result in a register if the first was not
 */
int translate_comparison(ASTree *operator, CompilerState * state,
                         InstructionData *data, InstructionEnum num,
                         unsigned int flags) {
  /* CMP operands, then SETG/SETGE/SETL/SETLE/SETE/SETNE */
  InstructionData *first_data = calloc(1, sizeof(*first_data));
  int status = translate_expr(astree_get(operator, 0), state, first_data,
                              NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, first_data);

  InstructionData *second_data = calloc(1, sizeof(*second_data));
  status = translate_expr(astree_get(operator, 1), state, second_data,
                          NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, second_data);

  InstructionData *cmp_data = calloc(1, sizeof(InstructionData));
  cmp_data->opcode = OPCODES[OP_CMP];
  strcpy(cmp_data->dest_operand, first_data->dest_operand);
  strcpy(cmp_data->src_operand, second_data->dest_operand);
  llist_push_back(text_section, cmp_data);

  data->opcode = OPCODES[num];
  status = assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
  return status;
}

int translate_indirection(ASTree *indirection, CompilerState *state,
                          InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating indirection operation.");
  InstructionData *src_data = calloc(1, sizeof(*src_data));
  int status =
      translate_expr(astree_get(indirection, 0), state, src_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, src_data);

  sprintf(data->src_operand, INDIRECT_FMT, src_data->dest_operand);
  status = assign_vreg(indirection->type, data->dest_operand, vreg_count++);
  if (status) return status;
  data->opcode = OPCODES[OP_MOV];
  return 0;
}

int translate_addrof(ASTree *addrof, CompilerState *state,
                     InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating address operation.");
  /* TODO(Robert): handle other types of lvalues, like struct and union
   * members
   */
  return translate_expr(astree_get(addrof, 0), state, data, WANT_ADDR);
}

int translate_inc_dec(ASTree *operator, CompilerState * state,
                      InstructionData *data, InstructionEnum num,
                      unsigned int flags) {
  DEBUGS('g', "Translating increment/decrement: %s", OPCODES[num]);
  InstructionData *mov_data = NULL;
  InstructionData *inc_dec_data = NULL;
  InstructionData *to_push_data = NULL;

  /* change which instruction gets pushed first depending on pofx vs prfx */
  if (operator->symbol == TOK_INC || operator->symbol == TOK_DEC) {
    inc_dec_data = calloc(1, sizeof(*mov_data));
    mov_data = data;
    to_push_data = inc_dec_data;
  } else {
    mov_data = calloc(1, sizeof(*mov_data));
    inc_dec_data = data;
    to_push_data = mov_data;
  }

  int status =
      translate_expr(astree_get(operator, 0), state, mov_data, NO_INSTR_FLAGS);
  if (status) return status;

  inc_dec_data->opcode = OPCODES[num];
  strcpy(inc_dec_data->dest_operand, mov_data->src_operand);

  llist_push_back(text_section, to_push_data);
  return 0;
}

int translate_unop(ASTree *operator, CompilerState * state,
                   InstructionData *data, InstructionEnum num,
                   unsigned int flags) {
  DEBUGS('g', "Translating unary operation: %s", OPCODES[num]);
  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  /* put value in register so that it is not modified in place */
  int status =
      translate_expr(astree_get(operator, 0), state, dest_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  data->opcode = OPCODES[num];
  strcpy(data->dest_operand, dest_data->dest_operand);
  return 0;
}

int translate_binop(ASTree *operator, CompilerState * state,
                    InstructionData *data, InstructionEnum num,
                    unsigned int flags) {
  DEBUGS('g', "Translating binary operation: %s", OPCODES[num]);
  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  int status =
      translate_expr(astree_get(operator, 0), state, dest_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  status =
      translate_expr(astree_get(operator, 1), state, src_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, src_data);

  strcpy(data->dest_operand, dest_data->dest_operand);
  strcpy(data->src_operand, src_data->dest_operand);
  data->opcode = OPCODES[num];
  return 0;
}

int translate_mul_div_mod(ASTree *operator, CompilerState * state,
                          InstructionData *data, InstructionEnum num,
                          unsigned int flags) {
  /* TODO(Robert): designate vregs for quotient/lo bits and remainder/hi bits
   */
  if (operator->symbol == '%') {
    /* return remainder instead of quotient */
    InstructionData *remainder_data = calloc(1, sizeof(*remainder_data));
    int status = translate_binop(operator, state, remainder_data, num, flags);
    if (status) return status;
    llist_push_back(text_section, remainder_data);
    data->opcode = OPCODES[OP_NOP];
    return assign_vreg(operator->type, data->dest_operand, vreg_count++);
  } else {
    int status = translate_binop(operator, state, data, num, flags);
    if (status) return status;
    /* use up another vreg for the remainder/high-order bits */
    ++vreg_count;
    return 0;
  }
}

/* TODO(Robert): this may be unnecessary */
int translate_cast(ASTree *cast, InstructionData *data) {
  ASTree *casted_expr = NULL;
  if (astree_count(cast) == 1)
    casted_expr = astree_get(cast, 0);
  else
    casted_expr = astree_get(cast, 1);
  return 0;
}

int translate_assignment(ASTree *assignment, CompilerState *state,
                         InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating assignment");

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  /* place result in register since destination will be a memory location */
  int status =
      translate_expr(astree_get(assignment, 1), state, src_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, src_data);

  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  status = translate_expr(astree_get(assignment, 0), state, dest_data,
                          NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  strcpy(data->dest_operand, dest_data->dest_operand);
  strcpy(data->src_operand, src_data->dest_operand);
  data->opcode = OPCODES[OP_MOV];

  return 0;
}

int translate_subscript(ASTree *subscript, CompilerState *state,
                        InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the offset and
   * scale addressing mode can be used
   */
  InstructionData *pointer_data = calloc(1, sizeof(*pointer_data));
  int status =
      translate_expr(astree_get(subscript, 0), state, pointer_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, pointer_data);

  InstructionData *index_data = calloc(1, sizeof(*index_data));
  status = translate_expr(astree_get(subscript, 1), state, index_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, index_data);

  data->opcode = OPCODES[OP_MOV];
  sprintf(data->src_operand, INDEX_FMT, pointer_data->dest_operand,
          index_data->dest_operand, subscript->type->width, 0);
  return assign_vreg(subscript->type, data->dest_operand, vreg_count++);
}

/* When fetching a struct member, we must be able to return either the
 * location or value of the symbol, depending on the presence of WANT_ADDR.
 *
 * Because of the way structures are used, it may make the most sense to have
 * expressions of structure value always result in the address of the
 * structure.
 */
int translate_reference(ASTree *reference, CompilerState *state,
                        InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating reference operator");
  ASTree *struct_ = astree_get(reference, 0);
  InstructionData *struct_data = calloc(1, sizeof(*struct_data));
  unsigned int struct_flags =
      reference->symbol == TOK_ARROW ? USE_REG : USE_REG | WANT_ADDR;
  int status = translate_expr(struct_, state, struct_data, struct_flags);
  if (status) return status;
  llist_push_back(text_section, struct_data);

  const AuxSpec *struct_aux = reference->symbol == TOK_ARROW
                                  ? llist_get(&struct_->type->auxspecs, 1)
                                  : llist_front(&struct_->type->auxspecs);
  if (struct_aux->aux == AUX_STRUCT) {
    if (flags & WANT_ADDR)
      data->opcode = OPCODES[OP_LEA];
    else
      data->opcode = OPCODES[OP_MOV];
    SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
    const char *member_name = astree_get(reference, 1)->lexinfo;
    size_t member_name_len = strlen(member_name);
    SymbolValue *member_symbol =
        symbol_table_get(member_table, member_name, member_name_len);
    char temp[MAX_OPERAND_LENGTH];
    sprintf(temp, member_symbol->obj_loc, struct_data->dest_operand);
    sprintf(data->src_operand, INDIRECT_FMT, temp);
    return assign_vreg(reference->type, data->dest_operand, vreg_count++);
  } else if (flags & WANT_ADDR) {
    /* use nop to communicate address to parent expression */
    data->opcode = OPCODES[OP_NOP];
    strcpy(data->dest_operand, struct_data->dest_operand);
    return 0;
  } else {
    data->opcode = OPCODES[OP_MOV];
    sprintf(data->src_operand, INDIRECT_FMT, struct_data->dest_operand);
    return assign_vreg(reference->type, data->dest_operand, vreg_count++);
  }
}

int translate_call(ASTree *call, CompilerState *state, InstructionData *data,
                   unsigned int flags) {
  DEBUGS('g', "Translating function call");

  size_t i;
  for (i = 1; i < astree_count(call); ++i) {
    DEBUGS('g', "Translating parameter %i", i);
    /* compute parameter */
    ASTree *param = astree_get(call, i);
    InstructionData *param_data = calloc(1, sizeof(*param_data));
    int status = translate_expr(param, state, param_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, param_data);

    /* TODO(Robert): temporarily restrict number of arguments to 4 until I
     * have implemented passing subroutine parameters on the stack
     */
    /* mov parameter to argument register */
    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->opcode = OPCODES[OP_MOV];
    status = assign_vreg(param->type, mov_data->dest_operand, i);
    if (status) return status;
    strcpy(mov_data->src_operand, param_data->dest_operand);
    llist_push_back(text_section, mov_data);
  }

  int status = save_registers(VOLATILE_START, VOLATILE_COUNT);
  if (status) return status;

  /* compute function pointer value */
  InstructionData *function_data = calloc(1, sizeof(InstructionData));
  ASTree *function_expr = astree_get(call, 0);
  status = translate_expr(function_expr, state, function_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, function_data);

  InstructionData *call_data = calloc(1, sizeof(*call_data));
  call_data->opcode = OPCODES[OP_CALL];
  strcpy(call_data->dest_operand, function_data->dest_operand);
  llist_push_back(text_section, call_data);

  /* mov result to any other register if return type isn't void */
  if (call->type->base != TYPE_VOID) {
    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->opcode = OPCODES[OP_MOV];
    int status = assign_vreg(call->type, mov_data->src_operand, RETURN_VREG);
    if (status) return status;
    status = assign_vreg(call->type, mov_data->dest_operand, vreg_count++);
    if (status) return status;
    llist_push_back(text_section, mov_data);
    strcpy(data->dest_operand, mov_data->dest_operand);
  }

  /* only give the parent recursive call the result register, if applicable */
  data->opcode = OPCODES[OP_NOP];

  return restore_registers(VOLATILE_START, VOLATILE_COUNT);
}

int translate_param(ASTree *param, CompilerState *state,
                    InstructionData *data) {
  DEBUGS('g', "Translating parameter");
  ASTree *declarator = astree_get(param, 1);
  ASTree *param_ident = declarator;
  int status = assign_vreg(param_ident->type, data->src_operand, vreg_count++);
  if (status) return status;
  SymbolValue *symval = NULL;
  int is_param = state_get_symbol(state, param_ident->lexinfo,
                                  strlen(param_ident->lexinfo), &symval);
  if (symval == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n",
            param_ident->lexinfo);
    return -1;
  } else if (!is_param) {
    fprintf(stderr, "ERROR: resolved symbol %s was not a function parameter.\n",
            param_ident->lexinfo);
  }
  status = assign_space(symval, STACK_POINTER_STRING);
  if (status) return status;
  status = resolve_object(state, param_ident->lexinfo, data->dest_operand,
                          INDIRECT_FMT);
  if (status) return status;
  data->opcode = OPCODES[OP_MOV];
  return 0;
}

int translate_list_initialization(ASTree *declarator, ASTree *init_list,
                                  CompilerState *state) {
  DEBUGS('g', "Transating struct initiazation by initializer list");
  InstructionData *struct_data = calloc(1, sizeof(InstructionData));
  struct_data->opcode = OPCODES[OP_LEA];
  ASTree *struct_ident = declarator;
  int status = resolve_object(state, struct_ident->lexinfo,
                              struct_data->dest_operand, INDIRECT_FMT);
  if (status) return status;
  const TypeSpec *struct_type = declarator->type;
  status = assign_vreg(&SPEC_ULONG, struct_data->dest_operand, vreg_count++);
  if (status) return status;
  llist_push_back(text_section, struct_data);

  AuxSpec *struct_aux = llist_back(&struct_type->auxspecs);
  const LinkedList *member_symbols =
      &struct_aux->data.tag.val->data.members.in_order;
  size_t i;
  for (i = 0; i < llist_size(member_symbols); ++i) {
    if (i < astree_count(init_list)) {
      ASTree *initializer = astree_get(init_list, i);
      InstructionData *initializer_data = calloc(1, sizeof(InstructionData));
      int status =
          translate_expr(initializer, state, initializer_data, USE_REG);
      if (status) return status;
      llist_push_back(text_section, initializer_data);

      InstructionData *assignment_data = calloc(1, sizeof(InstructionData));
      assignment_data->opcode = OPCODES[OP_MOV];
      strcpy(assignment_data->src_operand, initializer_data->dest_operand);
      SymbolValue *member_symbol = llist_get(member_symbols, i);
      char temp[MAX_OPERAND_LENGTH];
      sprintf(temp, member_symbol->obj_loc, struct_data->dest_operand);
      sprintf(assignment_data->dest_operand, INDIRECT_FMT, temp);
      llist_push_back(text_section, assignment_data);
    } else if (struct_aux->aux == AUX_UNION) {
      /* do not initialize other members of the union */
      break;
    } else {
      /* TODO(Robert): initialize unset struct members and array elements to
       * zero if the object is static
       */
      break;
    }
  }
  return 0;
}

int translate_local_decl(ASTree *declaration, CompilerState *state,
                         InstructionData *data) {
  DEBUGS('g', "Translating local declaration");
  size_t i;
  /* skip typespec list */
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *declarator = astree_get(declaration, i);
    ASTree *ident = declarator;
    SymbolValue *symval = NULL;
    state_get_symbol(state, (char *)ident->lexinfo, strlen(ident->lexinfo),
                     &symval);
    if (symval == NULL) {
      fprintf(stderr, "ERROR: unable to resolve symbol %s\n", ident->lexinfo);
      return -1;
    }
    int status = assign_space(symval, STACK_POINTER_STRING);
    if (status) return status;

    /* check if next child is an initializer */
    if (i < astree_count(declaration) - 1) {
      ASTree *next_child = astree_get(declaration, i + 1);
      if (next_child->symbol == TOK_INIT_LIST) {
        int status =
            translate_list_initialization(declarator, next_child, state);
        if (status) return status;
        ++i;
      } else if (next_child->symbol !=
                 TOK_IDENT) { /* TODO(Robert): this used to be TOK_DECLARATOR;
                                 check that it is correct */
        int status = resolve_object(state, ident->lexinfo, data->dest_operand,
                                    INDIRECT_FMT);
        if (status) return status;
        InstructionData *value_data = calloc(1, sizeof(*value_data));
        status = translate_expr(next_child, state, value_data, USE_REG);
        if (status) return status;
        llist_push_back(text_section, value_data);

        strcpy(data->src_operand, value_data->dest_operand);
        data->opcode = OPCODES[OP_MOV];
        ++i;
      }
    }
  }
  return 0;
}

static int translate_expr(ASTree *tree, CompilerState *state,
                          InstructionData *out, unsigned int flags) {
  int status = 0;

  /* TODO(Robert): make a mapping from symbols to OPCODES so that most
   * of the case statements can be collapsed together
   */
  switch (tree->symbol) {
    /* arithmetic operators */
    case '+':
      status = translate_binop(tree, state, out, OP_ADD, flags);
      break;
    case '-':
      status = translate_binop(tree, state, out, OP_SUB, flags);
      break;
    case '*':
      if (astree_get(tree, 0)->type->base == TYPE_SIGNED)
        status = translate_mul_div_mod(tree, state, out, OP_IMUL, flags);
      else
        status = translate_mul_div_mod(tree, state, out, OP_MUL, flags);
      break;
    case '/':
    case '%':
      if (astree_get(tree, 0)->type->base == TYPE_SIGNED)
        status = translate_mul_div_mod(tree, state, out, OP_IDIV, flags);
      else
        status = translate_mul_div_mod(tree, state, out, OP_DIV, flags);
      break;
    case TOK_INC:
    case TOK_POST_INC:
      status = translate_inc_dec(tree, state, out, OP_INC, flags);
      break;
    case TOK_DEC:
    case TOK_POST_DEC:
      status = translate_inc_dec(tree, state, out, OP_DEC, flags);
      break;
    case TOK_NEG:
      status = translate_unop(tree, state, out, OP_NEG, flags);
      break;
    case TOK_POS:
      status = translate_conversion(tree, state, out, flags);
      break;
    /* bitwise operators */
    case '&':
      status = translate_binop(tree, state, out, OP_AND, flags);
      break;
    case '|':
      status = translate_binop(tree, state, out, OP_OR, flags);
      break;
    case '^':
      status = translate_binop(tree, state, out, OP_XOR, flags);
      break;
    case '~':
      status = translate_unop(tree, state, out, OP_NOT, flags);
      break;
    /* shifts */
    case TOK_SHL:
      status = translate_binop(tree, state, out, OP_SHL, flags);
      break;
    case TOK_SHR:
      if (astree_get(tree, 0)->type->base == TYPE_SIGNED)
        status = translate_binop(tree, state, out, OP_SAR, flags);
      else
        status = translate_binop(tree, state, out, OP_SHR, flags);
      break;
    /* comparison operators */
    case '>':
      status = translate_comparison(tree, state, out, OP_SETG, flags);
      break;
    case TOK_GE:
      status = translate_comparison(tree, state, out, OP_SETGE, flags);
      break;
    case '<':
      status = translate_comparison(tree, state, out, OP_SETL, flags);
      break;
    case TOK_LE:
      status = translate_comparison(tree, state, out, OP_SETLE, flags);
      break;
    case TOK_EQ:
      status = translate_comparison(tree, state, out, OP_SETE, flags);
      break;
    case TOK_NE:
      status = translate_comparison(tree, state, out, OP_SETNE, flags);
      break;
    /* logical operators */
    case '!':
      status = translate_logical_not(astree_get(tree, 0), state, out);
      break;
    case TOK_AND:
      status = translate_logical(tree, state, out, OP_AND, flags);
      break;
    case TOK_OR:
      status = translate_logical(tree, state, out, OP_OR, flags);
      break;
    /* constants */
    case TOK_INTCON:
      status = translate_intcon(tree, out, flags);
      break;
    case TOK_CHARCON:
      break;
    /* miscellaneous */
    case TOK_ADDROF:
      status = translate_addrof(tree, state, out, flags);
      break;
    case TOK_INDIRECTION:
      status = translate_indirection(tree, state, out, flags);
      break;
    case TOK_IDENT:
      status = translate_ident(tree, state, out, flags);
      break;
    case '=':
      status = translate_assignment(tree, state, out, flags);
      break;
    case TOK_CAST:
      status = translate_conversion(tree, state, out, flags);
      break;
    case TOK_CALL:
      status = translate_call(tree, state, out, flags);
      break;
    case TOK_SUBSCRIPT:
      status = translate_subscript(tree, state, out, flags);
      break;
    case '.':
    case TOK_ARROW:
      status = translate_reference(tree, state, out, flags);
      break;
    default:
      fprintf(stderr, "ERROR: Unimplemented token: %s, lexinfo: %s\n",
              parser_get_tname(tree->symbol), tree->lexinfo);
      status = -1;
      break;
  }
  return status;
}

static int translate_ifelse(ASTree *ifelse, CompilerState *state) {
  size_t current_branch = branch_count++;
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  int status = translate_expr(astree_get(ifelse, 0), state, cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is zero and jump if it is */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->opcode = OPCODES[OP_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);

  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->opcode = OPCODES[OP_JZ];
  sprintf(test_jmp_data->dest_operand, END_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  /* translate if body */
  status = translate_stmt(astree_get(ifelse, 1), state);
  if (status) return status;
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  /* translate else body if present */
  if (astree_count(ifelse) == 3) {
    translate_stmt(astree_get(ifelse, 2), state);
  }
  return 0;
}

static int translate_switch(ASTree *switch_, CompilerState *state) {
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  int status =
      translate_expr(astree_get(switch_, 0), state, cond_data, USE_REG);
  if (status) return status;

  size_t current_branch = branch_count++;
  JumpEntry jump_entry;
  jump_entry.type = JUMP_SWITCH;
  jump_entry.data.switch_.next_case = current_branch;
  strcpy(jump_entry.data.switch_.control_register, cond_data->dest_operand);
  jump_entry.data.switch_.case_labels = malloc(sizeof(LinkedList));
  status = llist_init(jump_entry.data.switch_.case_labels, NULL, NULL);
  if (status) return status;
  sprintf(jump_entry.end_label, END_FMT, current_branch);
  status = state_push_jump(state, &jump_entry);

  status = translate_stmt(astree_get(switch_, 1), state);
  if (status) return status;

  status = state_pop_jump(state);
  if (status) return status;

  InstructionData *default_label = calloc(1, sizeof(*default_label));
  sprintf(default_label->label, COND_FMT, jump_entry.data.switch_.next_case);
  llist_push_back(text_section, default_label);

  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  return 0;
}

static int translate_while(ASTree *while_, CompilerState *state) {
  size_t current_branch = branch_count++;
  /* emit label at beginning of condition */
  InstructionData *cond_label = calloc(1, sizeof(*cond_label));
  sprintf(cond_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, cond_label);
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  int status = translate_expr(astree_get(while_, 0), state, cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is zero */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->opcode = OPCODES[OP_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);
  /* emit jump to end of loop */
  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->opcode = OPCODES[OP_JZ];
  sprintf(test_jmp_data->dest_operand, END_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  /* emit label at beginning of body */
  InstructionData *body_label = calloc(1, sizeof(*body_label));
  sprintf(body_label->label, STMT_FMT, current_branch);
  llist_push_back(text_section, body_label);
  /* create and push jump stack entry */
  JumpEntry jump_entry;
  jump_entry.type = JUMP_ITERATION;
  sprintf(jump_entry.data.iteration.cond_label, COND_FMT, current_branch);
  sprintf(jump_entry.data.iteration.stmt_label, STMT_FMT, current_branch);
  sprintf(jump_entry.end_label, END_FMT, current_branch);
  status = state_push_jump(state, &jump_entry);
  /* translate while body */
  status = translate_stmt(astree_get(while_, 1), state);
  if (status) return status;
  /* pop jump stack entry */
  status = state_pop_jump(state);
  if (status) return status;
  /* emit jump to condition */
  InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
  cond_jmp_data->opcode = OPCODES[OP_JMP];
  sprintf(cond_jmp_data->dest_operand, COND_FMT, current_branch);
  llist_push_back(text_section, cond_jmp_data);
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  return 0;
}

static int translate_for(ASTree *for_, CompilerState *state) {
  size_t current_branch = branch_count++;
  ASTree *for_exprs = astree_get(for_, 0);
  /* translate initialization expression, if present */
  ASTree *init_expr = astree_get(for_exprs, 0);
  if (init_expr->symbol != ';') {
    InstructionData *init_data = calloc(1, sizeof(*init_data));
    int status = translate_expr(init_expr, state, init_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, init_data);
  }

  /* emit label at beginning of condition */
  InstructionData *cond_label = calloc(1, sizeof(*cond_label));
  sprintf(cond_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, cond_label);
  /* translate conditional expression, if present */
  ASTree *cond_expr = astree_get(for_exprs, 1);
  if (cond_expr->symbol != ';') {
    /* translate conditional expression */
    InstructionData *cond_data = calloc(1, sizeof(*cond_data));
    int status = translate_expr(cond_expr, state, cond_data, USE_REG);
    if (status) return status;
    llist_push_back(text_section, cond_data);
    /* check if condition is zero */
    InstructionData *test_data = calloc(1, sizeof(*test_data));
    test_data->opcode = OPCODES[OP_TEST];
    strcpy(test_data->dest_operand, cond_data->dest_operand);
    strcpy(test_data->src_operand, cond_data->dest_operand);
    llist_push_back(text_section, test_data);
    /* emit jump to end of loop */
    InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
    test_jmp_data->opcode = OPCODES[OP_JZ];
    sprintf(test_jmp_data->dest_operand, END_FMT, current_branch);
    llist_push_back(text_section, test_jmp_data);
  }

  /* emit label at beginning of body */
  InstructionData *body_label = calloc(1, sizeof(*body_label));
  sprintf(body_label->label, STMT_FMT, current_branch);
  llist_push_back(text_section, body_label);
  /* create and push jump stack entry */
  JumpEntry jump_entry;
  jump_entry.type = JUMP_ITERATION;
  sprintf(jump_entry.data.iteration.cond_label, COND_FMT, current_branch);
  sprintf(jump_entry.data.iteration.stmt_label, STMT_FMT, current_branch);
  sprintf(jump_entry.end_label, END_FMT, current_branch);
  int status = state_push_jump(state, &jump_entry);
  /* translate for body */
  status = translate_stmt(astree_get(for_, 1), state);
  if (status) return status;
  /* pop jump stack entry */
  status = state_pop_jump(state);
  if (status) return status;
  /* translate re-initialization, if present */
  ASTree *reinit_expr = astree_get(for_exprs, 2);
  if (reinit_expr->symbol != ';' && reinit_expr->symbol != ')') {
    InstructionData *reinit_data = calloc(1, sizeof(*reinit_data));
    int status =
        translate_expr(reinit_expr, state, reinit_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, reinit_data);
  }

  /* emit jump to condition */
  InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
  cond_jmp_data->opcode = OPCODES[OP_JMP];
  sprintf(cond_jmp_data->dest_operand, COND_FMT, current_branch);
  llist_push_back(text_section, cond_jmp_data);
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  return 0;
}

static int translate_do(ASTree *do_, CompilerState *state) {
  size_t current_branch = branch_count++;
  /* emit label at beginning of body */
  InstructionData *body_label = calloc(1, sizeof(*body_label));
  sprintf(body_label->label, STMT_FMT, current_branch);
  llist_push_back(text_section, body_label);
  /* create and push jump stack entry */
  JumpEntry jump_entry;
  jump_entry.type = JUMP_ITERATION;
  sprintf(jump_entry.data.iteration.cond_label, COND_FMT, current_branch);
  sprintf(jump_entry.data.iteration.stmt_label, STMT_FMT, current_branch);
  sprintf(jump_entry.end_label, END_FMT, current_branch);
  int status = state_push_jump(state, &jump_entry);
  /* translate body */
  status = translate_stmt(astree_get(do_, 0), state);
  if (status) return status;
  /* pop jump stack entry */
  status = state_pop_jump(state);
  if (status) return status;
  /* emit label at beginning of condition */
  InstructionData *cond_label = calloc(1, sizeof(*cond_label));
  sprintf(cond_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, cond_label);
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  status = translate_expr(astree_get(do_, 1), state, cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is one */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->opcode = OPCODES[OP_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);
  /* emit jump to beginning of body */
  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->opcode = OPCODES[OP_JNZ];
  sprintf(test_jmp_data->dest_operand, STMT_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  return 0;
}

static int translate_block(ASTree *block, CompilerState *state) {
  DEBUGS('g', "Translating compound statement");
  LinkedList *block_contents = &block->children;
  int status = state_push_table(state, block->symbol_table);
  if (status) return status;
  size_t i;
  for (i = 0; i < block_contents->size; ++i) {
    ASTree *stmt = llist_get(block_contents, i);
    int status = translate_stmt(stmt, state);
    if (status) return status;
  }
  return state_pop_table(state);
}

int translate_return(ASTree *ret, CompilerState *state, InstructionData *data) {
  DEBUGS('g', "Translating return statement");

  if (astree_count(ret) > 0) {
    InstructionData *value_data = calloc(1, sizeof(*value_data));
    int status =
        translate_expr(astree_get(ret, 0), state, value_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, value_data);

    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->opcode = OPCODES[OP_MOV];
    strcpy(mov_data->src_operand, value_data->dest_operand);

    SymbolValue *function_symval = state_get_function(state);
    const TypeSpec *function_spec = &function_symval->type;
    /* strip function */
    TypeSpec return_spec = SPEC_EMPTY;
    status = strip_aux_type(&return_spec, function_spec);
    if (status) return status;
    status = assign_vreg(&return_spec, mov_data->dest_operand, RETURN_VREG);
    if (status) return status;
    /* free typespec copies */
    typespec_destroy(&return_spec);

    llist_push_back(text_section, mov_data);
  }

  /* restore non-volatile registers */
  int status = restore_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  data->opcode = OPCODES[OP_RET];
  return 0;
}

static int translate_continue(ASTree *continue_, CompilerState *state) {
  if (llist_empty(&state->jump_stack)) {
    fprintf(stderr,
            "ERROR: continue statements are invalid outside of an "
            "iteration statement.\n");
    return -1;
  } else {
    /* emit jump to condition */
    InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
    cond_jmp_data->opcode = OPCODES[OP_JMP];
    JumpEntry *current_entry = state_get_iteration(state);
    if (current_entry == NULL) return -1;
    strcpy(cond_jmp_data->dest_operand,
           current_entry->data.iteration.cond_label);
    llist_push_back(text_section, cond_jmp_data);
    return 0;
  }
}

static int translate_break(ASTree *break_, CompilerState *state) {
  if (llist_empty(&state->jump_stack)) {
    fprintf(stderr,
            "ERROR: break statements are invalid outside of an "
            "iteration statement.\n");
    return -1;
  } else {
    /* emit jump to end of loop */
    InstructionData *end_jmp_data = calloc(1, sizeof(*end_jmp_data));
    end_jmp_data->opcode = OPCODES[OP_JMP];
    JumpEntry *current_entry = state_get_jump(state);
    if (current_entry == NULL) return -1;
    strcpy(end_jmp_data->dest_operand, current_entry->end_label);
    llist_push_back(text_section, end_jmp_data);
    return 0;
  }
}

static int translate_goto(ASTree *goto_, CompilerState *state) {
  ASTree *ident = astree_get(goto_, 0);
  const char *ident_str = ident->lexinfo;
  size_t ident_str_len = strlen(ident->lexinfo);
  LabelValue *labval = state_get_label(state, ident_str, ident_str_len);
  if (labval == NULL || !labval->is_defined) {
    fprintf(stderr, "ERROR: unable to resolve label %s.\n", ident_str);
    return -1;
  } else {
    /* emit jump to label */
    InstructionData *jmp_data = calloc(1, sizeof(*jmp_data));
    jmp_data->opcode = OPCODES[OP_JMP];
    strcpy(jmp_data->dest_operand, ident_str);
    llist_push_back(text_section, jmp_data);
    return 0;
  }
}

static int translate_label(ASTree *label, CompilerState *state) {
  ASTree *ident = astree_get(label, 0);
  const char *ident_str = ident->lexinfo;
  InstructionData *label_data = calloc(1, sizeof(*label_data));
  strcpy(label_data->label, ident_str);
  llist_push_back(text_section, label_data);
  return translate_stmt(astree_get(label, 1), state);
}

static int translate_case(ASTree *case_, CompilerState *state) {
  /* get location of control value and case number */
  JumpEntry *switch_entry = state_get_switch(state);
  if (switch_entry == NULL) {
    fprintf(stderr,
            "ERROR: case statements must be enclosed in a switch statement.\n");
    return -1;
  }
  /* reserve new case number for next case/default statement */
  size_t current_branch = switch_entry->data.switch_.next_case;
  switch_entry->data.switch_.next_case = branch_count++;
  /* emit case label */
  InstructionData *case_label = calloc(1, sizeof(*case_label));
  sprintf(case_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, case_label);
  /* evaluate case constant (should be optimized away later) */
  InstructionData *data = calloc(1, sizeof(*data));
  int status = translate_expr(astree_get(case_, 0), state, data, USE_REG);
  if (status) return status;
  /* compare constant to control value */
  InstructionData *cmp_data = calloc(1, sizeof(*cmp_data));
  cmp_data->opcode = OPCODES[OP_CMP];
  strcpy(cmp_data->dest_operand, data->dest_operand);
  strcpy(cmp_data->src_operand, switch_entry->data.switch_.control_register);
  llist_push_back(text_section, cmp_data);
  /* jump to next case statement if not equal */
  InstructionData *jmp_data = calloc(1, sizeof(*jmp_data));
  jmp_data->opcode = OPCODES[OP_JNE];
  sprintf(jmp_data->dest_operand, COND_FMT,
          switch_entry->data.switch_.next_case);
  llist_push_back(text_section, jmp_data);
  /* emit statement belonging to this case label */
  return translate_stmt(astree_get(case_, 1), state);
}

static int translate_default(ASTree *default_, CompilerState *state) {
  /* get location of case number */
  JumpEntry *switch_entry = state_get_switch(state);
  if (switch_entry == NULL) {
    fprintf(
        stderr,
        "ERROR: default statements must be enclosed in a switch statement.\n");
    return -1;
  }
  /* reserve new case number for the default label that is always emitted by the
   * switch translation routine, which is equivalent to the end label
   */
  size_t current_branch = switch_entry->data.switch_.next_case;
  switch_entry->data.switch_.next_case = branch_count++;
  /* emit case label */
  InstructionData *case_label = calloc(1, sizeof(*case_label));
  sprintf(case_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, case_label);
  /* emit statement belonging to this case label */
  return translate_stmt(astree_get(default_, 0), state);
}

static int translate_stmt(ASTree *stmt, CompilerState *state) {
  InstructionData *data;
  int status = 0;

  /* TODO(Robert): add badlib function to verify that a data structure is
   * valid
   */
  switch (stmt->symbol) {
    case TOK_BLOCK:
      status = translate_block(stmt, state);
      break;
    case TOK_RETURN:
      data = calloc(1, sizeof(*data));
      status = translate_return(stmt, state, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    case TOK_WHILE:
      status = translate_while(stmt, state);
      break;
    case TOK_DO:
      status = translate_do(stmt, state);
      break;
    case TOK_FOR:
      status = translate_for(stmt, state);
      break;
    case TOK_IF:
      status = translate_ifelse(stmt, state);
      break;
    case TOK_SWITCH:
      status = translate_switch(stmt, state);
      break;
    case TOK_CONTINUE:
      status = translate_continue(stmt, state);
      break;
    case TOK_BREAK:
      status = translate_break(stmt, state);
      break;
    case TOK_GOTO:
      status = translate_goto(stmt, state);
      break;
    case TOK_LABEL:
      status = translate_label(stmt, state);
      break;
    case TOK_CASE:
      status = translate_case(stmt, state);
      break;
    case TOK_DEFAULT:
      status = translate_default(stmt, state);
      break;
    case TOK_DECLARATION:
      data = calloc(1, sizeof(*data));
      status = translate_local_decl(stmt, state, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    default:
      data = calloc(1, sizeof(*data));
      status = translate_expr(stmt, state, data, 0);
      if (status) break;
      llist_push_back(text_section, data);
      break;
  }

  return status;
}

int translate_global_decl(ASTree *declaration, InstructionData *data) {
  DEBUGS('g', "Translating global declaration");
  size_t i;
  /* skip typespec list */
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *declarator = astree_get(declaration, i);
    ASTree *ident = declarator;
    sprintf(data->label, "%s:", ident->lexinfo);

    /* TODO(Robert): indicate somehow in the tree or symbol table that this
     * is a global variable and should be referenced by its name, as
     * opposed to a stack offset
     */
    /* TODO(Robert): figure out how to initialize data for a struct or any
     * type wider than a quadword
     */
    /* TODO(Robert): have the compiler evaluate compile-time constants
     */

    /* check if next child is an initializer */
    if (i < astree_count(declaration) - 1 &&
        astree_get(declaration, i + 1)->symbol !=
            TOK_IDENT) { /* TODO(Robert): this used to be TOK_DECLARATOR */
      /* put in data section */
      ASTree *init_value = astree_get(declarator, ++i);
      strcpy(data->dest_operand, "COMPILE-TIME CONSTANT");

      switch (ident->type->width) {
        case X64_SIZEOF_LONG:
          data->opcode = OPCODES[OP_DQ];
          break;
        case X64_SIZEOF_INT:
          data->opcode = OPCODES[OP_DD];
          break;
        case X64_SIZEOF_SHORT:
          data->opcode = OPCODES[OP_DW];
          break;
        case X64_SIZEOF_CHAR:
          data->opcode = OPCODES[OP_DB];
          break;
        default:
          fprintf(stderr,
                  "ERROR: unable to determine instruction for initialized"
                  " data of width %lu for symbol %s\n",
                  ident->type->width, ident->lexinfo);
          return -1;
          break;
      }
    } else {
      /* put in bss/uninitialized data section */
      data->dest_operand[0] = '1';
      data->dest_operand[1] = 0;
      switch (ident->type->width) {
        case X64_SIZEOF_LONG:
          data->opcode = OPCODES[OP_RESQ];
          break;
        case X64_SIZEOF_INT:
          data->opcode = OPCODES[OP_RESD];
          break;
        case X64_SIZEOF_SHORT:
          data->opcode = OPCODES[OP_RESW];
          break;
        case X64_SIZEOF_CHAR:
          data->opcode = OPCODES[OP_RESB];
          break;
        default:
          fprintf(stderr,
                  "ERROR: unable to determine instruction for uninitialized"
                  " data of width %lu for symbol %s\n",
                  ident->type->width, ident->lexinfo);
          return -1;
          break;
      }
    }
  }
  return 0;
}

int translate_function(ASTree *function, CompilerState *state,
                       InstructionData *data) {
  DEBUGS('g', "Translating function definition");
  ASTree *declarator = astree_get(function, 1);
  ASTree *name_node = declarator;
  strcpy(data->label, name_node->lexinfo);

  SymbolValue *function_symval = NULL;
  state_get_symbol(state, name_node->lexinfo, strlen(name_node->lexinfo),
                   &function_symval);
  strcpy(function_symval->obj_loc, name_node->lexinfo);

  int status = save_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  size_t i;
  /* cleanup vregs from last function and skip return reg since we don't need
   * to save or store it
   */
  vreg_count = 1;
  /* remember to use function symbol table to translate parameters */
  ASTree *function_block = astree_get(function, 2);
  SymbolTable *function_table = function_block->symbol_table;
  status = state_push_table(state, function_table);
  if (status) return status;
  /* last direct declarator should be parent of parameter nodes */
  ASTree *function_dirdecl =
      astree_get(declarator, astree_count(declarator) - 1);
  for (i = 0; i < astree_count(function_dirdecl); ++i) {
    ASTree *param = astree_get(function_dirdecl, i);
    InstructionData *param_data = calloc(1, sizeof(*param_data));
    int status = translate_param(param, state, param_data);
    if (status) return status;
    llist_push_back(text_section, param_data);
  }
  status = state_pop_table(state);
  if (status) return status;

  /* start vreg counter outside of numbers which can be automatically mapped
   * to real registers to eliminate duplicates and clobbered values when
   * saving and restoring registers
   */
  vreg_count = VOLATILE_COUNT + NONVOLATILE_COUNT;
  status = state_set_function(state, function_symval);
  if (status) return status;
  status = translate_stmt(astree_get(function, 2), state);
  if (status) return status;
  status = state_unset_function(state);
  if (status) return status;
  status = restore_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  /* insert return in case function did not have one */
  InstructionData *return_data = calloc(1, sizeof(*return_data));
  return_data->opcode = OPCODES[OP_RET];
  llist_push_back(text_section, return_data);
  return 0;
}

int translate_file(ASTree *root) {
  CompilerState *state = state_init();
  int status = state_push_table(state, root->symbol_table);
  if (status) return status;
  size_t i;
  for (i = 0; i < astree_count(root); ++i) {
    ASTree *topdecl = astree_get(root, i);
    if (topdecl->symbol != TOK_DECLARATION) {
      fprintf(stderr, "ERROR: unrecognized symbol %s at top level\n",
              parser_get_tname(topdecl->symbol));
      return -1;
    } else if (astree_count(topdecl) == 1) {
      /* declares nothing; do nothing */
      continue;
    } else {
      ASTree *declarator = astree_get(topdecl, 1);
      if (typespec_is_function(declarator->type)) {
        /* skip if this is a function declaration, not definition */
        if (astree_count(topdecl) == 3) {
          InstructionData *label_data = calloc(1, sizeof(*label_data));
          /* put label before function body */
          llist_push_back(text_section, label_data);
          int status = translate_function(topdecl, state, label_data);
          if (status) return status;
        }
      } else {
        InstructionData *global_data = calloc(1, sizeof(*global_data));
        int status = translate_global_decl(topdecl, global_data);
        llist_push_back(data_section, global_data);
        if (status) return status;
      }
    }
  }
  status = state_pop_table(state);
  if (status) return status;
  return state_destroy(state);
}

int write_instruction(InstructionData *data, FILE *out) {
  /* all fields except for the instruction are character arrays, not pointers,
   * which are always legal to pass to printf and co., so we only need to
   * check to see whether or not the instruction is NULL before printing
   */
  /* we want to defer as much of the formatting as possible until we get to
   * this point so that we have as much information as possible to align
   * fields in the generated assembly
   */

  if (data->opcode == NULL && strlen(data->label) > 0) {
    /* only a label */
    fprintf(out, LABEL_FMT, data->label);
  } else if (data->opcode != NULL) {
    /* instruction, possible with a label */
    int instruction_type =
        (!!data->dest_operand[0]) | (!!data->src_operand[0] << 1);
    if (data->opcode == OPCODES[OP_NOP]) instruction_type = 0;
    switch (instruction_type) {
      case 0:
        /* nullary */
        fprintf(out, NULLOP_FMT, data->label, data->opcode);
        break;
      case 1:
        /* unary, destination */
        fprintf(out, UNOP_FMT, data->label, data->opcode, data->dest_operand);
        break;
      case 2:
        /* unary, source */
        fprintf(out, UNOP_FMT, data->label, data->opcode, data->src_operand);
        break;
      case 3:
        /* binary operation */
        fprintf(out, BINOP_FMT, data->label, data->opcode, data->dest_operand,
                data->src_operand);
        break;
      default:
        break;
    }
  }

  return 0;
}

int write_text_section(FILE *out) {
  size_t i;
  for (i = 0; i < text_section->size; ++i) {
    write_instruction(llist_get(text_section, i), out);
  }
  return 0;
}

int write_data_section(FILE *out) {
  size_t i;
  for (i = 0; i < data_section->size; ++i) {
    write_instruction(llist_get(data_section, i), out);
  }
  return 0;
}

int write_bss_section(FILE *out) {
  size_t i;
  for (i = 0; i < bss_section->size; ++i) {
    write_instruction(llist_get(bss_section, i), out);
  }
  return 0;
}

int write_asm(FILE *out) {
  /* TODO(Robert): write assembler directives */
  write_text_section(out);
  write_data_section(out);
  write_bss_section(out);
  return 0;
}

void asmgen_init_globals() {
  text_section = malloc(sizeof(*text_section));
  llist_init(text_section, free, NULL);
  data_section = malloc(sizeof(*data_section));
  llist_init(data_section, free, NULL);
  bss_section = malloc(sizeof(*bss_section));
  llist_init(bss_section, free, NULL);
  memset(current_label, 0, MAX_LABEL_LENGTH);
}

void asmgen_free_globals() {
  llist_destroy(text_section);
  llist_destroy(data_section);
  llist_destroy(bss_section);
}
