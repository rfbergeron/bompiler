#include "asmgen.h"

#include "assert.h"
#include "astree.h"
#include "attributes.h"
#include "badalist.h"
#include "debug.h"
#include "lyutils.h"
#include "state.h"
#include "symtable.h"
#include "tchk_common.h"

#define MAX_OPCODE_LENGTH 8
#define MAX_OPERAND_LENGTH 32
#define MAX_LABEL_LENGTH 32
#define NO_DISP 0

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
  GENERATOR(SAL)                  \
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
#define DEST_IS_REG(tree)                                             \
  (((InstructionData *)liter_get(tree->last_instr))->dest.all.mode == \
   MODE_REGISTER)
#define FIX_LVAL(tree)                                                        \
  (tree->attributes & ATTR_EXPR_LVAL                                          \
       ? ((status = resolve_lval(tree)) ? NULL : liter_get(tree->last_instr)) \
       : liter_get(tree->last_instr))
#define FIX_DEST(tree)                                                        \
  (tree->attributes & ATTR_EXPR_LVAL                                          \
       ? ((status = resolve_lval(tree)) ? NULL : liter_get(tree->last_instr)) \
       : (!DEST_IS_REG(tree)                                                  \
              ? ((status = mov_to_reg(tree)) ? NULL                           \
                                             : liter_get(tree->last_instr))   \
              : (status = 0, liter_get(tree->last_instr))))

typedef enum opcode { FOREACH_OPCODE(GENERATE_ENUM) } Opcode;

typedef enum address_mode {
  MODE_NONE,
  MODE_REGISTER,
  MODE_IMMEDIATE,
  MODE_DIRECT,
  MODE_INDIRECT,
  MODE_SCALE
} AddressMode;
typedef enum reg_width {
  REG_NONE = 0,
  REG_BYTE = 1,
  REG_WORD = 2,
  REG_DWORD = 4,
  REG_QWORD = 8
} RegWidth;
typedef enum index_scale {
  SCALE_NONE = 0,
  SCALE_BYTE = 1,
  SCALE_WORD = 2,
  SCALE_DWORD = 4,
  SCALE_QWORD = 8
} IndexScale;

typedef union operand {
  struct opall {
    AddressMode mode;
  } all;
  struct opreg {
    AddressMode mode;
    RegWidth width;
    size_t num;
  } reg;
  struct opimm {
    AddressMode mode;
    uintmax_t val;
  } imm;
  struct opdir {
    AddressMode mode;
    intmax_t disp;
    char lab[MAX_LABEL_LENGTH];
  } dir;
  struct opind {
    AddressMode mode;
    intmax_t disp;
    size_t num;
  } ind;
  struct opsca {
    AddressMode mode;
    IndexScale scale;
    intmax_t disp;
    size_t base;
    size_t index;
  } sca;
} Operand;
typedef struct opall Opall;

typedef struct instruction_data {
  Opcode opcode;
  char label[MAX_LABEL_LENGTH];
  char comment[MAX_LABEL_LENGTH];
  Operand dest;
  Operand src;
} InstructionData;

/* rbx, rsp, rbp, r12-r15 are preserved across function calls in the System V
 * ABI; Microsoft's ABI additionally preserves rdi and rsi
 */
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
static const char DEF_FMT[] = ".D%lu";
static const char CASE_FMT[] = ".S%luC%lu";
static const char FALL_FMT[] = ".S%luF%lu";
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
size_t vreg_count = 0;
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
int translate_expr(ASTree *tree, CompilerState *state, InstructionData *data);

InstructionData *instr_init(Opcode opcode) {
  InstructionData *ret = calloc(1, sizeof(InstructionData));
  ret->opcode = opcode;
  return ret;
}

void set_op_reg(Operand *operand, RegWidth width, size_t num) {
  operand->reg.mode = MODE_REGISTER;
  operand->reg.width = width;
  operand->reg.num = num;
}

void set_op_imm(Operand *operand, uintmax_t val) {
  operand->imm.mode = MODE_IMMEDIATE;
  operand->imm.val = val;
}

void set_op_dir(Operand *operand, intmax_t disp, const char *format, ...) {
  operand->dir.mode = MODE_DIRECT;
  operand->dir.disp = disp;
  va_list args;
  va_start(args, format);
  vsprintf(operand->dir.lab, format, args);
  va_end(args);
}

void set_op_ind(Operand *operand, intmax_t disp, size_t num) {
  operand->ind.mode = MODE_INDIRECT;
  operand->ind.disp = disp;
  operand->ind.num = num;
}

void set_op_sca(Operand *operand, IndexScale scale, intmax_t disp, size_t base,
                size_t index) {
  operand->sca.mode = MODE_SCALE;
  operand->sca.scale = scale;
  operand->sca.disp = disp;
  operand->sca.base = base;
  operand->sca.index = index;
}

Opcode opcode_from_operator(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_NEG:
      return OP_NEG;
    case TOK_POST_INC:
      /* fallthrough */
    case TOK_INC:
      return OP_INC;
    case TOK_POST_DEC:
      /* fallthrough */
    case TOK_DEC:
      return OP_DEC;
    case '+':
      return OP_ADD;
    case '-':
      return OP_SUB;
    case '*':
      return tree->type->base == TYPE_SIGNED ? OP_IMUL : OP_MUL;
    case '%':
      /* fallthrough */
    case '/':
      return tree->type->base == TYPE_SIGNED ? OP_IDIV : OP_DIV;
    case '|':
      return OP_OR;
    case '&':
      return OP_AND;
    case '^':
      return OP_XOR;
    case '~':
      return OP_NOT;
    case TOK_SHL:
      return tree->type->base == TYPE_SIGNED ? OP_SAL : OP_SHL;
    case TOK_SHR:
      return tree->type->base == TYPE_SIGNED ? OP_SAR : OP_SHR;
    case TOK_GE:
      return tree->type->base == TYPE_SIGNED ? OP_SETGE : OP_SETAE;
    case TOK_LE:
      return tree->type->base == TYPE_SIGNED ? OP_SETLE : OP_SETBE;
    case '>':
      return tree->type->base == TYPE_SIGNED ? OP_SETG : OP_SETA;
    case '<':
      return tree->type->base == TYPE_SIGNED ? OP_SETL : OP_SETB;
    case TOK_EQ:
      return OP_SETE;
    case TOK_NE:
      return OP_SETNE;
    case TOK_OR:
      return OP_JNZ;
    case TOK_AND:
      return OP_JZ;
    default:
      return OP_NOP;
  }
}

/* assigns space for a symbol given an existing offset. inserts padding as
 * needed and returns the sum of the previous offset, the padding and the width
 * of the symbol
 */
size_t assign_space(SymbolValue *symval, size_t reg, size_t offset) {
  size_t alignment = typespec_get_alignment(&symval->type);
  size_t width = typespec_get_width(&symval->type);
  size_t padding = alignment - (offset % alignment);
  if (padding != alignment) offset += padding;
  symval->offset = offset;
  symval->reg = reg;
  offset += width;
  return offset;
}

/* TODO(Robert): use flags to assign specific numbers for opcodes which
 * use specific registers as their source or destination to make register
 * allocation easier
 */
void assign_vreg(const TypeSpec *type, Operand *operand, size_t vreg_num) {
  assert(type->base != TYPE_VOID);
  size_t width = typespec_get_width((TypeSpec *)type);
  assert(width == X64_SIZEOF_LONG || width == X64_SIZEOF_INT ||
         width == X64_SIZEOF_SHORT || width == X64_SIZEOF_CHAR);
  operand->reg.mode = MODE_REGISTER;
  operand->reg.num = vreg_num;
  switch (type->width) {
    case X64_SIZEOF_LONG:
      operand->reg.width = REG_QWORD;
      return;
    case X64_SIZEOF_INT:
      operand->reg.width = REG_DWORD;
      return;
    case X64_SIZEOF_SHORT:
      operand->reg.width = REG_WORD;
      return;
    case X64_SIZEOF_CHAR:
      operand->reg.width = REG_BYTE;
      return;
  }
}

void copy_lvalue(Operand *dest, const Operand *src) {
  dest->ind.mode = MODE_INDIRECT;
  dest->ind.disp = 0;
  dest->ind.num = src->reg.num;
}

int resolve_lval(ASTree *lvalue_tree) {
  InstructionData *lvalue_data = liter_get(lvalue_tree->last_instr);
  InstructionData *mov_data = calloc(1, sizeof(InstructionData));
  mov_data->opcode = OP_MOV;
  copy_lvalue(&mov_data->src, &lvalue_data->dest);
  assign_vreg(lvalue_tree->type, &mov_data->dest, vreg_count++);
  int status = liter_ins_after(lvalue_tree->last_instr, mov_data);
  if (status) {
    free(mov_data);
    return status;
  }
  status = liter_advance(lvalue_tree->last_instr, 1);
  if (status) return status;
  return 0;
}

int mov_to_reg(ASTree *tree) {
  InstructionData *tree_data = liter_get(tree->last_instr);
  InstructionData *mov_data = calloc(1, sizeof(InstructionData));
  tree_data->opcode = OP_MOV;
  mov_data->src = tree_data->dest;
  assign_vreg(tree->type, &tree_data->dest, vreg_count++);
  int status = liter_ins_after(tree->last_instr, tree_data);
  if (status) {
    free(mov_data);
    return status;
  }
  status = liter_advance(tree->last_instr, 1);
  if (status) return status;
  return 0;
}

void resolve_object(CompilerState *state, Operand *operand, const char *ident) {
  SymbolValue *symval = NULL;
  state_get_symbol(state, ident, strlen(ident), &symval);
  assert(symval != NULL);
  if (symval->reg == 0) {
    set_op_dir(operand, symval->offset, ident);
  } else {
    operand->ind.mode = MODE_INDIRECT;
    operand->ind.num = symval->reg;
    operand->ind.disp = symval->offset;
  }
}

int save_registers(size_t start, size_t count, ListIter *iter) {
  int status = liter_advance(iter, 1);
  if (status) return status;
  size_t i;
  for (i = 0; i < count; ++i) {
    InstructionData *push_data = calloc(1, sizeof(InstructionData));
    assert(push_data != NULL);
    push_data->opcode = OP_PUSH;
    assign_vreg((TypeSpec *)&SPEC_LONG, &push_data->dest, start + i);
    int status = liter_ins_before(iter, push_data);
    if (status) return status;
  }
  status = liter_advance(iter, -1);
  if (status) return status;
  return 0;
}

int restore_registers(size_t start, size_t count, ListIter *iter) {
  int status = liter_advance(iter, 1);
  if (status) return status;
  size_t i;
  for (i = 1; i <= count; ++i) {
    InstructionData *pop_data = calloc(1, sizeof(InstructionData));
    assert(pop_data != NULL);
    pop_data->opcode = OP_POP;
    assign_vreg((TypeSpec *)&SPEC_LONG, &pop_data->dest, start + (count - i));
    int status = liter_ins_before(iter, pop_data);
    if (status) return status;
  }
  status = liter_advance(iter, -1);
  if (status) return status;
  return 0;
}

int translate_ident(ASTree *ident, CompilerState *state) {
  InstructionData *data = calloc(1, sizeof(InstructionData));
  resolve_object(state, &data->src, ident->lexinfo);
  const TypeSpec *ident_type = ident->type;
  AuxSpec *ident_aux = llist_back(&ident_type->auxspecs);
  data->opcode = OP_LEA;
  assign_vreg(&SPEC_LONG, &data->dest, vreg_count++);
  int status = llist_push_back(text_section, data);
  if (status) return status;
  ident->first_instr = llist_iter_last(text_section);
  if (ident->first_instr == NULL) return -1;
  ident->last_instr = liter_copy(ident->first_instr);
  if (ident->last_instr == NULL) return -1;
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
int translate_conversion(ASTree *conversion) {
  DEBUGS('g', "Translating conversion");
  assert(astree_count(conversion) <= 2);

  ASTree *converted_expr = astree_count(conversion) == 1
                               ? astree_get(conversion, 0)
                               : astree_get(conversion, 1);

  InstructionData *src_data = liter_get(converted_expr->last_instr);
  InstructionData *data = calloc(1, sizeof(InstructionData));
  if (converted_expr->attributes & ATTR_EXPR_LVAL)
    copy_lvalue(&data->src, &src_data->dest);
  else
    data->src = src_data->dest;
  assign_vreg(conversion->type, &data->dest, vreg_count++);

  const TypeSpec *target_type = conversion->type;
  const TypeSpec *source_type = converted_expr->type;
  AuxSpec *source_aux = llist_back(&source_type->auxspecs);

  if (source_aux &&
      (source_aux->aux == AUX_ARRAY || source_aux->aux == AUX_FUNCTION)) {
    data->opcode = OP_MOV;
  } else if (source_type->width > target_type->width) {
    data->opcode = OP_MOV;
  } else if (source_type->width == target_type->width) {
    data->opcode = OP_NOP;
  } else if (source_type->base == TYPE_SIGNED) {
    if (target_type->base == TYPE_SIGNED) {
      data->opcode = OP_MOVSX;
    } else if (target_type->base == TYPE_UNSIGNED) {
      data->opcode = OP_MOVZX;
    }
  } else if (source_type->base == TYPE_UNSIGNED) {
    data->opcode = OP_MOVZX;
  } else {
    fprintf(stderr, "ERROR: unable to determine conversion\n");
    return -1;
  }

  conversion->first_instr = liter_copy(converted_expr->first_instr);
  if (conversion->first_instr == NULL) return -1;
  conversion->last_instr = liter_insert(converted_expr->last_instr, 1, data);
  if (conversion->last_instr == NULL) return -1;
  return 0;
}

int translate_intcon(ASTree *constant) {
  DEBUGS('g', "Translating integer constant");
  InstructionData *data = calloc(1, sizeof(InstructionData));
  assign_vreg(constant->type, &data->dest, vreg_count++);
  data->src.imm.mode = MODE_IMMEDIATE;
  data->src.imm.val = constant->constval;
  data->opcode = OP_MOV;
  int status = llist_push_back(text_section, data);
  if (status) return status;
  constant->first_instr = llist_iter_last(text_section);
  if (constant->first_instr == NULL) return -1;
  constant->last_instr = liter_copy(constant->first_instr);
  if (constant->last_instr == NULL) return -1;
  return 0;
}

/* Two classes of operators whose result is a boolean:
 * - comparison: >, <, >=, <=, ==, !=
 * - logical: &&, ||, !
 *
 * logical NOT does the same thing as the conversion from an arbitrary value
 * to a boolean except instead of using SETNZ it does SETZ
 */
int translate_logical_not(ASTree * not ) {
  ASTree *operand = astree_get(not, 0);

  /* move lval into reg, if necessary */
  int status = 0;
  InstructionData *operand_data = FIX_DEST(operand);
  if (operand_data == NULL) return status ? status : -1;

  /* TEST operand with itself */
  InstructionData *test_data = calloc(1, sizeof(InstructionData));
  test_data->opcode = OP_TEST;
  test_data->dest = operand_data->dest;
  test_data->src = operand_data->dest;

  InstructionData *setz_data = calloc(1, sizeof(InstructionData));
  setz_data->opcode = OP_SETZ;
  assign_vreg(&SPEC_INT, &setz_data->dest, vreg_count++);

  not ->first_instr = liter_copy(operand->first_instr);
  if (not ->first_instr == NULL) return -1;
  not ->last_instr = liter_insert(operand->last_instr, 2, test_data, setz_data);
  if (not ->last_instr == NULL) return -1;
  return 0;
}

int translate_logical(ASTree *operator) {
  /* create label used to skip second operand */
  /* result will always be the truth value of the last evaluated expression */
  InstructionData *setnz_data = calloc(1, sizeof(InstructionData));
  setnz_data->opcode = OP_SETNZ;
  assign_vreg(&SPEC_INT, &setnz_data->dest, vreg_count++);
  sprintf(setnz_data->label, BOOL_FMT, branch_count++);

  /* test first operand; jump on false for && and true for || */
  ASTree *first = astree_get(operator, 0);
  int status = 0;
  InstructionData *first_data = FIX_DEST(first);
  if (first_data == NULL) return status ? status : -1;

  InstructionData *test_first_data = calloc(1, sizeof(InstructionData));
  test_first_data->opcode = OP_TEST;
  test_first_data->dest = first_data->dest;
  test_first_data->src = first_data->dest;

  InstructionData *jmp_first_data = calloc(1, sizeof(InstructionData));
  jmp_first_data->opcode = opcode_from_operator(operator);
  set_op_dir(&jmp_first_data->dest, NO_DISP, setnz_data->label);

  /* insert in reverse (correct) order */
  status = liter_ins_after(first->last_instr, jmp_first_data);
  if (status) return status;
  status = liter_ins_after(first->last_instr, test_first_data);
  if (status) return status;

  ASTree *second = astree_get(operator, 1);
  InstructionData *second_data = FIX_DEST(second);
  if (second_data == NULL) return status ? status : -1;

  InstructionData *test_second_data = calloc(1, sizeof(InstructionData));
  test_second_data->opcode = OP_TEST;
  test_second_data->dest = second_data->dest;
  test_second_data->src = second_data->dest;

  operator->first_instr = liter_copy(first->first_instr);
  if (operator->first_instr == NULL) return -1;
  operator->last_instr =
      liter_insert(second->last_instr, 2, test_second_data, setnz_data);
  if (operator->last_instr == NULL) return -1;
  return 0;
}

/* TODO(Robert): check location of result of first subexpression, and require
 * second subexpression to place result in a register if the first was not
 */
int translate_comparison(ASTree *operator) {
  /* CMP operands, then SETG/SETGE/SETL/SETLE/SETE/SETNE */
  ASTree *first = astree_get(operator, 0);
  int status = 0;
  InstructionData *first_data = FIX_DEST(first);
  if (first_data == NULL) return status ? status : -1;

  ASTree *second = astree_get(operator, 1);
  InstructionData *second_data = FIX_LVAL(second);
  if (status) return status ? status : -1;

  InstructionData *cmp_data = calloc(1, sizeof(InstructionData));
  cmp_data->opcode = OP_CMP;
  cmp_data->dest = first_data->dest;
  cmp_data->src = second_data->dest;

  InstructionData *setcc_data = calloc(1, sizeof(InstructionData));
  setcc_data->opcode = opcode_from_operator(operator);
  /* this points to an existing type; no need to free */
  const TypeSpec *common_type;
  status = determine_conversion(first->type, second->type, &common_type);
  if (status) return status;
  assign_vreg(&SPEC_INT, &setcc_data->dest, vreg_count++);
  return status;
}

int translate_indirection(ASTree *indirection) {
  /* does nothing. we don't know if the parent wants the object of the value,
   * so we keep the address in the register
   */
  DEBUGS('g', "Translating indirection operation.");
  ASTree *operand = astree_get(indirection, 0);
  indirection->first_instr = liter_copy(operand->first_instr);
  if (indirection->first_instr == NULL) return -1;
  indirection->last_instr = liter_copy(operand->last_instr);
  if (indirection->last_instr == NULL) return -1;
  return 0;
}

int translate_addrof(ASTree *addrof) {
  /* same as above */
  DEBUGS('g', "Translating address operation.");
  ASTree *operand = astree_get(addrof, 0);
  addrof->first_instr = liter_copy(operand->first_instr);
  if (addrof->first_instr == NULL) return -1;
  addrof->last_instr = liter_copy(operand->last_instr);
  if (addrof->last_instr == NULL) return -1;
  return 0;
}

int translate_subscript(ASTree *subscript) {
  DEBUGS('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the offset and
   * scale addressing mode can be used
   */
  ASTree *pointer = astree_get(subscript, 0);
  int status = 0;
  InstructionData *pointer_data = FIX_DEST(pointer);
  if (status) return status ? status : -1;

  ASTree *index = astree_get(subscript, 1);
  InstructionData *index_data = FIX_DEST(index);
  if (status) return status ? status : -1;

  subscript->first_instr = liter_copy(pointer->first_instr);
  if (subscript->first_instr == NULL) return -1;

  InstructionData *lea_data = calloc(1, sizeof(InstructionData));
  lea_data->opcode = OP_LEA;
  /* TODO(Robert): define pointer type constant */
  assign_vreg(&SPEC_LONG, &lea_data->dest, vreg_count++);
  lea_data->src.sca.mode = MODE_SCALE;
  lea_data->src.sca.base = pointer_data->dest.ind.num;
  lea_data->src.sca.index = index_data->dest.reg.num;
  size_t scale = typespec_get_width((TypeSpec *)subscript->type);
  if (scale <= 8 && scale % 2 == 0 && scale != 6) {
    lea_data->src.sca.scale = scale;
    subscript->last_instr = liter_insert(index->last_instr, 1, lea_data);
  } else {
    lea_data->src.sca.scale = SCALE_BYTE;
    InstructionData *mul_data = calloc(1, sizeof(*mul_data));
    mul_data->opcode = OP_IMUL;
    mul_data->dest = index_data->dest;
    mul_data->src.imm.mode = MODE_IMMEDIATE;
    mul_data->src.imm.val = typespec_get_width((TypeSpec *)subscript->type);
    subscript->last_instr =
        liter_insert(index->last_instr, 2, mul_data, lea_data);
  }
  if (subscript->last_instr == NULL) return -1;
  return 0;
}

int translate_reference(ASTree *reference) {
  DEBUGS('g', "Translating reference operator");
  ASTree *struct_ = astree_get(reference, 0);
  InstructionData *struct_data = liter_get(struct_->last_instr);

  AuxSpec *struct_aux = reference->symbol == TOK_ARROW
                            ? llist_get(&struct_->type->auxspecs, 1)
                            : llist_front(&struct_->type->auxspecs);
  SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
  const char *member_name = astree_get(reference, 1)->lexinfo;
  size_t member_name_len = strlen(member_name);
  SymbolValue *member_symbol =
      symbol_table_get(member_table, member_name, member_name_len);
  InstructionData *lea_data = calloc(1, sizeof(InstructionData));
  lea_data->opcode = OP_LEA;
  lea_data->src.ind.disp = member_symbol->offset;
  lea_data->src.ind.mode = MODE_INDIRECT;
  lea_data->src.ind.num = struct_data->dest.reg.num;
  assign_vreg(&SPEC_LONG, &lea_data->dest, vreg_count++);

  reference->first_instr = liter_copy(struct_->first_instr);
  if (reference->first_instr == NULL) return -1;
  reference->last_instr = liter_insert(struct_->last_instr, 1, lea_data);
  if (reference->last_instr == NULL) return -1;
  return 0;
}

int translate_inc_dec(ASTree *inc_dec) {
  DEBUGS('g', "Translating increment/decrement");
  ASTree *operand = astree_get(inc_dec, 0);
  InstructionData *operand_data = liter_get(operand->last_instr);
  InstructionData *mov_data = calloc(1, sizeof(InstructionData));
  mov_data->opcode = OP_MOV;
  copy_lvalue(&mov_data->src, &operand_data->dest);
  assign_vreg(inc_dec->type, &mov_data->dest, vreg_count++);

  InstructionData *inc_dec_data = calloc(1, sizeof(InstructionData));
  inc_dec_data->opcode = opcode_from_operator(inc_dec);
  copy_lvalue(&inc_dec_data->dest, &operand_data->dest);

  inc_dec->first_instr = liter_copy(operand->first_instr);
  if (inc_dec->first_instr == NULL) return -1;
  if (inc_dec->symbol == TOK_POST_DEC || inc_dec->symbol == TOK_POST_INC) {
    inc_dec->last_instr =
        liter_insert(operand->last_instr, 2, mov_data, inc_dec_data);
    if (inc_dec->last_instr == NULL) return -1;
  } else {
    inc_dec->last_instr =
        liter_insert(operand->last_instr, 2, inc_dec_data, mov_data);
    if (inc_dec->last_instr == NULL) return -1;
  }
  return 0;
}

int translate_unop(ASTree *operator) {
  DEBUGS('g', "Translating unary operation");
  ASTree *operand = astree_get(operator, 0);
  int status = 0;
  InstructionData *operand_data = FIX_LVAL(operand);
  if (operand_data == NULL) return status ? status : -1;

  InstructionData *operator_data = calloc(1, sizeof(InstructionData));
  operator_data->opcode = opcode_from_operator(operator);
  operator_data->dest = operand_data->dest;

  operator->first_instr = liter_copy(operand->first_instr);
  if (operator->first_instr == NULL) return -1;
  operator->last_instr = liter_insert(operand->last_instr, 1, operator_data);
  if (operator->last_instr == NULL) return -1;
  return 0;
}

int translate_binop(ASTree *operator) {
  DEBUGS('g', "Translating binary operation");
  ASTree *left = astree_get(operator, 0);
  int status = 0;
  InstructionData *left_data = FIX_DEST(left);
  if (left_data == NULL) return status ? status : -1;

  ASTree *right = astree_get(operator, 1);
  InstructionData *right_data = FIX_LVAL(right);
  if (right_data == NULL) return status ? status : -1;

  InstructionData *operator_data = calloc(1, sizeof(InstructionData));
  operator_data->opcode = opcode_from_operator(operator);
  operator_data->dest = left_data->dest;
  operator_data->src = right_data->dest;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) return -1;

  if (operator->symbol == '*' || operator->symbol == '/' || operator->symbol ==
      '%') {
    InstructionData *mov_data = calloc(1, sizeof(InstructionData));
    mov_data->opcode = OP_MOV;
    /* dummy vreg for quotient */
    if (operator->symbol == '%') ++vreg_count;
    /* mov from vreg representing rax/rdx to old vreg */
    assign_vreg(operator->type, &mov_data->src, vreg_count++);
    mov_data->dest = left_data->dest;
    operator->last_instr =
        liter_insert(right->last_instr, 2, operator_data, mov_data);
    if (operator->last_instr == NULL) return -1;
  } else {
    operator->last_instr = liter_insert(right->last_instr, 1, operator_data);
    if (operator->last_instr == NULL) return -1;
  }
  return 0;
}

int translate_assignment(ASTree *assignment) {
  DEBUGS('g', "Translating assignment");

  ASTree *lvalue = astree_get(assignment, 0);
  InstructionData *lvalue_data = liter_get(lvalue->last_instr);

  ASTree *expr = astree_get(assignment, 1);
  int status = 0;
  /* it's called FIX_DEST, but it just gets the values of objects and puts non-
   * register operands into registers
   */
  InstructionData *expr_data = FIX_DEST(expr);
  if (expr_data == NULL) return status ? status : -1;

  InstructionData *assignment_data = calloc(1, sizeof(InstructionData));
  assignment_data->opcode = OP_MOV;
  copy_lvalue(&assignment_data->dest, &lvalue_data->dest);
  assignment_data->src = expr_data->dest;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;
  assignment->last_instr = liter_insert(lvalue->last_instr, 1, assignment_data);
  if (assignment->last_instr == NULL) return -1;
  return 0;
}

int translate_call(ASTree *call) {
  DEBUGS('g', "Translating function call");
  ASTree *pointer = astree_get(call, 0);
  call->first_instr = liter_copy(pointer->first_instr);
  if (call->first_instr == NULL) return -1;
  ASTree *last_arg = astree_get(call, astree_count(call) - 1);
  call->last_instr = liter_copy(last_arg->last_instr);
  if (call->last_instr == NULL) return -1;
  int status = save_registers(VOLATILE_START, VOLATILE_COUNT, call->last_instr);
  if (status) return status;

  size_t i;
  for (i = 1; i < astree_count(call); ++i) {
    DEBUGS('g', "Translating parameter %i", i);
    ASTree *arg = astree_get(call, i);
    InstructionData *arg_data = liter_get(arg->last_instr);
    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->opcode = OP_MOV;
    mov_data->src = arg_data->dest;
    assign_vreg(arg->type, &mov_data->dest, i);
    int status = liter_ins_after(call->last_instr, mov_data);
    if (status) return status;
    status = liter_advance(call->last_instr, 1);
    if (status) return status;
  }

  InstructionData *pointer_data = liter_get(pointer->last_instr);
  InstructionData *call_data = calloc(1, sizeof(*call_data));
  call_data->opcode = OP_CALL;
  call_data->dest = pointer_data->dest;
  status = liter_ins_after(call->last_instr, call_data);
  if (status) return status;
  status = liter_advance(call->last_instr, 1);
  if (status) return status;

  if (call->type->base != TYPE_VOID) {
    InstructionData *mov_data = calloc(1, sizeof(InstructionData));
    mov_data->opcode = OP_MOV;
    assign_vreg(call->type, &mov_data->src, RETURN_VREG);
    assign_vreg(call->type, &mov_data->dest, vreg_count++);
    int status = liter_ins_after(call->last_instr, mov_data);
    if (status) return status;
    status = liter_advance(call->last_instr, 1);
    if (status) return status;
    status =
        restore_registers(VOLATILE_START, VOLATILE_COUNT, call->last_instr);
    if (status) return status;
    /* dummy mov since parent expects last instr to have result */
    InstructionData *dummy_data = calloc(1, sizeof(InstructionData));
    dummy_data->opcode = OP_MOV;
    dummy_data->dest = mov_data->dest;
    dummy_data->src = mov_data->dest;
    status = liter_ins_after(call->last_instr, mov_data);
    if (status) return status;
    status = liter_advance(call->last_instr, 1);
    if (status) return status;
  } else {
    int status =
        restore_registers(VOLATILE_START, VOLATILE_COUNT, call->last_instr);
    if (status) return status;
  }
  return 0;
}

int translate_param(ASTree *param, CompilerState *state,
                    InstructionData *data) {
  DEBUGS('g', "Translating parameter");
  ASTree *declarator = astree_get(param, 1);
  SymbolValue *symval = NULL;
  assert(state_get_symbol(state, declarator->lexinfo,
                          strlen(declarator->lexinfo), &symval));
  assert(symval);
  stack_window = assign_space(symval, STACK_POINTER_VREG, stack_window);
  resolve_object(state, &data->dest, declarator->lexinfo);
  assign_vreg(declarator->type, &data->src, vreg_count++);
  data->opcode = OP_MOV;
  return 0;
}

int translate_list_initialization(ASTree *declarator, ASTree *init_list,
                                  CompilerState *state) {
  DEBUGS('g', "Translating struct initialization by initializer list");
  InstructionData *struct_data = calloc(1, sizeof(InstructionData));
  resolve_object(state, &struct_data->src, declarator->lexinfo);
  assign_vreg(&SPEC_ULONG, &struct_data->dest, vreg_count++);
  struct_data->opcode = OP_LEA;
  llist_push_back(text_section, struct_data);

  /* TODO(Robert): initialize unset struct members and array elements to
   * zero if the object is static
   */
  /* TODO(Robert): handle array initializers here or create a separate function
   * to handle them
   */
  AuxSpec *struct_aux = llist_back(&declarator->type->auxspecs);
  const LinkedList *member_symbols =
      &struct_aux->data.tag.val->data.members.in_order;
  size_t i;
  for (i = 0; i < astree_count(init_list); ++i) {
    ASTree *initializer = astree_get(init_list, i);
    InstructionData *initializer_data = calloc(1, sizeof(InstructionData));
    initializer_data->flags |= USE_REG;
    int status = translate_expr(initializer, state, initializer_data);
    if (status) return status;
    llist_push_back(text_section, initializer_data);

    InstructionData *mov_data = calloc(1, sizeof(InstructionData));
    mov_data->src = initializer_data->dest;
    SymbolValue *member_symbol = llist_get(member_symbols, i);
    mov_data->dest.ind.mode = MODE_INDIRECT;
    mov_data->dest.ind.num = struct_data->dest.reg.num;
    mov_data->dest.ind.disp = member_symbol->offset;
    mov_data->opcode = OP_MOV;
    llist_push_back(text_section, mov_data);
  }
  return 0;
}

int translate_local_decl(ASTree *declaration, CompilerState *state) {
  DEBUGS('g', "Translating local declaration");
  size_t i;
  /* skip typespec list */
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *child = astree_get(declaration, i);
    ASTree *declarator = child->symbol == '=' ? astree_get(child, 0) : child;
    ASTree *initializer = child->symbol == '=' ? astree_get(child, 1) : NULL;
    SymbolValue *symval = NULL;
    assert(state_get_symbol(state, (char *)declarator->lexinfo,
                            strlen(declarator->lexinfo), &symval));
    assert(symval);
    stack_window = assign_space(symval, STACK_POINTER_VREG, stack_window);

    if (initializer != NULL) {
      if (initializer->symbol == TOK_INIT_LIST) {
        int status =
            translate_list_initialization(declarator, initializer, state);
        if (status) return status;
      } else {
        InstructionData *value_data = calloc(1, sizeof(*value_data));
        value_data->flags |= USE_REG;
        int status = translate_expr(initializer, state, value_data);
        if (status) return status;
        llist_push_back(text_section, value_data);

        InstructionData *mov_data = calloc(1, sizeof(*mov_data));
        resolve_object(state, &mov_data->dest, declarator->lexinfo);
        mov_data->src = value_data->dest;
        mov_data->opcode = OP_MOV;
        llist_push_back(text_section, mov_data);
      }
    }
  }
  return 0;
}

static int translate_ifelse(ASTree *ifelse) {
  ASTree *condition = astree_get(ifelse, 0);
  int status;
  InstructionData *condition_data = FIX_DEST(condition);
  if (status) return status;

  ifelse->first_instr = liter_copy(condition->first_instr);
  if (ifelse->first_instr == NULL) return -1;

  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;

  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, ifelse->jump_id);
  ASTree *if_body = astree_get(ifelse, 1);
  if (astree_count(ifelse) == 3) {
    ASTree *else_body = astree_get(ifelse, 2);
    InstructionData *else_label = instr_init(OP_NOP);
    sprintf(else_label->label, STMT_FMT, ifelse->jump_id);
    int status = liter_push_front(else_body->first_instr, NULL, 1, else_label);
    if (status) return status;

    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, NO_DISP, else_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) return status;

    InstructionData *jmp_data = instr_init(OP_JMP);
    set_op_dir(&jmp_data->dest, NO_DISP, end_label->label);
    status = liter_push_back(if_body->last_instr, NULL, 1, jmp_data);
    if (status) return status;
    status = liter_push_back(else_body->last_instr, &ifelse->last_instr, 1,
                             end_label);
    if (status) return status;
  } else {
    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, NO_DISP, end_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) return status;
    status =
        liter_push_back(if_body->last_instr, &ifelse->last_instr, 1, end_label);
    if (status) return status;
  }
  return 0;
}

static int translate_switch(ASTree *switch_, CompilerState *state) {
  ASTree *condition = astree_get(switch_, 0);
  int status;
  InstructionData *cond_data = liter_get(condition->last_instr);
  if (cond_data == NULL) return -1;

  InstructionData *mov_data = instr_init(OP_MOV);
  if (condition->attributes & ATTR_EXPR_LVAL)
    copy_lvalue(&mov_data->src, &cond_data->dest);
  else
    mov_data->src = cond_data->dest;
  /* we are casting all values to unsigned long since it does not really matter
   * and communicating type information is annoying */
  set_op_reg(&mov_data->dest, REG_QWORD, state_get_control_reg(state));
  status = liter_push_back(condition->last_instr, NULL, 1, mov_data);
  if (status) return status;

  /* switch epilogue */
  ASTree *body = astree_get(switch_, 1);
  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, switch_->jump_id);
  InstructionData *jmp_end_data = instr_init(OP_JMP);
  set_op_dir(&jmp_end_data->dest, NO_DISP, end_label->label);
  InstructionData *dummy_case_label = instr_init(OP_NOP);
  sprintf(dummy_case_label->label, CASE_FMT, switch_->jump_id,
          state_get_case_id(state));
  if (state_get_selection_default(state)) {
    InstructionData *jmp_def_data = instr_init(OP_JMP);
    set_op_dir(&jmp_def_data->dest, NO_DISP, DEF_FMT, switch_->jump_id);
    int status =
        liter_push_back(body->last_instr, &switch_->last_instr, 4, jmp_end_data,
                        dummy_case_label, jmp_def_data, end_label);
    if (status) return status;
  } else {
    int status = liter_push_back(body->last_instr, &switch_->last_instr, 3,
                                 jmp_end_data, dummy_case_label, end_label);
    if (status) return status;
  }
  return 0;
}

static int translate_while(ASTree *while_, CompilerState *state) {
  /* emit label at beginning of condition */
  InstructionData *cond_lab_data = calloc(1, sizeof(*cond_lab_data));
  sprintf(cond_lab_data->label, COND_FMT, while_->jump_id);
  cond_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, cond_lab_data);
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  cond_data->flags |= USE_REG;
  int status = translate_expr(astree_get(while_, 0), state, cond_data);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is zero */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->opcode = OP_TEST;
  test_data->dest = cond_data->dest;
  test_data->src = cond_data->dest;
  llist_push_back(text_section, test_data);
  /* create end of statement label, but do not emit */
  InstructionData *end_lab_data = calloc(1, sizeof(*end_lab_data));
  sprintf(end_lab_data->label, END_FMT, while_->jump_id);
  end_lab_data->opcode = OP_NOP;
  /* emit jump to end of loop */
  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->dest.dir.mode = MODE_DIRECT;
  test_jmp_data->dest.dir.lab = end_lab_data->label;
  test_jmp_data->opcode = OP_JZ;
  llist_push_back(text_section, test_jmp_data);
  /* emit label at beginning of body */
  InstructionData *body_lab_data = calloc(1, sizeof(*body_lab_data));
  sprintf(body_lab_data->label, STMT_FMT, while_->jump_id);
  body_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, body_lab_data);
  /* translate while body */
  status = translate_stmt(astree_get(while_, 1), state);
  if (status) return status;
  /* emit jump to condition */
  InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
  cond_jmp_data->opcode = OP_JMP;
  cond_jmp_data->dest.dir.mode = MODE_DIRECT;
  cond_jmp_data->dest.dir.lab = cond_lab_data->label;
  llist_push_back(text_section, cond_jmp_data);
  /* emit label at end of statement */
  llist_push_back(text_section, end_lab_data);
  return 0;
}

static int translate_for(ASTree *for_, CompilerState *state) {
  ASTree *for_exprs = astree_get(for_, 0);
  /* translate initialization expression, if present */
  ASTree *init_expr = astree_get(for_exprs, 0);
  if (init_expr->symbol != ';') {
    InstructionData *init_data = calloc(1, sizeof(*init_data));
    int status = translate_expr(init_expr, state, init_data);
    if (status) return status;
    llist_push_back(text_section, init_data);
  }

  /* emit label at beginning of condition */
  InstructionData *cond_lab_data = calloc(1, sizeof(*cond_lab_data));
  sprintf(cond_lab_data->label, COND_FMT, for_->jump_id);
  cond_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, cond_lab_data);
  /* create label at end of statement, but do not emit */
  InstructionData *end_lab_data = calloc(1, sizeof(*end_lab_data));
  sprintf(end_lab_data->label, END_FMT, for_->jump_id);
  end_lab_data->opcode = OP_NOP;
  /* translate conditional expression, if present */
  ASTree *cond_expr = astree_get(for_exprs, 1);
  if (cond_expr->symbol != ';') {
    /* translate conditional expression */
    InstructionData *cond_data = calloc(1, sizeof(*cond_data));
    cond_data->flags |= USE_REG;
    int status = translate_expr(cond_expr, state, cond_data);
    if (status) return status;
    llist_push_back(text_section, cond_data);
    /* check if condition is zero */
    InstructionData *test_data = calloc(1, sizeof(*test_data));
    test_data->dest = cond_data->dest;
    test_data->src = cond_data->src;
    test_data->opcode = OP_TEST;
    llist_push_back(text_section, test_data);
    /* emit jump to end of loop */
    InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
    test_jmp_data->dest.dir.mode = MODE_DIRECT;
    test_jmp_data->dest.dir.lab = end_lab_data->label;
    test_jmp_data->opcode = OP_JZ;
    llist_push_back(text_section, test_jmp_data);
  }

  /* emit label at beginning of body */
  InstructionData *body_lab_data = calloc(1, sizeof(*body_lab_data));
  sprintf(body_lab_data->label, STMT_FMT, for_->jump_id);
  body_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, body_lab_data);
  /* translate for body */
  int status = translate_stmt(astree_get(for_, 1), state);
  if (status) return status;
  /* translate re-initialization, if present */
  ASTree *reinit_expr = astree_get(for_exprs, 2);
  if (reinit_expr->symbol != ';' && reinit_expr->symbol != ')') {
    InstructionData *reinit_data = calloc(1, sizeof(*reinit_data));
    int status = translate_expr(reinit_expr, state, reinit_data);
    if (status) return status;
    llist_push_back(text_section, reinit_data);
  }

  /* emit jump to condition */
  InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
  cond_jmp_data->dest.dir.mode = MODE_DIRECT;
  cond_jmp_data->dest.dir.lab = cond_lab_data->label;
  cond_jmp_data->opcode = OP_JMP;
  llist_push_back(text_section, cond_jmp_data);
  /* emit label at end of loop */
  llist_push_back(text_section, end_lab_data);
  return 0;
}

static int translate_do(ASTree *do_, CompilerState *state) {
  /* emit label at beginning of body */
  InstructionData *body_lab_data = calloc(1, sizeof(*body_lab_data));
  sprintf(body_lab_data->label, STMT_FMT, do_->jump_id);
  body_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, body_lab_data);
  /* translate body */
  int status = translate_stmt(astree_get(do_, 0), state);
  if (status) return status;
  /* emit label at beginning of condition */
  InstructionData *cond_lab_data = calloc(1, sizeof(*cond_lab_data));
  sprintf(cond_lab_data->label, COND_FMT, do_->jump_id);
  cond_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, cond_lab_data);
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  cond_data->flags |= USE_REG;
  status = translate_expr(astree_get(do_, 1), state, cond_data);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is one */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->dest = cond_data->dest;
  test_data->src = cond_data->src;
  test_data->opcode = OP_TEST;
  llist_push_back(text_section, test_data);
  /* emit jump to beginning of body */
  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->dest.dir.mode = MODE_DIRECT;
  test_jmp_data->dest.dir.lab = body_lab_data->label;
  test_jmp_data->opcode = OP_JNZ;
  llist_push_back(text_section, test_jmp_data);
  /* emit label at end of statement */
  InstructionData *end_lab_data = calloc(1, sizeof(*end_lab_data));
  sprintf(end_lab_data->label, END_FMT, do_->jump_id);
  end_lab_data->opcode = OP_NOP;
  llist_push_back(text_section, end_lab_data);
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
  ASTree *expr = astree_get(case_, 0);
  InstructionData *expr_data = liter_get(expr->last_instr);
  if (expr_data == NULL) return -1;

  InstructionData *test_data = instr_init(OP_TEST);
  set_op_reg(&test_data->dest, REG_QWORD, state_get_control_reg(state));
  test_data->src = expr_data->dest;

  InstructionData *jmp_data = instr_init(OP_JNE);
  set_op_dir(&jmp_data->dest, NO_DISP, CASE_FMT, state_get_case_id(state));

  if (case_->case_id != 0) {
    InstructionData *fall_label = instr_init(OP_NOP);
    sprintf(fall_label->label, FALL_FMT, case_->jump_id, case_->case_id);
    InstructionData *case_label = instr_init(OP_NOP);
    sprintf(case_label->label, CASE_FMT, case_->jump_id, case_->case_id);
    InstructionData *fall_jmp_data = instr_init(OP_JMP);
    set_op_dir(&fall_jmp_data->dest, NO_DISP, fall_label->label);
    /* set first instr of case to fallthrough jump */
    int status = liter_push_front(expr->first_instr, &case_->first_instr, 2,
                                  fall_jmp_data, case_label);
    if (status) return status;
  } else {
    /* set first instr of case to first instr of condition */
    case_->first_instr = liter_copy(expr->first_instr);
    if (case_->first_instr == NULL) return -1;
  }
  int status = liter_push_back(expr->last_instr, NULL, 3, test_data, jmp_data,
                               fall_label);
  if (status) return status;
  return 0;
}

static int translate_default(ASTree *default_, CompilerState *state) {
  InstructionData *def_label = instr_init(OP_NOP);
  sprintf(def_label->label, DEF_FMT, default_->jump_id);

  ASTree *stmt = astree_get(default_, 0);
  int status =
      liter_push_front(stmt->first_instr, &default_->first_instr, 1, def_label);
  if (status) return status;
  default_->last_instr = liter_copy(stmt->last_instr);
  if (default_->last_instr == NULL) return -1;
  return 0;
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

int translate_global_init(ASTree *declarator, ASTree *initializer,
                          InstructionData *data) {
  if (typespec_is_array(declarator->type)) {
    data->opcode = OP_NOP;
    strcpy(data->comment, "Array init");
    return 0;
  } else if (typespec_is_union(declarator->type) ||
             typespec_is_struct(declarator->type)) {
    data->opcode = OP_NOP;
    strcpy(data->comment, "Struct init");
    return 0;
  } else {
    size_t width = typespec_get_width(declarator->type);
    switch (width) {
      case X64_SIZEOF_LONG:
        data->opcode = OP_DQ;
        goto dx_common_case;
      case X64_SIZEOF_INT:
        data->opcode = OP_DD;
        goto dx_common_case;
      case X64_SIZEOF_SHORT:
        data->opcode = OP_DW;
        goto dx_common_case;
      case X64_SIZEOF_CHAR:
        data->opcode = OP_DB;
      common_case:
        data->dest.imm.mode = MODE_IMMEDIATE;
        data->dest.imm.val = initializer->constval;
        break;
      default:
        fprintf(stderr,
                "ERROR: cannot initialize non-aggregate type with width %lu\n",
                width);
        abort();
    }
    assert(!llist_push_back(data_section, data));
    return 0;
  }
}

int translate_global_decl(ASTree *declaration, InstructionData *data) {
  DEBUGS('g', "Translating global declaration");
  size_t i;
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *child = astree_get(declaration, i);
    ASTree *declarator = child->symbol == '=' ? astree_get(child, 0) : child;
    ASTree *initializer = child->symbol == '=' ? astree_get(child, 1) : NULL;
    strcpy(data->label, declarator->lexinfo);
    if (initializer != NULL) {
      /* put in data section */
      return translate_global_init(declarator, initializer, data);
    } else {
      /* put in bss/uninitialized data section */
      size_t width = typespec_get_width(declarator->type);
      size_t align = typespec_get_alignment(declarator->type);
      size_t res_count = width / align;
      data->dest.imm.mode = MODE_IMMEDIATE;
      data->dest.imm.val = res_count;
      switch (align) {
        case X64_ALIGNOF_LONG:
          data->opcode = OP_RESQ;
          break;
        case X64_ALIGNOF_INT:
          data->opcode = OP_RESD;
          break;
        case X64_ALIGNOF_SHORT:
          data->opcode = OP_RESW;
          break;
        case X64_ALIGNOF_CHAR:
          data->opcode = OP_RESB;
          break;
        default:
          fprintf(stderr,
                  "ERROR: cannot reserve space for object with alignment %lu\n",
                  align);
          abort();
      }
      assert(!llist_push_back(bss_section, data));
      return 0;
    }
  }
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
