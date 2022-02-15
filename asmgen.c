#include "asmgen.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badalist.h"
#include "debug.h"
#include "lyutils.h"
#include "symtable.h"

#define MAX_INSTRUCTION_LENGTH 8
#define MAX_OP_LENGTH 16
#define MAX_LABEL_LENGTH 32

/* macros used to generate string constants for instructions */
#define FOREACH_INSTRUCTION(GENERATOR) \
  /* arithmetic */                     \
  GENERATOR(ADD)                       \
  GENERATOR(SUB)                       \
  GENERATOR(MUL)                       \
  GENERATOR(DIV)                       \
  GENERATOR(INC)                       \
  GENERATOR(DEC)                       \
  GENERATOR(NEG)                       \
  GENERATOR(IMUL)                      \
  GENERATOR(IDIV)                      \
  /* compare and test */               \
  GENERATOR(TEST)                      \
  GENERATOR(CMP)                       \
  GENERATOR(SETE)                      \
  GENERATOR(SETNE)                     \
  GENERATOR(SETG)                      \
  GENERATOR(SETGE)                     \
  GENERATOR(SETL)                      \
  GENERATOR(SETLE)                     \
  GENERATOR(SETA)                      \
  GENERATOR(SETAE)                     \
  GENERATOR(SETB)                      \
  GENERATOR(SETBE)                     \
  GENERATOR(SETZ)                      \
  GENERATOR(SETNZ)                     \
  /* jump */                           \
  GENERATOR(JMP)                       \
  GENERATOR(JE)                        \
  GENERATOR(JNE)                       \
  GENERATOR(JG)                        \
  GENERATOR(JGE)                       \
  GENERATOR(JL)                        \
  GENERATOR(JLE)                       \
  GENERATOR(JA)                        \
  GENERATOR(JAE)                       \
  GENERATOR(JB)                        \
  GENERATOR(JBE)                       \
  GENERATOR(JZ)                        \
  GENERATOR(JNZ)                       \
  /* logical and bitwise */            \
  GENERATOR(NOT)                       \
  GENERATOR(OR)                        \
  GENERATOR(AND)                       \
  GENERATOR(LEA)                       \
  GENERATOR(XOR)                       \
  /* shifts */                         \
  GENERATOR(SHL)                       \
  GENERATOR(SHR)                       \
  GENERATOR(SAR)                       \
  /* code movement */                  \
  GENERATOR(MOV)                       \
  GENERATOR(MOVZX)                     \
  GENERATOR(MOVSX)                     \
  GENERATOR(PUSH)                      \
  GENERATOR(POP)                       \
  GENERATOR(CALL)                      \
  GENERATOR(LEAVE)                     \
  GENERATOR(RET)                       \
  GENERATOR(NOP)                       \
  /* data section definitions */       \
  GENERATOR(DB)                        \
  GENERATOR(DW)                        \
  GENERATOR(DD)                        \
  GENERATOR(DQ)                        \
  /* bss section definitions */        \
  GENERATOR(RESB)                      \
  GENERATOR(RESW)                      \
  GENERATOR(RESD)                      \
  GENERATOR(RESQ)                      \
  /* pseudo-ops; should not be present \
   * in generated code                 \
   */                                  \
  GENERATOR(POINC)                     \
  GENERATOR(PODEC)                     \
  GENERATOR(MOD)

#define GENERATE_ENUM(ENUM) INSTR_##ENUM,
#define GENERATE_STRING(STRING) #STRING,

typedef struct vreg_entry {
  char *object_name;
  TypeSpec *object_type;
  size_t vreg_num;
  struct llist assigned_vregs;
} VregEntry;

typedef struct instruction_data {
  const char *instruction;
  char label[MAX_LABEL_LENGTH];
  char dest_operand[MAX_OP_LENGTH];
  char src_operand[MAX_OP_LENGTH];
} InstructionData;

typedef enum instruction_enum {
  FOREACH_INSTRUCTION(GENERATE_ENUM)
} InstructionEnum;

/* rbx, rsp, rbp, r12-r15 are preserved across function calls in the System V
 * ABI; Microsoft's ABI additionally preserves rdi and rsi
 */
enum instruction_flag {
  NO_INSTR_FLAGS = 0,
  USE_REG = 1 << 1,   /* result must be placed in a register */
  WANT_ADDR = 1 << 2, /* result should be object address, not value */
};

const char instructions[][MAX_INSTRUCTION_LENGTH] = {
    FOREACH_INSTRUCTION(GENERATE_STRING)};

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and offset
 * is a signed 32-bit integer. Having unnecessary offsets and scales shouldn't
 * affect the validity of the code.
 *
 * stars indicate that field width is an argument
 * TODO(Robert): make field width an argument
 */
static const char INDEX_FMT[] = "[%s+%s*%zu+%i]";
static const char OFFSET_FMT[] = "[%s+%i]";
static const char INDIRECT_FMT[] = "[%s]";
static const char VREG_FMT[] = "vr%zu%c";
static const char BINOP_FMT[] = "%8s%8s %16s, %s\n";
static const char UNOP_FMT[] = "%8s%8s %16s\n";
static const char NULLOP_FMT[] = "%8s%8s\n";
static const char LABEL_FMT[] = "%s: \n";
static const char COND_FMT[] = ".C%zu";
static const char END_FMT[] = ".E%zu";
static const char LOOP_FMT[] = ".L%zu";
static const char BOOL_FMT[] = ".B%zu";
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
static ASTree *current_function;

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
static int translate_stmt(ASTree *stmt);
static int translate_block(ASTree *block);
static int translate_expr(ASTree *tree, InstructionData *data,
                          unsigned int flags);

int assign_space(SymbolValue *symval, const char *location) {
  size_t required_padding =
      symval->type.alignment - (stack_window % symval->type.alignment);
  stack_window += required_padding;
  sprintf(symval->obj_loc, "%s+%zu", location, stack_window);
  stack_window += symval->type.width;
  return 0;
}

/* TODO(Robert): use flags to assign specific numbers for instructions which
 * use specific registers as their source or destination to make register
 * allocation easier
 */
int assign_vreg(const TypeSpec *type, char *dest, const size_t vreg_num) {
  char reg_width = 0;

  if (type->base == TYPE_VOID) {
    /* do not attempt to assign vreg to void type */
    dest[0] = 0;
    return 0;
  } else {
    switch (type->width) {
      case X64_SIZEOF_LONG:
        reg_width = 'q';
        break;
      case X64_SIZEOF_INT:
        reg_width = 'd';
        break;
      case X64_SIZEOF_SHORT:
        reg_width = 'w';
        break;
      case X64_SIZEOF_CHAR:
        reg_width = 'b';
        break;
      default:
        fprintf(stderr, "ERROR: unable to assign vreg of width %zu\n",
                type->width);
        return -1;
        break;
    }
  }

  sprintf(dest, VREG_FMT, vreg_num, reg_width);
  return 0;
}

int resolve_object(const char *ident, char *dest, const char *fmt) {
  SymbolValue *symval = NULL;
  locate_symbol((void *)ident, strlen(ident), &symval);
  if (symval == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n", ident);
    return -1;
  }

  sprintf(dest, fmt, symval->obj_loc);
  return 0;
}

int save_registers(size_t start, size_t count) {
  size_t i;
  for (i = 0; i < count; ++i) {
    DEBUGS('g', "Saving register %lu to stack", start + i);
    InstructionData *data = calloc(1, sizeof(InstructionData));
    data->instruction = instructions[INSTR_PUSH];
    int status = assign_vreg(&SPEC_ULONG, data->src_operand, start + i);
    llist_push_back(text_section, data);
  }
  return 0;
}

int restore_registers(size_t start, size_t count) {
  size_t i;
  for (i = 1; i <= count; ++i) {
    DEBUGS('g', "Restoring register %lu from stack", start + (count - i));
    InstructionData *data = calloc(1, sizeof(InstructionData));
    data->instruction = instructions[INSTR_POP];
    int status =
        assign_vreg(&SPEC_ULONG, data->src_operand, start + (count - i));
    llist_push_back(text_section, data);
  }
  return 0;
}

int translate_ident(ASTree *ident, InstructionData *data, unsigned int flags) {
  int status = resolve_object(ident->lexinfo, data->src_operand, INDIRECT_FMT);
  if (status) return status;
  if (flags & WANT_ADDR) {
    data->instruction = instructions[INSTR_LEA];
    /* TODO(Robert): define pointer type constant */
    int status = assign_vreg(&SPEC_ULONG, data->dest_operand, vreg_count++);
    if (status) return status;
  } else {
    data->instruction = instructions[INSTR_MOV];
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
int translate_conversion(ASTree *operator, InstructionData * data,
                         unsigned int flags) {
  DEBUGS('g', "Translating conversion");

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  ASTree *converted_expr = NULL;
  if (astree_count(operator) == 1) {
    converted_expr = astree_first(operator);
  } else if (astree_count(operator) == 2) {
    converted_expr = astree_second(operator);
  } else if (astree_count(operator) == 3) {
    converted_expr = astree_third(operator);
  } else {
    fprintf(
        stderr,
        "ERROR: unable to perform conversion on node which has %zu children\n",
        astree_count(operator));
    return -1;
  }

  int status = translate_expr(converted_expr, src_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, src_data);
  strcpy(data->src_operand, src_data->dest_operand);
  status = assign_vreg(operator->type, data->dest_operand, vreg_count++);
  if (status) return status;

  const TypeSpec *target_type = operator->type;
  const TypeSpec *source_type = converted_expr->type;

  if (source_type->width > target_type->width) {
    data->instruction = instructions[INSTR_MOV];
  } else if (source_type->width == target_type->width) {
    data->instruction = instructions[INSTR_NOP];
  } else if (source_type->base == TYPE_SIGNED) {
    if (target_type->base == TYPE_SIGNED) {
      data->instruction = instructions[INSTR_MOVSX];
    } else if (target_type->base == TYPE_UNSIGNED) {
      data->instruction = instructions[INSTR_MOVZX];
    }
  } else if (source_type->base == TYPE_UNSIGNED) {
    data->instruction = instructions[INSTR_MOVZX];
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
    data->instruction = instructions[INSTR_MOV];
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
int translate_logical_not(ASTree *tree, InstructionData *data) {
  /* TODO(Robert): add attribute indicating when the result of an expression
   * is boolean so that we can skip doing all of this if we don't need to, or
   * maybe even add a whole boolean type, though that may be overkill
   */

  /* evaluate operand */
  InstructionData *tree_data = calloc(1, sizeof(InstructionData));
  int status = translate_expr(tree, tree_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, tree_data);

  /* TEST operand with itself */
  InstructionData *test_data = calloc(1, sizeof(InstructionData));
  test_data->instruction = instructions[INSTR_TEST];
  strcpy(test_data->dest_operand, tree_data->dest_operand);
  strcpy(test_data->src_operand, tree_data->dest_operand);
  llist_push_back(text_section, test_data);

  data->instruction = instructions[INSTR_SETZ];
  return assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
}

int translate_logical(ASTree *operator, InstructionData * data,
                      InstructionEnum num, unsigned int flags) {
  /* create label used to skip second operand */
  InstructionData *label_data = calloc(1, sizeof(InstructionData));
  sprintf(label_data->label, BOOL_FMT, branch_count++);

  /* test first operand; jump on false for && and true for || */
  InstructionData *first_data = calloc(1, sizeof(InstructionData));
  int status = translate_expr(astree_first(operator), first_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, first_data);

  InstructionData *test_first_data = calloc(1, sizeof(InstructionData));
  test_first_data->instruction = instructions[INSTR_TEST];
  strcpy(test_first_data->dest_operand, first_data->dest_operand);
  strcpy(test_first_data->src_operand, first_data->dest_operand);
  llist_push_back(text_section, test_first_data);

  InstructionData *jmp_first_data = calloc(1, sizeof(InstructionData));
  if (operator->symbol == TOK_AND)
    jmp_first_data->instruction = instructions[INSTR_JZ];
  else if (operator->symbol == TOK_OR)
    jmp_first_data->instruction = instructions[INSTR_JNZ];
  strcpy(jmp_first_data->dest_operand, label_data->label);
  llist_push_back(text_section, jmp_first_data);

  /* test second operand; no need to jump afterwards */
  InstructionData *second_data = calloc(1, sizeof(InstructionData));
  status = translate_expr(astree_second(operator), second_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, second_data);

  InstructionData *test_second_data = calloc(1, sizeof(InstructionData));
  test_second_data->instruction = instructions[INSTR_TEST];
  strcpy(test_second_data->dest_operand, second_data->dest_operand);
  strcpy(test_second_data->src_operand, second_data->dest_operand);
  llist_push_back(text_section, test_second_data);

  /* write label used to skip second operand */
  llist_push_back(text_section, label_data);

  /* result will always be the truth value of the last evaluated expression */
  data->instruction = instructions[INSTR_SETNZ];
  return assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
}

/* TODO(Robert): check location of result of first subexpression, and require
 * second subexpression to place result in a register if the first was not
 */
int translate_comparison(ASTree *operator, InstructionData * data,
                         InstructionEnum num, unsigned int flags) {
  /* CMP operands, then SETG/SETGE/SETL/SETLE/SETE/SETNE */
  InstructionData *first_data = calloc(1, sizeof(*first_data));
  int status =
      translate_expr(astree_first(operator), first_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, first_data);

  InstructionData *second_data = calloc(1, sizeof(*second_data));
  status = translate_expr(astree_second(operator), second_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, second_data);

  InstructionData *cmp_data = calloc(1, sizeof(InstructionData));
  cmp_data->instruction = instructions[INSTR_CMP];
  strcpy(cmp_data->dest_operand, first_data->dest_operand);
  strcpy(cmp_data->src_operand, second_data->dest_operand);
  llist_push_back(text_section, cmp_data);

  data->instruction = instructions[num];
  status = assign_vreg(&SPEC_INT, data->dest_operand, vreg_count++);
  return status;
}

int translate_indirection(ASTree *indirection, InstructionData *data,
                          unsigned int flags) {
  DEBUGS('g', "Translating indirection operation.");
  InstructionData *src_data = calloc(1, sizeof(*src_data));
  int status = translate_expr(astree_first(indirection), src_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, src_data);

  sprintf(data->src_operand, INDIRECT_FMT, src_data->dest_operand);
  status = assign_vreg(indirection->type, data->dest_operand, vreg_count++);
  if (status) return status;
  data->instruction = instructions[INSTR_MOV];
  return 0;
}

int translate_addrof(ASTree *addrof, InstructionData *data,
                     unsigned int flags) {
  DEBUGS('g', "Translating address operation.");
  /* TODO(Robert): handle other types of lvalues, like struct and union
   * members
   */
  return translate_expr(astree_first(addrof), data, WANT_ADDR);
}

int translate_inc_dec(ASTree *operator, InstructionData * data,
                      InstructionEnum num, unsigned int flags) {
  DEBUGS('g', "Translating increment/decrement: %s", instructions[num]);
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

  int status = translate_expr(astree_first(operator), mov_data, NO_INSTR_FLAGS);
  if (status) return status;

  inc_dec_data->instruction = instructions[num];
  strcpy(inc_dec_data->dest_operand, mov_data->src_operand);

  llist_push_back(text_section, to_push_data);
  return 0;
}

int translate_unop(ASTree *operator, InstructionData * data,
                   InstructionEnum num, unsigned int flags) {
  DEBUGS('g', "Translating unary operation: %s", instructions[num]);
  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  /* put value in register so that it is not modified in place */
  int status = translate_expr(astree_first(operator), dest_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  data->instruction = instructions[num];
  strcpy(data->dest_operand, dest_data->dest_operand);
  return 0;
}

int translate_binop(ASTree *operator, InstructionData * data,
                    InstructionEnum num, unsigned int flags) {
  DEBUGS('g', "Translating binary operation: %s", instructions[num]);
  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  int status = translate_expr(astree_first(operator), dest_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  status = translate_expr(astree_second(operator), src_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, src_data);

  strcpy(data->dest_operand, dest_data->dest_operand);
  strcpy(data->src_operand, src_data->dest_operand);
  data->instruction = instructions[num];
  return 0;
}

int translate_mul_div_mod(ASTree *operator, InstructionData * data,
                          InstructionEnum num, unsigned int flags) {
  /* TODO(Robert): designate vregs for quotient/lo bits and remainder/hi bits
   */
  if (operator->symbol == '%') {
    /* return remainder instead of quotient */
    InstructionData *remainder_data = calloc(1, sizeof(*remainder_data));
    int status = translate_binop(operator, remainder_data, num, flags);
    if (status) return status;
    llist_push_back(text_section, remainder_data);
    data->instruction = instructions[INSTR_NOP];
    return assign_vreg(operator->type, data->dest_operand, vreg_count++);
  } else {
    int status = translate_binop(operator, data, num, flags);
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
    casted_expr = astree_first(cast);
  else
    casted_expr = astree_second(cast);
  return 0;
}

int translate_assignment(ASTree *assignment, InstructionData *data,
                         unsigned int flags) {
  DEBUGS('g', "Translating assignment");

  InstructionData *src_data = calloc(1, sizeof(*src_data));
  /* place result in register since destination will be a memory location */
  int status = translate_expr(astree_second(assignment), src_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, src_data);

  InstructionData *dest_data = calloc(1, sizeof(*dest_data));
  status = translate_expr(astree_first(assignment), dest_data, NO_INSTR_FLAGS);
  if (status) return status;
  llist_push_back(text_section, dest_data);

  strcpy(data->dest_operand, dest_data->dest_operand);
  strcpy(data->src_operand, src_data->dest_operand);
  data->instruction = instructions[INSTR_MOV];

  return 0;
}

int translate_subscript(ASTree *subscript, InstructionData *data,
                        unsigned int flags) {
  DEBUGS('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the offset and
   * scale addressing mode can be used
   */
  InstructionData *pointer_data = calloc(1, sizeof(*pointer_data));
  int status = translate_expr(astree_first(subscript), pointer_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, pointer_data);

  InstructionData *index_data = calloc(1, sizeof(*index_data));
  status = translate_expr(astree_second(subscript), index_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, pointer_data);

  data->instruction = instructions[INSTR_MOV];
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
int translate_reference(ASTree *reference, InstructionData *data,
                        unsigned int flags) {
  DEBUGS('g', "Translating reference operator");
  ASTree *struct_ = astree_first(reference);
  InstructionData *struct_data = calloc(1, sizeof(*struct_data));
  unsigned int struct_flags =
      reference->symbol == TOK_ARROW ? USE_REG : USE_REG | WANT_ADDR;
  int status = translate_expr(struct_, struct_data, struct_flags);
  if (status) return status;

  const AuxSpec *struct_aux = reference->symbol == TOK_ARROW
                                  ? llist_get(&struct_->type->auxspecs, 1)
                                  : llist_front(&struct_->type->auxspecs);
  if (struct_aux->aux == AUX_STRUCT) {
    if (flags & WANT_ADDR)
      data->instruction = instructions[INSTR_LEA];
    else
      data->instruction = instructions[INSTR_MOV];
    const char *member_name = astree_second(reference)->lexinfo;
    size_t member_name_len = strlen(member_name);
    SymbolValue *member_symbol =
        map_get(struct_aux->data.composite.symbol_table, (char *)member_name,
                member_name_len);
    char temp[MAX_OP_LENGTH];
    sprintf(temp, member_symbol->obj_loc, struct_data->dest_operand);
    sprintf(data->src_operand, INDIRECT_FMT, temp);
    return assign_vreg(reference->type, data->dest_operand, vreg_count++);
  } else if (flags & WANT_ADDR) {
    /* use nop to communicate address to parent expression */
    data->instruction = instructions[INSTR_NOP];
    strcpy(data->dest_operand, struct_data->dest_operand);
    return 0;
  } else {
    data->instruction = instructions[INSTR_MOV];
    sprintf(data->src_operand, INDIRECT_FMT, struct_data->dest_operand);
    return assign_vreg(reference->type, data->dest_operand, vreg_count++);
  }
}

int translate_call(ASTree *call, InstructionData *data, unsigned int flags) {
  DEBUGS('g', "Translating function call");
  size_t i;
  for (i = 1; i < astree_count(call); ++i) {
    DEBUGS('g', "Translating parameter %i", i);
    /* compute parameter */
    ASTree *param = astree_get(call, i);
    InstructionData *param_data = calloc(1, sizeof(*param_data));
    int status = translate_expr(param, param_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, param_data);

    /* TODO(Robert): temporarily restrict number of arguments to 4 until I
     * have implemented passing subroutine parameters on the stack
     */
    /* mov parameter to argument register */
    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->instruction = instructions[INSTR_MOV];
    status = assign_vreg(param->type, mov_data->dest_operand, i);
    if (status) return status;
    strcpy(mov_data->src_operand, param_data->dest_operand);
    llist_push_back(text_section, mov_data);
  }

  int status = save_registers(VOLATILE_START, VOLATILE_COUNT);
  if (status) return status;

  InstructionData *call_data = calloc(1, sizeof(*call_data));
  call_data->instruction = instructions[INSTR_CALL];
  strcpy(call_data->dest_operand, astree_first(call)->lexinfo);
  llist_push_back(text_section, call_data);

  /* mov result to any other register if return type isn't void */
  if (call->type->base != TYPE_VOID) {
    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->instruction = instructions[INSTR_MOV];
    int status = assign_vreg(call->type, mov_data->src_operand, RETURN_VREG);
    if (status) return status;
    status = assign_vreg(call->type, mov_data->dest_operand, vreg_count++);
    if (status) return status;
    llist_push_back(text_section, mov_data);
    strcpy(data->dest_operand, mov_data->dest_operand);
  }

  /* only give the parent recursive call the result register, if applicable */
  data->instruction = instructions[INSTR_NOP];

  return restore_registers(VOLATILE_START, VOLATILE_COUNT);
}

int translate_param(ASTree *param, InstructionData *data) {
  DEBUGS('g', "Translating parameter");
  ASTree *declarator = astree_second(param);
  ASTree *param_ident = extract_ident(declarator);
  int status = assign_vreg(param_ident->type, data->src_operand, vreg_count++);
  if (status) return status;
  SymbolValue *symval = NULL;
  locate_symbol((void *)param_ident->lexinfo, strlen(param_ident->lexinfo),
                &symval);
  if (symval == NULL) {
    fprintf(stderr, "ERROR: unable to resolve symbol %s\n",
            param_ident->lexinfo);
    return -1;
  }
  status = assign_space(symval, STACK_POINTER_STRING);
  if (status) return status;
  status =
      resolve_object(param_ident->lexinfo, data->dest_operand, INDIRECT_FMT);
  if (status) return status;
  data->instruction = instructions[INSTR_MOV];
  return 0;
}

int translate_list_initialization(ASTree *declarator, ASTree *init_list) {
  DEBUGS('g', "Transating struct initiazation by initializer list");
  InstructionData *struct_data = calloc(1, sizeof(InstructionData));
  struct_data->instruction = instructions[INSTR_LEA];
  ASTree *struct_ident = extract_ident(declarator);
  int status = resolve_object(struct_ident->lexinfo, struct_data->dest_operand,
                              INDIRECT_FMT);
  if (status) return status;
  const TypeSpec *struct_type = extract_type(declarator);
  status = assign_vreg(&SPEC_ULONG, struct_data->dest_operand, vreg_count++);
  if (status) return status;
  llist_push_back(text_section, struct_data);

  AuxSpec *struct_aux = llist_back(&struct_type->auxspecs);
  LinkedList *member_symbols = &struct_aux->data.composite.members;
  size_t i;
  for (i = 0; i < llist_size(member_symbols); ++i) {
    if (i < astree_count(init_list)) {
      ASTree *initializer = astree_get(init_list, i);
      InstructionData *initializer_data = calloc(1, sizeof(InstructionData));
      int status = translate_expr(initializer, initializer_data, USE_REG);
      if (status) return status;

      SymbolValue *member_symbol = llist_get(member_symbols, i);
      char temp[MAX_OP_LENGTH];
      sprintf(temp, member_symbol->obj_loc, struct_data->dest_operand);
      InstructionData *assignment_data = calloc(1, sizeof(InstructionData));
      assignment_data->instruction = instructions[INSTR_MOV];
      sprintf(initializer_data->dest_operand, INDIRECT_FMT, temp);
      strcpy(assignment_data->src_operand, initializer_data->dest_operand);
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

int translate_local_decl(ASTree *declaration, InstructionData *data) {
  DEBUGS('g', "Translating local declaration");
  size_t i;
  /* skip typespec list */
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *declarator = astree_get(declaration, i);
    ASTree *ident = extract_ident(declarator);
    SymbolValue *symval = NULL;
    locate_symbol((char *)ident->lexinfo, strlen(ident->lexinfo), &symval);
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
        int status = translate_list_initialization(declarator, next_child);
        if (status) return status;
        ++i;
      } else if (next_child->symbol != TOK_DECLARATOR) {
        int status =
            resolve_object(ident->lexinfo, data->dest_operand, INDIRECT_FMT);
        if (status) return status;
        InstructionData *value_data = calloc(1, sizeof(*value_data));
        status = translate_expr(next_child, value_data, USE_REG);
        if (status) return status;
        llist_push_back(text_section, value_data);

        strcpy(data->src_operand, value_data->dest_operand);
        data->instruction = instructions[INSTR_MOV];
        ++i;
      }
    }
  }
  return 0;
}

static int translate_expr(ASTree *tree, InstructionData *out,
                          unsigned int flags) {
  int status = 0;

  /* TODO(Robert): make a mapping from symbols to instructions so that most
   * of the case statements can be collapsed together
   */
  switch (tree->symbol) {
    /* arithmetic operators */
    case '+':
      status = translate_binop(tree, out, INSTR_ADD, flags);
      break;
    case '-':
      status = translate_binop(tree, out, INSTR_SUB, flags);
      break;
    case '*':
      if (astree_first(tree)->type->base == TYPE_SIGNED)
        status = translate_mul_div_mod(tree, out, INSTR_IMUL, flags);
      else
        status = translate_mul_div_mod(tree, out, INSTR_MUL, flags);
      break;
    case '/':
    case '%':
      if (astree_first(tree)->type->base == TYPE_SIGNED)
        status = translate_mul_div_mod(tree, out, INSTR_IDIV, flags);
      else
        status = translate_mul_div_mod(tree, out, INSTR_DIV, flags);
      break;
    case TOK_INC:
    case TOK_POST_INC:
      status = translate_inc_dec(tree, out, INSTR_INC, flags);
      break;
    case TOK_DEC:
    case TOK_POST_DEC:
      status = translate_inc_dec(tree, out, INSTR_DEC, flags);
      break;
    case TOK_NEG:
      status = translate_unop(tree, out, INSTR_NEG, flags);
      break;
    case TOK_POS:
      status = translate_conversion(tree, out, flags);
      break;
    /* bitwise operators */
    case '&':
      status = translate_binop(tree, out, INSTR_AND, flags);
      break;
    case '|':
      status = translate_binop(tree, out, INSTR_OR, flags);
      break;
    case '^':
      status = translate_binop(tree, out, INSTR_XOR, flags);
      break;
    case '~':
      status = translate_unop(tree, out, INSTR_NOT, flags);
      break;
    /* shifts */
    case TOK_SHL:
      status = translate_binop(tree, out, INSTR_SHL, flags);
      break;
    case TOK_SHR:
      if (astree_first(tree)->type->base == TYPE_SIGNED)
        status = translate_binop(tree, out, INSTR_SAR, flags);
      else
        status = translate_binop(tree, out, INSTR_SHR, flags);
      break;
    /* comparison operators */
    case '>':
      status = translate_comparison(tree, out, INSTR_SETG, flags);
      break;
    case TOK_GE:
      status = translate_comparison(tree, out, INSTR_SETGE, flags);
      break;
    case '<':
      status = translate_comparison(tree, out, INSTR_SETL, flags);
      break;
    case TOK_LE:
      status = translate_comparison(tree, out, INSTR_SETLE, flags);
      break;
    case TOK_EQ:
      status = translate_comparison(tree, out, INSTR_SETE, flags);
      break;
    case TOK_NE:
      status = translate_comparison(tree, out, INSTR_SETNE, flags);
      break;
    /* logical operators */
    case '!':
      status = translate_logical_not(astree_first(tree), out);
      break;
    case TOK_AND:
      status = translate_logical(tree, out, INSTR_AND, flags);
      break;
    case TOK_OR:
      status = translate_logical(tree, out, INSTR_OR, flags);
      break;
    /* constants */
    case TOK_INTCON:
      status = translate_intcon(tree, out, flags);
      break;
    case TOK_CHARCON:
      break;
    /* miscellaneous */
    case TOK_ADDROF:
      status = translate_addrof(tree, out, flags);
      break;
    case TOK_INDIRECTION:
      status = translate_indirection(tree, out, flags);
      break;
    case TOK_IDENT:
      status = translate_ident(tree, out, flags);
      break;
    case '=':
      status = translate_assignment(tree, out, flags);
      break;
    case TOK_CAST:
      status = translate_conversion(tree, out, flags);
      break;
    case TOK_CALL:
      status = translate_call(tree, out, flags);
      break;
    case TOK_SUBSCRIPT:
      status = translate_subscript(tree, out, flags);
      break;
    case '.':
    case TOK_ARROW:
      status = translate_reference(tree, out, flags);
      break;
    default:
      fprintf(stderr, "ERROR: Unimplemented token: %s, lexinfo: %s\n",
              parser_get_tname(tree->symbol), tree->lexinfo);
      status = -1;
      break;
  }
  return status;
}

static int translate_ifelse(ASTree *ifelse) {
  size_t current_branch = branch_count++;
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  int status = translate_expr(astree_first(ifelse), cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is zero and jump if it is */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->instruction = instructions[INSTR_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);

  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->instruction = instructions[INSTR_JZ];
  sprintf(test_jmp_data->dest_operand, END_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  /* translate if body */
  status = translate_stmt(astree_second(ifelse));
  if (status) return status;
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  /* translate else body if present */
  if (astree_count(ifelse) == 3) {
    translate_stmt(astree_third(ifelse));
  }
  return 0;
}

static int translate_while(ASTree *while_) {
  size_t current_branch = branch_count++;
  /* emit label at beginning of condition */
  InstructionData *cond_label = calloc(1, sizeof(*cond_label));
  sprintf(cond_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, cond_label);
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  int status = translate_expr(astree_first(while_), cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is zero and jump if it is */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->instruction = instructions[INSTR_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);

  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->instruction = instructions[INSTR_JZ];
  sprintf(test_jmp_data->dest_operand, END_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  /* translate while body */
  status = translate_stmt(astree_second(while_));
  if (status) return status;
  /* emit jump to condition */
  InstructionData *cond_jmp_data = calloc(1, sizeof(*cond_jmp_data));
  cond_jmp_data->instruction = instructions[INSTR_JMP];
  sprintf(cond_jmp_data->dest_operand, COND_FMT, current_branch);
  llist_push_back(text_section, cond_jmp_data);
  /* emit label at end of statement */
  InstructionData *end_label = calloc(1, sizeof(*end_label));
  sprintf(end_label->label, END_FMT, current_branch);
  llist_push_back(text_section, end_label);
  return 0;
}

static int translate_do(ASTree *do_) {
  size_t current_branch = branch_count++;
  /* emit label at beginning of body */
  InstructionData *body_label = calloc(1, sizeof(*body_label));
  sprintf(body_label->label, COND_FMT, current_branch);
  llist_push_back(text_section, body_label);
  /* translate body */
  int status = translate_stmt(astree_first(do_));
  if (status) return status;
  /* translate conditional expression */
  InstructionData *cond_data = calloc(1, sizeof(*cond_data));
  status = translate_expr(astree_second(do_), cond_data, USE_REG);
  if (status) return status;
  llist_push_back(text_section, cond_data);
  /* check if condition is one and jump if it is */
  InstructionData *test_data = calloc(1, sizeof(*test_data));
  test_data->instruction = instructions[INSTR_TEST];
  strcpy(test_data->dest_operand, cond_data->dest_operand);
  strcpy(test_data->src_operand, cond_data->dest_operand);
  llist_push_back(text_section, test_data);

  InstructionData *test_jmp_data = calloc(1, sizeof(*test_jmp_data));
  test_jmp_data->instruction = instructions[INSTR_JNZ];
  sprintf(test_jmp_data->dest_operand, COND_FMT, current_branch);
  llist_push_back(text_section, test_jmp_data);
  return 0;
}

static int translate_block(ASTree *block) {
  DEBUGS('g', "Translating compound statement");
  LinkedList *stmts = &block->children;
  size_t i;
  for (i = 0; i < stmts->size; ++i) {
    ASTree *stmt = llist_get(stmts, i);
    int status = translate_stmt(stmt);
    if (status) return status;
  }
  return 0;
}

int translate_return(ASTree *ret, InstructionData *data) {
  DEBUGS('g', "Translating return statement");

  if (astree_count(ret) > 0) {
    InstructionData *value_data = calloc(1, sizeof(*value_data));
    int status = translate_expr(astree_first(ret), value_data, NO_INSTR_FLAGS);
    if (status) return status;
    llist_push_back(text_section, value_data);

    InstructionData *mov_data = calloc(1, sizeof(*mov_data));
    mov_data->instruction = instructions[INSTR_MOV];
    strcpy(mov_data->src_operand, value_data->dest_operand);

    const TypeSpec *function_spec = extract_type(current_function);
    TypeSpec return_spec = SPEC_EMPTY;
    /* strip pointer */
    status = strip_aux_type(&return_spec, function_spec);
    if (status) return status;
    /* strip function */
    /* TODO(Robert): free temporary copies created by the first strip */
    status = strip_aux_type(&return_spec, function_spec);
    status = assign_vreg(&return_spec, mov_data->dest_operand, RETURN_VREG);
    if (status) return status;

    llist_push_back(text_section, mov_data);
  }

  /* restore non-volatile registers */
  int status = restore_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  data->instruction = instructions[INSTR_RET];
  return 0;
}

static int translate_stmt(ASTree *stmt) {
  InstructionData *data;
  int status = 0;

  /* TODO(Robert): add badlib function to verify that a data structure is
   * valid
   */
  if (stmt->symbol_table.buckets) enter_scope(&stmt->symbol_table);
  switch (stmt->symbol) {
    case TOK_BLOCK:
      status = translate_block(stmt);
      break;
    case TOK_RETURN:
      data = calloc(1, sizeof(*data));
      status = translate_return(stmt, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    case TOK_WHILE:
      status = translate_while(stmt);
      break;
    case TOK_DO:
      status = translate_do(stmt);
      break;
    case TOK_IF:
      status = translate_ifelse(stmt);
      break;
    case TOK_DECLARATION:
      data = calloc(1, sizeof(*data));
      status = translate_local_decl(stmt, data);
      if (status) break;
      llist_push_back(text_section, data);
      break;
    default:
      data = calloc(1, sizeof(*data));
      status = translate_expr(stmt, data, 0);
      if (status) break;
      llist_push_back(text_section, data);
      break;
  }

  if (stmt->symbol_table.buckets) leave_scope(&stmt->symbol_table);
  return status;
}

int translate_global_decl(ASTree *declaration, InstructionData *data) {
  DEBUGS('g', "Translating global declaration");
  size_t i;
  /* skip typespec list */
  for (i = 1; i < astree_count(declaration); ++i) {
    ASTree *declarator = astree_get(declaration, i);
    ASTree *ident = extract_ident(declarator);
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
        astree_get(declaration, i + 1)->symbol != TOK_DECLARATOR) {
      /* put in data section */
      ASTree *init_value = astree_get(declarator, ++i);
      strcpy(data->dest_operand, "COMPILE-TIME CONSTANT");

      switch (ident->type->width) {
        case X64_SIZEOF_LONG:
          data->instruction = instructions[INSTR_DQ];
          break;
        case X64_SIZEOF_INT:
          data->instruction = instructions[INSTR_DD];
          break;
        case X64_SIZEOF_SHORT:
          data->instruction = instructions[INSTR_DW];
          break;
        case X64_SIZEOF_CHAR:
          data->instruction = instructions[INSTR_DB];
          break;
        default:
          fprintf(stderr,
                  "ERROR: unable to determine instruction for initialized"
                  " data of width %zu for symbol %s\n",
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
          data->instruction = instructions[INSTR_RESQ];
          break;
        case X64_SIZEOF_INT:
          data->instruction = instructions[INSTR_RESD];
          break;
        case X64_SIZEOF_SHORT:
          data->instruction = instructions[INSTR_RESW];
          break;
        case X64_SIZEOF_CHAR:
          data->instruction = instructions[INSTR_RESB];
          break;
        default:
          fprintf(stderr,
                  "ERROR: unable to determine instruction for uninitialized"
                  " data of width %zu for symbol %s\n",
                  ident->type->width, ident->lexinfo);
          return -1;
          break;
      }
    }
  }
  return 0;
}

int translate_function(ASTree *function, InstructionData *data) {
  DEBUGS('g', "Translating function definition");
  ASTree *declarator = astree_second(function);
  ASTree *name_node = extract_ident(declarator);
  current_function = name_node;
  strcpy(data->label, name_node->lexinfo);
  int status = save_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  size_t i;
  ASTree *params = astree_second(function);
  ASTree *body = astree_third(function);
  /* cleanup vregs from last function and skip return reg since we don't need
   * to save or store it
   */
  vreg_count = 1;
  /* enter function parameter/body scope briefly to handle parameters */
  enter_scope(&name_node->symbol_table);
  /* last direct declarator should be parent of parameter nodes */
  ASTree *function_dirdecl =
      astree_get(declarator, astree_count(declarator) - 1);
  for (i = 0; i < astree_count(function_dirdecl); ++i) {
    ASTree *param = astree_get(function_dirdecl, i);
    InstructionData *param_data = calloc(1, sizeof(*param_data));
    int status = translate_param(param, param_data);
    if (status) return status;
    llist_push_back(text_section, param_data);
  }
  leave_scope(&name_node->symbol_table);

  /* start vreg counter outside of numbers which can be automatically mapped
   * to real registers to eliminate duplicates and clobbered values when
   * saving and restoring registers
   */
  vreg_count = VOLATILE_COUNT + NONVOLATILE_COUNT;
  status = translate_stmt(astree_third(function));
  if (status) return status;
  status = restore_registers(NONVOLATILE_START, NONVOLATILE_COUNT);
  if (status) return status;

  /* insert return in case function did not have one */
  InstructionData *return_data = calloc(1, sizeof(*return_data));
  return_data->instruction = instructions[INSTR_RET];
  llist_push_back(text_section, return_data);
  return 0;
}

int translate_file(ASTree *root) {
  size_t i;
  enter_scope(&root->symbol_table);
  for (i = 0; i < astree_count(root); ++i) {
    ASTree *topdecl = astree_get(root, i);
    if (topdecl->symbol == TOK_DECLARATION) {
      ASTree *declarator = astree_second(topdecl);
      if (typespec_is_function(extract_type(declarator))) {
        /* skip if this is a function declaration, not definition */
        if (astree_count(topdecl) == 3) {
          InstructionData *label_data = calloc(1, sizeof(*label_data));
          /* put label before function body */
          llist_push_back(text_section, label_data);
          int status = translate_function(topdecl, label_data);
          if (status) return status;
        }
      } else {
        InstructionData *global_data = calloc(1, sizeof(*global_data));
        int status = translate_global_decl(topdecl, global_data);
        llist_push_back(data_section, global_data);
        if (status) return status;
      }
    } else if (topdecl->symbol == TOK_STRUCT || topdecl->symbol == TOK_UNION) {
      /* nothing to do here; member location should already have been set */
    } else {
      fprintf(stderr, "ERROR: unrecognized symbol %s at top level\n",
              parser_get_tname(topdecl->symbol));
      return -1;
    }
  }

  leave_scope(&root->symbol_table);
  return 0;
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

  if (data->instruction == NULL && strlen(data->label) > 0) {
    /* only a label */
    fprintf(out, LABEL_FMT, data->label);
  } else if (data->instruction != NULL) {
    /* instruction, possible with a label */
    int instruction_type =
        (!!data->dest_operand[0]) | (!!data->src_operand[0] << 1);
    if (data->instruction == instructions[INSTR_NOP]) instruction_type = 0;
    switch (instruction_type) {
      case 0:
        /* nullary */
        fprintf(out, NULLOP_FMT, data->label, data->instruction);
        break;
      case 1:
        /* unary, destination */
        fprintf(out, UNOP_FMT, data->label, data->instruction,
                data->dest_operand);
        break;
      case 2:
        /* unary, source */
        fprintf(out, UNOP_FMT, data->label, data->instruction,
                data->src_operand);
        break;
      case 3:
        /* binary operation */
        fprintf(out, BINOP_FMT, data->label, data->instruction,
                data->dest_operand, data->src_operand);
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
