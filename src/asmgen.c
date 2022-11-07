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
#define MAX_OPERAND_LENGTH 64
#define MAX_LABEL_LENGTH 64
#define NO_DISP 0

/* macros used to generate string constants for OPCODES */
#define FOREACH_OPCODE(GENERATOR)         \
  GENERATOR(INVALID, OPTYPE_INVALID, 0)   \
  /* arithmetic */                        \
  GENERATOR(ADD, OPTYPE_BINARY, 1)        \
  GENERATOR(SUB, OPTYPE_BINARY, 1)        \
  GENERATOR(MUL, OPTYPE_BINARY, 1)        \
  GENERATOR(DIV, OPTYPE_BINARY, 1)        \
  GENERATOR(INC, OPTYPE_UNARY, 1)         \
  GENERATOR(DEC, OPTYPE_UNARY, 1)         \
  GENERATOR(NEG, OPTYPE_UNARY, 1)         \
  GENERATOR(IMUL, OPTYPE_BINARY, 1)       \
  GENERATOR(IDIV, OPTYPE_BINARY, 1)       \
  /* compare and test */                  \
  GENERATOR(TEST, OPTYPE_BINARY, 1)       \
  GENERATOR(CMP, OPTYPE_BINARY, 1)        \
  GENERATOR(SETE, OPTYPE_UNARY, 0)        \
  GENERATOR(SETNE, OPTYPE_UNARY, 0)       \
  GENERATOR(SETG, OPTYPE_UNARY, 0)        \
  GENERATOR(SETGE, OPTYPE_UNARY, 0)       \
  GENERATOR(SETL, OPTYPE_UNARY, 0)        \
  GENERATOR(SETLE, OPTYPE_UNARY, 0)       \
  GENERATOR(SETA, OPTYPE_UNARY, 0)        \
  GENERATOR(SETAE, OPTYPE_UNARY, 0)       \
  GENERATOR(SETB, OPTYPE_UNARY, 0)        \
  GENERATOR(SETBE, OPTYPE_UNARY, 0)       \
  GENERATOR(SETZ, OPTYPE_UNARY, 0)        \
  GENERATOR(SETNZ, OPTYPE_UNARY, 0)       \
  /* jump */                              \
  GENERATOR(JMP, OPTYPE_UNARY, 0)         \
  GENERATOR(JE, OPTYPE_UNARY, 0)          \
  GENERATOR(JNE, OPTYPE_UNARY, 0)         \
  GENERATOR(JG, OPTYPE_UNARY, 0)          \
  GENERATOR(JGE, OPTYPE_UNARY, 0)         \
  GENERATOR(JL, OPTYPE_UNARY, 0)          \
  GENERATOR(JLE, OPTYPE_UNARY, 0)         \
  GENERATOR(JA, OPTYPE_UNARY, 0)          \
  GENERATOR(JAE, OPTYPE_UNARY, 0)         \
  GENERATOR(JB, OPTYPE_UNARY, 0)          \
  GENERATOR(JBE, OPTYPE_UNARY, 0)         \
  GENERATOR(JZ, OPTYPE_UNARY, 0)          \
  GENERATOR(JNZ, OPTYPE_UNARY, 0)         \
  /* logical and bitwise */               \
  GENERATOR(NOT, OPTYPE_UNARY, 1)         \
  GENERATOR(OR, OPTYPE_BINARY, 1)         \
  GENERATOR(AND, OPTYPE_BINARY, 1)        \
  GENERATOR(LEA, OPTYPE_BINARY, 1)        \
  GENERATOR(XOR, OPTYPE_BINARY, 1)        \
  /* shifts */                            \
  GENERATOR(SHL, OPTYPE_BINARY, 1)        \
  GENERATOR(SAL, OPTYPE_BINARY, 1)        \
  GENERATOR(SHR, OPTYPE_BINARY, 1)        \
  GENERATOR(SAR, OPTYPE_BINARY, 1)        \
  /* code movement */                     \
  GENERATOR(MOV, OPTYPE_BINARY, 1)        \
  GENERATOR(MOVZ, OPTYPE_BINARY, 1)       \
  GENERATOR(MOVS, OPTYPE_BINARY, 1)       \
  GENERATOR(PUSH, OPTYPE_UNARY, 1)        \
  GENERATOR(POP, OPTYPE_UNARY, 1)         \
  GENERATOR(CALL, OPTYPE_UNARY, 0)        \
  GENERATOR(LEAVE, OPTYPE_NULLARY, 0)     \
  GENERATOR(RET, OPTYPE_NULLARY, 0)       \
  GENERATOR(NOP, OPTYPE_NULLARY, 0)       \
  /* directives */                        \
  GENERATOR(GLOBL, OPTYPE_DIRECTIVE, 0)   \
  GENERATOR(ZERO, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(BYTE, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(VALUE, OPTYPE_DIRECTIVE, 0)   \
  GENERATOR(LONG, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(QUAD, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(ALIGN, OPTYPE_DIRECTIVE, 0)   \
  GENERATOR(SIZE, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(TYPE, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(ASCII, OPTYPE_DIRECTIVE, 0)   \
  GENERATOR(ASCIZ, OPTYPE_DIRECTIVE, 0)   \
  GENERATOR(SECTION, OPTYPE_DIRECTIVE, 0) \
  GENERATOR(BSS, OPTYPE_DIRECTIVE, 0)     \
  GENERATOR(TEXT, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(DATA, OPTYPE_DIRECTIVE, 0)    \
  GENERATOR(FILE, OPTYPE_DIRECTIVE, 0)

#define GENERATE_ENUM(CODE, TYPE, BOOL) OP_##CODE,
#define GENERATE_STRING(CODE, TYPE, BOOL) #CODE,
#define GENERATE_TYPE(CODE, TYPE, BOOL) \
  case OP_##CODE:                       \
    return TYPE;
#define GENERATE_NEEDS_WIDTH(CODE, TYPE, BOOL) \
  case OP_##CODE:                              \
    return BOOL;

typedef enum opcode { FOREACH_OPCODE(GENERATE_ENUM) OPCODE_COUNT } Opcode;
typedef enum optype {
  OPTYPE_INVALID = -1,
  OPTYPE_NULLARY,
  OPTYPE_UNARY,
  OPTYPE_BINARY,
  OPTYPE_DIRECTIVE
} OpType;

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
const char WIDTH_TO_CHAR[] = {'@', 'B', 'W', '@', 'L', '@', '@', '@', 'Q'};

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
    char lab[MAX_LABEL_LENGTH];
    size_t num;
  } ind;
  struct opsca {
    AddressMode mode;
    IndexScale scale;
    intmax_t disp;
    char lab[MAX_LABEL_LENGTH];
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

const char OPCODES[][MAX_OPCODE_LENGTH] = {FOREACH_OPCODE(GENERATE_STRING)};

static const char LABEL_FMT[] = "%s: \n";
static const char COND_FMT[] = ".C%lu";
static const char END_FMT[] = ".E%lu";
static const char STMT_FMT[] = ".S%lu";
static const char REINIT_FMT[] = ".R%lu";
static const char BOOL_FMT[] = ".B%lu";
static const char BOOL_END_FMT[] = ".BE%lu";
static const char DEF_FMT[] = ".D%lu";
static const char CASE_FMT[] = ".S%luC%lu";
static const char FALL_FMT[] = ".S%luF%lu";
static const char STR_FMT[] = ".STR%lu";

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and
 * displacement is a signed 32-bit integer.
 */
/* register order: rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8-r15 */
/* argument registers, in order: rdi, rsi, rdx, rcx, r8, r9 */
/* return registers: rax, rdx
 * preserved registers: rbx, rsp, rbp, r12-r15
 * other registers: r10, r11
 */
static const size_t RIP_VREG = SIZE_MAX;
static const size_t RAX_VREG = 0;
static const size_t RCX_VREG = 1;
static const size_t RDX_VREG = 2;
static const size_t RBX_VREG = 3;
static const size_t RSP_VREG = 4;
static const size_t RBP_VREG = 5;
static const size_t RSI_VREG = 6;
static const size_t RDI_VREG = 7;
static const size_t PARAM_REGS[] = {RDI_VREG, RSI_VREG, RDX_VREG,
                                    RCX_VREG, 8,        9};
static const size_t RETURN_REGS[] = {RAX_VREG, RDX_VREG};
static const size_t PRESERVED_REGS[] = {RBX_VREG, RSP_VREG, RBP_VREG, 12,
                                        13,       14,       15};
static const size_t VOLATILE_REGS[] = {
    RAX_VREG, RDX_VREG, RCX_VREG, RSI_VREG, RDI_VREG, 8, 9, 10, 11};
static const size_t PARAM_REG_COUNT = 6;
static const size_t RETURN_REG_COUNT = 2;
static const size_t PRESERVED_REG_COUNT = 7;
static const size_t VOLATILE_REG_COUNT = 9;
static const size_t REAL_REG_COUNT = PRESERVED_REG_COUNT + VOLATILE_REG_COUNT;
/* number of eightbytes occupied by the function prologue on the stack */
static const size_t PROLOGUE_EIGHTBYTES = 8;

static size_t branch_count;
static size_t reg_eightbytes;
static size_t stack_eightbytes;
static ptrdiff_t window_size;

static LinkedList *instructions;
static ListIter *before_definition;
static struct {
  const char *literal;
  const char *label;
} * literals;
static size_t literals_cap = 10;
static size_t literals_size;
static Map *static_locals;

size_t next_vreg(void) {
  static size_t vreg_count = REAL_REG_COUNT;
  return vreg_count++;
}

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

void set_op_ind(Operand *operand, intmax_t disp, size_t num,
                const char *label) {
  operand->ind.mode = MODE_INDIRECT;
  operand->ind.disp = disp;
  operand->ind.num = num;
  if (label != NULL) strcpy(operand->ind.lab, label);
}

void set_op_sca(Operand *operand, IndexScale scale, intmax_t disp, size_t base,
                size_t index, const char *label) {
  operand->sca.mode = MODE_SCALE;
  operand->sca.scale = scale;
  operand->sca.disp = disp;
  operand->sca.base = base;
  operand->sca.index = index;
  if (label != NULL) strcpy(operand->sca.lab, label);
}

int bulk_rtom(size_t dest_memreg, ptrdiff_t dest_disp, const size_t *src_regs,
              const TypeSpec *type, ListIter *where) {
  size_t alignment = typespec_get_alignment(type);
  size_t width = typespec_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = typespec_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = dest_disp + i * 8 + j;
        InstructionData *mov_data = instr_init(OP_MOV);
        set_op_reg(&mov_data->src, alignment, src_regs[i]);
        set_op_ind(&mov_data->dest, chunk_disp, dest_memreg, NULL);
        InstructionData *shr_data = instr_init(OP_SHR);
        set_op_reg(&shr_data->dest, REG_QWORD, src_regs[i]);
        set_op_imm(&shr_data->src, alignment);
        int status = liter_push_back(where, NULL, 2, mov_data, shr_data);
        if (status) return status;
      }
    }
  } else {
    size_t mov_count = width / alignment;
    size_t i;
    for (i = 0; i < mov_count; ++i) {
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->src, alignment, src_regs[i]);
      set_op_ind(&mov_data->dest, dest_disp + i * alignment, dest_memreg, NULL);
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
    }
  }
  return 0;
}

int bulk_mtor(const size_t *dest_regs, size_t src_memreg, ptrdiff_t src_disp,
              const TypeSpec *type, ListIter *where) {
  size_t alignment = typespec_get_alignment(type);
  size_t width = typespec_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = typespec_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = src_disp + i * 8 + j;
        InstructionData *mov_data = instr_init(OP_MOV);
        set_op_ind(&mov_data->src, chunk_disp, src_memreg, NULL);
        set_op_reg(&mov_data->dest, alignment, next_vreg());
        InstructionData *movz_data = instr_init(OP_MOVZ);
        movz_data->src = mov_data->dest;
        set_op_reg(&movz_data->dest, REG_QWORD, next_vreg());
        InstructionData *shl_data = instr_init(OP_SHL);
        shl_data->dest = movz_data->dest;
        set_op_imm(&shl_data->src, j);
        InstructionData *bitor_data = instr_init(OP_OR);
        bitor_data->src = movz_data->dest;
        set_op_reg(&bitor_data->dest, REG_QWORD, dest_regs[i]);
        int status = liter_push_back(where, NULL, 4, mov_data, movz_data,
                                     shl_data, bitor_data);
        if (status) return status;
      }
    }
  } else {
    size_t mov_count = width / alignment;
    size_t i;
    for (i = 0; i < mov_count; ++i) {
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->dest, alignment, dest_regs[i]);
      set_op_ind(&mov_data->src, src_disp + i * alignment, src_memreg, NULL);
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
    }
  }
  return 0;
}

int bulk_mtom(size_t dest_reg, size_t src_reg, const TypeSpec *type,
              ListIter *where) {
  size_t alignment = typespec_get_alignment(type);
  size_t width = typespec_get_width(type);
  size_t mov_count = width / alignment;
  size_t i;
  for (i = 0; i < mov_count; ++i) {
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_reg(&mov_data->dest, alignment, next_vreg());
    set_op_ind(&mov_data->src, i * alignment, src_reg, NULL);
    InstructionData *mov_data_2 = instr_init(OP_MOV);
    mov_data_2->src = mov_data->dest;
    set_op_ind(&mov_data->dest, i * alignment, dest_reg, NULL);
    int status = liter_push_back(where, NULL, 2, mov_data, mov_data_2);
    if (status) return status;
  }
  return 0;
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
    case '?':
      /* fallthrough */
    case TOK_AND:
      return OP_JZ;
    default:
      return OP_NOP;
  }
}

OpType optype_from_opcode(Opcode opcode) {
  switch (opcode) {
    FOREACH_OPCODE(GENERATE_TYPE)
    default:
      return OPTYPE_INVALID;
  }
}

int opcode_needs_width(Opcode opcode) {
  switch (opcode) {
    FOREACH_OPCODE(GENERATE_NEEDS_WIDTH);
    default:
      return 0;
  }
}

int opcode_is_directive(Opcode opcode) {
  switch (opcode) {
    case OP_GLOBL:
    case OP_ZERO:
    case OP_BYTE:
    case OP_VALUE:
    case OP_LONG:
    case OP_QUAD:
    case OP_ALIGN:
    case OP_SIZE:
    case OP_TYPE:
    case OP_ASCII:
    case OP_ASCIZ:
    case OP_SECTION:
    case OP_BSS:
    case OP_TEXT:
    case OP_DATA:
      return 1;
    default:
      return 0;
  }
}

const char *asmgen_literal_label(const char *literal) {
  size_t i;
  for (i = 0; i < literals_size; ++i) {
    if (strcmp(literals[i].literal, literal) == 0) {
      return literals[i].label;
    }
  }

  if (literals_size >= literals_cap) {
    literals = realloc(literals, sizeof(*literals) * (literals_cap *= 2));
  }

  InstructionData *section_data = instr_init(OP_SECTION);
  set_op_dir(&section_data->dest, 0, ".rodata");
  /* TODO(Robert): determine when alignment needs to be set, if ever */
  InstructionData *label_data = instr_init(OP_INVALID);
  sprintf(label_data->label, STR_FMT, literals_size);
  InstructionData *string_data = instr_init(OP_ASCIZ);
  /* TODO(Robert): ugly cast */
  set_op_imm(&string_data->dest, (uintmax_t)literal);
  int status = liter_push_back(before_definition, &before_definition, 3,
                               section_data, label_data, string_data);
  if (status) abort();

  literals[literals_size].literal = literal;
  literals[literals_size++].label = label_data->label;
  return label_data->label;
}

void assign_stack_space(SymbolValue *symval) {
  size_t width = typespec_get_width(&symval->type);
  size_t alignment = typespec_get_alignment(&symval->type);
  size_t padding = alignment - (window_size % alignment);
  size_t to_add = width + padding == alignment ? 0 : padding;
  if (to_add > PTRDIFF_MAX) abort();
  window_size += to_add;
  if (window_size < 0) abort();
  symval->disp = -window_size;
}

void assign_static_space(const char *ident, SymbolValue *symval) {
  size_t *static_count = map_get(static_locals, ident, strlen(ident));
  if (!static_count) {
    static_count = calloc(1, sizeof(size_t));
    int status = map_insert(static_locals, ident, strlen(ident), static_count);
    if (status) abort();
  }
  symval->static_id = *static_count++;
}

/* NOTE: on x64, most operations that write to the lower 32 bits of a
 * register will zero the upper 32 bits.
 *
 * any signed int -> any wider unsigned int: movz
 * any signed int -> any wider signed int: movs
 * any unsigned int -> any wider int: movz
 * any int -> any narrower int: simple mov
 * any int -> any int of same width: nop
 */
int scalar_conversions(ASTree *expr, const TypeSpec *to) {
  const TypeSpec *from = expr->type;
  size_t from_width = typespec_get_width(from);
  if (expr->attributes & ATTR_EXPR_LVAL) {
    InstructionData *lvalue_data = liter_get(expr->last_instr);
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_ind(&mov_data->src, NO_DISP, lvalue_data->dest.reg.num, NULL);
    set_op_reg(&mov_data->dest, from_width, next_vreg());
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_data);
    if (status) return status;
  }
  InstructionData *expr_data = liter_get(expr->last_instr);
  if (expr_data->dest.all.mode != MODE_REGISTER) {
    InstructionData *mov_data = instr_init(OP_MOV);
    mov_data->src = expr_data->dest;
    set_op_reg(&mov_data->dest, from_width, next_vreg());
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_data);
    if (status) return status;
    expr_data = liter_get(expr->last_instr);
  }

  size_t to_width = typespec_get_width(to);
  if (from_width == to_width) {
    return 0;
  } else if (from_width > to_width) {
    /* unnecessary mov so that the width of the destination is set correctly,
     * and the whole structure describing the operand can just be copied to the
     * next instruction that needs it
     */
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_reg(&mov_data->src, to_width, expr_data->dest.reg.num);
    set_op_reg(&mov_data->dest, to_width, next_vreg());
    return liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_data);
  } else if (from->base == TYPE_SIGNED) {
    InstructionData *movs_data = instr_init(OP_MOVS);
    movs_data->src = expr_data->dest;
    set_op_reg(&movs_data->dest, to_width, next_vreg());
    return liter_push_back(expr->last_instr, &expr->last_instr, 1, movs_data);
  } else if (from->base == TYPE_UNSIGNED) {
    InstructionData *movz_data = instr_init(OP_MOVZ);
    movz_data->src = expr_data->dest;
    set_op_reg(&movz_data->dest, to_width, next_vreg());
    return liter_push_back(expr->last_instr, &expr->last_instr, 1, movz_data);
  } else {
    return -1;
  }
}

int save_preserved_regs(void) {
  size_t i;
  for (i = 1; i <= PRESERVED_REG_COUNT; ++i) {
    InstructionData *push_data = instr_init(OP_PUSH);
    set_op_reg(&push_data->dest, REG_QWORD,
               PRESERVED_REGS[PRESERVED_REG_COUNT - i]);
    int status = llist_push_back(instructions, push_data);
    if (status) return status;
  }
  return 0;
}

int save_volatile_regs(ListIter *where) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
    InstructionData *push_data = instr_init(OP_PUSH);
    set_op_reg(&push_data->dest, REG_QWORD, VOLATILE_REGS[i]);
    int status = liter_push_front(where, &where, 1, push_data);
    if (status) return status;
  }
  return 0;
}

int restore_preserved_regs(void) {
  size_t i;
  for (i = 0; i < PRESERVED_REG_COUNT; ++i) {
    InstructionData *pop_data = instr_init(OP_POP);
    set_op_reg(&pop_data->dest, REG_QWORD, PRESERVED_REGS[i]);
    int status = llist_push_back(instructions, pop_data);
    if (status) return status;
  }
  return 0;
}

int restore_volatile_regs(void) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
    InstructionData *pop_data = instr_init(OP_POP);
    set_op_reg(&pop_data->dest, REG_QWORD, VOLATILE_REGS[i]);
    int status = llist_push_back(instructions, pop_data);
    if (status) return status;
  }
  return 0;
}

ASTree *translate_empty_expr(ASTree *empty_expr) {
  InstructionData *nop_data = instr_init(OP_NOP);
  llist_push_back(instructions, nop_data);
  empty_expr->first_instr = llist_iter_last(instructions);
  if (empty_expr->first_instr == NULL) abort();
  empty_expr->last_instr = llist_iter_last(instructions);
  if (empty_expr->last_instr == NULL) abort();
  return empty_expr;
}

void maybe_load_cexpr(ASTree *expr) {
  if (expr->attributes & ATTR_CONST_ADDR) {
    InstructionData *lea_data = instr_init(OP_LEA);
    set_op_dir(&lea_data->src, expr->constant.address.disp,
               expr->constant.address.label);
    set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
    int status = llist_push_back(instructions, lea_data);
    if (status) abort();
    expr->first_instr = llist_iter_last(instructions);
    if (expr->first_instr == NULL) abort();
    expr->last_instr = llist_iter_last(instructions);
    if (expr->last_instr == NULL) abort();
  } else if (expr->attributes & ATTR_EXPR_CONST) {
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_imm(&mov_data->src, expr->constant.integral.value);
    set_op_reg(&mov_data->dest, typespec_get_width(expr->type), next_vreg());
    int status = llist_push_back(instructions, mov_data);
    if (status) abort();
    expr->first_instr = llist_iter_last(instructions);
    if (expr->first_instr == NULL) abort();
    expr->last_instr = llist_iter_last(instructions);
    if (expr->last_instr == NULL) abort();
  }
}

ASTree *translate_ident(ASTree *ident) {
  InstructionData *lea_data = instr_init(OP_LEA);
  SymbolValue *symval = NULL;
  state_get_symbol(state, ident->lexinfo, strlen(ident->lexinfo), &symval);
  assert(symval != NULL);

  if (symval->flags & SYMFLAG_STORE_STAT) {
    if (symval->flags & SYMFLAG_LINK_NONE) {
      char temp[MAX_LABEL_LENGTH];
      sprintf(temp, "%s.%lu", ident->lexinfo, symval->static_id);
      set_op_ind(&lea_data->src, NO_DISP, RIP_VREG, temp);
    } else {
      set_op_ind(&lea_data->src, NO_DISP, RIP_VREG, ident->lexinfo);
    }
  } else {
    set_op_ind(&lea_data->src, symval->disp, RBP_VREG, NULL);
  }
  const TypeSpec *ident_type = ident->type;
  AuxSpec *ident_aux = llist_back(&ident_type->auxspecs);
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
  int status = llist_push_back(instructions, lea_data);
  if (status) abort();
  ident->first_instr = llist_iter_last(instructions);
  if (ident->first_instr == NULL) abort();
  ident->last_instr = liter_copy(ident->first_instr);
  if (ident->last_instr == NULL) abort();
  return ident;
}

ASTree *translate_cast(ASTree *cast, ASTree *expr) {
  DEBUGS('g', "Translating cast");

  int status = scalar_conversions(expr, cast->type);
  if (status) abort();

  cast->first_instr = liter_copy(expr->first_instr);
  if (cast->first_instr == NULL) abort();
  cast->last_instr = liter_copy(expr->last_instr);
  if (cast->last_instr == NULL) abort();
  return astree_adopt(cast, 1, expr);
}

ASTree *translate_intcon(ASTree *constant) {
  DEBUGS('g', "Translating integer constant");
  InstructionData *data = instr_init(OP_MOV);
  set_op_reg(&data->dest, typespec_get_width(constant->type), next_vreg());
  set_op_imm(&data->src, constant->constant.integral.value);
  int status = llist_push_back(instructions, data);
  if (status) abort();
  constant->first_instr = llist_iter_last(instructions);
  if (constant->first_instr == NULL) abort();
  constant->last_instr = liter_copy(constant->first_instr);
  if (constant->last_instr == NULL) abort();
  return constant;
}

/* Two classes of operators whose result is a boolean:
 * - comparison: >, <, >=, <=, ==, !=
 * - logical: &&, ||, !
 *
 * logical NOT does the same thing as the conversion from an arbitrary value
 * to a boolean except instead of using SETNZ it does SETZ
 */
ASTree *translate_logical_not(ASTree * not, ASTree *operand) {
  /* move lval into reg, if necessary */
  int status = scalar_conversions(operand, operand->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  /* TEST operand with itself */
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = operand_data->dest;

  InstructionData *setz_data = instr_init(OP_SETZ);
  set_op_reg(&setz_data->dest, REG_BYTE, next_vreg());

  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setz_data->dest;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());

  not ->first_instr = liter_copy(operand->first_instr);
  if (not ->first_instr == NULL) abort();
  status = liter_push_back(operand->last_instr, &not ->first_instr, 3,
                           test_data, setz_data, movz_data);
  if (status) abort();
  return astree_adopt(not, 1, operand);
}

ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  /* test first operand; jump on false for && and true for || */
  int status = scalar_conversions(left, left->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  InstructionData *test_left_data = instr_init(OP_TEST);
  test_left_data->dest = test_left_data->src = left_data->dest;
  InstructionData *jmp_left_data = instr_init(opcode_from_operator(operator));
  set_op_dir(&jmp_left_data->dest, NO_DISP, BOOL_FMT, branch_count);

  status =
      liter_push_back(left->last_instr, NULL, 2, test_left_data, jmp_left_data);
  if (status) abort();

  status = scalar_conversions(right, right->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *test_right_data = instr_init(OP_TEST);
  test_right_data->dest = test_right_data->src = right_data->dest;

  /* result will always be the truth value of the last evaluated expression */
  InstructionData *setnz_data = instr_init(OP_SETNZ);
  set_op_reg(&setnz_data->dest, REG_BYTE, next_vreg());
  sprintf(setnz_data->label, BOOL_FMT, branch_count++);
  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setnz_data->src;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();
  status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                           test_right_data, setnz_data, movz_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_comparison(ASTree *operator, ASTree * left, ASTree *right) {
  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  const TypeSpec *common_type = NULL;
  size_t left_width = typespec_get_width(left->type);
  size_t right_width = typespec_get_width(right->type);
  size_t max_width = left_width > right_width ? left_width : right_width;
  switch (max_width) {
    case X64_SIZEOF_LONG:
      common_type = &SPEC_LONG;
      break;
    case X64_SIZEOF_INT:
      common_type = &SPEC_INT;
      break;
    case X64_SIZEOF_SHORT:
      common_type = &SPEC_SHRT;
      break;
    case X64_SIZEOF_CHAR:
      common_type = &SPEC_CHAR;
      break;
    default:
      abort();
  }

  int status = scalar_conversions(left, common_type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, common_type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *cmp_data = instr_init(OP_CMP);
  cmp_data->dest = left_data->dest;
  cmp_data->src = right_data->dest;
  InstructionData *setcc_data = instr_init(opcode_from_operator(operator));
  set_op_reg(&setcc_data->dest, REG_BYTE, next_vreg());
  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setcc_data->dest;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());
  status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                           cmp_data, setcc_data, movz_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_indirection(ASTree *indirection, ASTree *operand) {
  DEBUGS('g', "Translating indirection operation.");
  indirection->first_instr = liter_copy(operand->first_instr);
  if (indirection->first_instr == NULL) abort();

  /* TODO(Robert): (void?) pointer type constant */
  int status = scalar_conversions(operand, &SPEC_LONG);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  TypeSpec element_type;
  status = strip_aux_type(&element_type, operand->type);
  if (status) abort();
  InstructionData *load_data =
      instr_init(typespec_is_array(&element_type) ? OP_LEA : OP_MOV);
  status = typespec_destroy(&element_type);
  if (status) abort();
  set_op_ind(&load_data->src, NO_DISP, operand_data->dest.reg.num, NULL);
  set_op_reg(&load_data->dest, typespec_get_width(indirection->type),
             next_vreg());
  status = liter_push_back(operand->last_instr, &indirection->last_instr, 1,
                           load_data);
  if (status) abort();
  return astree_adopt(indirection, 1, operand);
}

ASTree *translate_addrof(ASTree *addrof, ASTree *operand) {
  DEBUGS('g', "Translating address operation.");
  if (operand->symbol == TOK_INDIRECTION) {
    /* just remove the code if some idiot does &*&*&*&*&* */
    /* NOTE: liter_delete deletes the node (not the iterator) and moves the
     * iterator to the next node, unless the next node is the anchor, in which
     * case it moves to the previous node. the extra mov should be the last
     * instruction, so the iterator should move to the previous node
     *
     * not the greatest interface, but it is a one-liner solution here
     */
    int status = liter_delete(operand->last_instr);
    if (status) abort();
  }
  addrof->first_instr = liter_copy(operand->first_instr);
  if (addrof->first_instr == NULL) abort();
  addrof->last_instr = liter_copy(operand->last_instr);
  if (addrof->last_instr == NULL) abort();
  return astree_adopt(addrof, 1, operand);
}

ASTree *translate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  DEBUGS('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the displacement
   * and scale addressing mode can be used
   */
  /* TODO(Robert): (void?) pointer type constant */
  int status = scalar_conversions(pointer, &SPEC_LONG);
  if (status) abort();
  InstructionData *pointer_data = liter_get(pointer->last_instr);

  /* TODO(Robert): ptrdiff_t type constant (maybe?) */
  status = scalar_conversions(index, &SPEC_LONG);
  if (status) abort();
  InstructionData *index_data = liter_get(index->last_instr);

  subscript->first_instr = liter_copy(pointer->first_instr);
  if (subscript->first_instr == NULL) abort();

  InstructionData *lea_data = instr_init(OP_LEA);
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
  size_t scale = typespec_get_width((TypeSpec *)subscript->type);
  if (scale == 1 || scale == 2 || scale == 4 || scale == 8) {
    set_op_sca(&lea_data->src, scale, NO_DISP, pointer_data->dest.ind.num,
               index_data->dest.reg.num, NULL);
    int status =
        liter_push_back(index->last_instr, &subscript->last_instr, 1, lea_data);
    if (status) abort();
  } else {
    set_op_sca(&lea_data->src, SCALE_BYTE, NO_DISP, pointer_data->dest.ind.num,
               index_data->dest.reg.num, NULL);
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = index_data->dest;
    set_op_imm(&mul_data->src, scale);
    int status = liter_push_back(index->last_instr, &subscript->last_instr, 2,
                                 mul_data, lea_data);
    if (status) abort();
  }
  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *translate_reference(ASTree *reference, ASTree *struct_,
                            ASTree *member) {
  DEBUGS('g', "Translating reference operator");
  if (reference->symbol == TOK_ARROW) {
    int status = scalar_conversions(struct_, struct_->type);
    if (status) abort();
  }
  InstructionData *struct_data = liter_get(struct_->last_instr);
  SymbolValue *member_symbol =
      typespec_member_name(struct_->type, member->lexinfo);
  assert(member_symbol);
  InstructionData *lea_data = instr_init(OP_LEA);
  set_op_ind(&lea_data->src, member_symbol->disp, struct_data->dest.reg.num,
             NULL);
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());

  reference->first_instr = liter_copy(struct_->first_instr);
  if (reference->first_instr == NULL) abort();
  int status =
      liter_push_back(struct_->last_instr, &reference->last_instr, 1, lea_data);
  if (status) abort();
  return astree_adopt(reference, 2, struct_, member);
}

ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand) {
  DEBUGS('g', "Translating postfix increment/decrement");
  post_inc_dec->first_instr = liter_copy(operand->first_instr);
  if (post_inc_dec->first_instr == NULL) abort();
  InstructionData *lvalue_data = liter_get(operand->last_instr);
  if (lvalue_data == NULL) abort();
  int status = scalar_conversions(operand, post_inc_dec->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = operand_data->dest;
  set_op_reg(&mov_data->dest, typespec_get_width(post_inc_dec->type),
             next_vreg());

  InstructionData *inc_dec_data =
      instr_init(opcode_from_operator(post_inc_dec));
  inc_dec_data->dest = operand_data->dest;

  InstructionData *mov_data_2 = instr_init(OP_MOV);
  set_op_ind(&mov_data_2->dest, NO_DISP, lvalue_data->dest.reg.num, NULL);
  set_op_reg(&mov_data_2->src, typespec_get_width(operand->type),
             operand_data->dest.reg.num);

  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = mov_data->dest;
  status = liter_push_back(operand->last_instr, &post_inc_dec->last_instr, 4,
                           mov_data, inc_dec_data, mov_data_2, dummy_data);
  if (status) abort();
  return astree_adopt(post_inc_dec, 1, operand);
}

ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand) {
  DEBUGS('g', "Translating prefix increment/decrement");
  inc_dec->first_instr = liter_copy(operand->first_instr);
  if (inc_dec->first_instr == NULL) abort();
  InstructionData *lvalue_data = liter_get(operand->last_instr);
  if (lvalue_data == NULL) abort();
  int status = scalar_conversions(operand, inc_dec->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *inc_dec_data = instr_init(opcode_from_operator(inc_dec));
  inc_dec_data->dest = operand_data->dest;

  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_ind(&mov_data->dest, NO_DISP, lvalue_data->dest.reg.num, NULL);
  set_op_reg(&mov_data->src, typespec_get_width(operand->type),
             operand_data->dest.reg.num);

  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = operand_data->dest;

  status = liter_push_back(operand->last_instr, &inc_dec->first_instr, 3,
                           inc_dec_data, mov_data, dummy_data);
  if (status) abort();
  return astree_adopt(inc_dec, 1, operand);
}

ASTree *translate_unop(ASTree *operator, ASTree * operand) {
  DEBUGS('g', "Translating unary operation");
  int status = scalar_conversions(operand, operator->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *operator_data = instr_init(opcode_from_operator(operator));
  operator_data->dest = operand_data->dest;

  operator->first_instr = liter_copy(operand->first_instr);
  if (operator->first_instr == NULL) abort();
  status = liter_push_back(operand->last_instr, &operator->last_instr, 1,
                           operator_data);
  if (status) abort();
  return astree_adopt(operator, 1, operand);
}

ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  DEBUGS('g', "Translating additive operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *operator_data = instr_init(opcode_from_operator(operator));
  operator_data->dest = left_data->dest;
  operator_data->src = right_data->dest;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  /* use two-operand IMUL, since it is more convenient in this case */
  if (typespec_is_pointer(left->type) && !typespec_is_pointer(right->type)) {
    TypeSpec element_type;
    int status = strip_aux_type(&element_type, left->type);
    if (status) abort();
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = right_data->dest;
    set_op_imm(&mul_data->src, typespec_get_width(&element_type));
    status = liter_push_back(right->last_instr, &operator->last_instr, 2,
                             mul_data, operator_data);
    if (status) abort();
  } else if (!typespec_is_pointer(left->type) &&
             typespec_is_pointer(right->type)) {
    TypeSpec element_type;
    status = strip_aux_type(&element_type, right->type);
    if (status) abort();
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = left_data->dest;
    set_op_imm(&mul_data->src, typespec_get_width(&element_type));
    status = liter_push_back(right->last_instr, &operator->last_instr, 2,
                             mul_data, operator_data);
    if (status) abort();
  } else {
    int status = liter_push_back(right->last_instr, &operator->last_instr, 1,
                                 operator_data);
    if (status) abort();
  }
  return astree_adopt(operator, 2, left, right);
}

/* DIV/IDIV: divide ax by operand; quotient -> ax; remainder -> dx except
 * when the operand is al, in which case upper bits -> ah
 * MUL/IMUL: multipy ax by operand; lower bits -> ax; upper bits -> dx except
 * when the operand is al, in which case upper bits -> ah
 *
 * IMUL has 2- and 3-operand forms, but we use the 1-operand form here since it
 * lets us reuse the whole procedure to emit it
 *
 * because of integral promotion, the operands should be at least 32 bits, and
 * we don't need to worry about the case where the remainder is in ah
 */
ASTree *translate_multiplication(ASTree *operator, ASTree * left,
                                 ASTree *right) {
  DEBUGS('g', "Translating binary operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *mov_rax_data = instr_init(OP_MOV);
  set_op_reg(&mov_rax_data->dest, typespec_get_width(operator->type), RAX_VREG);
  mov_rax_data->src = left_data->dest;

  InstructionData *operator_data = instr_init(opcode_from_operator(operator));
  operator_data->dest = right_data->dest;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  InstructionData *mov_data = instr_init(OP_MOV);
  /* mov from vreg representing rax/rdx to new vreg */
  set_op_reg(&mov_data->src, typespec_get_width(operator->type),
                             operator->symbol == '%' ? RDX_VREG : RAX_VREG);
  set_op_reg(&mov_data->dest, typespec_get_width(operator->type), next_vreg());
  status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                           mov_rax_data, operator_data, mov_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  DEBUGS('g', "Translating binary operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *operator_data = instr_init(opcode_from_operator(operator));
  operator_data->dest = left_data->dest;
  operator_data->src = right_data->dest;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  status = liter_push_back(right->last_instr, &operator->last_instr, 1,
                           operator_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_conditional(ASTree *qmark, ASTree *condition,
                              ASTree *true_expr, ASTree *false_expr) {
  int status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;
  InstructionData *jmp_false_data = instr_init(opcode_from_operator(condition));
  set_op_dir(&jmp_false_data->dest, NO_DISP, BOOL_FMT, branch_count);
  status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                           jmp_false_data);
  if (status) abort();

  status = scalar_conversions(true_expr, qmark->type);
  if (status) abort();
  InstructionData *true_expr_data = liter_get(true_expr->last_instr);
  InstructionData *mov_true_data = instr_init(OP_MOV);
  mov_true_data->src = true_expr_data->dest;
  set_op_reg(&mov_true_data->dest, typespec_get_width(qmark->type),
             next_vreg());
  InstructionData *jmp_end_data = instr_init(OP_JMP);
  set_op_dir(&jmp_end_data->dest, NO_DISP, BOOL_END_FMT, branch_count);
  status = liter_push_back(true_expr->last_instr, NULL, 2, mov_true_data,
                           jmp_end_data);

  status = scalar_conversions(false_expr, qmark->type);
  if (status) abort();
  InstructionData *false_label = instr_init(OP_INVALID);
  sprintf(false_label->label, BOOL_FMT, branch_count);
  status = liter_push_front(false_expr->first_instr, NULL, 1, false_label);
  if (status) abort();
  InstructionData *false_expr_data = liter_get(false_expr->last_instr);
  InstructionData *mov_false_data = instr_init(OP_MOV);
  mov_false_data->src = false_expr_data->dest;
  mov_false_data->dest = mov_true_data->dest;
  /* dummy mov so that last instruction has destination reg */
  InstructionData *end_label = instr_init(OP_MOV);
  sprintf(end_label->label, BOOL_END_FMT, branch_count++);
  end_label->src = end_label->dest = mov_false_data->dest;
  status = liter_push_back(false_expr->last_instr, &qmark->last_instr, 2,
                           mov_false_data, end_label);
  if (status) abort();

  qmark->first_instr = liter_copy(condition->first_instr);
  if (qmark->first_instr == NULL) abort();
  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

int assign_aggregate(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;

  InstructionData *lvalue_data = liter_get(lvalue->last_instr);
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);
  ListIter *temp = llist_iter_last(instructions);
  int status = bulk_mtom(lvalue_data->dest.reg.num, rvalue_data->dest.reg.num,
                         assignment->type, temp);
  free(temp);
  if (status) return status;
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = lvalue_data->dest;
  status = llist_push_back(instructions, dummy_data);
  if (status) return status;
  assignment->last_instr = llist_iter_last(instructions);
  if (assignment->last_instr == NULL) return -1;
  return 0;
}

int assign_scalar(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  InstructionData *lvalue_data = liter_get(lvalue->last_instr);

  int status = scalar_conversions(rvalue, lvalue->type);
  if (status) return status;
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);

  InstructionData *assignment_data = instr_init(OP_MOV);
  set_op_ind(&assignment_data->dest, NO_DISP, lvalue_data->dest.reg.num, NULL);
  assignment_data->src = rvalue_data->dest;

  InstructionData *dummy_data = instr_init(OP_MOV);
  set_op_reg(&dummy_data->dest, typespec_get_width(assignment->type),
             next_vreg());
  dummy_data->src = rvalue_data->dest;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;
  status = liter_push_back(lvalue->last_instr, &assignment->last_instr, 2,
                           assignment_data, dummy_data);
  if (status) return status;
  return 0;
}

ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  DEBUGS('g', "Translating assignment");
  if (typespec_is_union(assignment->type) ||
      typespec_is_struct(assignment->type)) {
    int status = assign_aggregate(assignment, lvalue, rvalue);
    if (status) abort();
  } else {
    int status = assign_scalar(assignment, lvalue, rvalue);
    if (status) abort();
  }
  return astree_adopt(assignment, 2, lvalue, rvalue);
}

int translate_agg_arg(ASTree *call, ASTree *arg) {
  size_t arg_eightbytes = typespec_get_eightbytes(arg->type);
  int status;
  InstructionData *arg_data = liter_get(arg->last_instr);
  if (arg_eightbytes <= 2 &&
      arg_eightbytes + reg_eightbytes <= PARAM_REG_COUNT) {
    int status = bulk_mtor(PARAM_REGS + reg_eightbytes, arg_data->dest.reg.num,
                           NO_DISP, arg->type, call->last_instr);
    reg_eightbytes += arg_eightbytes;
    if (status) return status;
  } else {
    int status = bulk_mtom(RSP_VREG, arg_data->dest.reg.num, arg->type,
                           call->last_instr);
    if (status) return status;
    stack_eightbytes += arg_eightbytes;
    InstructionData *sub_data = instr_init(OP_SUB);
    set_op_reg(&sub_data->dest, REG_QWORD, RSP_VREG);
    set_op_imm(&sub_data->src, arg_eightbytes * 8);
    /* push after since instructions will be reversed */
    status = liter_push_back(call->last_instr, NULL, 1, sub_data);
    if (status) return status;
  }
  return 0;
}

int translate_scalar_arg(ASTree *call, TypeSpec *param_type, ASTree *arg) {
  assert(typespec_get_eightbytes(arg->type) == 1);

  int status = scalar_conversions(arg, param_type);
  if (status) return status;
  InstructionData *arg_data = liter_get(arg->last_instr);

  if (reg_eightbytes < PARAM_REG_COUNT) {
    InstructionData *mov_data = instr_init(OP_MOV);
    mov_data->src = arg_data->dest;
    set_op_reg(&mov_data->dest, typespec_get_width(arg->type),
               PARAM_REGS[reg_eightbytes++]);
    int status = liter_push_back(call->last_instr, NULL, 1, mov_data);
    if (status) return status;
  } else {
    InstructionData *push_data = instr_init(OP_PUSH);
    set_op_reg(&push_data->dest, REG_QWORD, arg_data->dest.reg.num);
    int status = liter_push_back(call->last_instr, NULL, 1, push_data);
    if (status) return status;
    ++stack_eightbytes;
  }
  return 0;
}

int translate_args(ASTree *call) {
  /* account for hidden out param */
  int out_param = typespec_get_eightbytes(call->type) > 2;
  reg_eightbytes = out_param ? 1 : 0;
  stack_eightbytes = 0;
  if (typespec_is_struct(call->type) || typespec_is_union(call->type)) {
    SymbolValue dummy;
    dummy.type = *call->type;
    assign_stack_space(&dummy);
    if (out_param) {
      InstructionData *lea_data = instr_init(OP_LEA);
      set_op_reg(&lea_data->dest, REG_QWORD, RDI_VREG);
      set_op_ind(&lea_data->src, -window_size, RBP_VREG, NULL);
      int status = liter_push_back(call->last_instr, NULL, 1, lea_data);
      if (status) return status;
    }
  }
  size_t i;
  for (i = 1; i < astree_count(call); ++i) {
    DEBUGS('g', "Translating parameter %i", i);
    ASTree *arg = astree_get(call, i);
    SymbolValue *param_symval = typespec_param_index(call->type, i - 1);
    if (typespec_is_union(arg->type) || typespec_is_struct(arg->type)) {
      int status = translate_agg_arg(call, arg);
      if (status) return status;
    } else {
      int status = translate_scalar_arg(call, &param_symval->type, arg);
      if (status) return status;
    }
  }
  return 0;
}

ASTree *translate_call(ASTree *call) {
  DEBUGS('g', "Translating function call");
  ASTree *fn_pointer = astree_get(call, 0);
  call->first_instr = liter_copy(fn_pointer->first_instr);
  if (call->first_instr == NULL) abort();
  int status = save_volatile_regs(call->first_instr);
  if (status) abort();
  /* temporary iterator for inserting args in reverse order */
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();

  status = translate_args(call);
  if (status) abort();
  free(call->last_instr);
  call->last_instr = NULL;

  /* TODO(Robert): (void?) pointer type constant */
  status = scalar_conversions(fn_pointer, &SPEC_LONG);
  if (status) abort();
  InstructionData *fn_pointer_data = liter_get(fn_pointer->last_instr);
  InstructionData *call_data = instr_init(OP_CALL);
  call_data->dest = fn_pointer_data->dest;
  status = llist_push_back(instructions, call_data);
  if (status) abort();

  InstructionData *rsp_reset_data = instr_init(OP_ADD);
  set_op_reg(&rsp_reset_data->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_reset_data->src, stack_eightbytes);
  status = llist_push_back(instructions, rsp_reset_data);
  if (status) abort();

  if (!typespec_is_void(call->type)) {
    size_t result_vreg = next_vreg();
    if (typespec_is_struct(call->type) || typespec_is_union(call->type)) {
      if (typespec_get_eightbytes(call->type) <= 2) {
        ListIter *temp = llist_iter_last(instructions);
        int status =
            bulk_rtom(RBP_VREG, -window_size, RETURN_REGS, call->type, temp);
        free(temp);
        if (status) abort();
      }
      InstructionData *lea_data = instr_init(OP_LEA);
      set_op_ind(&lea_data->src, -window_size, RBP_VREG, NULL);
      set_op_reg(&lea_data->dest, REG_QWORD, result_vreg);
      int status = llist_push_back(instructions, lea_data);
      if (status) abort();
    } else {
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->src, typespec_get_width(call->type), RAX_VREG);
      set_op_reg(&mov_data->dest, typespec_get_width(call->type), result_vreg);
      int status = llist_push_back(instructions, mov_data);
      if (status) abort();
    }
    status = restore_volatile_regs();
    if (status) abort();
    /* dummy mov since parent expects last instr to have result */
    InstructionData *dummy_data = instr_init(OP_MOV);
    set_op_reg(&dummy_data->dest, REG_QWORD, result_vreg);
    set_op_reg(&dummy_data->src, REG_QWORD, result_vreg);
    status = llist_push_back(instructions, dummy_data);
    if (status) abort();
  } else {
    int status = restore_volatile_regs();
    if (status) abort();
  }
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();
  return call;
}

int translate_params(ASTree *declarator) {
  ASTree *fn_dirdecl = astree_get(declarator, astree_count(declarator) - 1);
  const TypeSpec *fn_type = declarator->type;
  TypeSpec ret_type;
  int status = strip_aux_type(&ret_type, fn_type);
  if (status) return status;
  /* account for hidden out param */
  reg_eightbytes = typespec_get_eightbytes(&ret_type) > 2 ? 1 : 0;
  if (reg_eightbytes == 1) {
    SymbolValue dummy;
    /* TODO(Robert): (void?) pointer type constant */
    dummy.type = SPEC_LONG;
    assign_stack_space(&dummy);
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_reg(&mov_data->src, REG_QWORD, RDI_VREG);
    set_op_ind(&mov_data->dest, -window_size, RBP_VREG, NULL);
    int status = llist_push_back(instructions, mov_data);
    if (status) return status;
  }
  /* offset to account for preserved regs and return address */
  stack_eightbytes = PROLOGUE_EIGHTBYTES;
  size_t i;
  for (i = 0; i < astree_count(fn_dirdecl); ++i) {
    ASTree *param = astree_get(fn_dirdecl, i);
    ASTree *param_decl = astree_get(param, 1);
    SymbolValue *param_symval = NULL;
    assert(state_get_symbol(state, param_decl->lexinfo,
                            strlen(param_decl->lexinfo), &param_symval));
    assert(param_symval);
    size_t param_symval_eightbytes =
        typespec_get_eightbytes(&param_symval->type);
    if (param_symval_eightbytes <= 2 &&
        reg_eightbytes + param_symval_eightbytes <= PARAM_REG_COUNT) {
      assign_stack_space(param_symval);
      ListIter *temp = llist_iter_last(instructions);
      int status =
          bulk_rtom(RBP_VREG, param_symval->disp, PARAM_REGS + reg_eightbytes,
                    &param_symval->type, temp);
      reg_eightbytes += param_symval_eightbytes;
      free(temp);
      if (status) return status;
    } else {
      param_symval->disp = stack_eightbytes * 8;
      stack_eightbytes += param_symval_eightbytes;
    }
  }
  return 0;
}

ASTree *translate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                         ASTree *else_body) {
  int status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);

  ifelse->first_instr = liter_copy(condition->first_instr);
  if (ifelse->first_instr == NULL) abort();

  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;

  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, ifelse->jump_id);
  if (else_body != &EMPTY_EXPR) {
    ASTree *else_body = astree_get(ifelse, 2);
    InstructionData *else_label = instr_init(OP_NOP);
    sprintf(else_label->label, STMT_FMT, ifelse->jump_id);
    int status = liter_push_front(else_body->first_instr, NULL, 1, else_label);
    if (status) abort();

    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, NO_DISP, else_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) abort();

    InstructionData *jmp_data = instr_init(OP_JMP);
    set_op_dir(&jmp_data->dest, NO_DISP, end_label->label);
    status = liter_push_back(if_body->last_instr, NULL, 1, jmp_data);
    if (status) abort();
    status = liter_push_back(else_body->last_instr, &ifelse->last_instr, 1,
                             end_label);
    if (status) abort();
  } else {
    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, NO_DISP, end_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) abort();
    status =
        liter_push_back(if_body->last_instr, &ifelse->last_instr, 1, end_label);
    if (status) abort();
  }
  return astree_adopt(ifelse, 3, condition, if_body, else_body);
}

ASTree *translate_switch(ASTree *switch_, ASTree *condition, ASTree *body) {
  const TypeSpec *control_type = state_get_control_type(state);
  int status = scalar_conversions(condition, control_type);
  if (status) abort();
  InstructionData *cond_data = liter_get(condition->last_instr);
  if (cond_data == NULL) abort();
  switch_->first_instr = liter_copy(condition->first_instr);
  if (switch_->first_instr == NULL) abort();

  /* switch prologue */
  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = cond_data->dest;
  set_op_reg(&mov_data->dest, typespec_get_width(control_type),
             state_get_control_reg(state));
  InstructionData *jmp_case1_data = instr_init(OP_JMP);
  set_op_dir(&jmp_case1_data->dest, NO_DISP, CASE_FMT, switch_->jump_id, 0);
  status =
      liter_push_back(condition->last_instr, NULL, 2, mov_data, jmp_case1_data);
  if (status) abort();

  /* switch epilogue */
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
    if (status) abort();
  } else {
    int status = liter_push_back(body->last_instr, &switch_->last_instr, 3,
                                 jmp_end_data, dummy_case_label, end_label);
    if (status) abort();
  }
  return astree_adopt(switch_, 2, condition, body);
}

ASTree *translate_while(ASTree *while_, ASTree *condition, ASTree *body) {
  InstructionData *condition_label = instr_init(OP_NOP);
  sprintf(condition_label->label, COND_FMT, while_->jump_id);
  /* set first instr to label */
  int status = liter_push_front(condition->first_instr, &while_->first_instr, 1,
                                condition_label);
  if (status) abort();

  status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->src = test_data->dest = condition_data->dest;

  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, while_->jump_id);

  InstructionData *test_jmp_data = instr_init(OP_JZ);
  set_op_dir(&test_jmp_data->dest, NO_DISP, end_label->label);
  status =
      liter_push_back(condition->last_instr, NULL, 2, test_data, test_jmp_data);
  if (status) abort();

  InstructionData *cond_jmp_data = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_data->dest, NO_DISP, condition_label->label);
  /* set last instr to end label */
  status = liter_push_back(body->last_instr, &while_->last_instr, 2,
                           cond_jmp_data, end_label);
  if (status) abort();
  return astree_adopt(while_, 2, condition, body);
}

ASTree *translate_for(ASTree *for_, ASTree *initializer, ASTree *condition,
                      ASTree *reinitializer, ASTree *body) {
  for_->first_instr = liter_copy(initializer->first_instr);
  if (for_->first_instr == NULL) abort();

  InstructionData *condition_start_data = liter_get(condition->first_instr);
  sprintf(condition_start_data->label, COND_FMT, for_->jump_id);
  if (condition->symbol != ';') {
    int status = scalar_conversions(condition, condition->type);
    if (status) abort();
    InstructionData *condition_data = liter_get(condition->last_instr);
    InstructionData *test_data = instr_init(OP_TEST);
    test_data->dest = test_data->src = condition_data->dest;
    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, NO_DISP, END_FMT, for_->jump_id);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) abort();
  }
  InstructionData *body_jmp_data = instr_init(OP_JMP);
  set_op_dir(&body_jmp_data->dest, NO_DISP, STMT_FMT, for_->jump_id);
  int status = liter_push_back(condition->last_instr, NULL, 1, body_jmp_data);
  if (status) abort();

  InstructionData *reinit_start_data = liter_get(reinitializer->first_instr);
  sprintf(reinit_start_data->label, REINIT_FMT, for_->jump_id);
  InstructionData *cond_jmp_data = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_data->dest, NO_DISP, COND_FMT, for_->jump_id);
  status = liter_push_back(reinitializer->last_instr, NULL, 1, cond_jmp_data);
  if (status) abort();

  InstructionData *body_label = instr_init(OP_NOP);
  sprintf(body_label->label, STMT_FMT, for_->jump_id);
  status = liter_push_front(body->first_instr, NULL, 1, body_label);
  if (status) abort();
  InstructionData *reinit_jmp_data = instr_init(OP_JMP);
  set_op_dir(&reinit_jmp_data->dest, NO_DISP, COND_FMT, for_->jump_id);
  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, for_->jump_id);
  status = liter_push_back(body->last_instr, &for_->last_instr, 2,
                           reinit_jmp_data, end_label);
  if (status) abort();
  return astree_adopt(for_, 4, initializer, condition, reinitializer, body);
}

ASTree *translate_do(ASTree *do_, ASTree *body, ASTree *condition) {
  InstructionData *body_label = instr_init(OP_NOP);
  sprintf(body_label->label, STMT_FMT, do_->jump_id);
  int status =
      liter_push_front(body->first_instr, &do_->first_instr, 1, body_label);

  InstructionData *condition_label = instr_init(OP_NOP);
  sprintf(condition_label->label, COND_FMT, do_->jump_id);
  status = liter_push_front(condition->first_instr, NULL, 1, condition_label);
  if (status) abort();

  status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);
  if (condition_data == NULL) abort();
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;

  InstructionData *test_jmp_data = instr_init(OP_JNZ);
  set_op_dir(&test_jmp_data->dest, NO_DISP, body_label->label);

  InstructionData *end_label = instr_init(OP_NOP);
  sprintf(end_label->label, END_FMT, do_->jump_id);
  status = liter_push_back(condition->last_instr, &do_->first_instr, 3,
                           test_data, test_jmp_data, end_label);
  return astree_adopt(do_, 2, body, condition);
}

ASTree *translate_block(ASTree *block) {
  DEBUGS('g', "Translating compound statement");
  if (astree_count(block) == 0) {
    InstructionData *nop_data = instr_init(OP_NOP);
    int status = llist_push_back(instructions, nop_data);
    if (status) abort();
    block->first_instr = llist_iter_last(instructions);
    if (block->first_instr == NULL) abort();
    block->last_instr = liter_copy(block->first_instr);
    if (block->last_instr == NULL) abort();
  } else {
    ASTree *first_stmt = astree_get(block, 0);
    ASTree *last_stmt = astree_get(block, astree_count(block) - 1);
    block->first_instr = liter_copy(first_stmt->first_instr);
    if (block->first_instr == NULL) abort();
    block->last_instr = liter_copy(last_stmt->last_instr);
    if (block->last_instr == NULL) abort();
  }
  return block;
}

int return_scalar(ASTree *ret, ASTree *expr) {
  ret->first_instr = liter_copy(expr->first_instr);
  if (ret->first_instr == NULL) return -1;
  SymbolValue *function_symval = state_get_function(state);
  const TypeSpec *function_spec = &function_symval->type;
  /* strip function */
  TypeSpec return_spec = SPEC_EMPTY;
  int status = strip_aux_type(&return_spec, function_spec);
  if (status) return status;
  ASTree *retval = astree_get(ret, 0);
  status = scalar_conversions(retval, &return_spec);
  if (status) return status;
  InstructionData *retval_data = liter_get(retval->last_instr);

  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = retval_data->dest;
  set_op_reg(&mov_data->dest, typespec_get_width(&return_spec), RAX_VREG);
  status = llist_push_back(instructions, mov_data);
  if (status) return status;
  /* free typespec copies */
  typespec_destroy(&return_spec);

  status = restore_preserved_regs();
  if (status) return status;
  InstructionData *ret_data = instr_init(OP_RET);
  status = llist_push_back(instructions, ret_data);
  if (status) return status;
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) return -1;
  return 0;
}

int return_aggregate(ASTree *ret, ASTree *expr) {
  ret->first_instr = liter_copy(expr->first_instr);
  if (ret->first_instr == NULL) return -1;
  InstructionData *expr_data = liter_get(expr->last_instr);
  size_t expr_eightbytes = typespec_get_eightbytes(expr->type);

  if (expr_eightbytes <= 2) {
    ListIter *temp = llist_iter_last(instructions);
    int status = bulk_mtor(RETURN_REGS, expr_data->dest.reg.num, NO_DISP,
                           expr->type, temp);
    free(temp);
    if (status) return status;
  } else {
    InstructionData *hidden_mov_data = instr_init(OP_MOV);
    set_op_reg(&hidden_mov_data->dest, REG_QWORD, RAX_VREG);
    set_op_ind(&hidden_mov_data->src, -8, RBP_VREG, NULL);
    int status = llist_push_back(instructions, hidden_mov_data);
    if (status) return status;
    ListIter *temp = llist_iter_last(instructions);
    status = bulk_mtom(RAX_VREG, expr_data->dest.reg.num, expr->type, temp);
    free(temp);
    if (status) return status;
  }

  if (ret->first_instr == NULL) return -1;
  int status = restore_preserved_regs();
  if (status) return status;
  InstructionData *ret_data = instr_init(OP_RET);
  status = llist_push_back(instructions, ret_data);
  if (status) return status;
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) return -1;
  return 0;
}

int return_void(ASTree *ret) {
  InstructionData *nop_data = instr_init(OP_NOP);
  int status = llist_push_back(instructions, nop_data);
  if (status) return status;
  ret->first_instr = llist_iter_last(instructions);
  if (ret->first_instr == NULL) return -1;
  status = restore_preserved_regs();
  if (status) return status;
  InstructionData *ret_data = instr_init(OP_RET);
  status = llist_push_back(instructions, ret_data);
  if (status) return status;
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) return -1;
  return 0;
}

ASTree *translate_return(ASTree *ret, ASTree *expr) {
  DEBUGS('g', "Translating return statement");
  if (expr == &EMPTY_EXPR) {
    int status = return_void(ret);
    if (status) abort();
  } else {
    if (typespec_is_union(expr->type) || typespec_is_struct(expr->type)) {
      int status = return_aggregate(ret, expr);
      if (status) abort();
    } else {
      int status = return_scalar(ret, expr);
      if (status) abort();
    }
  }
  return astree_adopt(ret, 1, expr);
}

ASTree *translate_continue(ASTree *continue_) {
  InstructionData *cond_jmp_data = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_data->dest, NO_DISP, COND_FMT, continue_->jump_id);
  int status = llist_push_back(instructions, cond_jmp_data);
  if (status) abort();
  continue_->first_instr = llist_iter_last(instructions);
  if (continue_->first_instr == NULL) abort();
  continue_->last_instr = liter_copy(continue_->first_instr);
  if (continue_->last_instr == NULL) abort();
  return continue_;
}

ASTree *translate_break(ASTree *break_) {
  InstructionData *end_jmp_data = instr_init(OP_JMP);
  set_op_dir(&end_jmp_data->dest, NO_DISP, END_FMT, break_->jump_id);
  int status = llist_push_back(instructions, break_);
  if (status) abort();
  break_->first_instr = llist_iter_last(instructions);
  if (break_->first_instr == NULL) abort();
  break_->last_instr = liter_copy(break_->first_instr);
  if (break_->last_instr == NULL) abort();
  return break_;
}

ASTree *translate_goto(ASTree *goto_, ASTree *ident) {
  const char *ident_str = ident->lexinfo;
  InstructionData *jmp_data = instr_init(OP_JMP);
  set_op_dir(&jmp_data->dest, NO_DISP, ident_str);
  int status = llist_push_back(instructions, jmp_data);
  if (status) abort();
  goto_->first_instr = llist_iter_last(instructions);
  if (goto_->first_instr == NULL) abort();
  goto_->last_instr = liter_copy(goto_->first_instr);
  if (goto_->last_instr == NULL) abort();
  return astree_adopt(goto_, 1, ident);
}

ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt) {
  const char *ident_str = ident->lexinfo;
  InstructionData *label_data = instr_init(OP_NOP);
  strcpy(label_data->label, ident_str);

  int status =
      liter_push_front(stmt->first_instr, &label->first_instr, 1, label_data);
  if (status) abort();
  label->last_instr = liter_copy(stmt->last_instr);
  if (label->last_instr == NULL) abort();
  return astree_adopt(label, 2, ident, stmt);
}

ASTree *translate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  case_->last_instr = liter_copy(stmt->last_instr);
  if (case_->last_instr == 0) abort();

  const TypeSpec *control_type = state_get_control_type(state);
  if (control_type == &SPEC_EMPTY) abort();

  int status = scalar_conversions(expr, control_type);
  InstructionData *expr_data = liter_get(expr->last_instr);
  if (expr_data == NULL) abort();

  InstructionData *test_data = instr_init(OP_TEST);
  set_op_reg(&test_data->dest, typespec_get_width(control_type),
             state_get_control_reg(state));
  test_data->src = expr_data->dest;
  InstructionData *jmp_data = instr_init(OP_JNE);
  set_op_dir(&jmp_data->dest, NO_DISP, CASE_FMT, state_get_case_id(state));
  InstructionData *fall_label = instr_init(OP_NOP);
  sprintf(fall_label->label, FALL_FMT, case_->jump_id, case_->case_id);
  InstructionData *case_label = instr_init(OP_NOP);
  sprintf(case_label->label, CASE_FMT, case_->jump_id, case_->case_id);
  InstructionData *fall_jmp_data = instr_init(OP_JMP);
  set_op_dir(&fall_jmp_data->dest, NO_DISP, fall_label->label);
  status = liter_push_front(expr->first_instr, &case_->first_instr, 2,
                            fall_jmp_data, case_label);
  if (status) abort();
  status = liter_push_back(expr->last_instr, NULL, 3, test_data, jmp_data,
                           fall_label);
  if (status) abort();
  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *translate_default(ASTree *default_, ASTree *stmt) {
  InstructionData *def_label = instr_init(OP_NOP);
  sprintf(def_label->label, DEF_FMT, default_->jump_id);

  int status =
      liter_push_front(stmt->first_instr, &default_->first_instr, 1, def_label);
  if (status) abort();
  default_->last_instr = liter_copy(stmt->last_instr);
  if (default_->last_instr == NULL) abort();
  return astree_adopt(default_, 1, stmt);
}

int init_scalar(const TypeSpec *type, ptrdiff_t displacement,
                ASTree *initializer, ListIter *where) {
  assert(!typespec_is_aggregate(type));
  if (initializer->symbol == TOK_INIT_LIST) {
    assert(astree_count(initializer) == 1);
    int status =
        init_scalar(type, displacement, astree_get(initializer, 0), where);
    if (status) return status;
    initializer->first_instr =
        liter_copy(astree_get(initializer, 0)->first_instr);
    if (initializer->first_instr == NULL) return -1;
    initializer->last_instr = liter_copy(where);
    if (initializer->last_instr == NULL) return -1;
    return 0;
  } else if (displacement >= 0) {
    Opcode directive;
    switch (typespec_get_width(type)) {
      case X64_SIZEOF_LONG:
        directive = OP_QUAD;
        break;
      case X64_SIZEOF_INT:
        directive = OP_LONG;
        break;
      case X64_SIZEOF_SHORT:
        directive = OP_VALUE;
        break;
      case X64_SIZEOF_CHAR:
        directive = OP_BYTE;
        break;
      default:
        abort();
    }
    InstructionData *data = instr_init(directive);
    set_op_imm(&data->dest, initializer->constant.integral.value);
    return liter_push_back(where, &where, 1, data);
  } else if (initializer->attributes & ATTR_EXPR_CONST) {
    /* TODO(Robert): update flag once merged with main, and remove all
     * instructions there were emitted for this initializer. or, just don't
     * emit any instructions for valid constant expressions until they are
     * used in a non-constant expression
     */
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_imm(&mov_data->src, initializer->constant.integral.value);
    set_op_reg(&mov_data->dest, typespec_get_width(type), next_vreg());
    InstructionData *mov_data_2 = instr_init(OP_MOV);
    mov_data_2->src = mov_data->dest;
    set_op_ind(&mov_data_2->dest, displacement, RBP_VREG, NULL);
    return liter_push_back(where, &where, 2, mov_data, mov_data_2);
  } else {
    int status = scalar_conversions(initializer, type);
    if (status) return status;
    InstructionData *initializer_data = liter_get(initializer->last_instr);
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_ind(&mov_data->dest, displacement, RBP_VREG, NULL);
    mov_data->src = initializer_data->dest;
    return liter_push_back(where, &where, 1, mov_data);
  }
}

int init_aggregate(const TypeSpec *agg_type, ptrdiff_t agg_disp,
                   ASTree *initializer, size_t start, ListIter *where);

int init_literal(const TypeSpec *arr_type, ptrdiff_t arr_disp,
                 ASTree *initializer, ListIter *where) {
  if (arr_disp >= 0) {
    size_t i;
    for (i = 0; i < literals_size; ++i) {
      if (literals[i].label == initializer->constant.address.label) {
        InstructionData *ascii_data = instr_init(OP_ASCII);
        /* TODO(Robert): ugly cast */
        set_op_imm(&ascii_data->dest, (uintmax_t)literals[i].literal);
        /* TODO(Robert): check that the string literal fits in the array
         * during type checking
         */
        /* TODO(Robert): emit .asciz when the array size is deduced */
        assert(typespec_get_width(arr_type) > strlen(literals[i].literal) - 2);
        size_t zero_count =
            typespec_get_width(arr_type) - (strlen(literals[i].literal) - 2);
        if (zero_count > 0) {
          InstructionData *zero_data = instr_init(OP_ZERO);
          set_op_imm(&zero_data->dest, zero_count);
          return liter_push_back(where, &where, 2, ascii_data, zero_data);
        } else {
          return liter_push_back(where, &where, 1, ascii_data);
        }
      }
    }
    /* literal not found */
    abort();
  } else {
    InstructionData *literal_lea_data = instr_init(OP_LEA);
    set_op_dir(&literal_lea_data->src, initializer->constant.address.disp,
               initializer->constant.address.label);
    set_op_reg(&literal_lea_data->dest, REG_QWORD, next_vreg());
    InstructionData *arr_lea_data = instr_init(OP_LEA);
    set_op_ind(&arr_lea_data->src, arr_disp, RBP_VREG, NULL);
    set_op_reg(&arr_lea_data->dest, REG_QWORD, next_vreg());
    int status =
        liter_push_back(where, &where, 2, literal_lea_data, arr_lea_data);
    if (status) abort();
    /* TODO(Robert): make sure that iterators allow you to move freely to and
     * from the anchor, just not over it
     */
    status = liter_advance(where, 1);
    if (status) abort();
    ListIter *saved_location = liter_prev(where, 1);
    status =
        bulk_mtom(arr_lea_data->dest.reg.num, literal_lea_data->dest.reg.num,
                  arr_type, saved_location);
    free(saved_location);
    if (status) abort();
    status = liter_advance(where, -1);
    if (status) abort();
    return 0;
  }
}

int init_array(const TypeSpec *arr_type, ptrdiff_t arr_disp, ASTree *init_list,
               size_t start, ListIter *where) {
  TypeSpec elem_type;
  int status = strip_aux_type(&elem_type, arr_type);
  if (status) return -1;
  AuxSpec *arr_aux = llist_front(&arr_type->auxspecs);
  size_t elem_width = typespec_get_width(&elem_type);
  size_t elem_count = arr_aux->data.memory_loc.length;
  size_t init_count = astree_count(init_list);
  size_t init_index, elem_index;
  for (elem_index = 0, init_index = start;
       elem_index < elem_count && init_index < init_count;
       ++elem_index, ++init_index) {
    ASTree *initializer = astree_get(init_list, init_index);
    ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
    if (typespec_is_aggregate(&elem_type)) {
      if (initializer->symbol == TOK_INIT_LIST) {
        int consumed =
            init_aggregate(&elem_type, elem_disp, initializer, 0, where);
        if (consumed < 0) return consumed;
      } else {
        int consumed =
            init_aggregate(&elem_type, elem_disp, init_list, init_index, where);
        if (consumed < 0) return consumed;
        init_index += consumed - 1;
      }
    } else {
      int status = init_scalar(&elem_type, elem_disp, initializer, where);
      if (status) return -1;
    }
  }
  if (arr_disp >= 0) {
    size_t zero_count =
        (elem_count - elem_index) * typespec_get_width(&elem_type);
    if (zero_count > 0) {
      InstructionData *zero_data = instr_init(OP_ZERO);
      set_op_imm(&zero_data->dest, zero_count);
      int status = liter_push_back(where, &where, 1, zero_data);
      if (status) return -1;
    }
  } else {
    for (; elem_index < elem_count; ++elem_index) {
      ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->dest, typespec_get_width(&elem_type), next_vreg());
      set_op_imm(&mov_data->src, 0);
      InstructionData *mov_data_2 = instr_init(OP_MOV);
      mov_data_2->src = mov_data->dest;
      set_op_ind(&mov_data_2->dest, elem_disp, RBP_VREG, NULL);
      int status = liter_push_back(where, &where, 2, mov_data, mov_data_2);
      if (status) return -1;
    }
  }
  typespec_destroy(&elem_type);
  return init_index - start;
}

int init_struct(const TypeSpec *struct_type, ptrdiff_t struct_disp,
                ASTree *init_list, size_t start, ListIter *where) {
  size_t member_count = typespec_member_count(struct_type);
  size_t init_count = astree_count(init_list);
  size_t bytes_initialized = 0;
  size_t init_index, member_index;
  for (member_index = 0, init_index = start;
       member_index < member_count && init_index < init_count;
       ++member_index, ++init_index) {
    SymbolValue *member_symval =
        typespec_member_index(struct_type, member_index);
    TypeSpec *member_type = &member_symval->type;
    ASTree *initializer = astree_get(init_list, init_index);
    size_t member_alignment = typespec_get_alignment(member_type);
    ptrdiff_t member_disp = struct_disp + member_symval->disp;
    size_t padding = member_alignment - (bytes_initialized % member_alignment);
    if (padding == member_alignment) padding = 0;
    if (member_disp >= 0 && padding > 0) {
      InstructionData *zero_data = instr_init(OP_ZERO);
      set_op_imm(&zero_data->dest, padding);
      int status = liter_push_back(where, &where, 1, zero_data);
      if (status) return -1;
    }
    if (typespec_is_aggregate(member_type)) {
      if (initializer->symbol == TOK_INIT_LIST) {
        int consumed =
            init_aggregate(member_type, member_disp, initializer, 0, where);
        if (consumed < 0) return consumed;
      } else {
        int consumed = init_aggregate(member_type, member_disp, init_list,
                                      init_index, where);
        if (consumed < 0) return consumed;
        init_index += consumed - 1;
      }
    } else {
      int status = init_scalar(member_type, member_disp, initializer, where);
      if (status) return -1;
    }
    bytes_initialized += padding + typespec_get_width(member_type);
  }
  if (struct_disp >= 0) {
    size_t zero_count = typespec_get_width(struct_type) - bytes_initialized;
    if (zero_count > 0) {
      InstructionData *zero_data = instr_init(OP_ZERO);
      set_op_imm(&zero_data->dest, zero_count);
      int status = liter_push_back(where, &where, 1, zero_data);
      if (status) return -1;
    }
  } else {
    for (; member_index < member_count; ++member_index) {
      SymbolValue *member_symval =
          typespec_member_index(struct_type, member_index);
      TypeSpec *member_type = &member_symval->type;
      ptrdiff_t member_disp = struct_disp + member_symval->disp;
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->dest, typespec_get_width(member_type), next_vreg());
      set_op_imm(&mov_data->src, 0);
      InstructionData *mov_data_2 = instr_init(OP_MOV);
      mov_data_2->src = mov_data->dest;
      set_op_ind(&mov_data_2->dest, member_disp, RBP_VREG, NULL);
      int status = liter_push_back(where, &where, 2, mov_data, mov_data_2);
      if (status) return -1;
    }
  }
  return init_index - start;
}

int init_union(const TypeSpec *union_type, ptrdiff_t union_disp,
               ASTree *init_list, size_t start, ListIter *where) {
  SymbolValue *member_symval = typespec_member_index(union_type, 0);
  TypeSpec *member_type = &member_symval->type;
  ASTree *initializer = astree_get(init_list, start);
  int consumed;
  if (typespec_is_aggregate(member_type)) {
    if (initializer->symbol == TOK_INIT_LIST) {
      consumed = init_aggregate(member_type, union_disp, initializer, 0, where);
      if (consumed < 0) return consumed;
    } else {
      consumed =
          init_aggregate(member_type, union_disp, init_list, start, where);
      if (consumed < 0) return consumed;
    }
  } else {
    int status = init_scalar(member_type, union_disp, initializer, where);
    if (status) return -1;
    consumed = 1;
  }
  size_t zero_count =
      typespec_get_width(union_type) - typespec_get_width(member_type);
  if (union_disp >= 0 && zero_count > 0) {
    InstructionData *zero_data = instr_init(OP_ZERO);
    set_op_imm(&zero_data->dest, zero_count);
    int status = liter_push_back(where, &where, 1, zero_data);
    if (status) return -1;
  }
  return consumed;
}

int init_aggregate(const TypeSpec *agg_type, ptrdiff_t agg_disp,
                   ASTree *initializer, size_t start, ListIter *where) {
  assert(typespec_is_aggregate(agg_type) &&
         (initializer->symbol == TOK_INIT_LIST ||
          initializer->symbol == TOK_STRINGCON));
  int consumed = -1;
  if (typespec_is_array(agg_type)) {
    if (initializer->symbol == TOK_INIT_LIST) {
      consumed = init_array(agg_type, agg_disp, initializer, start, where);
    } else if (initializer->symbol == TOK_STRINGCON) {
      consumed = init_literal(agg_type, agg_disp, initializer, where);
    }
  } else if (typespec_is_struct(agg_type)) {
    consumed = init_struct(agg_type, agg_disp, initializer, start, where);
  } else if (typespec_is_union(agg_type)) {
    consumed = init_union(agg_type, agg_disp, initializer, start, where);
  }
  if (consumed < 0) return consumed;
  initializer->first_instr =
      liter_copy(astree_get(initializer, 0)->first_instr);
  if (initializer->first_instr == NULL) return -1;
  initializer->last_instr = liter_copy(where);
  if (initializer->last_instr == NULL) return -1;
  return consumed;
}

int translate_static_prelude(ASTree *declarator, SymbolValue *symval,
                             ListIter *where) {
  if (symval->flags & SYMFLAG_LINK_EXT) {
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, NO_DISP, declarator->lexinfo);
    int status = liter_push_back(where, &where, 1, globl_data);
    if (status) return status;
  }

  char identifier[MAX_LABEL_LENGTH];
  if (symval->flags & SYMFLAG_LINK_NONE) {
    sprintf(identifier, "%s.%lu", declarator->lexinfo, symval->static_id);
  } else {
    strcpy(identifier, declarator->lexinfo);
  }

  InstructionData *align_data = instr_init(OP_ALIGN);
  set_op_imm(&align_data->dest, typespec_get_alignment(declarator->type));
  InstructionData *type_data = instr_init(OP_TYPE);
  set_op_dir(&type_data->dest, NO_DISP, identifier);
  set_op_dir(&type_data->src, NO_DISP, "@object");
  InstructionData *size_data = instr_init(OP_SIZE);
  set_op_dir(&size_data->dest, NO_DISP, identifier);
  set_op_imm(&size_data->src, typespec_get_width(declarator->type));
  InstructionData *label_data = instr_init(OP_INVALID);
  strcpy(label_data->label, identifier);
  return liter_push_back(where, &where, 4, align_data, type_data, size_data,
                         label_data);
}

ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                             ASTree *declarator, ASTree *initializer) {
  DEBUGS('g', "Translating local initialization");
  SymbolValue *symval = NULL;
  assert(state_get_symbol(state, (char *)declarator->lexinfo,
                          strlen(declarator->lexinfo), &symval));
  assert(symval);

  if (symval->flags & SYMFLAG_STORE_STAT) {
    assign_static_space(declarator->lexinfo, symval);
    int status =
        translate_static_prelude(declarator, symval, before_definition);
    if (status) abort();
  } else {
    assign_stack_space(symval);
  }

  if (typespec_is_aggregate(declarator->type)) {
    int consumed = init_aggregate(declarator->type, symval->disp, initializer,
                                  0, before_definition);
    if (consumed < 0) abort();
  } else {
    int status = init_scalar(declarator->type, symval->disp, initializer,
                             before_definition);
    if (status) abort();
  }
  return astree_adopt(declaration, 1,
                      astree_adopt(assignment, 2, declarator, initializer));
}

ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator) {
  DEBUGS('g', "Translating local declaration");
  SymbolValue *symval = NULL;
  assert(state_get_symbol(state, (char *)declarator->lexinfo,
                          strlen(declarator->lexinfo), &symval));
  assert(symval);
  if (!(symval->flags & SYMFLAG_INHERIT)) {
    if (symval->flags & SYMFLAG_STORE_STAT) {
      assign_static_space(declarator->lexinfo, symval);
      int status =
          translate_static_prelude(declarator, symval, before_definition);
      if (status) abort();
    } else if (symval->flags & SYMFLAG_STORE_AUTO) {
      assign_stack_space(symval);
    }
  }
  return astree_adopt(declaration, 1, declarator);
}

ASTree *translate_global_init(ASTree *declaration, ASTree *assignment,
                              ASTree *declarator, ASTree *initializer) {
  DEBUGS('g', "Translating global initialization");
  SymbolValue *symval = NULL;
  assert(state_get_symbol(state, (char *)declarator->lexinfo,
                          strlen(declarator->lexinfo), &symval));
  assert(symval);

  InstructionData *data_data = instr_init(OP_DATA);
  int status = llist_push_back(instructions, data_data);
  if (status) abort();
  ListIter *temp = llist_iter_last(instructions);
  status = translate_static_prelude(declarator, symval, temp);
  if (status) abort();
  if (typespec_is_aggregate(declarator->type)) {
    int consumed = init_aggregate(declarator->type, 0, initializer, 0, temp);
    free(temp);
    if (consumed < 0) abort();
  } else {
    int status = init_scalar(declarator->type, 0, initializer, temp);
    free(temp);
    if (status) abort();
  }

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return astree_adopt(declaration, 1,
                      astree_adopt(assignment, 2, declarator, initializer));
  ;
}

ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator) {
  DEBUGS('g', "Translating global declaration");
  SymbolValue *symval = NULL;
  assert(state_get_symbol(state, (char *)declarator->lexinfo,
                          strlen(declarator->lexinfo), &symval));
  assert(symval);

  if (symval->flags & SYMFLAG_STORE_STAT) {
    InstructionData *bss_data = instr_init(OP_BSS);
    int status = llist_push_back(instructions, bss_data);
    if (status) abort();
    ListIter *temp = llist_iter_last(instructions);
    status = translate_static_prelude(declarator, symval, temp);
    free(temp);
    if (status) abort();
    InstructionData *zero_data = instr_init(OP_ZERO);
    set_op_imm(&zero_data->dest, typespec_get_width(declarator->type));
    status = llist_push_back(instructions, zero_data);
    if (status) abort();
  }

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return astree_adopt(declaration, 1, declarator);
}

int translate_fn_prelude(ASTree *declarator) {
  InstructionData *text_data = instr_init(OP_TEXT);
  int status = llist_push_back(instructions, text_data);
  if (status) return status;
  SymbolValue *symval = NULL;
  state_get_symbol(state, declarator->lexinfo, strlen(declarator->lexinfo),
                   &symval);
  assert(symval != NULL);
  if (symval->flags & SYMFLAG_LINK_EXT) {
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, NO_DISP, declarator->lexinfo);
    status = llist_push_back(instructions, globl_data);
    if (status) return status;
  }
  InstructionData *type_data = instr_init(OP_TYPE);
  set_op_dir(&type_data->dest, NO_DISP, declarator->lexinfo);
  set_op_dir(&type_data->src, NO_DISP, "@function");
  status = llist_push_back(instructions, type_data);
  if (status) return status;
  InstructionData *label_data = instr_init(OP_NOP);
  strcpy(label_data->label, declarator->lexinfo);
  return llist_push_back(instructions, label_data);
}

ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body) {
  DEBUGS('g', "Translating function prologue");
  branch_count = window_size = 0;
  int status = translate_fn_prelude(declarator);
  if (status) abort();

  declaration->first_instr = llist_iter_last(instructions);
  if (declaration->first_instr == NULL) abort();

  status = save_preserved_regs();
  if (status) abort();

  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_reg(&mov_data->dest, REG_QWORD, RBP_VREG);
  set_op_reg(&mov_data->src, REG_QWORD, RSP_VREG);
  status = llist_push_back(instructions, mov_data);
  if (status) abort();

  /* save location for later rsp adjustment */
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  status = translate_params(declarator);
  if (status) abort();
  return astree_adopt(declaration, 2, declarator, body);
}

ASTree *end_translate_fn(ASTree *declaration) {
  /* emit rsp adjustment now that we know the amount of stack space */
  InstructionData *rsp_sub_data = instr_init(OP_SUB);
  set_op_reg(&rsp_sub_data->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_sub_data->src, window_size);
  int status = liter_push_back(declaration->last_instr, NULL, 1, rsp_sub_data);
  if (status) abort();
  free(declaration->last_instr);
  declaration->last_instr = NULL;

  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_reg(&mov_data->dest, REG_QWORD, RSP_VREG);
  set_op_reg(&mov_data->src, REG_QWORD, RBP_VREG);
  status = llist_push_back(instructions, mov_data);
  if (status) abort();
  status = restore_preserved_regs();
  if (status) abort();
  InstructionData *return_data = instr_init(OP_RET);
  status = llist_push_back(instructions, return_data);
  if (status) abort();
  ASTree *declarator = astree_get(declaration, 1);
  InstructionData *size_data = instr_init(OP_SIZE);
  set_op_dir(&size_data->dest, NO_DISP, declarator->lexinfo);
  set_op_dir(&size_data->src, NO_DISP, ".-%s", declarator->lexinfo);
  status = llist_push_back(instructions, size_data);
  if (status) abort();
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return declaration;
}

int operand_to_str(Operand *operand, char *str, size_t size) {
  switch (operand->all.mode) {
    case MODE_NONE:
      str[0] = 0;
      return 0;
    case MODE_REGISTER:
      return sprintf(str, "vr%lu%c", operand->reg.num,
                     WIDTH_TO_CHAR[operand->reg.num]);
    case MODE_SCALE:
      return sprintf(str, "%s%+li(%%vr%luq, %%vr%luq, %u)", operand->sca.lab,
                     operand->sca.disp, operand->sca.base, operand->sca.index,
                     operand->sca.scale);
    case MODE_IMMEDIATE:
      return sprintf(str, "$%lu", operand->imm.val);
    case MODE_DIRECT:
      return sprintf(str, "%s%+li", operand->dir.lab, operand->dir.disp);
    case MODE_INDIRECT:
      return sprintf(str, "%s%+li(vr%luq)", operand->ind.lab, operand->ind.disp,
                     operand->ind.num);
    default:
      abort();
  }
}

int opcode_to_str(InstructionData *data, char *str, size_t size) {
  switch (optype_from_opcode(data->opcode)) {
    case OPTYPE_BINARY:
      if (data->opcode == OP_MOVS || data->opcode == OP_MOVZ) {
        assert(data->src.all.mode == MODE_REGISTER &&
               data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->src.reg.num],
                       WIDTH_TO_CHAR[data->dest.reg.num]);
      } else if (data->src.all.mode == MODE_REGISTER) {
        assert(data->dest.all.mode != MODE_REGISTER ||
               data->src.reg.width == data->dest.reg.width);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->src.reg.num]);
      } else {
        assert(data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->dest.reg.num]);
      }
    case OPTYPE_UNARY:
      if (opcode_needs_width(data->opcode)) {
        assert(data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->dest.reg.num]);
      } else {
        return sprintf(str, "%s", OPCODES[data->opcode]);
      }
    case OPTYPE_NULLARY:
      return sprintf(str, "%s", OPCODES[data->opcode]);
    case OPTYPE_INVALID:
      /* fallthrough */
    case OPTYPE_DIRECTIVE:
      /* fallthrough */
    default:
      abort();
  }
}

int bin_to_str(InstructionData *data, char *str, size_t size) {
  char opcode_str[MAX_OPCODE_LENGTH];
  int chars_written = opcode_to_str(data, opcode_str, MAX_OPCODE_LENGTH);
  if (chars_written < 0) return chars_written;

  char dest_str[MAX_OPERAND_LENGTH];
  chars_written = operand_to_str(&data->dest, dest_str, MAX_OPERAND_LENGTH);
  if (chars_written < 0) return chars_written;

  char src_str[MAX_OPERAND_LENGTH];
  chars_written = operand_to_str(&data->src, src_str, MAX_OPERAND_LENGTH);
  if (chars_written < 0) return chars_written;

  return sprintf(str, "%s %s, %s", OPCODES[data->opcode], src_str, dest_str);
}

int un_to_str(InstructionData *data, char *str, size_t size) {
  char opcode_str[MAX_OPCODE_LENGTH];
  int chars_written = opcode_to_str(data, opcode_str, MAX_OPCODE_LENGTH);
  if (chars_written < 0) return chars_written;

  char dest_str[MAX_OPERAND_LENGTH];
  chars_written = operand_to_str(&data->dest, dest_str, MAX_OPERAND_LENGTH);
  if (chars_written < 0) return chars_written;

  return sprintf(str, "%s %s", OPCODES[data->opcode], dest_str);
}

int dir_to_str(InstructionData *data, char *str, size_t size) {
  switch (data->opcode) {
    case OP_FILE:
      /* fallthrough */
    case OP_GLOBL:
      /* fallthrough */
    case OP_SECTION:
      assert(data->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s", OPCODES[data->opcode], data->dest.dir.lab);
    case OP_ZERO:
      /* fallthrough */
    case OP_ALIGN:
      assert(data->dest.all.mode == MODE_IMMEDIATE);
      return sprintf(str, ".%s %lu", OPCODES[data->opcode], data->dest.imm.val);
    case OP_BYTE:
      /* fallthrough */
    case OP_VALUE:
      /* fallthrough */
    case OP_LONG:
      /* fallthrough */
    case OP_QUAD:
      assert(data->dest.all.mode == MODE_IMMEDIATE ||
             data->dest.all.mode == MODE_DIRECT);
      if (data->dest.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %li", OPCODES[data->opcode],
                       data->dest.imm.val);
      else
        return sprintf(str, ".%s %s%+li", OPCODES[data->opcode],
                       data->dest.dir.lab, data->dest.dir.disp);
    case OP_SIZE:
      assert(data->dest.all.mode == MODE_DIRECT);
      assert(data->src.all.mode == MODE_DIRECT ||
             data->src.all.mode == MODE_IMMEDIATE);
      if (data->src.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %s, %lu", OPCODES[OP_SIZE], data->dest.dir.lab,
                       data->src.imm.val);
      else
        return sprintf(str, ".%s %s, %s%+li", OPCODES[OP_SIZE],
                       data->dest.dir.lab, data->src.dir.lab,
                       data->src.dir.disp);
    case OP_TYPE:
      assert(data->dest.all.mode == MODE_DIRECT);
      assert(data->src.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s, %s", OPCODES[OP_TYPE], data->dest.dir.lab,
                     data->src.dir.lab);
    case OP_ASCIZ:
    case OP_ASCII:
      assert(data->dest.all.mode == MODE_IMMEDIATE);
      return sprintf(str, ".%s %s", OPCODES[data->opcode],
                     (const char *)data->dest.imm.val);
    case OP_BSS:
      /* fallthrough */
    case OP_TEXT:
      /* fallthrough */
    case OP_DATA:
      return sprintf(str, ".%s", OPCODES[data->opcode]);
    default:
      abort();
  }
}

int instr_to_str(InstructionData *data, char *str, size_t size) {
  int ret = 0;
  if (strlen(data->label) > 0) {
    int chars_written = sprintf(
        str + ret,
        data->opcode == OP_INVALID && strlen(data->comment) == 0 ? "%s:"
                                                                 : "%s: ",
        data->label);
    if (chars_written < 0) return chars_written;
    ret += chars_written;
  } else {
    str[ret++] = '\t';
    str[ret] = '\0';
  }
  switch (optype_from_opcode(data->opcode)) {
    int chars_written;
    case OPTYPE_DIRECTIVE:
      chars_written = dir_to_str(data, str + ret, size - ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_BINARY:
      chars_written = bin_to_str(data, str + ret, size - ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_UNARY:
      chars_written = un_to_str(data, str + ret, size - ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_NULLARY:
      chars_written = opcode_to_str(data, str + ret, size - ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_INVALID:
      break;
    default:
      abort();
  }
  if (strlen(data->comment) > 0) {
    int chars_written =
        sprintf(str + ret, ret > 0 ? " # %s" : "# %s", data->comment);
    if (chars_written < 0)
      return chars_written;
    else
      ret += chars_written;
  }
  return ret;
}

int generator_print_il(FILE *out) {
  char buffer[1024];
  size_t i;
  for (i = 0; i < llist_size(instructions); ++i) {
    InstructionData *data = llist_get(instructions, i);
    int chars_written = instr_to_str(data, buffer, 1024);
    if (chars_written < 0) return chars_written;
    chars_written = fprintf(out, "%s\n", buffer);
    if (chars_written < 0) return chars_written;
  }
  return 0;
}

static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_IDENT_LEN);
  }
  return ret;
}

void asmgen_init_globals(const char *filename) {
  instructions = malloc(sizeof(*instructions));
  assert(!llist_init(instructions, free, NULL));
  InstructionData *file_data = instr_init(OP_FILE);
  set_op_dir(&file_data->dest, NO_DISP, filename);
  assert(!llist_push_back(instructions, file_data));
  assert(before_definition = llist_iter_last(instructions));
  literals = malloc(sizeof(*literals) * literals_cap);
  static_locals = malloc(sizeof(*static_locals));
  assert(
      !map_init(static_locals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper));
}

void asmgen_free_globals(void) {
  free(before_definition);
  assert(!llist_destroy(instructions));
  free(literals);
  assert(!map_destroy(static_locals));
}
