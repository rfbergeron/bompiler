#include "asmgen.h"

#include <string.h>

#include "assert.h"
#include "astree.h"
#include "badalist.h"
#include "debug.h"
#include "init.h"
#include "lyutils.h"
#include "regalloc.h"
#include "state.h"
#include "symtable.h"

#define MAX_OPCODE_LENGTH 8
#define MAX_OPERAND_LENGTH 64
#define MAX_LABEL_LENGTH 64
#define MAX_INSTR_LENGTH 1024
#define NO_DISP 0
#define IMM_SIGNED 1
#define IMM_UNSIGNED 0
#define MAX_IDENT_LEN 31
#define MAX_INSTR_DEBUG_LENGTH 4096
#define MAX_OPERAND_DEBUG_LENGTH 1024

#define GENERATE_STRING(CODE, TYPE, BOOL, WRITES) #CODE,
#define GENERATE_TYPE(CODE, TYPE, BOOL, WRITES) \
  case OP_##CODE:                               \
    return TYPE;
#define GENERATE_NEEDS_WIDTH(CODE, TYPE, BOOL, WRITES) \
  case OP_##CODE:                                      \
    return BOOL;

#define GENERATE_T1_REGS(REGBASE) \
  #REGBASE "L", #REGBASE "X", "E" #REGBASE "X", "R" #REGBASE "X"
#define GENERATE_T2_REGS(REGBASE) \
  #REGBASE "L", #REGBASE, "E" #REGBASE, "R" #REGBASE
#define GENERATE_T3_REGS(REGBASE) \
  #REGBASE "B", #REGBASE "W", #REGBASE "D", #REGBASE
#define SELECT_REG(NUM, WIDTH)                            \
  VREG_REG_TABLE[((NUM * 4) + ((WIDTH) == REG_BYTE    ? 0 \
                               : (WIDTH) == REG_WORD  ? 1 \
                               : (WIDTH) == REG_DWORD ? 2 \
                               : (WIDTH) == REG_QWORD ? 3 \
                                                      : (abort(), -1)))]

const char WIDTH_TO_CHAR[] = {'@', 'B', 'W', '@', 'L', '@', '@', '@', 'Q'};
const char OPCODES[][MAX_OPCODE_LENGTH] = {FOREACH_OPCODE(GENERATE_STRING)};
static const char LOCAL_FMT[] = ".L%s";
static const char STATIC_FMT[] = "%s.%lu";
static const char COND_FMT[] = ".LC%lu";
static const char END_FMT[] = ".LE%lu";
static const char STMT_FMT[] = ".LS%lu";
static const char REINIT_FMT[] = ".LR%lu";
static const char TRUE_FMT[] = ".LT%lu";
static const char FALSE_FMT[] = ".LF%lu";
static const char DEF_FMT[] = ".LD%lu";
static const char CASE_FMT[] = ".LS%luC%lu";
static const char FALL_FMT[] = ".LS%luF%lu";
static const char STR_FMT[] = ".LSTR%lu";
static const char FN_SIZE_FMT[] = ".-%s";
static const char FN_PTR_FMT[] = "%s@GOTPCREL";

/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and
 * displacement is a signed 32-bit integer.
 */
/* register order: rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8-r15 */
/* argument registers, in order: rdi, rsi, rdx, rcx, r8, r9 */
/* return registers: rax, rdx
 * preserved registers: rbx, rsp, rbp, r12-r15
 * other registers: r10, r11
 */
static const size_t RAX_VREG = 0;
static const size_t RCX_VREG = 1;
static const size_t RDX_VREG = 2;
static const size_t RBX_VREG = 3;
const size_t RSP_VREG = 4;
const size_t RBP_VREG = 5;
static const size_t RSI_VREG = 6;
static const size_t RDI_VREG = 7;
static const size_t PARAM_REGS[] = {RDI_VREG, RSI_VREG, RDX_VREG,
                                    RCX_VREG, 8,        9};
static const size_t RETURN_REGS[] = {RAX_VREG, RDX_VREG};
static const size_t PRESERVED_REGS[] = {RBX_VREG, RSP_VREG, RBP_VREG, 12,
                                        13,       14,       15};
const size_t VOLATILE_REGS[] = {
    RAX_VREG, RDX_VREG, RCX_VREG, RSI_VREG, RDI_VREG, 8, 9, 10, 11};
static const size_t PARAM_REG_COUNT = 6;
/* static const size_t RETURN_REG_COUNT = 2; */
static const size_t PRESERVED_REG_COUNT = 7;
const size_t VOLATILE_REG_COUNT = 9;
const size_t REAL_REG_COUNT = PRESERVED_REG_COUNT + VOLATILE_REG_COUNT;
/* number of eightbytes occupied by the function prologue on the stack */
static const size_t PROLOGUE_EIGHTBYTES = 8;
static const char VREG_REG_TABLE[][5] = {
    GENERATE_T1_REGS(A),   GENERATE_T1_REGS(C),   GENERATE_T1_REGS(D),
    GENERATE_T1_REGS(B),   GENERATE_T2_REGS(SP),  GENERATE_T2_REGS(BP),
    GENERATE_T2_REGS(SI),  GENERATE_T2_REGS(DI),  GENERATE_T3_REGS(R8),
    GENERATE_T3_REGS(R9),  GENERATE_T3_REGS(R10), GENERATE_T3_REGS(R11),
    GENERATE_T3_REGS(R12), GENERATE_T3_REGS(R13), GENERATE_T3_REGS(R14),
    GENERATE_T3_REGS(R15)};

static const ptrdiff_t FP_OFFSET = 304;
static const ptrdiff_t GP_OFFSET_MAX = 48;
static const ptrdiff_t GP_OFFSET_MEMBER_DISP = NO_DISP;
static const ptrdiff_t FP_OFFSET_MEMBER_DISP = X64_SIZEOF_INT;
static const ptrdiff_t OVERFLOW_ARG_AREA_MEMBER_DISP = 2 * X64_SIZEOF_INT;
static const ptrdiff_t REG_SAVE_AREA_MEMBER_DISP =
    2 * X64_SIZEOF_INT + X64_SIZEOF_LONG;
static ptrdiff_t reg_save_area_disp;
static size_t arg_reg_index;
static ptrdiff_t arg_stack_disp;
static size_t param_reg_index;
static ptrdiff_t param_stack_disp;
ptrdiff_t window_size;

static LinkedList *instructions;
static ListIter *before_definition;
static struct {
  const char *literal;
  const char *label;
} *literals;
static size_t literals_cap = 10;
static size_t literals_size;
static Map *static_locals;
static Map *generated_text;

static ptrdiff_t *spill_regions = NULL;
static size_t spill_regions_count = 0;

extern int skip_allocator;
extern int skip_liveness;

static const char *deduplicate_text(size_t size, const char *fmt, ...) {
  char *label = malloc(size);
  va_list args;
  va_start(args, fmt);
  int status = vsprintf(label, fmt, args);
  va_end(args);
  if (status < 0) {
    free(label);
    return NULL;
  } else {
    const char *existing = map_get(generated_text, label, size - 1);
    if (existing) {
      free(label);
      return existing;
    } else {
      int status = map_insert(generated_text, label, size - 1, label);
      if (status) {
        free(label);
        return NULL;
      } else {
        return label;
      }
    }
  }
}

const char *mk_generic_label(const char *fmt, size_t unique_id) {
  char temp[64];
  sprintf(temp, "%lu", unique_id);
  size_t label_len = (strlen(fmt) - 3) + strlen(temp) + 1;
  return deduplicate_text(label_len, fmt, unique_id);
}

#define mk_def_label(id) mk_generic_label(DEF_FMT, id)
#define mk_stmt_label(id) mk_generic_label(STMT_FMT, id)
#define mk_cond_label(id) mk_generic_label(COND_FMT, id)
#define mk_end_label(id) mk_generic_label(END_FMT, id)
#define mk_reinit_label(id) mk_generic_label(REINIT_FMT, id)
#define mk_true_label(id) mk_generic_label(TRUE_FMT, id)
#define mk_false_label(id) mk_generic_label(FALSE_FMT, id)
#define mk_literal_label(id) mk_generic_label(STR_FMT, id)

const char *mk_static_label(const char *name, size_t unique_id) {
  char temp[64];
  sprintf(temp, "%lu", unique_id);
  size_t label_len = strlen(name) + strlen(temp) + sizeof(STATIC_FMT) - 5;
  return deduplicate_text(label_len, STATIC_FMT, name, unique_id);
}

const char *mk_fnptr_text(const char *name) {
  size_t text_len = strlen(name) + sizeof(FN_PTR_FMT) - 2;
  return deduplicate_text(text_len, FN_PTR_FMT, name);
}

const char *mk_fallthru_label(size_t switch_id, size_t case_id) {
  char temp1[64];
  sprintf(temp1, "%lu", switch_id);
  char temp2[64];
  sprintf(temp2, "%lu", case_id);
  size_t label_len = strlen(temp1) + strlen(temp2) + sizeof(FALL_FMT) - 6;
  return deduplicate_text(label_len, FALL_FMT, switch_id, case_id);
}

const char *mk_case_label(size_t switch_id, size_t case_id) {
  char temp1[64];
  sprintf(temp1, "%lu", switch_id);
  char temp2[64];
  sprintf(temp2, "%lu", case_id);
  size_t label_len = strlen(temp1) + strlen(temp2) + sizeof(CASE_FMT) - 6;
  return deduplicate_text(label_len, CASE_FMT, switch_id, case_id);
}

const char *mk_local_label(const char *name) {
  size_t label_len = strlen(name) + sizeof(LOCAL_FMT) - 2;
  return deduplicate_text(label_len, LOCAL_FMT, name);
}

const char *mk_fn_size(const char *name) {
  size_t text_len = strlen(name) + sizeof(FN_SIZE_FMT) - 2;
  return deduplicate_text(text_len, FN_SIZE_FMT, name);
}

size_t next_vreg(void) {
  static size_t vreg_count = REAL_REG_COUNT;
  return vreg_count++;
}

size_t next_branch(void) {
  static size_t branch_count;
  return branch_count++;
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
  operand->reg.next_use = NULL;
}

void set_op_imm(Operand *operand, uintmax_t val, int is_signed) {
  operand->imm.mode = MODE_IMMEDIATE;
  operand->imm.val = val;
  operand->imm.is_signed = is_signed;
}

void set_op_dir(Operand *operand, const char *label) {
  operand->dir.mode = MODE_DIRECT;
  operand->dir.lab = label;
}

void set_op_pic(Operand *operand, intmax_t disp, const char *label) {
  operand->pic.mode = MODE_PIC;
  operand->pic.disp = disp;
  operand->pic.lab = label;
}

void set_op_ind(Operand *operand, intmax_t disp, size_t num) {
  operand->ind.mode = MODE_INDIRECT;
  operand->ind.disp = disp;
  operand->ind.num = num;
  operand->ind.next_use = NULL;
}

void set_op_sca(Operand *operand, IndexScale scale, intmax_t disp, size_t base,
                size_t index) {
  operand->sca.mode = MODE_SCALE;
  operand->sca.scale = scale;
  operand->sca.disp = disp;
  operand->sca.base = base;
  operand->sca.index = index;
  operand->sca.base_next_use = NULL;
  operand->sca.index_next_use = NULL;
}

int bulk_rtom(size_t dest_memreg, ptrdiff_t dest_disp, const size_t *src_regs,
              const Type *type, ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = type_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = dest_disp + i * 8 + j;
        InstructionData *mov_data = instr_init(OP_MOV);
        set_op_reg(&mov_data->src, alignment, src_regs[i]);
        set_op_ind(&mov_data->dest, chunk_disp, dest_memreg);
        if (i == 0 && dest_memreg >= REAL_REG_COUNT)
          mov_data->persist_flags |= PERSIST_DEST_SET;
        InstructionData *shr_data = instr_init(OP_SHR);
        set_op_reg(&shr_data->dest, REG_QWORD, src_regs[i]);
        set_op_imm(&shr_data->src, alignment, IMM_UNSIGNED);
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
      set_op_ind(&mov_data->dest, dest_disp + i * alignment, dest_memreg);
      if (i == 0 && dest_memreg >= REAL_REG_COUNT)
        mov_data->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
    }
  }
  return 0;
}

int bulk_mtor(const size_t *dest_regs, size_t src_memreg, ptrdiff_t src_disp,
              const Type *type, ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = type_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = src_disp + i * 8 + j;
        InstructionData *mov_data = instr_init(OP_MOV);
        set_op_ind(&mov_data->src, chunk_disp, src_memreg);
        set_op_reg(&mov_data->dest, alignment, next_vreg());
        /* persist vregs across basic blocks */
        if (i == 0 && src_memreg >= REAL_REG_COUNT)
          mov_data->persist_flags |= PERSIST_SRC_SET;
        InstructionData *movz_data = instr_init(OP_MOVZ);
        movz_data->src = mov_data->dest;
        set_op_reg(&movz_data->dest, REG_QWORD, next_vreg());
        InstructionData *shl_data = instr_init(OP_SHL);
        shl_data->dest = movz_data->dest;
        set_op_imm(&shl_data->src, j, IMM_UNSIGNED);
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
      set_op_ind(&mov_data->src, src_disp + i * alignment, src_memreg);
      /* persist vregs across basic blocks */
      if (i == 0 && src_memreg >= REAL_REG_COUNT)
        mov_data->persist_flags |= PERSIST_SRC_SET;
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
    }
  }
  return 0;
}

int bulk_mtom(size_t dest_reg, size_t src_reg, const Type *type,
              ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  size_t mov_count = width / alignment;
  size_t i;
  for (i = 0; i < mov_count; ++i) {
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_reg(&mov_data->dest, alignment, next_vreg());
    set_op_ind(&mov_data->src, i * alignment, src_reg);
    InstructionData *mov_data_2 = instr_init(OP_MOV);
    mov_data_2->src = mov_data->dest;
    set_op_ind(&mov_data_2->dest, i * alignment, dest_reg);
    if (i == 0) {
      if (src_reg >= REAL_REG_COUNT) mov_data->persist_flags |= PERSIST_SRC_SET;
      if (dest_reg >= REAL_REG_COUNT)
        mov_data_2->persist_flags |= PERSIST_DEST_SET;
    }
    int status = liter_push_back(where, NULL, 2, mov_data, mov_data_2);
    if (status) return status;
  }
  return 0;
}

int bulk_mzero(size_t dest_memreg, ptrdiff_t dest_disp, size_t skip_bytes,
               const Type *type, ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  size_t i = skip_bytes;
  InstructionData *zero_data = instr_init(OP_MOV);
  set_op_imm(&zero_data->src, 0, IMM_UNSIGNED);
  set_op_reg(&zero_data->dest, alignment, next_vreg());

  while (i < width) {
    ptrdiff_t chunk_disp = i + dest_disp;
    if ((i + dest_disp) % alignment != 0) {
      InstructionData *mov_data = instr_init(OP_MOV);
      set_op_reg(&mov_data->src, REG_BYTE, zero_data->dest.reg.num);
      set_op_ind(&mov_data->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_data->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
      ++i;
    } else {
      InstructionData *mov_data = instr_init(OP_MOV);
      mov_data->src = zero_data->dest;
      set_op_ind(&mov_data->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_data->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_data);
      if (status) return status;
      i += alignment;
    }
  }

  /* push afterwards since `where` does not move */
  return liter_push_back(where, NULL, 1, zero_data);
}

int static_zero_pad(size_t count, ListIter *where) {
  InstructionData *zero_data = instr_init(OP_ZERO);
  set_op_imm(&zero_data->dest, count, IMM_UNSIGNED);
  int status = liter_push_back(where, &where, 1, zero_data);
  if (status) free(zero_data);
  return status;
}

Opcode opcode_from_operator(int symbol, const Type *type) {
  switch (symbol) {
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
    case TOK_ADDEQ:
      /* fallthrough */
    case '+':
      return OP_ADD;
    case TOK_SUBEQ:
      /* fallthrough */
    case '-':
      return OP_SUB;
    case TOK_MULEQ:
      /* fallthrough */
    case '*':
      return type_is_signed(type) ? OP_IMUL : OP_MUL;
    case TOK_REMEQ:
      /* fallthrough */
    case '%':
      /* fallthrough */
    case TOK_DIVEQ:
      /* fallthrough */
    case '/':
      return type_is_signed(type) ? OP_IDIV : OP_DIV;
    case TOK_OREQ:
      /* fallthrough */
    case '|':
      return OP_OR;
    case TOK_ANDEQ:
      /* fallthrough */
    case '&':
      return OP_AND;
    case TOK_XOREQ:
      /* fallthrough */
    case '^':
      return OP_XOR;
    case '~':
      return OP_NOT;
    case TOK_SHLEQ:
      /* fallthrough */
    case TOK_SHL:
      return type_is_signed(type) ? OP_SAL : OP_SHL;
    case TOK_SHREQ:
      /* fallthrough */
    case TOK_SHR:
      return type_is_signed(type) ? OP_SAR : OP_SHR;
    case TOK_GE:
      return type_is_signed(type) ? OP_SETGE : OP_SETAE;
    case TOK_LE:
      return type_is_signed(type) ? OP_SETLE : OP_SETBE;
    case '>':
      return type_is_signed(type) ? OP_SETG : OP_SETA;
    case '<':
      return type_is_signed(type) ? OP_SETL : OP_SETB;
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
    case '=':
      return OP_MOV;
    default:
      return OP_INVALID;
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

size_t asmgen_literal_label(const char *literal, const char **out) {
  /* TODO(Robert): bad time complexity */
  size_t i;
  for (i = 0; i < literals_size; ++i)
    if (strcmp(literals[i].literal, literal) == 0)
      return *out = literals[i].label, i;

  if (literals_size >= literals_cap)
    literals = realloc(literals, sizeof(*literals) * (literals_cap *= 2));

  InstructionData *section_data = instr_init(OP_SECTION);
  set_op_dir(&section_data->dest, ".rodata");
  /* TODO(Robert): determine when alignment needs to be set, if ever */
  InstructionData *label_data = instr_init(OP_NONE);
  label_data->label = mk_literal_label(literals_size);
  InstructionData *string_data = instr_init(OP_ASCIZ);
  set_op_dir(&string_data->dest, literal);
  int status = liter_push_back(before_definition, &before_definition, 3,
                               section_data, label_data, string_data);
  if (status) abort();

  literals[literals_size].literal = literal;
  literals[literals_size].label = label_data->label;
  return *out = label_data->label, literals_size++;
}

/* TODO(Robert): have this function take only a the type as an argument and
 * return the displacement. currently half of the calls to this function make
 * use of dummy symbols because the signature is bad.
 */
void assign_stack_space(SymbolValue *symval) {
  size_t width = type_get_width(symval->type);
  size_t alignment = type_get_alignment(symval->type);
  size_t padding = alignment - (window_size % alignment);
  size_t to_add = width + ((padding == alignment) ? 0 : padding);
  if (to_add > PTRDIFF_MAX) abort();
  window_size += to_add;
  if (window_size < 0) abort();
  symval->disp = -window_size;
}

void assign_static_space(const char *ident, SymbolValue *symval) {
  size_t *static_count = map_get(static_locals, (void *)ident, strlen(ident));
  if (!static_count) {
    static_count = calloc(1, sizeof(size_t));
    int status =
        map_insert(static_locals, (void *)ident, strlen(ident), static_count);
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
/* NOTE: all casts are semantically valid unless one of the following is true:
 * - the source type is a struct, union, or function
 * - the destination type is a struct, union, function or array
 * - the source type is void and the destination type is not void
 */
/* TODO(Robert): this function still does not handle enums correctly. in
 * particular, make sure that it can handle conversions to and from enums
 */
int scalar_conversions(ASTree *expr, const Type *to) {
  if (type_is_record(expr->type) || type_is_function(expr->type)) {
    return -1;
  } else if (type_is_aggregate(to) || type_is_function(to)) {
    return -1;
  } else if (type_is_void(expr->type) && !type_is_void(to)) {
    return -1;
  } else if (type_is_void(to)) {
    InstructionData *nop_data = instr_init(OP_NOP);
    return liter_push_back(expr->last_instr, &expr->last_instr, 1, nop_data);
  }

  const Type *from = expr->type;
  size_t from_width =
      type_is_array(from) ? X64_SIZEOF_LONG : type_get_width(from);
  if (expr->attributes & ATTR_EXPR_LVAL && !type_is_array(from)) {
    InstructionData *lvalue_data = liter_get(expr->last_instr);
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_ind(&mov_data->src, NO_DISP, lvalue_data->dest.reg.num);
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

  size_t to_width = type_get_width(to);
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
  } else if (type_is_signed(from) || type_is_enum(from)) {
    InstructionData *movs_data = instr_init(OP_MOVS);
    movs_data->src = expr_data->dest;
    set_op_reg(&movs_data->dest, to_width, next_vreg());
    return liter_push_back(expr->last_instr, &expr->last_instr, 1, movs_data);
  } else if (type_is_unsigned(from)) {
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
  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_reg(&mov_data->dest, REG_QWORD, RBP_VREG);
  set_op_reg(&mov_data->src, REG_QWORD, RSP_VREG);
  return llist_push_back(instructions, mov_data);
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
  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_reg(&mov_data->dest, REG_QWORD, RSP_VREG);
  set_op_reg(&mov_data->src, REG_QWORD, RBP_VREG);
  int status = llist_push_back(instructions, mov_data);
  if (status) return status;
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

void maybe_load_cexpr(ASTree *expr, ListIter *where) {
  if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE) {
    InstructionData *load_data;
    if (type_is_void(expr->type)) {
      /* emit NOP when casting a constant to void */
      load_data = instr_init(OP_NOP);
    } else if (expr->constant.label != NULL) {
      /* use LEA when expression has an address component */
      load_data = instr_init(OP_LEA);
      set_op_pic(&load_data->src, expr->constant.integral.signed_value,
                 expr->constant.label);
      set_op_reg(&load_data->dest, REG_QWORD, next_vreg());
      load_data->persist_flags |= PERSIST_DEST_CLEAR;
    } else {
      load_data = instr_init(OP_MOV);
      if (type_is_unsigned(expr->type)) {
        set_op_imm(&load_data->src, expr->constant.integral.unsigned_value, 1);
      } else {
        set_op_imm(&load_data->src, expr->constant.integral.signed_value, 0);
      }
      set_op_reg(&load_data->dest, type_get_width(expr->type), next_vreg());
      load_data->persist_flags |= PERSIST_DEST_CLEAR;
    }
    if (where) {
      int status = liter_push_front(where, &expr->first_instr, 1, load_data);
      if (status) abort();
      expr->last_instr = liter_copy(expr->first_instr);
      if (expr->last_instr == NULL) abort();
    } else {
      int status = llist_push_back(instructions, load_data);
      if (status) abort();
      expr->first_instr = llist_iter_last(instructions);
      if (expr->first_instr == NULL) abort();
      expr->last_instr = llist_iter_last(instructions);
      if (expr->last_instr == NULL) abort();
    }
  }
}

ASTree *translate_ident(ASTree *ident) {
  InstructionData *lea_data = instr_init(OP_LEA);
  SymbolValue *symval = NULL;
  state_get_symbol(state, ident->lexinfo, strlen(ident->lexinfo), &symval);
  assert(symval != NULL);

  if (symval->flags & SYMFLAG_STORE_STAT) {
    if (symval->flags & SYMFLAG_LINK_NONE) {
      set_op_pic(&lea_data->src, NO_DISP,
                 mk_static_label(ident->lexinfo, symval->static_id));
    } else {
      set_op_pic(&lea_data->src, NO_DISP, ident->lexinfo);
    }
  } else {
    set_op_ind(&lea_data->src, symval->disp, RBP_VREG);
  }
  /* TODO(Robert): Type: why was this here? */
  /*
  const Type *ident_type = ident->type;
  AuxSpec *ident_aux = llist_back(&ident_type->auxspecs);
  */
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
  lea_data->persist_flags |= PERSIST_DEST_CLEAR;
  int status = llist_push_back(instructions, lea_data);
  if (status) abort();
  ident->first_instr = llist_iter_last(instructions);
  if (ident->first_instr == NULL) abort();
  ident->last_instr = liter_copy(ident->first_instr);
  if (ident->last_instr == NULL) abort();
  return ident;
}

/* TODO(Robert): determine how persistence should work for casts */
ASTree *translate_cast(ASTree *cast, ASTree *expr) {
  PFDBG0('g', "Translating cast");

  if (type_is_scalar(cast->type) || type_is_enum(cast->type) ||
      type_is_array(cast->type)) {
    assert(!scalar_conversions(expr, cast->type));
    assert((cast->last_instr = liter_copy(expr->last_instr)) != NULL);
  } else if (type_is_void(cast->type)) {
    InstructionData *nop_data = instr_init(OP_NOP);
    assert(!liter_push_back(expr->last_instr, &cast->last_instr, 1, nop_data));
  } else {
    assert((cast->last_instr = liter_copy(expr->last_instr)) != NULL);
  }

  assert((cast->first_instr = liter_copy(expr->first_instr)) != NULL);
  return astree_adopt(cast, 1, expr);
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
  test_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *setz_data = instr_init(OP_SETZ);
  set_op_reg(&setz_data->dest, REG_BYTE, next_vreg());

  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setz_data->dest;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());
  movz_data->persist_flags |= PERSIST_DEST_CLEAR;

  not ->first_instr = liter_copy(operand->first_instr);
  if (not ->first_instr == NULL) abort();
  status = liter_push_back(operand->last_instr, &not ->last_instr, 3, test_data,
                           setz_data, movz_data);
  if (status) abort();
  return astree_adopt(not, 1, operand);
}

ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  /* test first operand; jump on false for && and true for || */
  int status = scalar_conversions(left, left->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  const char *skip_label = operator->symbol == TOK_AND
                               ? mk_false_label(next_branch())
                               : mk_true_label(next_branch());
  InstructionData *test_left_data = instr_init(OP_TEST);
  test_left_data->dest = test_left_data->src = left_data->dest;
  test_left_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *jmp_left_data =
      instr_init(opcode_from_operator(operator->symbol, operator->type));
  set_op_dir(&jmp_left_data->dest, skip_label);

  status =
      liter_push_back(left->last_instr, NULL, 2, test_left_data, jmp_left_data);
  if (status) abort();

  status = scalar_conversions(right, right->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *test_right_data = instr_init(OP_TEST);
  test_right_data->dest = test_right_data->src = right_data->dest;
  test_right_data->persist_flags |= PERSIST_DEST_SET;

  /* result will always be the truth value of the last evaluated expression */
  InstructionData *setnz_data = instr_init(OP_SETNZ);
  set_op_reg(&setnz_data->dest, REG_BYTE, next_vreg());
  setnz_data->label = skip_label;
  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setnz_data->dest;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());
  movz_data->persist_flags |= PERSIST_DEST_CLEAR;

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

  Type *common_type;
  if (type_is_pointer(left->type) || type_is_pointer(right->type)) {
    common_type = (Type *)TYPE_LONG;
  } else {
    int status =
        type_arithmetic_conversions(&common_type, left->type, right->type);
    if (status) abort();
  }

  int status = scalar_conversions(left, common_type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, common_type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *cmp_data = instr_init(OP_CMP);
  cmp_data->dest = right_data->dest;
  cmp_data->src = left_data->dest;
  cmp_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  InstructionData *setcc_data =
      instr_init(opcode_from_operator(operator->symbol, common_type));
  set_op_reg(&setcc_data->dest, REG_BYTE, next_vreg());

  InstructionData *movz_data = instr_init(OP_MOVZ);
  movz_data->src = setcc_data->dest;
  set_op_reg(&movz_data->dest, REG_DWORD, next_vreg());
  movz_data->persist_flags = PERSIST_DEST_CLEAR;

  status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                           cmp_data, setcc_data, movz_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

/* TODO(Robert): i find myself questioning whether or not this is correct again
 * on account of the way `scalar_conversions` works
 */
ASTree *translate_indirection(ASTree *indirection, ASTree *operand) {
  PFDBG0('g', "Translating indirection operation.");
  indirection->first_instr = liter_copy(operand->first_instr);
  if (indirection->first_instr == NULL) abort();

  /* TODO(Robert): (void?) pointer type constant */
  int status = scalar_conversions(operand, (Type *)TYPE_LONG);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  /* NOTE: if the operand is an aggregate, technically no operation is necessary
   * but because the address-of operator removes instructions when applied to
   * the result of the indirection operator, it makes sense to always emit an
   * instruction which can always be removed by the address-of
   */
  InstructionData *load_data;
  if (type_is_aggregate(indirection->type)) {
    load_data = instr_init(OP_LEA);
    set_op_reg(&load_data->dest, type_get_width(TYPE_LONG), next_vreg());
  } else {
    load_data = instr_init(OP_MOV);
    set_op_reg(&load_data->dest, type_get_width(indirection->type),
               next_vreg());
  }
  set_op_ind(&load_data->src, NO_DISP, operand_data->dest.reg.num);
  load_data->persist_flags = PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  status = liter_push_back(operand->last_instr, &indirection->last_instr, 1,
                           load_data);
  if (status) abort();
  return astree_adopt(indirection, 1, operand);
}

/* TODO(Robert): determine how persistence works in this case */
ASTree *translate_addrof(ASTree *addrof, ASTree *operand) {
  PFDBG0('g', "Translating address operation.");
  /* TODO(Robert): shouldn't this also be done for the '[]' operator? */
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
  PFDBG0('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the displacement
   * and scale addressing mode can be used
   */
  /* TODO(Robert): (void?) pointer type constant */
  int status = scalar_conversions(pointer, (Type *)TYPE_LONG);
  if (status) abort();
  InstructionData *pointer_data = liter_get(pointer->last_instr);

  /* TODO(Robert): ptrdiff_t type constant (maybe?) */
  status = scalar_conversions(index, (Type *)TYPE_LONG);
  if (status) abort();
  InstructionData *index_data = liter_get(index->last_instr);

  subscript->first_instr = liter_copy(pointer->first_instr);
  if (subscript->first_instr == NULL) abort();

  InstructionData *lea_data = instr_init(OP_LEA);
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
  lea_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  size_t scale = type_get_width((Type *)subscript->type);
  if (scale == 1 || scale == 2 || scale == 4 || scale == 8) {
    set_op_sca(&lea_data->src, scale, NO_DISP, pointer_data->dest.reg.num,
               index_data->dest.reg.num);
    int status =
        liter_push_back(index->last_instr, &subscript->last_instr, 1, lea_data);
    if (status) abort();
  } else {
    set_op_sca(&lea_data->src, SCALE_BYTE, NO_DISP, pointer_data->dest.reg.num,
               index_data->dest.reg.num);
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = index_data->dest;
    set_op_imm(&mul_data->src, scale, IMM_UNSIGNED);
    int status = liter_push_back(index->last_instr, &subscript->last_instr, 2,
                                 mul_data, lea_data);
    if (status) abort();
  }
  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *translate_reference(ASTree *reference, ASTree *struct_,
                            ASTree *member) {
  PFDBG0('g', "Translating reference operator");
  Type *record_type;
  if (reference->symbol == TOK_ARROW) {
    int status = scalar_conversions(struct_, struct_->type);
    if (status) abort();
    status = type_strip_declarator(&record_type, struct_->type);
    if (status) abort();
  } else {
    record_type = struct_->type;
  }

  InstructionData *struct_data = liter_get(struct_->last_instr);
  SymbolValue *member_symbol = type_member_name(record_type, member->lexinfo);
  assert(member_symbol);

  InstructionData *lea_data = instr_init(OP_LEA);
  set_op_ind(&lea_data->src, member_symbol->disp, struct_data->dest.reg.num);
  set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
  lea_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  reference->first_instr = liter_copy(struct_->first_instr);
  if (reference->first_instr == NULL) abort();
  int status =
      liter_push_back(struct_->last_instr, &reference->last_instr, 1, lea_data);
  if (status) abort();
  return astree_adopt(reference, 2, struct_, member);
}

ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating postfix increment/decrement");
  post_inc_dec->first_instr = liter_copy(operand->first_instr);
  if (post_inc_dec->first_instr == NULL) abort();
  InstructionData *lvalue_data = liter_get(operand->last_instr);
  if (lvalue_data == NULL) abort();
  int status = scalar_conversions(operand, post_inc_dec->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = operand_data->dest;
  set_op_reg(&mov_data->dest, type_get_width(post_inc_dec->type), next_vreg());

  InstructionData *inc_dec_data = instr_init(
      opcode_from_operator(post_inc_dec->symbol, post_inc_dec->type));
  inc_dec_data->dest = operand_data->dest;

  InstructionData *mov_data_2 = instr_init(OP_MOV);
  set_op_ind(&mov_data_2->dest, NO_DISP, lvalue_data->dest.reg.num);
  set_op_reg(&mov_data_2->src, type_get_width(operand->type),
             operand_data->dest.reg.num);
  /* both the location and value of the object must persist until this point */
  mov_data_2->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = mov_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  status = liter_push_back(operand->last_instr, &post_inc_dec->last_instr, 4,
                           mov_data, inc_dec_data, mov_data_2, dummy_data);
  if (status) abort();
  return astree_adopt(post_inc_dec, 1, operand);
}

ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating prefix increment/decrement");
  inc_dec->first_instr = liter_copy(operand->first_instr);
  if (inc_dec->first_instr == NULL) abort();
  InstructionData *lvalue_data = liter_get(operand->last_instr);
  if (lvalue_data == NULL) abort();
  int status = scalar_conversions(operand, inc_dec->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *inc_dec_data =
      instr_init(opcode_from_operator(inc_dec->symbol, inc_dec->type));
  inc_dec_data->dest = operand_data->dest;

  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_ind(&mov_data->dest, NO_DISP, lvalue_data->dest.reg.num);
  set_op_reg(&mov_data->src, type_get_width(operand->type),
             operand_data->dest.reg.num);
  /* both the location and value of the object must persist until this point */
  mov_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = operand_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  status = liter_push_back(operand->last_instr, &inc_dec->last_instr, 3,
                           inc_dec_data, mov_data, dummy_data);
  if (status) abort();
  return astree_adopt(inc_dec, 1, operand);
}

ASTree *translate_unop(ASTree *operator, ASTree * operand) {
  PFDBG0('g', "Translating unary operation");
  int status = scalar_conversions(operand, operator->type);
  if (status) abort();
  InstructionData *operand_data = liter_get(operand->last_instr);

  InstructionData *operator_data =
      instr_init(opcode_from_operator(operator->symbol, operator->type));
  operator_data->dest = operand_data->dest;
  /* TODO(Robert): not sure if this is right, but I believe all unary ops
   * make use of the value in the register, so the value should persist aross
   * basic blocks
   */
  operator_data->persist_flags |= PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(operand->first_instr);
  if (operator->first_instr == NULL) abort();
  status = liter_push_back(operand->last_instr, &operator->last_instr, 1,
                           operator_data);
  if (status) abort();
  return astree_adopt(operator, 1, operand);
}

ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  PFDBG0('g', "Translating additive operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *operator_data =
      instr_init(opcode_from_operator(operator->symbol, operator->type));
  operator_data->dest = right_data->dest;
  operator_data->src = left_data->dest;
  operator_data->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  /* use two-operand IMUL, since it is more convenient in this case */
  if (type_is_pointer(left->type) && !type_is_pointer(right->type)) {
    Type *element_type;
    int status = type_strip_declarator(&element_type, left->type);
    if (status) abort();
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = right_data->dest;
    set_op_imm(&mul_data->src, type_get_width(element_type), IMM_UNSIGNED);
    if (status) abort();
    status = liter_push_back(right->last_instr, &operator->last_instr, 2,
                             mul_data, operator_data);
    if (status) abort();
  } else if (!type_is_pointer(left->type) && type_is_pointer(right->type)) {
    Type *element_type;
    int status = type_strip_declarator(&element_type, right->type);
    if (status) abort();
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = left_data->dest;
    set_op_imm(&mul_data->src, type_get_width(element_type), IMM_UNSIGNED);
    if (status) abort();
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
  PFDBG0('g', "Translating binary operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  /* unconditionally save rax and rdx so that we don't need to worry about
   * their values changing implicitly when performing register allocation.
   */
  InstructionData *push_rdx_data = instr_init(OP_PUSH);
  set_op_reg(&push_rdx_data->dest, REG_QWORD, RDX_VREG);
  InstructionData *push_rax_data = instr_init(OP_PUSH);
  set_op_reg(&push_rax_data->dest, REG_QWORD, RAX_VREG);

  InstructionData *zero_rdx_data = instr_init(OP_MOV);
  set_op_reg(&zero_rdx_data->dest, type_get_width(operator->type), RDX_VREG);
  set_op_imm(&zero_rdx_data->src, 0, IMM_UNSIGNED);

  InstructionData *mov_rax_data = instr_init(OP_MOV);
  set_op_reg(&mov_rax_data->dest, type_get_width(operator->type), RAX_VREG);
  mov_rax_data->src = left_data->dest;
  mov_rax_data->persist_flags |= PERSIST_SRC_SET;

  InstructionData *operator_data =
      instr_init(opcode_from_operator(operator->symbol, operator->type));
  operator_data->dest = right_data->dest;
  operator_data->persist_flags |= PERSIST_DEST_SET;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  InstructionData *mov_data = instr_init(OP_MOV);
  /* mov from vreg representing rax/rdx to new vreg */
  set_op_reg(&mov_data->src, type_get_width(operator->type),
                             operator->symbol == '%' ? RDX_VREG : RAX_VREG);
  set_op_reg(&mov_data->dest, type_get_width(operator->type), next_vreg());

  /* restore rax and rdx */
  InstructionData *pop_rax_data = instr_init(OP_POP);
  set_op_reg(&pop_rax_data->dest, REG_QWORD, RAX_VREG);
  InstructionData *pop_rdx_data = instr_init(OP_POP);
  set_op_reg(&pop_rdx_data->dest, REG_QWORD, RDX_VREG);

  /* dummy mov */
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->dest = dummy_data->src = mov_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  status = liter_push_back(right->last_instr, &operator->last_instr, 9,
                           push_rdx_data, push_rax_data, zero_rdx_data,
                           mov_rax_data, operator_data, mov_data, pop_rax_data,
                           pop_rdx_data, dummy_data);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  PFDBG0('g', "Translating binary operation");
  int status = scalar_conversions(left, operator->type);
  if (status) abort();
  InstructionData *left_data = liter_get(left->last_instr);

  status = scalar_conversions(right, operator->type);
  if (status) abort();
  InstructionData *right_data = liter_get(right->last_instr);

  InstructionData *operator_data =
      instr_init(opcode_from_operator(operator->symbol, operator->type));
  operator_data->dest = right_data->dest;
  operator_data->src = left_data->dest;
  operator_data->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

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
  size_t current_branch = next_branch();
  InstructionData *condition_data = liter_get(condition->last_instr);
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;
  test_data->persist_flags |= PERSIST_DEST_SET;
  InstructionData *jmp_false_data =
      instr_init(opcode_from_operator(qmark->symbol, qmark->type));
  set_op_dir(&jmp_false_data->dest, mk_false_label(current_branch));
  status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                           jmp_false_data);
  if (status) abort();

  if (type_is_void(qmark->type)) {
    InstructionData *nop_data = instr_init(OP_NOP);
    InstructionData *jmp_end_data = instr_init(OP_JMP);
    set_op_dir(&jmp_end_data->dest, mk_true_label(current_branch));
    status =
        liter_push_back(true_expr->last_instr, NULL, 2, nop_data, jmp_end_data);

    InstructionData *false_label = liter_get(false_expr->first_instr);
    false_label->label = mk_false_label(current_branch);
    InstructionData *end_label = instr_init(OP_NOP);
    end_label->label = mk_true_label(current_branch);
    status = liter_push_back(false_expr->last_instr, &qmark->last_instr, 1,
                             end_label);
  } else {
    status = scalar_conversions(true_expr, qmark->type);
    if (status) abort();
    InstructionData *true_expr_data = liter_get(true_expr->last_instr);
    InstructionData *mov_true_data = instr_init(OP_MOV);
    mov_true_data->src = true_expr_data->dest;
    set_op_reg(&mov_true_data->dest, type_get_width(qmark->type), next_vreg());
    /* clear persistence data for result vreg; set it for true expr result */
    mov_true_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    InstructionData *jmp_end_data = instr_init(OP_JMP);
    set_op_dir(&jmp_end_data->dest, mk_true_label(current_branch));
    status = liter_push_back(true_expr->last_instr, NULL, 2, mov_true_data,
                             jmp_end_data);

    status = scalar_conversions(false_expr, qmark->type);
    if (status) abort();
    InstructionData *false_label = liter_get(false_expr->first_instr);
    false_label->label = mk_false_label(current_branch);
    InstructionData *false_expr_data = liter_get(false_expr->last_instr);
    InstructionData *mov_false_data = instr_init(OP_MOV);
    mov_false_data->src = false_expr_data->dest;
    mov_false_data->dest = mov_true_data->dest;
    mov_false_data->persist_flags |= PERSIST_SRC_SET;
    /* dummy mov so that last instruction has destination reg */
    InstructionData *end_label = instr_init(OP_MOV);
    end_label->label = mk_true_label(current_branch);
    end_label->src = end_label->dest = mov_false_data->dest;
    /* persist result vreg across bblocks */
    end_label->persist_flags |= PERSIST_DEST_SET;
    status = liter_push_back(false_expr->last_instr, &qmark->last_instr, 2,
                             mov_false_data, end_label);
    if (status) abort();
  }

  qmark->first_instr = liter_copy(condition->first_instr);
  if (qmark->first_instr == NULL) abort();
  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

ASTree *translate_comma(ASTree *comma, ASTree *left, ASTree *right) {
  comma->first_instr = liter_copy(
      left->first_instr == NULL ? right->first_instr : left->first_instr);
  if (comma->first_instr == NULL) abort();
  comma->last_instr = liter_copy(right->last_instr);
  if (comma->last_instr == NULL) abort();
  return astree_adopt(comma, 2, left, right);
}

int assign_aggregate(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;

  InstructionData *lvalue_data = liter_get(lvalue->last_instr);
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);
  assignment->last_instr = liter_next(rvalue->last_instr, 1);
  if (assignment->last_instr == NULL) return -1;
  /* `bulk_mtom` should make registers persist */
  int status = bulk_mtom(lvalue_data->dest.reg.num, rvalue_data->dest.reg.num,
                         assignment->type, rvalue->last_instr);
  if (status) return status;
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = lvalue_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;
  /* push_front since iter is current past the last instruction */
  status = liter_push_front(assignment->last_instr, &assignment->last_instr, 1,
                            dummy_data);
  if (status) return status;
  return 0;
}

int assign_add(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  InstructionData *lvalue_data = liter_get(lvalue->last_instr);

  int status = scalar_conversions(rvalue, lvalue->type);
  if (status) return status;
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);

  InstructionData *assignment_data =
      instr_init(opcode_from_operator(assignment->symbol, assignment->type));
  set_op_ind(&assignment_data->dest, NO_DISP, lvalue_data->dest.reg.num);
  assignment_data->src = rvalue_data->dest;
  assignment_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  /* TODO(Robert): this shouldn't be a dummy instruction; the new value needs
   * to be loaded from memory, preferably to a fresh vreg
   */
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = rvalue_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;

  /* TODO(Robert): copied from translate_addition */
  /* TODO(Robert): is a += b valid if a has integral type and b is a pointer? */
  /* use two-operand IMUL, since it is more convenient in this case */
  if (type_is_pointer(lvalue->type) && !type_is_pointer(rvalue->type)) {
    Type *element_type;
    int status = type_strip_declarator(&element_type, lvalue->type);
    if (status) return status;
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = rvalue_data->dest;
    set_op_imm(&mul_data->src, type_get_width(element_type), IMM_UNSIGNED);
    if (status) abort();
    status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 3,
                             mul_data, assignment_data, dummy_data);
    if (status) return status;
  } else if (!type_is_pointer(lvalue->type) && type_is_pointer(rvalue->type)) {
    Type *element_type;
    int status = type_strip_declarator(&element_type, rvalue->type);
    if (status) return status;
    InstructionData *mul_data = instr_init(OP_IMUL);
    mul_data->dest = lvalue_data->dest;
    set_op_imm(&mul_data->src, type_get_width(element_type), IMM_UNSIGNED);
    if (status) abort();
    status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 3,
                             mul_data, assignment_data, dummy_data);
    if (status) return status;
  } else {
    int status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 2,
                                 assignment_data, dummy_data);
    if (status) return status;
  }
  return 0;
}

int assign_mul(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  Type *common_type;
  int status =
      type_arithmetic_conversions(&common_type, lvalue->type, rvalue->type);

  InstructionData *lvalue_data = liter_get(lvalue->last_instr);
  status = scalar_conversions(lvalue, common_type);
  if (status) return -1;
  InstructionData *left_data = liter_get(lvalue->last_instr);

  status = scalar_conversions(rvalue, common_type);
  if (status) return status;
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);

  /* unconditionally save rax and rdx so that we don't need to worry about
   * their values changing implicitly when performing register allocation.
   */
  InstructionData *push_rdx_data = instr_init(OP_PUSH);
  set_op_reg(&push_rdx_data->dest, REG_QWORD, RDX_VREG);
  InstructionData *push_rax_data = instr_init(OP_PUSH);
  set_op_reg(&push_rax_data->dest, REG_QWORD, RAX_VREG);

  InstructionData *zero_rdx_data = instr_init(OP_MOV);
  set_op_reg(&zero_rdx_data->dest, type_get_width(common_type), RDX_VREG);
  set_op_imm(&zero_rdx_data->src, 0, IMM_UNSIGNED);

  InstructionData *mov_rax_data = instr_init(OP_MOV);
  set_op_reg(&mov_rax_data->dest, type_get_width(common_type), RAX_VREG);
  mov_rax_data->src = left_data->dest;
  mov_rax_data->persist_flags |= PERSIST_SRC_SET;

  InstructionData *operator_data =
      instr_init(opcode_from_operator(assignment->symbol, common_type));
  operator_data->dest = rvalue_data->dest;
  operator_data->persist_flags |= PERSIST_DEST_SET;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;

  InstructionData *store_data = instr_init(OP_MOV);
  set_op_ind(&store_data->dest, NO_DISP, lvalue_data->dest.reg.num);
  set_op_reg(&store_data->src, type_get_width(lvalue->type),
             assignment->symbol == TOK_REMEQ ? RDX_VREG : RAX_VREG);
  store_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *mov_data = instr_init(OP_MOV);
  /* mov from vreg representing rax/rdx to new vreg and truncate */
  set_op_reg(&mov_data->src, type_get_width(lvalue->type),
             assignment->symbol == TOK_REMEQ ? RDX_VREG : RAX_VREG);
  set_op_reg(&mov_data->dest, type_get_width(lvalue->type), next_vreg());

  /* restore rax and rdx */
  InstructionData *pop_rax_data = instr_init(OP_POP);
  set_op_reg(&pop_rax_data->dest, REG_QWORD, RAX_VREG);
  InstructionData *pop_rdx_data = instr_init(OP_POP);
  set_op_reg(&pop_rdx_data->dest, REG_QWORD, RDX_VREG);

  /* dummy mov */
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->dest = dummy_data->src = mov_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  return liter_push_back(rvalue->last_instr, &assignment->last_instr, 10,
                         push_rdx_data, push_rax_data, zero_rdx_data,
                         mov_rax_data, operator_data, store_data, mov_data,
                         pop_rax_data, pop_rdx_data, dummy_data);
}

int assign_scalar(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  InstructionData *lvalue_data = liter_get(lvalue->last_instr);

  int status = scalar_conversions(rvalue, lvalue->type);
  if (status) return status;
  InstructionData *rvalue_data = liter_get(rvalue->last_instr);

  InstructionData *assignment_data =
      instr_init(opcode_from_operator(assignment->symbol, assignment->type));
  set_op_ind(&assignment_data->dest, NO_DISP, lvalue_data->dest.reg.num);
  assignment_data->src = rvalue_data->dest;
  assignment_data->persist_flags |= PERSIST_DEST_SET | PERSIST_SRC_SET;

  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = rvalue_data->dest;
  dummy_data->persist_flags |= PERSIST_DEST_CLEAR;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) return -1;
  status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 2,
                           assignment_data, dummy_data);
  if (status) return status;
  return 0;
}

/* NOTE: callers assume this function does not return errors */
ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  PFDBG0('g', "Translating assignment");
  if (type_is_union(assignment->type) || type_is_struct(assignment->type)) {
    int status = assign_aggregate(assignment, lvalue, rvalue);
    if (status) abort();
  } else if (assignment->symbol == TOK_ADDEQ ||
             assignment->symbol == TOK_SUBEQ) {
    int status = assign_add(assignment, lvalue, rvalue);
    if (status) abort();
  } else if (assignment->symbol == TOK_MULEQ ||
             assignment->symbol == TOK_DIVEQ ||
             assignment->symbol == TOK_REMEQ) {
    int status = assign_mul(assignment, lvalue, rvalue);
    if (status) abort();
  } else {
    int status = assign_scalar(assignment, lvalue, rvalue);
    if (status) abort();
  }
  /* don't adopt; this function gets used for initialization */
  return assignment;
}

/* TODO(Robert): ensure correct instruction order */
int translate_agg_arg(ASTree *call, ASTree *arg) {
  size_t arg_eightbytes = type_get_eightbytes(arg->type);
  InstructionData *arg_data = liter_get(arg->last_instr);
  assert(arg_data->dest.all.mode == MODE_INDIRECT);
  InstructionData *mov_data = instr_init(OP_MOV);
  /* use arbitrary volatile reg to store arg */
  set_op_reg(&mov_data->dest, REG_QWORD, RAX_VREG);
  mov_data->src = arg_data->dest;

  int status = liter_advance(call->last_instr, -1);
  if (status) return status;
  if (arg_eightbytes <= 2 &&
      arg_eightbytes + arg_reg_index <= PARAM_REG_COUNT) {
    int status = bulk_mtor(PARAM_REGS + arg_reg_index, mov_data->dest.reg.num,
                           NO_DISP, arg->type, call->last_instr);
    arg_reg_index += arg_eightbytes;
    if (status) return status;
  } else {
    int status = bulk_mtom(RSP_VREG, mov_data->dest.reg.num, arg->type,
                           call->last_instr);
    if (status) return status;
    arg_stack_disp += arg_eightbytes * X64_SIZEOF_LONG;
    InstructionData *sub_data = instr_init(OP_SUB);
    set_op_reg(&sub_data->dest, REG_QWORD, RSP_VREG);
    set_op_imm(&sub_data->src, arg_eightbytes * 8, IMM_UNSIGNED);
    /* push after since instructions will be reversed */
    status = liter_push_back(call->last_instr, NULL, 1, sub_data);
    if (status) return status;
  }

  /* push after since instructions will be reversed */
  return liter_push_back(call->last_instr, NULL, 1, mov_data) ||
         liter_advance(call->last_instr, 1);
}

int translate_scalar_arg(ASTree *call, const Type *param_type, ASTree *arg) {
  assert(type_get_eightbytes(param_type) == 1);
  InstructionData *arg_data = liter_get(arg->last_instr);
  assert(arg_data->dest.all.mode == MODE_INDIRECT);

  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = arg_data->dest;

  if (arg_reg_index < PARAM_REG_COUNT) {
    set_op_reg(&mov_data->dest, type_get_width(param_type),
               PARAM_REGS[arg_reg_index++]);
    int status =
        liter_push_front(call->last_instr, &call->last_instr, 1, mov_data);
    if (status) return status;
  } else {
    /* we can use any volatile, non-parameter register to perform the mov,
     * since they should be saved by this point. i'm picking rax arbitrarily. i
     * might be able to make use of the register allocator here; i'm not sure
     */
    /* TODO(Robert): consider using r10 or r11 since neither of those are param
     * or registers and rax is used for variadic functions; if i change the
     * code in `translate_call` i don't want it to break mysteriously because
     * rax is dirtied when moving around arguments
     */
    /* TODO(Robert): is the combination of PUSH, SUB, ADD and MOV instructions
     * used when moving parameters to the stack correct? If I'm correct, the
     * pre-call adjustment doesn't take into account the fact that PUSH already
     * adjusts RSP
     */
    set_op_reg(&mov_data->dest, type_get_width(param_type), RAX_VREG);
    InstructionData *push_data = instr_init(OP_PUSH);
    set_op_reg(&push_data->dest, REG_QWORD, RAX_VREG);
    int status = liter_push_front(call->last_instr, &call->last_instr, 2,
                                  mov_data, push_data);
    if (status) return status;
    arg_stack_disp += X64_SIZEOF_LONG;
  }
  return 0;
}

/* TODO(Robert): arguments without an associated parameter type (variable
 * arguments to variadic functions and all arguments to functions without
 * prototypes) should probably not have their type widened to be 64 bits.
 */
int translate_args(ASTree *call) {
  /* account for hidden out param */
  int out_param = type_get_eightbytes(call->type) > 2;
  arg_reg_index = out_param ? 1 : 0;
  arg_stack_disp = 0;
  if (type_is_struct(call->type) || type_is_union(call->type)) {
    SymbolValue dummy = {0};
    dummy.type = call->type;
    assign_stack_space(&dummy);
    if (out_param) {
      InstructionData *lea_data = instr_init(OP_LEA);
      set_op_reg(&lea_data->dest, REG_QWORD, RDI_VREG);
      set_op_ind(&lea_data->src, -window_size, RBP_VREG);
      int status =
          liter_push_front(call->last_instr, &call->last_instr, 1, lea_data);
      if (status) return status;
    }
  }
  Type *function_type;
  int status = type_strip_declarator(&function_type, astree_get(call, 0)->type);
  if (status) abort();
  size_t i, param_count = type_param_count(function_type);
  for (i = 1; i < astree_count(call); ++i) {
    PFDBG1('g', "Translating parameter %i", i);
    ASTree *arg = astree_get(call, i);
    assert(arg->type != NULL && !type_is_array(arg->type));
    const Type *param_type = ((i - 1) < param_count)
                                 ? type_param_index(function_type, i - 1)
                                 : (Type *)TYPE_LONG;
    if (type_is_union(arg->type) || type_is_struct(arg->type)) {
      int status = translate_agg_arg(call, arg);
      if (status) return status;
    } else {
      int status = translate_scalar_arg(call, param_type, arg);
      if (status) return status;
    }
  }
  return 0;
}

int save_call_subexprs(ASTree *call) {
  size_t i;
  for (i = 0; i < astree_count(call); ++i) {
    ASTree *subexpr = astree_get(call, i);
    if (call->spill_eightbytes < subexpr->spill_eightbytes + i + 1)
      call->spill_eightbytes = subexpr->spill_eightbytes + i + 1;
  }

  if (spill_regions_count < call->spill_eightbytes) {
    size_t old_count = spill_regions_count;
    spill_regions = realloc(spill_regions,
                            sizeof(*spill_regions) *
                                (spill_regions_count = call->spill_eightbytes));
    if (spill_regions == NULL) return -1;
    SymbolValue dummy = {0};
    dummy.type = (Type *)TYPE_LONG;
    for (i = old_count; i < spill_regions_count; ++i) {
      assign_stack_space(&dummy);
      spill_regions[i] = dummy.disp;
    }
  }

  Type *function_type;
  int status = type_strip_declarator(&function_type, astree_get(call, 0)->type);
  if (status) return status;
  size_t param_count = type_param_count(function_type);
  for (i = 0; i < astree_count(call); ++i) {
    ASTree *subexpr = astree_get(call, i);
    if (type_is_scalar(subexpr->type)) {
      /* TODO(Robert): converting to LONG isn't quite right for arguments
       * without a corresponding parameter type
       *
       * convert arguments to the appropriate parameter type, and the function
       * pointer and extra arguments to LONG
       */
      const Type *conv_type = i <= param_count && i > 0
                                  ? type_param_index(function_type, i - 1)
                                  : (Type *)TYPE_LONG;
      assert(type_is_scalar(conv_type));
      status = scalar_conversions(subexpr, conv_type);
      if (status) return status;
    }
    InstructionData *subexpr_data = liter_get(subexpr->last_instr);
    assert(subexpr_data->dest.all.mode == MODE_REGISTER);
    InstructionData *spill_data = instr_init(OP_MOV);
    spill_data->src = subexpr_data->dest;
    set_op_ind(&spill_data->dest,
               spill_regions[call->spill_eightbytes - (i + 1)], RBP_VREG);
    spill_data->persist_flags |= PERSIST_SRC_SET;
    status = liter_push_back(subexpr->last_instr, &subexpr->last_instr, 1,
                             spill_data);
    if (status) return status;
  }

  return 0;
}

ASTree *translate_call(ASTree *call) {
  PFDBG0('g', "Translating function call");
  ASTree *fn_pointer = astree_get(call, 0);
  call->first_instr = liter_copy(fn_pointer->first_instr);
  if (call->first_instr == NULL) abort();
  int status = save_call_subexprs(call);
  if (status) abort();
  InstructionData *sub_data = instr_init(OP_SUB);
  set_op_reg(&sub_data->dest, REG_QWORD, RSP_VREG);
  status = llist_push_back(instructions, sub_data);
  if (status) abort();

  /* temporary iterator for inserting args in reverse order */
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();

  /* do this after so that params are moved in reverse order */
  status = translate_args(call);
  if (status) abort();
  status = save_volatile_regs(call->last_instr);
  if (status) abort();
  free(call->last_instr);
  call->last_instr = NULL;

  /* set sub_data's src op now that we know stack param space */
  assert(arg_stack_disp % X64_SIZEOF_LONG == 0);
  /* align stack to 16-byte boundary; we can use a bitand since we know the
   * stack should already be aligned to an 8-byte boundary
   */
  if (arg_stack_disp & X64_SIZEOF_LONG) {
    set_op_imm(&sub_data->src, X64_SIZEOF_LONG, IMM_UNSIGNED);
    arg_stack_disp += X64_SIZEOF_LONG;
  } else {
    set_op_imm(&sub_data->src, 0, IMM_UNSIGNED);
  }

  if (type_is_variadic_function(fn_pointer->type)) {
    InstructionData *zero_eax_data = instr_init(OP_MOV);
    set_op_imm(&zero_eax_data->src, 0, IMM_UNSIGNED);
    set_op_reg(&zero_eax_data->dest, REG_DWORD, RAX_VREG);
    int status = llist_push_back(instructions, zero_eax_data);
    if (status) abort();
  }

  InstructionData *fn_pointer_data = liter_get(fn_pointer->last_instr);
  assert(fn_pointer_data->dest.all.mode == MODE_INDIRECT);
  InstructionData *load_fn_data = instr_init(OP_MOV);
  load_fn_data->src = fn_pointer_data->dest;
  /* use r10 or r11 since those are never used for args */
  set_op_reg(&load_fn_data->dest, type_get_width(fn_pointer->type), 11);
  InstructionData *call_data = instr_init(OP_CALL);
  call_data->dest = load_fn_data->dest;
  status = llist_push_back(instructions, load_fn_data);
  if (status) abort();
  status = llist_push_back(instructions, call_data);
  if (status) abort();

  InstructionData *rsp_reset_data = instr_init(OP_ADD);
  set_op_reg(&rsp_reset_data->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_reset_data->src, arg_stack_disp, IMM_UNSIGNED);
  status = llist_push_back(instructions, rsp_reset_data);
  if (status) abort();

  if (!type_is_void(call->type)) {
    /* store return value on the stack temporarily so that volatile registers
     * can be restored without worrying about the return value being clobbered
     */
    /* TODO(Robert): the memory location of the hidden out parameter is also
     * stored at -8(%rbp). this is a problem.
     */
    InstructionData *store_data = instr_init(OP_MOV);
    set_op_ind(&store_data->dest, -8, RBP_VREG);
    InstructionData *load_data = instr_init(OP_MOV);
    load_data->src = store_data->dest;
    if (type_is_struct(call->type) || type_is_union(call->type)) {
      if (type_get_eightbytes(call->type) <= 2) {
        ListIter *temp = llist_iter_last(instructions);
        int status =
            bulk_rtom(RBP_VREG, -window_size, RETURN_REGS, call->type, temp);
        free(temp);
        if (status) abort();
      }
      InstructionData *agg_addr_data = instr_init(OP_LEA);
      set_op_ind(&agg_addr_data->src, -window_size, RBP_VREG);
      /* any volatile register is fine since they will all be restored */
      set_op_reg(&agg_addr_data->dest, REG_QWORD, RCX_VREG);
      int status = llist_push_back(instructions, agg_addr_data);
      if (status) abort();
      store_data->src = agg_addr_data->dest;
      set_op_reg(&load_data->dest, REG_QWORD, next_vreg());
      load_data->persist_flags |= PERSIST_DEST_CLEAR;
    } else {
      set_op_reg(&store_data->src, type_get_width(call->type), RAX_VREG);
      set_op_reg(&load_data->dest, type_get_width(call->type), next_vreg());
      load_data->persist_flags |= PERSIST_DEST_CLEAR;
    }
    int status = llist_push_back(instructions, store_data);
    if (status) abort();
    status = restore_volatile_regs();
    if (status) abort();
    status = llist_push_back(instructions, load_data);
    if (status) abort();
  } else {
    int status = restore_volatile_regs();
    if (status) abort();
  }
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();
  return call;
}

ASTree *translate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident) {
  va_start_->first_instr = liter_copy(expr->first_instr);
  if (va_start_->first_instr == NULL) abort();
  int status = scalar_conversions(expr, (Type *)TYPE_LONG);
  if (status) abort();
  InstructionData *expr_data = liter_get(expr->last_instr);
  if (expr_data == NULL) abort();
  size_t va_list_vreg = expr_data->dest.reg.num;

  /* `list->gp_offset = param_reg_index * X64_SIZEOF_LONG;` */
  InstructionData *load_gp_offset_data = instr_init(OP_MOV);
  set_op_imm(&load_gp_offset_data->src, param_reg_index * X64_SIZEOF_LONG,
             IMM_UNSIGNED);
  set_op_reg(&load_gp_offset_data->dest, REG_DWORD, next_vreg());

  InstructionData *store_gp_offset_data = instr_init(OP_MOV);
  store_gp_offset_data->src = load_gp_offset_data->dest;
  set_op_ind(&store_gp_offset_data->dest, GP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* `list->fp_offset = 304;` */
  InstructionData *load_fp_offset_data = instr_init(OP_MOV);
  set_op_imm(&load_fp_offset_data->src, FP_OFFSET, IMM_UNSIGNED);
  set_op_reg(&load_fp_offset_data->dest, REG_DWORD, next_vreg());

  InstructionData *store_fp_offset_data = instr_init(OP_MOV);
  store_fp_offset_data->src = load_fp_offset_data->dest;
  set_op_ind(&store_fp_offset_data->dest, FP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* `list->overflow_arg_area = param_stack_disp + %rbp;` */
  InstructionData *param_stack_disp_data = instr_init(OP_MOV);
  set_op_imm(&param_stack_disp_data->src, param_stack_disp, IMM_SIGNED);
  set_op_reg(&param_stack_disp_data->dest, REG_QWORD, next_vreg());

  InstructionData *add_rbp_data_2 = instr_init(OP_ADD);
  set_op_reg(&add_rbp_data_2->src, REG_QWORD, RBP_VREG);
  add_rbp_data_2->dest = param_stack_disp_data->dest;

  InstructionData *store_overflow_arg_area_data = instr_init(OP_MOV);
  store_overflow_arg_area_data->src = add_rbp_data_2->dest;
  set_op_ind(&store_overflow_arg_area_data->dest, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);

  /* `list->reg_save_area = reg_save_area_disp + %rbp` */
  InstructionData *reg_save_area_disp_data = instr_init(OP_MOV);
  set_op_imm(&reg_save_area_disp_data->src, reg_save_area_disp, IMM_SIGNED);
  set_op_reg(&reg_save_area_disp_data->dest, REG_QWORD, next_vreg());

  InstructionData *add_rbp_data = instr_init(OP_ADD);
  set_op_reg(&add_rbp_data->src, REG_QWORD, RBP_VREG);
  add_rbp_data->dest = reg_save_area_disp_data->dest;

  InstructionData *store_reg_save_area_data = instr_init(OP_MOV);
  store_reg_save_area_data->src = add_rbp_data->dest;
  set_op_ind(&store_reg_save_area_data->dest, REG_SAVE_AREA_MEMBER_DISP,
             va_list_vreg);
  store_reg_save_area_data->persist_flags |= PERSIST_DEST_SET;

  /* TODO(Robert): are these pushed out of order intentionally? */
  ListIter *temp = llist_iter_last(instructions);
  status = liter_push_back(
      temp, &va_start_->last_instr, 10, load_gp_offset_data,
      store_gp_offset_data, load_fp_offset_data, store_fp_offset_data,
      reg_save_area_disp_data, add_rbp_data, store_reg_save_area_data,
      param_stack_disp_data, add_rbp_data_2, store_overflow_arg_area_data);
  free(temp);
  if (status) abort();
  return astree_adopt(va_start_, 2, expr, ident);
}

ASTree *translate_va_end(ASTree *va_end_, ASTree *expr) {
  va_end_->first_instr = liter_copy(expr->first_instr);
  if (va_end_->first_instr == NULL) abort();
  va_end_->last_instr = liter_copy(expr->last_instr);
  if (va_end_->last_instr == NULL) abort();

  return astree_adopt(va_end_, 1, expr);
}

/* NOTE: since floating point arithmetic has not been implemented whatsoever,
 * the `fp_offset` field is ignored by `va_arg`.
 */
int helper_va_arg_reg_param(ASTree *va_arg_, ASTree *expr, ASTree *type_name) {
  InstructionData *expr_data = liter_get(expr->last_instr);
  assert(expr_data != NULL && expr_data->dest.all.mode == MODE_REGISTER);
  size_t va_list_vreg = expr_data->dest.reg.num;
  size_t eightbytes = type_get_eightbytes(astree_get(type_name, 1)->type);
  size_t result_vreg = next_vreg(), current_branch = next_branch();

  /* load gp offset member */
  InstructionData *load_gp_offset_data = instr_init(OP_MOV);
  set_op_ind(&load_gp_offset_data->src, GP_OFFSET_MEMBER_DISP, va_list_vreg);
  set_op_reg(&load_gp_offset_data->dest, REG_DWORD, result_vreg);
  /* first use of result_vreg; it no longer needs to persist */
  load_gp_offset_data->persist_flags = PERSIST_DEST_CLEAR;

  /* jump if arg cannot fit into the save area */
  InstructionData *cmp_gp_offset_data = instr_init(OP_CMP);
  cmp_gp_offset_data->src = load_gp_offset_data->dest;
  /* if arg takes up two eightbytes, offset can't be >= 40 */
  if (eightbytes == 2)
    set_op_imm(&cmp_gp_offset_data->dest, GP_OFFSET_MAX - X64_SIZEOF_LONG,
               IMM_SIGNED);
  else
    set_op_imm(&cmp_gp_offset_data->dest, GP_OFFSET_MAX, IMM_SIGNED);

  InstructionData *jmp_ge_data = instr_init(OP_JGE);
  set_op_dir(&jmp_ge_data->dest, mk_true_label(current_branch));

  /* add gp offset to save area disp to get location of next arg */
  InstructionData *add_save_area_data = instr_init(OP_ADD);
  set_op_ind(&add_save_area_data->src, REG_SAVE_AREA_MEMBER_DISP, va_list_vreg);
  /* dword mov should zero hi 32 bits, so a qword add should behave here */
  set_op_reg(&add_save_area_data->dest, REG_QWORD, result_vreg);

  /* TODO(Robert): give all operands/instructions a width field; currently the
   * compiler cannot emit instructions with one immediate mode operand and one
   * indirect/scaled-index mode operand because neither of them carry width
   * information.
   */
  /* load arg eightbytes */
  InstructionData *load_eightbyte_data = instr_init(OP_MOV);
  set_op_imm(&load_eightbyte_data->src, eightbytes * X64_SIZEOF_LONG, 0);
  set_op_reg(&load_eightbyte_data->dest, REG_DWORD, next_vreg());

  /* update gp offset member */
  InstructionData *update_offset_data = instr_init(OP_ADD);
  update_offset_data->src = load_eightbyte_data->dest;
  set_op_ind(&update_offset_data->dest, GP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* jump to dummy instruction */
  InstructionData *jmp_false_data = instr_init(OP_JMP);
  set_op_dir(&jmp_false_data->dest, mk_false_label(current_branch));

  /* load stack param area */
  InstructionData *load_overflow_arg_area_data = instr_init(OP_MOV);
  set_op_ind(&load_overflow_arg_area_data->src, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  set_op_reg(&load_overflow_arg_area_data->dest, REG_QWORD, result_vreg);
  load_overflow_arg_area_data->label = mk_true_label(current_branch);

  /* update stack param save pointer */
  InstructionData *load_disp_data = instr_init(OP_MOV);
  set_op_imm(&load_disp_data->src, eightbytes * X64_SIZEOF_LONG, IMM_UNSIGNED);
  set_op_reg(&load_disp_data->dest, REG_QWORD, next_vreg());

  InstructionData *add_disp_data = instr_init(OP_ADD);
  add_disp_data->src = load_disp_data->dest;
  set_op_ind(&add_disp_data->dest, OVERFLOW_ARG_AREA_MEMBER_DISP, va_list_vreg);
  /* va_list result vreg must perist until at least this point */
  add_disp_data->persist_flags |= PERSIST_DEST_SET;

  /* dummy mov so last instruction holds result */
  InstructionData *dummy_data = instr_init(OP_MOV);
  set_op_reg(&dummy_data->src, REG_QWORD, result_vreg);
  dummy_data->dest = dummy_data->src;
  dummy_data->label = mk_false_label(current_branch);
  dummy_data->persist_flags = PERSIST_DEST_SET;

  return liter_push_back(
      expr->last_instr, &va_arg_->last_instr, 11, load_gp_offset_data,
      cmp_gp_offset_data, jmp_ge_data, add_save_area_data, load_eightbyte_data,
      update_offset_data, jmp_false_data, load_overflow_arg_area_data,
      load_disp_data, add_disp_data, dummy_data);
}

int helper_va_arg_stack_param(ASTree *va_arg_, ASTree *expr,
                              ASTree *type_name) {
  InstructionData *expr_data = liter_get(expr->last_instr);
  assert(expr_data != NULL && expr_data->dest.all.mode == MODE_REGISTER);
  size_t va_list_vreg = expr_data->dest.reg.num;
  size_t eightbytes = type_get_eightbytes(astree_get(type_name, 1)->type);

  /* load location of next stack parameter */
  InstructionData *load_overflow_arg_area_data = instr_init(OP_MOV);
  set_op_ind(&load_overflow_arg_area_data->src, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  set_op_reg(&load_overflow_arg_area_data->dest, REG_QWORD, next_vreg());
  /* first use of result vreg; no need to persist it above this point */
  load_overflow_arg_area_data->persist_flags |= PERSIST_DEST_CLEAR;

  /* load displacement to add to overflow pointer */
  InstructionData *load_disp_data = instr_init(OP_MOV);
  set_op_imm(&load_disp_data->src, eightbytes * X64_SIZEOF_LONG, IMM_UNSIGNED);
  set_op_reg(&load_disp_data->dest, REG_QWORD, next_vreg());

  /* update overflow pointer */
  InstructionData *add_disp_data = instr_init(OP_ADD);
  add_disp_data->src = load_disp_data->dest;
  set_op_ind(&add_disp_data->dest, OVERFLOW_ARG_AREA_MEMBER_DISP, va_list_vreg);
  /* va_list_vreg must persist until at least this point */
  add_disp_data->persist_flags |= PERSIST_DEST_SET;

  /* dummy mov so that last instruction contains result */
  InstructionData *dummy_data = instr_init(OP_MOV);
  dummy_data->src = dummy_data->dest = load_overflow_arg_area_data->dest;

  return liter_push_back(expr->last_instr, &va_arg_->last_instr, 4,
                         load_overflow_arg_area_data, load_disp_data,
                         add_disp_data, dummy_data);
}

ASTree *translate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name) {
  va_arg_->first_instr = liter_copy(expr->first_instr);
  if (va_arg_->first_instr == NULL) abort();

  if (type_get_eightbytes(astree_get(type_name, 1)->type) <= 2)
    assert(!helper_va_arg_reg_param(va_arg_, expr, type_name));
  else
    assert(!helper_va_arg_stack_param(va_arg_, expr, type_name));
  assert(va_arg_->last_instr != NULL);
  assert(liter_get(va_arg_->last_instr) == llist_back(instructions));

  return astree_adopt(va_arg_, 2, expr, type_name);
}

int translate_params(ASTree *declarator) {
  ASTree *fn_dirdecl = astree_get(declarator, astree_count(declarator) - 1);
  const Type *fn_type = declarator->type;
  Type *ret_type;
  int status = type_strip_declarator(&ret_type, fn_type);
  if (status) return status;
  /* account for hidden out param */
  param_reg_index = type_get_eightbytes(ret_type) > 2 ? 1 : 0;
  if (param_reg_index == 1) {
    SymbolValue dummy = {0};
    /* TODO(Robert): (void?) pointer type constant */
    dummy.type = (Type *)TYPE_LONG;
    assign_stack_space(&dummy);
    InstructionData *mov_data = instr_init(OP_MOV);
    set_op_reg(&mov_data->src, REG_QWORD, RDI_VREG);
    set_op_ind(&mov_data->dest, dummy.disp, RBP_VREG);
    int status = llist_push_back(instructions, mov_data);
    if (status) return status;
  }
  /* offset to account for preserved regs and return address */
  param_stack_disp = PROLOGUE_EIGHTBYTES * X64_SIZEOF_LONG;
  size_t i, param_count = astree_count(fn_dirdecl);
  if (param_count > 0 && type_is_variadic_function(fn_type)) --param_count;
  if (param_count != 0 && astree_get(fn_dirdecl, 0)->symbol != TOK_VOID) {
    for (i = 0; i < param_count; ++i) {
      ASTree *param = astree_get(fn_dirdecl, i);
      ASTree *param_decl = astree_get(param, 1);
      SymbolValue *param_symval = NULL;
      assert(state_get_symbol(state, param_decl->lexinfo,
                              strlen(param_decl->lexinfo), &param_symval));
      assert(param_symval);
      size_t param_symval_eightbytes = type_get_eightbytes(param_symval->type);
      if (param_symval_eightbytes <= 2 &&
          param_reg_index + param_symval_eightbytes <= PARAM_REG_COUNT) {
        assign_stack_space(param_symval);
        ListIter *temp = llist_iter_last(instructions);
        int status =
            bulk_rtom(RBP_VREG, param_symval->disp,
                      PARAM_REGS + param_reg_index, param_symval->type, temp);
        param_reg_index += param_symval_eightbytes;
        free(temp);
        if (status) return status;
      } else {
        param_symval->disp = param_stack_disp;
        param_stack_disp += param_symval_eightbytes * X64_SIZEOF_LONG;
      }
    }
  }

  /* TODO(Robert): this is clunky and it sucks */
  /* TODO(Robert): because of the way `va_list` is implemented, the varargs
   * register spill region must always be 48 bytes.
   */
  if (type_is_variadic_function(fn_type) && param_reg_index < PARAM_REG_COUNT) {
    TagValue dummy_tag = {0};
    dummy_tag.width = X64_SIZEOF_LONG * (PARAM_REG_COUNT - param_reg_index);
    dummy_tag.alignment = X64_ALIGNOF_LONG;

    Type dummy_type = {0};
    dummy_type.tag.code = TYPE_CODE_STRUCT;
    dummy_type.tag.value = &dummy_tag;

    SymbolValue dummy_symbol = {0};
    dummy_symbol.type = &dummy_type;
    dummy_type.tag.symbol = &dummy_symbol;

    assign_stack_space(&dummy_symbol);
    reg_save_area_disp = dummy_symbol.disp;
    ListIter *temp = llist_iter_last(instructions);
    int status =
        bulk_rtom(RBP_VREG, dummy_symbol.disp, PARAM_REGS + param_reg_index,
                  dummy_symbol.type, temp);
    if (status) abort();
    free(temp);
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
  test_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(ifelse->jump_id);
  if (else_body != &EMPTY_EXPR) {
    InstructionData *else_label = instr_init(OP_NONE);
    else_label->label = mk_stmt_label(ifelse->jump_id);
    int status = liter_push_front(else_body->first_instr, NULL, 1, else_label);
    if (status) abort();

    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, else_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) abort();

    InstructionData *jmp_data = instr_init(OP_JMP);
    set_op_dir(&jmp_data->dest, end_label->label);
    status = liter_push_back(if_body->last_instr, NULL, 1, jmp_data);
    if (status) abort();
    status = liter_push_back(else_body->last_instr, &ifelse->last_instr, 1,
                             end_label);
    if (status) abort();
  } else {
    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, end_label->label);
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
  size_t saved_break = SIZE_MAX, saved_selection = SIZE_MAX;
  if (state_get_break_id(state) != switch_->jump_id)
    saved_break = state_get_break_id(state), state_pop_break_id(state);
  assert(state_get_break_id(state) == switch_->jump_id);
  state_pop_break_id(state);

  if (state_get_selection_id(state) != switch_->jump_id)
    saved_selection = state_get_selection_id(state),
    state_pop_selection_id(state);
  assert(state_get_selection_id(state) == switch_->jump_id);
  /* save switch statement info before popping from stack */
  int has_default_stmt = state_get_selection_default(state);
  const Type *control_type = state_get_control_type(state);
  size_t fake_case_id = state_get_case_id(state);
  size_t control_vreg = state_get_control_reg(state);
  state_pop_selection_id(state);

  int status = scalar_conversions(condition, control_type);
  if (status) abort();
  InstructionData *cond_data = liter_get(condition->last_instr);
  if (cond_data == NULL) abort();
  switch_->first_instr = liter_copy(condition->first_instr);
  if (switch_->first_instr == NULL) abort();

  /* switch prologue */
  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = cond_data->dest;
  set_op_reg(&mov_data->dest, type_get_width(control_type), control_vreg);
  /* clear control vreg from persistence data; persist condition expression */
  mov_data->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
  InstructionData *jmp_case1_data = instr_init(OP_JMP);
  set_op_dir(&jmp_case1_data->dest, mk_case_label(switch_->jump_id, 0));
  status =
      liter_push_back(condition->last_instr, NULL, 2, mov_data, jmp_case1_data);
  if (status) abort();

  /* switch epilogue */
  InstructionData *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(switch_->jump_id);
  InstructionData *jmp_end_data = instr_init(OP_JMP);
  set_op_dir(&jmp_end_data->dest, end_label->label);
  InstructionData *dummy_case_label = instr_init(OP_NONE);
  dummy_case_label->label = mk_case_label(switch_->jump_id, fake_case_id);
  if (has_default_stmt) {
    InstructionData *jmp_def_data = instr_init(OP_JMP);
    set_op_dir(&jmp_def_data->dest, mk_def_label(switch_->jump_id));
    int status =
        liter_push_back(body->last_instr, &switch_->last_instr, 4, jmp_end_data,
                        dummy_case_label, jmp_def_data, end_label);
    if (status) abort();
  } else {
    int status = liter_push_back(body->last_instr, &switch_->last_instr, 3,
                                 jmp_end_data, dummy_case_label, end_label);
    if (status) abort();
  }

  if (saved_break != SIZE_MAX) state_push_break_id(state, saved_break);
  if (saved_selection != SIZE_MAX)
    state_push_selection_id(state, saved_selection);

  return astree_adopt(switch_, 2, condition, body);
}

ASTree *translate_while(ASTree *while_, ASTree *condition, ASTree *body) {
  size_t saved_break = SIZE_MAX, saved_continue = SIZE_MAX;
  if (state_get_break_id(state) != while_->jump_id)
    saved_break = state_get_break_id(state), state_pop_break_id(state);
  assert(state_get_break_id(state) == while_->jump_id);
  state_pop_break_id(state);

  if (state_get_continue_id(state) != while_->jump_id)
    saved_continue = state_get_continue_id(state), state_pop_continue_id(state);
  assert(state_get_continue_id(state) == while_->jump_id);
  state_pop_continue_id(state);

  InstructionData *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(while_->jump_id);
  /* set first instr to label */
  int status = liter_push_front(condition->first_instr, &while_->first_instr, 1,
                                condition_label);
  if (status) abort();

  status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);
  InstructionData *test_data = instr_init(OP_TEST);
  test_data->src = test_data->dest = condition_data->dest;
  test_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(while_->jump_id);

  InstructionData *test_jmp_data = instr_init(OP_JZ);
  set_op_dir(&test_jmp_data->dest, end_label->label);
  status =
      liter_push_back(condition->last_instr, NULL, 2, test_data, test_jmp_data);
  if (status) abort();

  InstructionData *cond_jmp_data = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_data->dest, condition_label->label);
  /* set last instr to end label */
  status = liter_push_back(body->last_instr, &while_->last_instr, 2,
                           cond_jmp_data, end_label);
  if (status) abort();

  if (saved_break != SIZE_MAX) state_push_break_id(state, saved_break);
  if (saved_continue != SIZE_MAX) state_push_continue_id(state, saved_continue);

  return astree_adopt(while_, 2, condition, body);
}

ASTree *translate_for(ASTree *for_, ASTree *initializer, ASTree *condition,
                      ASTree *reinitializer, ASTree *body) {
  size_t saved_break = SIZE_MAX, saved_continue = SIZE_MAX;
  if (state_get_break_id(state) != for_->jump_id)
    saved_break = state_get_break_id(state), state_pop_break_id(state);
  assert(state_get_break_id(state) == for_->jump_id);
  state_pop_break_id(state);

  if (state_get_continue_id(state) != for_->jump_id)
    saved_continue = state_get_continue_id(state), state_pop_continue_id(state);
  assert(state_get_continue_id(state) == for_->jump_id);
  state_pop_continue_id(state);

  for_->first_instr = liter_copy(initializer->first_instr);
  if (for_->first_instr == NULL) abort();

  InstructionData *condition_start_data = liter_get(condition->first_instr);
  condition_start_data->label = mk_cond_label(for_->jump_id);

  /* because of the way `liter_push_back` works, new instructions should be
   * added after the condition in reverse order
   */
  /* add dummy reinitializer if necessary */
  if (reinitializer->symbol == ';') {
    assert(reinitializer->first_instr == NULL &&
           reinitializer->last_instr == NULL && reinitializer != &EMPTY_EXPR);
    InstructionData *nop_data = instr_init(OP_NOP);
    int status = liter_push_back(condition->last_instr,
                                 &reinitializer->first_instr, 1, nop_data);
    if (status) abort();
    reinitializer->last_instr = liter_copy(reinitializer->first_instr);
    if (reinitializer->last_instr == NULL) abort();
  }

  /* add unconditional jump to function body, skipping reinitializer */
  InstructionData *body_jmp_data = instr_init(OP_JMP);
  set_op_dir(&body_jmp_data->dest, mk_stmt_label(for_->jump_id));
  int status = liter_push_back(condition->last_instr, NULL, 1, body_jmp_data);
  if (status) abort();

  /* emit loop exit jump if condition is not empty */
  if (condition->symbol != ';') {
    int status = scalar_conversions(condition, condition->type);
    if (status) abort();
    InstructionData *condition_data = liter_get(condition->last_instr);
    InstructionData *test_data = instr_init(OP_TEST);
    test_data->dest = test_data->src = condition_data->dest;
    test_data->persist_flags |= PERSIST_DEST_SET;

    InstructionData *test_jmp_data = instr_init(OP_JZ);
    set_op_dir(&test_jmp_data->dest, mk_end_label(for_->jump_id));
    status = liter_push_back(condition->last_instr, NULL, 2, test_data,
                             test_jmp_data);
    if (status) abort();
  }

  InstructionData *reinit_start_data = liter_get(reinitializer->first_instr);
  reinit_start_data->label = mk_reinit_label(for_->jump_id);
  InstructionData *cond_jmp_data = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_data->dest, mk_cond_label(for_->jump_id));
  status = liter_push_back(reinitializer->last_instr, NULL, 1, cond_jmp_data);
  if (status) abort();

  InstructionData *body_label = instr_init(OP_NONE);
  body_label->label = mk_stmt_label(for_->jump_id);
  status = liter_push_front(body->first_instr, NULL, 1, body_label);
  if (status) abort();
  InstructionData *reinit_jmp_data = instr_init(OP_JMP);
  set_op_dir(&reinit_jmp_data->dest, mk_reinit_label(for_->jump_id));
  InstructionData *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(for_->jump_id);
  status = liter_push_back(body->last_instr, &for_->last_instr, 2,
                           reinit_jmp_data, end_label);
  if (status) abort();

  if (saved_break != SIZE_MAX) state_push_break_id(state, saved_break);
  if (saved_continue != SIZE_MAX) state_push_continue_id(state, saved_continue);

  return astree_adopt(for_, 4, initializer, condition, reinitializer, body);
}

ASTree *translate_do(ASTree *do_, ASTree *body, ASTree *condition) {
  /* fix bogus jump id information created by TOK_WHILE */
  size_t saved_break = state_get_break_id(state);
  state_pop_break_id(state);
  if (state_get_break_id(state) == do_->jump_id)
    saved_break = SIZE_MAX;
  else
    state_pop_break_id(state);
  assert(state_get_break_id(state) == do_->jump_id);
  state_pop_break_id(state);

  size_t saved_continue = state_get_continue_id(state);
  state_pop_continue_id(state);
  if (state_get_continue_id(state) == do_->jump_id)
    saved_continue = SIZE_MAX;
  else
    state_pop_continue_id(state);
  assert(state_get_continue_id(state) == do_->jump_id);
  state_pop_continue_id(state);

  InstructionData *body_label = instr_init(OP_NONE);
  body_label->label = mk_stmt_label(do_->jump_id);
  int status =
      liter_push_front(body->first_instr, &do_->first_instr, 1, body_label);

  InstructionData *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(do_->jump_id);
  status = liter_push_front(condition->first_instr, NULL, 1, condition_label);
  if (status) abort();

  status = scalar_conversions(condition, condition->type);
  if (status) abort();
  InstructionData *condition_data = liter_get(condition->last_instr);
  if (condition_data == NULL) abort();

  InstructionData *test_data = instr_init(OP_TEST);
  test_data->dest = test_data->src = condition_data->dest;
  test_data->persist_flags |= PERSIST_DEST_SET;

  InstructionData *test_jmp_data = instr_init(OP_JNZ);
  set_op_dir(&test_jmp_data->dest, body_label->label);

  InstructionData *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(do_->jump_id);
  status = liter_push_back(condition->last_instr, &do_->last_instr, 3,
                           test_data, test_jmp_data, end_label);

  if (saved_break != SIZE_MAX) state_push_break_id(state, saved_break);
  if (saved_continue != SIZE_MAX) state_push_continue_id(state, saved_continue);

  return astree_adopt(do_, 2, body, condition);
}

/* TODO(Robert): iterate over block's children to find the first and last
 * instructions emitted (if there were any), and emit a nop instruction if
 * they weren't
 */
ASTree *translate_block(ASTree *block) {
  PFDBG0('g', "Translating compound statement");
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
  const Type *function_type = function_symval->type;
  /* strip function */
  Type *return_type;
  int status = type_strip_declarator(&return_type, function_type);
  if (status) return status;
  status = scalar_conversions(expr, return_type);
  if (status) return status;
  InstructionData *expr_data = liter_get(expr->last_instr);

  InstructionData *mov_data = instr_init(OP_MOV);
  mov_data->src = expr_data->dest;
  set_op_reg(&mov_data->dest, type_get_width(return_type), RAX_VREG);
  mov_data->persist_flags |= PERSIST_SRC_SET;
  status = llist_push_back(instructions, mov_data);
  if (status) return status;

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
  size_t expr_eightbytes = type_get_eightbytes(expr->type);

  if (expr_eightbytes <= 2) {
    ListIter *temp = llist_iter_last(instructions);
    /* `bulk_mtor` should handle persistence flags */
    int status = bulk_mtor(RETURN_REGS, expr_data->dest.reg.num, NO_DISP,
                           expr->type, temp);
    free(temp);
    if (status) return status;
  } else {
    InstructionData *hidden_mov_data = instr_init(OP_MOV);
    set_op_reg(&hidden_mov_data->dest, REG_QWORD, RAX_VREG);
    set_op_ind(&hidden_mov_data->src, -8, RBP_VREG);
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
  PFDBG0('g', "Translating return statement");
  if (expr == &EMPTY_EXPR || type_is_void(expr->type)) {
    int status = return_void(ret);
    if (status) abort();
  } else {
    if (type_is_union(expr->type) || type_is_struct(expr->type)) {
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
  set_op_dir(&cond_jmp_data->dest, mk_cond_label(continue_->jump_id));
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
  set_op_dir(&end_jmp_data->dest, mk_end_label(break_->jump_id));
  int status = llist_push_back(instructions, end_jmp_data);
  if (status) abort();
  break_->first_instr = llist_iter_last(instructions);
  if (break_->first_instr == NULL) abort();
  break_->last_instr = liter_copy(break_->first_instr);
  if (break_->last_instr == NULL) abort();
  return break_;
}

ASTree *translate_goto(ASTree *goto_, ASTree *ident) {
  InstructionData *jmp_data = instr_init(OP_JMP);
  set_op_dir(&jmp_data->dest, mk_local_label(ident->lexinfo));
  int status = llist_push_back(instructions, jmp_data);
  if (status) abort();
  goto_->first_instr = llist_iter_last(instructions);
  if (goto_->first_instr == NULL) abort();
  goto_->last_instr = liter_copy(goto_->first_instr);
  if (goto_->last_instr == NULL) abort();
  return astree_adopt(goto_, 1, ident);
}

ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt) {
  InstructionData *label_data = instr_init(OP_NONE);
  label_data->label = mk_local_label(ident->lexinfo);

  int status =
      liter_push_front(stmt->first_instr, &label->first_instr, 1, label_data);
  if (status) abort();
  label->last_instr = liter_copy(stmt->last_instr);
  if (label->last_instr == NULL) abort();
  return astree_adopt(label, 2, ident, stmt);
}

ASTree *translate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  case_->last_instr = liter_copy(stmt->last_instr);
  if (case_->last_instr == NULL) abort();

  const Type *control_type = state_get_control_type(state);
  if (control_type == NULL) abort();

  /* TODO(Robert): is the MOV necessary? the other operand is a register */
  InstructionData *mov_data = instr_init(OP_MOV);
  set_op_reg(&mov_data->dest, type_get_width(control_type), next_vreg());
  if (type_is_unsigned(control_type)) {
    set_op_imm(&mov_data->src, expr->constant.integral.unsigned_value, 0);
  } else {
    set_op_imm(&mov_data->src, expr->constant.integral.signed_value, 1);
  }
  InstructionData *test_data = instr_init(OP_TEST);
  set_op_reg(&test_data->dest, type_get_width(control_type),
             state_get_control_reg(state));
  test_data->src = mov_data->dest;
  /* persist test register between basic blocks */
  test_data->persist_flags = PERSIST_DEST_SET;
  InstructionData *jmp_data = instr_init(OP_JNE);
  /* NOTE: because of the way case ids are used, they do not require special
   * handling like jump, continue, and selection ids do
   */
  /* jump to next case statement if condition is false */
  set_op_dir(&jmp_data->dest,
             mk_case_label(case_->jump_id, case_->case_id + 1));
  InstructionData *fall_label = instr_init(OP_NONE);
  fall_label->label = mk_fallthru_label(case_->jump_id, case_->case_id);
  InstructionData *case_label = instr_init(OP_NONE);
  case_label->label = mk_case_label(case_->jump_id, case_->case_id);
  InstructionData *fall_jmp_data = instr_init(OP_JMP);
  set_op_dir(&fall_jmp_data->dest, fall_label->label);
  int status =
      liter_push_front(stmt->first_instr, &case_->first_instr, 6, fall_jmp_data,
                       case_label, mov_data, test_data, jmp_data, fall_label);
  if (status) abort();
  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *translate_default(ASTree *default_, ASTree *stmt) {
  InstructionData *def_label = instr_init(OP_NONE);
  def_label->label = mk_def_label(default_->jump_id);

  int status =
      liter_push_front(stmt->first_instr, &default_->first_instr, 1, def_label);
  if (status) abort();
  default_->last_instr = liter_copy(stmt->last_instr);
  if (default_->last_instr == NULL) abort();
  return astree_adopt(default_, 1, stmt);
}

ASTree *translate_static_scalar_init(const Type *type, ASTree *initializer,
                                     ListIter *where) {
  Opcode directive;
  switch (type_get_width(type)) {
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
  if (initializer->constant.label != NULL) {
    set_op_pic(&data->dest, initializer->constant.integral.signed_value,
               initializer->constant.label);
  } else if (type_is_unsigned(type)) {
    set_op_imm(&data->dest, initializer->constant.integral.unsigned_value, 1);
  } else {
    set_op_imm(&data->dest, initializer->constant.integral.signed_value, 0);
  }
  int status = liter_push_back(where, &where, 1, data);
  if (status) abort();
  initializer->first_instr = liter_copy(where);
  if (initializer->first_instr == NULL) abort();
  initializer->last_instr = liter_copy(where);
  if (initializer->last_instr == NULL) abort();
  return initializer;
}

ASTree *translate_auto_scalar_init(const Type *type, ptrdiff_t disp,
                                   ASTree *initializer, ListIter *where) {
  assert((initializer->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT);
  InstructionData *load_data;
  if (initializer->constant.label != NULL) {
    load_data = instr_init(OP_LEA);
    set_op_pic(&load_data->src, initializer->constant.integral.signed_value,
               initializer->constant.label);
  } else if (type_is_unsigned(type)) {
    load_data = instr_init(OP_MOV);
    set_op_imm(&load_data->src, initializer->constant.integral.unsigned_value,
               0);
  } else {
    load_data = instr_init(OP_MOV);
    set_op_imm(&load_data->src, initializer->constant.integral.signed_value, 1);
  }
  set_op_reg(&load_data->dest, type_get_width(type), next_vreg());
  InstructionData *store_data = instr_init(OP_MOV);
  store_data->src = load_data->dest;
  set_op_ind(&store_data->dest, disp, RBP_VREG);

  int status = liter_push_back(where, &where, 2, load_data, store_data);
  if (status) abort();
  initializer->first_instr = liter_prev(where, 1);
  if (initializer->first_instr == NULL) abort();
  initializer->last_instr = liter_copy(where);
  if (initializer->last_instr == NULL) abort();
  return initializer;
}

ASTree *translate_static_literal_init(const Type *arr_type, ASTree *literal,
                                      ListIter *where) {
  size_t i;
  for (i = 0; i < literals_size; ++i) {
    if (literals[i].label == literal->constant.label) {
      const char *str = literals[i].literal;
      size_t arr_width = type_get_width(arr_type);
      size_t literal_length = strlen(str) - 2;
      if (type_is_deduced_array(arr_type)) {
        assert(arr_width == literal_length + 1);
        InstructionData *asciz_data = instr_init(OP_ASCIZ);
        set_op_dir(&asciz_data->dest, str);
        int status = liter_push_back(where, &where, 1, asciz_data);
        if (status) abort();
        literal->first_instr = liter_copy(where);
        if (literal->first_instr == NULL) abort();
        literal->last_instr = liter_copy(where);
        if (literal->last_instr == NULL) abort();
        return literal;
      } else if (literal_length <= arr_width) {
        InstructionData *ascii_data = instr_init(OP_ASCII);
        set_op_dir(&ascii_data->dest, str);
        int status = liter_push_back(where, &where, 1, ascii_data);
        if (status) abort();
        literal->first_instr = liter_copy(where);
        if (literal->first_instr == NULL) abort();

        size_t zero_count = arr_width - literal_length;
        if (zero_count > 0) {
          int status = static_zero_pad(zero_count, where);
          if (status) abort();
        }
        literal->last_instr = liter_copy(where);
        if (literal->last_instr == NULL) abort();
        return literal;
      } else {
        size_t partial_len = arr_width + 2;
        char *partial_str = malloc(partial_len + 1);
        partial_str[partial_len] = '\0';
        partial_str[partial_len - 1] = '\"';
        memcpy(partial_str, str, arr_width + 1);

        char *existing = map_get(generated_text, partial_str, partial_len);
        if (existing) {
          free(partial_str);
          partial_str = existing;
        } else {
          int status =
              map_insert(generated_text, partial_str, partial_len, partial_str);
          if (status) abort();
        }

        InstructionData *ascii_data = instr_init(OP_ASCII);
        set_op_dir(&ascii_data->dest, partial_str);
        int status = liter_push_back(where, &where, 1, ascii_data);
        if (status) abort();
        literal->first_instr = liter_copy(where);
        if (literal->first_instr == NULL) abort();
        literal->last_instr = liter_copy(where);
        if (literal->last_instr == NULL) abort();
        return literal;
      }
    }
  }
  /* literal not found */
  abort();
}

ASTree *translate_auto_literal_init(const Type *arr_type, ptrdiff_t arr_disp,
                                    ASTree *literal, ListIter *where) {
  InstructionData *literal_lea_data = instr_init(OP_LEA);
  set_op_pic(&literal_lea_data->src, literal->constant.integral.signed_value,
             literal->constant.label);
  set_op_reg(&literal_lea_data->dest, REG_QWORD, next_vreg());
  /* need to clear persistence because `bulk_mtom` always sets it */
  literal_lea_data->persist_flags |= PERSIST_DEST_CLEAR;

  InstructionData *arr_lea_data = instr_init(OP_LEA);
  set_op_ind(&arr_lea_data->src, arr_disp, RBP_VREG);
  set_op_reg(&arr_lea_data->dest, REG_QWORD, next_vreg());
  /* need to clear persistence because `bulk_mtom` always sets it */
  arr_lea_data->persist_flags |= PERSIST_DEST_CLEAR;

  int status =
      liter_push_back(where, &where, 2, literal_lea_data, arr_lea_data);
  if (status) abort();
  literal->first_instr = liter_prev(where, 1);
  if (literal->first_instr == NULL) abort();

  size_t arr_width = type_get_width(arr_type);
  size_t literal_width = type_get_width(literal->type);
  liter_advance(where, 1);
  ListIter *temp = liter_prev(where, 1);
  /* we need to know where the last instruction was inserted */
  status =
      bulk_mtom(arr_lea_data->dest.reg.num, literal_lea_data->dest.reg.num,
                (arr_width > literal_width) ? literal->type : arr_type, temp);
  if (!status && arr_width > literal_width) {
    status = bulk_mzero(arr_lea_data->dest.reg.num, NO_DISP, literal_width,
                        arr_type, temp);
  }
  liter_advance(where, -1);
  free(temp);
  if (status) abort();
  literal->last_instr = liter_copy(where);
  if (literal->last_instr == NULL) abort();
  return literal;
}

int translate_static_prelude(ASTree *declarator, SymbolValue *symval,
                             ListIter *where) {
  if (symval->flags & SYMFLAG_LINK_EXT) {
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, declarator->lexinfo);
    int status = liter_push_back(where, &where, 1, globl_data);
    if (status) return status;
  }

  const char *identifier =
      symval->flags & SYMFLAG_LINK_NONE
          ? mk_static_label(declarator->lexinfo, symval->static_id)
          : declarator->lexinfo;

  InstructionData *align_data = instr_init(OP_ALIGN);
  set_op_imm(&align_data->dest, type_get_alignment(declarator->type),
             IMM_UNSIGNED);
  InstructionData *type_data = instr_init(OP_TYPE);
  set_op_dir(&type_data->dest, identifier);
  set_op_dir(&type_data->src, "@object");
  InstructionData *size_data = instr_init(OP_SIZE);
  set_op_dir(&size_data->dest, identifier);
  set_op_imm(&size_data->src, type_get_width(declarator->type), IMM_UNSIGNED);
  InstructionData *label_data = instr_init(OP_NONE);
  label_data->label = identifier;
  /* TODO(Robert): i think the behavior here is not what i expected, or at worst
   * incorrecty. while the instructions will be pushed in the correct order,
   * storing the iterator to the final instruction in `&where` will not work:
   * `where` is a temporary and the given iterator will not be changed. this is
   * at least an issure in `translate_local_decl`, which inserts instructions
   * before the function definition and does not change the value of
   * `before_definition` to match the new last instruction before the definition
   */
  return liter_push_back(where, &where, 4, align_data, type_data, size_data,
                         label_data);
}

/* TODO(Robert): This is really three different functions. Separate them. */
static ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                                    ASTree *declarator, ASTree *initializer) {
  PFDBG0('g', "Translating local initialization");
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symval);
  assert(in_current_scope && symval);

  if (symval->flags & SYMFLAG_STORE_STAT) {
    assign_static_space(declarator->lexinfo, symval);
    InstructionData *data_data = instr_init(OP_DATA);
    int status =
        liter_push_back(before_definition, &before_definition, 1, data_data);
    if (status) abort();
    ListIter *temp = liter_copy(before_definition);
    ASTree *errnode = traverse_initializer(declarator->type, symval->disp,
                                           initializer, before_definition);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      free(temp);
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, declaration);
    }

    /* wait to do this so that deduced array sizes are set */
    status = translate_static_prelude(declarator, symval, temp);
    if (status) abort();
    free(temp);

    if (declaration->first_instr == NULL) {
      assert(declaration->last_instr == NULL);
      InstructionData *nop_data = instr_init(OP_NOP);
      int status = llist_push_back(instructions, nop_data);
      if (status) abort();
      declaration->first_instr = llist_iter_last(instructions);
      if (declaration->first_instr == NULL) abort();
      declaration->last_instr = llist_iter_last(instructions);
      if (declaration->last_instr == NULL) abort();
    }
  } else if (initializer->symbol != TOK_INIT_LIST &&
             initializer->symbol != TOK_STRINGCON &&
             (initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    /* TODO(Robert): consider inserting this instruction later in the sequence,
     * perhaps after the initializer's code, so that it occurs closer to where
     * the actual assignment happens
     */
    assign_stack_space(symval);
    maybe_load_cexpr(initializer, NULL);

    InstructionData *lea_data = instr_init(OP_LEA);
    set_op_ind(&lea_data->src, symval->disp, RBP_VREG);
    set_op_reg(&lea_data->dest, REG_QWORD, next_vreg());
    lea_data->persist_flags |= PERSIST_DEST_CLEAR;

    int status = liter_push_front(initializer->first_instr,
                                  &declarator->first_instr, 1, lea_data);
    if (status) abort();
    declarator->last_instr = liter_copy(declarator->first_instr);
    if (declarator->last_instr == NULL) abort();
    (void)translate_assignment(assignment, declarator, initializer);
    if (declaration->first_instr == NULL)
      declaration->first_instr = liter_copy(declarator->first_instr);
    if (declaration->first_instr == NULL) abort();
    free(declaration->last_instr);
    declaration->last_instr = liter_copy(assignment->last_instr);
    if (declaration->last_instr == NULL) abort();
  } else {
    assign_stack_space(symval);
    free(declaration->last_instr);
    declaration->last_instr = llist_iter_last(instructions);
    if (declaration->last_instr == NULL) abort();
    ASTree *errnode = traverse_initializer(
        declarator->type, symval->disp, initializer, declaration->last_instr);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, declaration);
    }

    if (declaration->first_instr == NULL) {
      declaration->first_instr = liter_copy(initializer->first_instr);
      if (declaration->first_instr == NULL) abort();
    }
  }
  return declaration;
}

static ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating local declaration");
  if (declaration->first_instr == NULL) {
    InstructionData *nop_data = instr_init(OP_NOP);
    int status = llist_push_back(instructions, nop_data);
    if (status) abort();
    declaration->first_instr = llist_iter_last(instructions);
    if (declaration->first_instr == NULL) abort();
    declaration->last_instr = llist_iter_last(instructions);
    if (declaration->last_instr == NULL) abort();
  }

  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symval);
  assert(symval && in_current_scope);

  if (declarator->symbol == TOK_TYPE_NAME ||
      (symval->flags & SYMFLAG_INHERIT) || (symval->flags & SYMFLAG_TYPEDEF)) {
    return declaration;
  } else if (symval->flags & SYMFLAG_STORE_STAT) {
    assign_static_space(declarator->lexinfo, symval);
    InstructionData *bss_data = instr_init(OP_BSS);
    int status =
        liter_push_back(before_definition, &before_definition, 1, bss_data);
    if (status) abort();
    status = translate_static_prelude(declarator, symval, before_definition);
    if (status) abort();
  } else if (symval->flags & SYMFLAG_STORE_AUTO) {
    assign_stack_space(symval);
  } else if (symval->flags & SYMFLAG_STORE_EXT) {
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, declarator->lexinfo);
    int status =
        liter_push_back(before_definition, &before_definition, 1, globl_data);
    if (status) abort();
  }

  return declaration;
}

ASTree *translate_local_declarations(ASTree *block, ASTree *declarations) {
  /* skip typespec list */
  size_t i, decl_count = astree_count(declarations);
  for (i = 1; i < decl_count; ++i) {
    ASTree *declaration = astree_get(declarations, i);
    if (declaration->symbol == TOK_IDENT) {
      (void)translate_local_decl(declarations, declaration);
    } else if (declaration->symbol == '=') {
      ASTree *declarator = astree_get(declaration, 0),
             *initializer = astree_get(declaration, 1),
             *errnode = translate_local_init(declarations, declaration,
                                             declarator, initializer);
      if (errnode->symbol == TOK_TYPE_ERROR) {
        (void)astree_remove(errnode, 0);
        return astree_adopt(errnode, 1, astree_adopt(block, 1, declarations));
      }
    } else {
      abort();
    }
  }
  return astree_adopt(block, 1, declarations);
}

static ASTree *translate_global_init(ASTree *declaration, ASTree *declarator,
                                     ASTree *initializer) {
  PFDBG0('g', "Translating global initialization");
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symval);
  assert(in_current_scope && symval);

  InstructionData *data_data = instr_init(OP_DATA);
  int status = llist_push_back(instructions, data_data);
  if (status) abort();
  ListIter *temp = llist_iter_last(instructions);
  ListIter *temp2 = llist_iter_last(instructions);
  ASTree *errnode =
      traverse_initializer(declarator->type, NO_DISP, initializer, temp);
  free(temp);
  /* wait to do this so that deduced array sizes are set */
  status = translate_static_prelude(declarator, symval, temp2);
  free(temp2);
  if (status) abort();
  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  if (errnode->symbol == TOK_TYPE_ERROR) {
    (void)astree_remove(errnode, 0);
    return astree_adopt(errnode, 1, declaration);
  } else {
    return declaration;
  }
}

static ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating global declaration");
  assert(declarator->symbol == TOK_IDENT);
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symval);
  assert(in_current_scope && symval);

  if (symval->flags & SYMFLAG_STORE_EXT) {
    assert(symval->flags & SYMFLAG_LINK_EXT);
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, declarator->lexinfo);
    int status = llist_push_back(instructions, globl_data);
    if (status) abort();
  } else if (symval->flags & SYMFLAG_STORE_STAT) {
    InstructionData *bss_data = instr_init(OP_BSS);
    int status = llist_push_back(instructions, bss_data);
    if (status) abort();
    ListIter *temp = llist_iter_last(instructions);
    status = translate_static_prelude(declarator, symval, temp);
    free(temp);
    if (status) abort();
    InstructionData *zero_data = instr_init(OP_ZERO);
    set_op_imm(&zero_data->dest, type_get_width(declarator->type),
               IMM_UNSIGNED);
    status = llist_push_back(instructions, zero_data);
    if (status) abort();
  } else if (!(symval->flags & SYMFLAG_TYPEDEF)) {
    abort();
  }

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return declaration;
}

ASTree *translate_global_declarations(ASTree *root, ASTree *declarations) {
  if (astree_count(declarations) == 3 &&
      astree_get(declarations, 2)->symbol == TOK_BLOCK)
    /* function defnition; no further instructions to emit */
    return astree_adopt(root, 1, declarations);
  else if (astree_count(declarations) == 2 &&
           astree_get(declarations, 1)->symbol == TOK_TYPE_NAME)
    /* declares nothing; emit no instructions */
    return astree_adopt(root, 1, declarations);

  /* skip typespec list */
  size_t i, decl_count = astree_count(declarations);
  for (i = 1; i < decl_count; ++i) {
    ASTree *declaration = astree_get(declarations, i);
    if (declaration->symbol == TOK_IDENT) {
      (void)translate_global_decl(declarations, declaration);
    } else if (declaration->symbol == '=') {
      ASTree *declarator = astree_get(declaration, 0),
             *initializer = astree_get(declaration, 1),
             *errnode =
                 translate_global_init(declarations, declarator, initializer);
      if (errnode->symbol == TOK_TYPE_ERROR) {
        (void)astree_remove(errnode, 0);
        return astree_adopt(errnode, 1, astree_adopt(root, 1, declarations));
      }
    } else {
      abort();
    }
  }
  return astree_adopt(root, 1, declarations);
}

ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body) {
  PFDBG0('g', "Translating function prologue");
  /* reserve 8 bytes for shuffling around register return values, and another
   * 24 for the allocator to unspill registers to */
  window_size = 32;
  InstructionData *text_data = instr_init(OP_TEXT);
  int status = llist_push_back(instructions, text_data);
  if (status) abort();
  declaration->first_instr = llist_iter_last(instructions);
  if (declaration->first_instr == NULL) abort();
  SymbolValue *symval = NULL;
  state_get_symbol(state, declarator->lexinfo, strlen(declarator->lexinfo),
                   &symval);
  assert(symval != NULL);
  if (symval->flags & SYMFLAG_LINK_EXT) {
    InstructionData *globl_data = instr_init(OP_GLOBL);
    set_op_dir(&globl_data->dest, declarator->lexinfo);
    status = llist_push_back(instructions, globl_data);
    if (status) abort();
  }
  InstructionData *type_data = instr_init(OP_TYPE);
  set_op_dir(&type_data->dest, declarator->lexinfo);
  set_op_dir(&type_data->src, "@function");
  status = llist_push_back(instructions, type_data);
  if (status) abort();
  InstructionData *label_data = instr_init(OP_NONE);
  label_data->label = declarator->lexinfo;
  status = llist_push_back(instructions, label_data);

  status = save_preserved_regs();
  if (status) abort();

  /* save location for later rsp adjustment */
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  status = translate_params(declarator);
  if (status) abort();
  return astree_adopt(declaration, 2, declarator, body);
}

ASTree *end_translate_fn(ASTree *declaration) {
  /* emit rsp adjustment; set to bogus value initially since we don't know how
   * many bytes the register allocator will spill yet */
  InstructionData *rsp_sub_data = instr_init(OP_SUB);
  set_op_reg(&rsp_sub_data->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_sub_data->src, PTRDIFF_MAX, IMM_UNSIGNED);
  int status = liter_push_back(declaration->last_instr, NULL, 1, rsp_sub_data);
  if (status) abort();
  free(declaration->last_instr);

  status = restore_preserved_regs();
  if (status) abort();
  InstructionData *return_data = instr_init(OP_RET);
  status = llist_push_back(instructions, return_data);
  if (status) abort();
  ASTree *declarator = astree_get(declaration, 1);
  InstructionData *size_data = instr_init(OP_SIZE);
  set_op_dir(&size_data->dest, declarator->lexinfo);
  set_op_dir(&size_data->src, mk_fn_size(declarator->lexinfo));
  status = llist_push_back(instructions, size_data);
  if (status) abort();
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();

  if (skip_liveness) goto no_live;
  status = liveness_sr(declaration->first_instr, declaration->last_instr);
  if (status) abort();
  if (skip_allocator) goto no_alloc;
  /* this function will adjust window_size to account for spilled bytes */
  status = allocate_regs(declaration->first_instr, declaration->last_instr);
  if (status) abort();
no_live:;
no_alloc:;

  /* align to ensure stack alignment to 16x + 8 */
  size_t window_padding = (window_size % 16 > 0) ? 16 - (window_size % 16) : 0;
  assert(window_padding <= PTRDIFF_MAX);
  window_size += window_padding;

  /* set rsp adjustment to its actual value */
  rsp_sub_data->src.imm.val = window_size;
  return declaration;
}

int operand_to_str(Operand *operand, char *str) {
  switch (operand->all.mode) {
    case MODE_NONE:
      str[0] = 0;
      return 0;
    case MODE_REGISTER:
      if (operand->reg.num < REAL_REG_COUNT)
        return sprintf(str, "%%%s",
                       SELECT_REG(operand->reg.num, operand->reg.width));
      else
        return sprintf(str, "%%VR%lu%c", operand->reg.num,
                       WIDTH_TO_CHAR[operand->reg.width]);
    case MODE_SCALE:
      if (operand->reg.num < REAL_REG_COUNT)
        return sprintf(str, "%li(%%%s, %%%s, %u)", operand->sca.disp,
                       SELECT_REG(operand->sca.base, REG_QWORD),
                       SELECT_REG(operand->sca.index, REG_QWORD),
                       operand->sca.scale);
      else
        return sprintf(str, "%li(%%VR%luQ, %%VR%luQ, %u)", operand->sca.disp,
                       operand->sca.base, operand->sca.index,
                       operand->sca.scale);
    case MODE_IMMEDIATE:
      if (operand->imm.is_signed)
        return sprintf(str, "$%li", operand->imm.val);
      else
        return sprintf(str, "$%lu", operand->imm.val);
    case MODE_DIRECT:
      return sprintf(str, "%s", operand->dir.lab);
    case MODE_INDIRECT:
      if (operand->ind.num < REAL_REG_COUNT)
        return sprintf(str, "%li(%%%s)", operand->ind.disp,
                       SELECT_REG(operand->ind.num, REG_QWORD));
      else
        return sprintf(str, "%li(%%VR%luQ)", operand->ind.disp,
                       operand->ind.num);
    case MODE_PIC:
      if (operand->pic.disp != 0)
        return sprintf(str, "%s%+li(%%RIP)", operand->pic.lab,
                       operand->pic.disp);
      else
        return sprintf(str, "%s(%%RIP)", operand->pic.lab);
    default:
      abort();
  }
}

int opcode_to_str(InstructionData *data, char *str) {
  switch (optype_from_opcode(data->opcode)) {
    case OPTYPE_CONTEXTUAL:
      /* fallthrough */
    case OPTYPE_BINARY:
      if (data->opcode == OP_MOVS || data->opcode == OP_MOVZ) {
        assert(data->src.all.mode == MODE_REGISTER &&
               data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->src.reg.width],
                       WIDTH_TO_CHAR[data->dest.reg.width]);
      } else if (data->src.all.mode == MODE_REGISTER) {
        assert(data->dest.all.mode != MODE_REGISTER ||
               data->src.reg.width == data->dest.reg.width);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->src.reg.width]);
      } else {
        assert(data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->dest.reg.width]);
      }
    case OPTYPE_UNARY:
      if (opcode_needs_width(data->opcode)) {
        assert(data->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[data->opcode],
                       WIDTH_TO_CHAR[data->dest.reg.width]);
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

int bin_to_str(InstructionData *data, char *str) {
  static char opcode_str[MAX_OPCODE_LENGTH], dest_str[MAX_OPERAND_LENGTH],
      src_str[MAX_OPERAND_LENGTH];
  int chars_written = opcode_to_str(data, opcode_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&data->dest, dest_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&data->src, src_str);
  if (chars_written < 0) return chars_written;

  return sprintf(str, "%s %s, %s", opcode_str, src_str, dest_str);
}

int un_to_str(InstructionData *data, char *str) {
  static char opcode_str[MAX_OPCODE_LENGTH], dest_str[MAX_OPERAND_LENGTH];
  int chars_written = opcode_to_str(data, opcode_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&data->dest, dest_str);
  if (chars_written < 0) return chars_written;

  if (data->opcode == OP_CALL && data->dest.all.mode == MODE_REGISTER)
    return sprintf(str, "%s *%s", opcode_str, dest_str);
  else
    return sprintf(str, "%s %s", opcode_str, dest_str);
}

int dir_to_str(InstructionData *data, char *str) {
  switch (data->opcode) {
    case OP_FILE:
      assert(data->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s \"%s\"", OPCODES[OP_FILE], data->dest.dir.lab);
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
             data->dest.all.mode == MODE_PIC);
      if (data->dest.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %li", OPCODES[data->opcode],
                       data->dest.imm.val);
      else
        return sprintf(str, ".%s %s%+li", OPCODES[data->opcode],
                       data->dest.pic.lab, data->dest.pic.disp);
    case OP_SIZE:
      assert(data->dest.all.mode == MODE_DIRECT);
      assert(data->src.all.mode == MODE_DIRECT ||
             data->src.all.mode == MODE_IMMEDIATE);
      if (data->src.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %s, %lu", OPCODES[OP_SIZE], data->dest.dir.lab,
                       data->src.imm.val);
      else
        return sprintf(str, ".%s %s, %s", OPCODES[OP_SIZE], data->dest.dir.lab,
                       data->src.dir.lab);
    case OP_TYPE:
      assert(data->dest.all.mode == MODE_DIRECT);
      assert(data->src.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s, %s", OPCODES[OP_TYPE], data->dest.dir.lab,
                     data->src.dir.lab);
    case OP_ASCIZ:
    case OP_ASCII:
      assert(data->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s", OPCODES[data->opcode], data->dest.dir.lab);
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

int operand_debug(Operand *operand, char *str) {
  switch (operand->all.mode) {
    case MODE_REGISTER:
      return sprintf(str,
                     " (REGISTER):\n"
                     "\t\tRegister number: %lu\n"
                     "\t\tRegister width: %u\n"
                     "\t\tNext use: %p\n",
                     operand->reg.num, operand->reg.width,
                     (void *)operand->reg.next_use);
    case MODE_IMMEDIATE:
      return sprintf(str,
                     " (IMMEDIATE):\n"
                     "\t\tImmediate value: (unsigned) %lu, "
                     "(signed) %li, (pointer) %p\n",
                     operand->imm.val, (intmax_t)operand->imm.val,
                     (void *)operand->imm.val);
    case MODE_DIRECT:
      return sprintf(str, " (DIRECT):\n\t\tLabel: %p \"%s\"\n",
                     (void *)operand->dir.lab,
                     operand->dir.lab == NULL ? "" : operand->dir.lab);
    case MODE_PIC:
      return sprintf(str,
                     " (PIC):\n"
                     "\t\tSymbol: %p \"%s\"\n"
                     "\t\tDisplacement: %li\n",
                     operand->pic.lab,
                     operand->pic.lab == NULL ? "" : operand->pic.lab,
                     operand->pic.disp);
    case MODE_INDIRECT:
      return sprintf(str,
                     " (INDIRECT):\n"
                     "\t\tRegister number: %lu\n"
                     "\t\tDisplacement: %li\n"
                     "\t\tNext use: %p\n",
                     operand->ind.num, operand->ind.disp,
                     (void *)operand->ind.next_use);
    case MODE_SCALE:
      return sprintf(str,
                     " (SCALED):\n"
                     "\t\tBase register number: %lu\n"
                     "\t\tIndex register number: %lu\n"
                     "\t\tDisplacement: %li\n"
                     "\t\tScale: %u\n"
                     "\t\tBase next use: %p\n"
                     "\t\tIndex next use: %p\n",
                     operand->sca.base, operand->sca.index, operand->sca.disp,
                     operand->sca.scale, (void *)operand->sca.base_next_use,
                     (void *)operand->sca.index_next_use);
    case MODE_NONE:
      return sprintf(str, " (NONE):\n");
    default:
      abort();
  }
}

int instr_debug(InstructionData *data, char *str) {
  static char src_buf[MAX_OPERAND_DEBUG_LENGTH],
      dest_buf[MAX_OPERAND_DEBUG_LENGTH];
  int status = operand_debug(&data->src, src_buf);
  if (status < 0) return status;
  status = operand_debug(&data->dest, dest_buf);
  if (status < 0) return status;
  return sprintf(str,
                 "Instruction %p {\n"
                 "\tLabel: %p \"%s\"\n"
                 "\tOpcode: %i %s\n"
                 "\tSource Operand%s"
                 "\tDestination Operand%s}",
                 (void *)data, (void *)data->label, data->label, data->opcode,
                 OPCODES[data->opcode], src_buf, dest_buf);
}

int instr_to_str(InstructionData *data, char *str) {
  int ret = 0;
  if (data->label != NULL) {
    int pad_count;
    if ((data->opcode == OP_INVALID || data->opcode == OP_NONE) &&
        !data->comment)
      pad_count = 0;
    else if (strlen(data->label) > 6)
      pad_count = 1;
    else
      pad_count = 7 - (int)strlen(data->label);
    int chars_written =
        sprintf(str + ret, "%s:%*s", data->label, pad_count, "");
    if (chars_written < 0) return chars_written;
    ret += chars_written;
  } else {
    str[ret++] = '\t';
    str[ret] = '\0';
  }
  switch (optype_from_opcode(data->opcode)) {
    int chars_written;
    case OPTYPE_DIRECTIVE:
      chars_written = dir_to_str(data, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_CONTEXTUAL:
      chars_written =
          (data->src.all.mode == MODE_NONE ? un_to_str : bin_to_str)(data,
                                                                     str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_BINARY:
      chars_written = bin_to_str(data, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_UNARY:
      chars_written = un_to_str(data, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_NULLARY:
      if (data->opcode == OP_NONE) break;
      chars_written = opcode_to_str(data, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_INVALID:
      /* fallthrough */
    default:
      abort();
  }
  if (data->comment != NULL) {
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
  static char buffer[MAX_INSTR_LENGTH];
  size_t i;
  for (i = 0; i < llist_size(instructions); ++i) {
    InstructionData *data = llist_get(instructions, i);
    int chars_written = instr_to_str(data, buffer);
    if (chars_written < 0) return chars_written;
    chars_written = fprintf(out, "%s\n", buffer);
    if (chars_written < 0) return chars_written;
  }
  return 0;
}

int generator_debug_il(FILE *out) {
  static char buffer[MAX_INSTR_DEBUG_LENGTH];
  size_t i;
  for (i = 0; i < llist_size(instructions); ++i) {
    InstructionData *data = llist_get(instructions, i);
    int chars_written = instr_debug(data, buffer);
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
  set_op_dir(&file_data->dest, filename);
  assert(!llist_push_back(instructions, file_data));
  assert(before_definition = llist_iter_last(instructions));
  literals = malloc(sizeof(*literals) * literals_cap);
  static_locals = malloc(sizeof(*static_locals));
  assert(
      !map_init(static_locals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper));
  generated_text = malloc(sizeof(*generated_text));
  assert(
      !map_init(generated_text, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper));
}

void asmgen_free_globals(void) {
  free(before_definition);
  assert(!llist_destroy(instructions));
  free(literals);
  assert(!map_destroy(static_locals));
  free(static_locals);
  assert(!map_destroy(generated_text));
  free(generated_text);
}
