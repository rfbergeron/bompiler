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

/* TODO(Robert): make sure that function calls emit correct code when calling
 * a function that has not been prototyped
 */
/* TODO(Robert): function prototypes currently do not have the correct
 * directives emitted; they are being treated as objects
 */

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

static const char LOCAL_FMT[] = ".L%lu$%s";
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
static const size_t PROLOGUE_EIGHTBYTES = 8;
static const ptrdiff_t FP_OFFSET = 304;
static const ptrdiff_t GP_OFFSET_MAX = 48;
static const ptrdiff_t GP_OFFSET_MEMBER_DISP = NO_DISP;
static const ptrdiff_t FP_OFFSET_MEMBER_DISP = X64_SIZEOF_INT;
static const ptrdiff_t OVERFLOW_ARG_AREA_MEMBER_DISP = 2 * X64_SIZEOF_INT;
static const ptrdiff_t REG_SAVE_AREA_MEMBER_DISP =
    2 * X64_SIZEOF_INT + X64_SIZEOF_LONG;
/* reserve offset -8 for function call return values, offset -16 for hidden
 * parameter storage, and offsets -40, -32 and -24 for the first, second and
 * third register unspill regions, repsectively
 */
static const ptrdiff_t RETURN_VAL_DISP = -8;
static const ptrdiff_t HIDDEN_PARAM_DISP = -16;
static const ptrdiff_t INIT_WINDOW_SIZE = 40;
/* locations of 8-byte regions for contents of unspilled registers */
const ptrdiff_t UNSPILL_REGIONS[] = {-40, -32, -24};
const size_t UNSPILL_REGIONS_SIZE = ARRAY_ELEM_COUNT(UNSPILL_REGIONS);
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

static size_t fn_count;

const Type *TYPE_VA_SPILL_REGION;

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
  char temp[64];
  sprintf(temp, "%lu", fn_count);
  size_t label_len = strlen(name) + strlen(temp) + sizeof(LOCAL_FMT) - 5;
  return deduplicate_text(label_len, LOCAL_FMT, fn_count, name);
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

static void bulk_rtom(size_t dest_memreg, ptrdiff_t dest_disp,
                      const size_t *src_regs, const Type *type,
                      ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = type_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = dest_disp + i * 8 + j;
        Instruction *mov_instr = instr_init(OP_MOV);
        set_op_reg(&mov_instr->src, alignment, src_regs[i]);
        set_op_ind(&mov_instr->dest, chunk_disp, dest_memreg);
        if (i == 0 && dest_memreg >= REAL_REG_COUNT)
          mov_instr->persist_flags |= PERSIST_DEST_SET;
        Instruction *shr_instr = instr_init(OP_SHR);
        set_op_reg(&shr_instr->dest, REG_QWORD, src_regs[i]);
        set_op_imm(&shr_instr->src, alignment, IMM_UNSIGNED);
        int status = liter_push_back(where, NULL, 2, mov_instr, shr_instr);
        if (status) abort();
      }
    }
  } else {
    size_t mov_count = width / alignment;
    size_t i;
    for (i = 0; i < mov_count; ++i) {
      Instruction *mov_instr = instr_init(OP_MOV);
      set_op_reg(&mov_instr->src, alignment, src_regs[i]);
      set_op_ind(&mov_instr->dest, dest_disp + i * alignment, dest_memreg);
      if (i == 0 && dest_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_instr);
      if (status) abort();
    }
  }
}

static void bulk_mtor(const size_t *dest_regs, size_t src_memreg,
                      ptrdiff_t src_disp, const Type *type, ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = type_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = src_disp + i * 8 + j;
        Instruction *mov_instr = instr_init(OP_MOV);
        set_op_ind(&mov_instr->src, chunk_disp, src_memreg);
        set_op_reg(&mov_instr->dest, alignment, next_vreg());
        /* persist vregs across basic blocks */
        if (i == 0 && src_memreg >= REAL_REG_COUNT)
          mov_instr->persist_flags |= PERSIST_SRC_SET;
        Instruction *movz_instr = instr_init(OP_MOVZ);
        movz_instr->src = mov_instr->dest;
        set_op_reg(&movz_instr->dest, REG_QWORD, next_vreg());
        Instruction *shl_instr = instr_init(OP_SHL);
        shl_instr->dest = movz_instr->dest;
        set_op_imm(&shl_instr->src, j, IMM_UNSIGNED);
        Instruction *bitor_instr = instr_init(OP_OR);
        bitor_instr->src = movz_instr->dest;
        set_op_reg(&bitor_instr->dest, REG_QWORD, dest_regs[i]);
        int status = liter_push_back(where, NULL, 4, mov_instr, movz_instr,
                                     shl_instr, bitor_instr);
        if (status) abort();
      }
    }
  } else {
    size_t mov_count = width / alignment;
    size_t i;
    for (i = 0; i < mov_count; ++i) {
      Instruction *mov_instr = instr_init(OP_MOV);
      set_op_reg(&mov_instr->dest, alignment, dest_regs[i]);
      set_op_ind(&mov_instr->src, src_disp + i * alignment, src_memreg);
      /* persist vregs across basic blocks */
      if (i == 0 && src_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_SRC_SET;
      int status = liter_push_back(where, NULL, 1, mov_instr);
      if (status) abort();
    }
  }
}

static void bulk_mtom(size_t dest_reg, size_t src_reg, const Type *type,
                      ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  size_t mov_count = width / alignment;
  size_t i;
  for (i = 0; i < mov_count; ++i) {
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->dest, alignment, next_vreg());
    set_op_ind(&mov_instr->src, i * alignment, src_reg);
    Instruction *mov_instr_2 = instr_init(OP_MOV);
    mov_instr_2->src = mov_instr->dest;
    set_op_ind(&mov_instr_2->dest, i * alignment, dest_reg);
    if (i == 0) {
      if (src_reg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_SRC_SET;
      if (dest_reg >= REAL_REG_COUNT)
        mov_instr_2->persist_flags |= PERSIST_DEST_SET;
    }
    int status = liter_push_back(where, NULL, 2, mov_instr, mov_instr_2);
    if (status) abort();
  }
}

void bulk_mzero(size_t dest_memreg, ptrdiff_t dest_disp, size_t skip_bytes,
                const Type *type, ListIter *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  size_t i = skip_bytes;
  Instruction *zero_instr = instr_init(OP_MOV);
  set_op_imm(&zero_instr->src, 0, IMM_UNSIGNED);
  set_op_reg(&zero_instr->dest, alignment, next_vreg());

  while (i < width) {
    ptrdiff_t chunk_disp = i + dest_disp;
    if ((i + dest_disp) % alignment != 0) {
      Instruction *mov_instr = instr_init(OP_MOV);
      set_op_reg(&mov_instr->src, REG_BYTE, zero_instr->dest.reg.num);
      set_op_ind(&mov_instr->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_instr);
      if (status) abort();
      ++i;
    } else {
      Instruction *mov_instr = instr_init(OP_MOV);
      mov_instr->src = zero_instr->dest;
      set_op_ind(&mov_instr->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_DEST_SET;
      int status = liter_push_back(where, NULL, 1, mov_instr);
      if (status) abort();
      i += alignment;
    }
  }

  /* push afterwards since `where` does not move */
  int status = liter_push_back(where, NULL, 1, zero_instr);
  if (status) abort();
}

void static_zero_pad(size_t count, ListIter *where) {
  Instruction *zero_instr = instr_init(OP_ZERO);
  set_op_imm(&zero_instr->dest, count, IMM_UNSIGNED);
  /* although `where` was passed by value, `liter_push_back` should mutate it
   * in-place since the output parameter points to the input parameter
   */
  int status = liter_push_back(where, &where, 1, zero_instr);
  if (status) abort();
}

static Opcode opcode_from_operator(int tok_kind, const Type *type) {
  switch (tok_kind) {
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

size_t asmgen_literal_label(const char *literal, const char **out) {
  /* TODO(Robert): bad time complexity */
  size_t i;
  for (i = 0; i < literals_size; ++i)
    if (strcmp(literals[i].literal, literal) == 0)
      return *out = literals[i].label, i;

  if (literals_size >= literals_cap)
    literals = realloc(literals, sizeof(*literals) * (literals_cap *= 2));

  Instruction *section_instr = instr_init(OP_SECTION);
  set_op_dir(&section_instr->dest, ".rodata");
  /* TODO(Robert): determine when alignment needs to be set, if ever */
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = mk_literal_label(literals_size);
  Instruction *string_instr = instr_init(OP_ASCIZ);
  set_op_dir(&string_instr->dest, literal);
  int status = liter_push_back(before_definition, &before_definition, 3,
                               section_instr, label_instr, string_instr);
  if (status) abort();

  literals[literals_size].literal = literal;
  literals[literals_size].label = label_instr->label;
  return *out = label_instr->label, literals_size++;
}

static ptrdiff_t assign_stack_space(const Type *type) {
  size_t width = type_get_width(type);
  size_t alignment = type_get_alignment(type);
  size_t padding = alignment - (window_size % alignment);
  ptrdiff_t padded_space = width + ((padding == alignment) ? 0 : padding);
  assert(padded_space >= 0);
  assert(window_size >= 0 && PTRDIFF_MAX - window_size >= padded_space);
  window_size += padded_space;
  return -window_size;
}

static void assign_static_space(const char *ident, Symbol *symbol) {
  size_t *static_count = map_get(static_locals, (void *)ident, strlen(ident));
  if (!static_count) {
    static_count = calloc(1, sizeof(size_t));
    int status =
        map_insert(static_locals, (void *)ident, strlen(ident), static_count);
    if (status) abort();
  }
  symbol->static_id = *static_count++;
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
static void scalar_conversions(ASTree *expr, const Type *to) {
  assert(!type_is_record(expr->type) && !type_is_function(expr->type));
  assert(!type_is_aggregate(to) && !type_is_function(to));
  assert(!type_is_void(expr->type) || type_is_void(to));

  if (type_is_void(to)) {
    Instruction *nop_instr = instr_init(OP_NOP);
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, nop_instr);
    if (status) abort();
    return;
  }

  int src_persistence_set = 0;
  const Type *from = expr->type;
  size_t from_width =
      type_is_array(from) ? X64_SIZEOF_LONG : type_get_width(from);
  Instruction *expr_instr = liter_get(expr->last_instr);
  /* TODO(Robert): is it possible for this to happen? and if it is, is it
   * correct?
   */
  if (expr_instr->dest.all.mode != MODE_REGISTER) {
    assert(expr_instr->dest.all.mode == MODE_INDIRECT ||
           expr_instr->dest.all.mode == MODE_SCALE);
    Instruction *mov_instr = instr_init(OP_MOV);
    mov_instr->src = expr_instr->dest;
    set_op_reg(&mov_instr->dest, from_width, next_vreg());
    mov_instr->persist_flags |= PERSIST_SRC_SET;
    src_persistence_set = 1;
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_instr);
    if (status) abort();
    expr_instr = mov_instr;
  }

  if ((expr->attributes & ATTR_EXPR_LVAL) && !type_is_array(from)) {
    assert(!type_is_function(from));
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_ind(&mov_instr->src, NO_DISP, expr_instr->dest.reg.num);
    set_op_reg(&mov_instr->dest, from_width, next_vreg());
    mov_instr->persist_flags |= PERSIST_DEST_CLEAR;
    if (!src_persistence_set)
      mov_instr->persist_flags |= PERSIST_SRC_SET, src_persistence_set = 1;
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_instr);
    if (status) abort();
    expr_instr = mov_instr;
  }

  size_t to_width = type_get_width(to);
  if (from_width == to_width) {
    return;
  } else if (from_width > to_width) {
    /* unnecessary mov so that the width of the destination is set correctly,
     * and the whole structure describing the operand can just be copied to the
     * next instruction that needs it.
     */
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, to_width, expr_instr->dest.reg.num);
    set_op_reg(&mov_instr->dest, to_width, next_vreg());
    mov_instr->persist_flags |= src_persistence_set
                                    ? PERSIST_DEST_CLEAR
                                    : PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, mov_instr);
    if (status) abort();
  } else if (type_is_signed(from) || type_is_enum(from)) {
    Instruction *movs_instr = instr_init(OP_MOVS);
    movs_instr->src = expr_instr->dest;
    set_op_reg(&movs_instr->dest, to_width, next_vreg());
    movs_instr->persist_flags |= src_persistence_set
                                     ? PERSIST_DEST_CLEAR
                                     : PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, movs_instr);
    if (status) abort();
  } else if (type_is_unsigned(from)) {
    Instruction *movz_instr = instr_init(OP_MOVZ);
    movz_instr->src = expr_instr->dest;
    set_op_reg(&movz_instr->dest, to_width, next_vreg());
    movz_instr->persist_flags |= src_persistence_set
                                     ? PERSIST_DEST_CLEAR
                                     : PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    int status =
        liter_push_back(expr->last_instr, &expr->last_instr, 1, movz_instr);
    if (status) abort();
  } else {
    abort();
  }
}

static void save_preserved_regs(void) {
  size_t i;
  for (i = 1; i <= PRESERVED_REG_COUNT; ++i) {
    Instruction *push_instr = instr_init(OP_PUSH);
    set_op_reg(&push_instr->dest, REG_QWORD,
               PRESERVED_REGS[PRESERVED_REG_COUNT - i]);
    int status = llist_push_back(instructions, push_instr);
    if (status) abort();
  }
  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_reg(&mov_instr->dest, REG_QWORD, RBP_VREG);
  set_op_reg(&mov_instr->src, REG_QWORD, RSP_VREG);
  int status = llist_push_back(instructions, mov_instr);
  if (status) abort();
}

static void save_volatile_regs(ListIter *where) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
    Instruction *push_instr = instr_init(OP_PUSH);
    set_op_reg(&push_instr->dest, REG_QWORD, VOLATILE_REGS[i]);
    int status = liter_push_front(where, &where, 1, push_instr);
    if (status) abort();
  }
}

static void restore_preserved_regs(void) {
  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_reg(&mov_instr->dest, REG_QWORD, RSP_VREG);
  set_op_reg(&mov_instr->src, REG_QWORD, RBP_VREG);
  int status = llist_push_back(instructions, mov_instr);
  if (status) abort();
  size_t i;
  for (i = 0; i < PRESERVED_REG_COUNT; ++i) {
    Instruction *pop_instr = instr_init(OP_POP);
    set_op_reg(&pop_instr->dest, REG_QWORD, PRESERVED_REGS[i]);
    int status = llist_push_back(instructions, pop_instr);
    if (status) abort();
  }
}

static void restore_volatile_regs(void) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
    Instruction *pop_instr = instr_init(OP_POP);
    set_op_reg(&pop_instr->dest, REG_QWORD, VOLATILE_REGS[i]);
    int status = llist_push_back(instructions, pop_instr);
    if (status) abort();
  }
}

ASTree *translate_empty_expr(ASTree *empty_expr) {
  Instruction *nop_instr = instr_init(OP_NOP);
  llist_push_back(instructions, nop_instr);
  empty_expr->first_instr = llist_iter_last(instructions);
  if (empty_expr->first_instr == NULL) abort();
  empty_expr->last_instr = llist_iter_last(instructions);
  if (empty_expr->last_instr == NULL) abort();
  return empty_expr;
}

void maybe_load_cexpr(ASTree *expr, ListIter *where) {
  if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE) {
    assert(expr->first_instr == NULL && expr->last_instr == NULL);
    Instruction *load_instr;
    if (type_is_void(expr->type)) {
      /* emit NOP when casting a constant to void */
      load_instr = instr_init(OP_NOP);
    } else if (expr->constant.label != NULL) {
      /* use LEA when expression has an address component */
      load_instr = instr_init(OP_LEA);
      set_op_pic(&load_instr->src, expr->constant.integral.signed_value,
                 expr->constant.label);
      set_op_reg(&load_instr->dest, REG_QWORD, next_vreg());
      load_instr->persist_flags |= PERSIST_DEST_CLEAR;
    } else {
      load_instr = instr_init(OP_MOV);
      if (type_is_unsigned(expr->type)) {
        set_op_imm(&load_instr->src, expr->constant.integral.unsigned_value, 1);
      } else {
        set_op_imm(&load_instr->src, expr->constant.integral.signed_value, 0);
      }
      set_op_reg(&load_instr->dest, type_get_width(expr->type), next_vreg());
      load_instr->persist_flags |= PERSIST_DEST_CLEAR;
    }
    if (where) {
      int status = liter_push_front(where, &expr->first_instr, 1, load_instr);
      if (status) abort();
      expr->last_instr = liter_copy(expr->first_instr);
      if (expr->last_instr == NULL) abort();
    } else {
      int status = llist_push_back(instructions, load_instr);
      if (status) abort();
      expr->first_instr = llist_iter_last(instructions);
      if (expr->first_instr == NULL) abort();
      expr->last_instr = llist_iter_last(instructions);
      if (expr->last_instr == NULL) abort();
    }
  }
}

ASTree *translate_ident(ASTree *ident) {
  Instruction *lea_instr = instr_init(OP_LEA);
  Symbol *symbol = NULL;
  state_get_symbol(state, ident->lexinfo, strlen(ident->lexinfo), &symbol);
  assert(symbol != NULL);

  if (symbol->storage == STORE_STAT) {
    if (symbol->linkage == LINK_NONE) {
      set_op_pic(&lea_instr->src, NO_DISP,
                 mk_static_label(ident->lexinfo, symbol->static_id));
    } else {
      set_op_pic(&lea_instr->src, NO_DISP, ident->lexinfo);
    }
  } else {
    set_op_ind(&lea_instr->src, symbol->disp, RBP_VREG);
  }
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  lea_instr->persist_flags |= PERSIST_DEST_CLEAR;
  int status = llist_push_back(instructions, lea_instr);
  if (status) abort();
  ident->first_instr = llist_iter_last(instructions);
  if (ident->first_instr == NULL) abort();
  ident->last_instr = liter_copy(ident->first_instr);
  if (ident->last_instr == NULL) abort();
  return ident;
}

ASTree *translate_cast(ASTree *cast, ASTree *expr) {
  PFDBG0('g', "Translating cast");

  if (type_is_scalar(cast->type) || type_is_enum(cast->type) ||
      type_is_array(cast->type)) {
    /* `scalar_conversions` should do everything we need */
    scalar_conversions(expr, cast->type);
    cast->last_instr = liter_copy(expr->last_instr);
    if (cast->last_instr == NULL) abort();
  } else if (type_is_void(cast->type)) {
    Instruction *nop_instr = instr_init(OP_NOP);
    int status =
        liter_push_back(expr->last_instr, &cast->last_instr, 1, nop_instr);
    if (status) abort();
  } else {
    cast->last_instr = liter_copy(expr->last_instr);
    if (cast->last_instr == NULL) abort();
  }

  cast->first_instr = liter_copy(expr->first_instr);
  if (cast->first_instr == NULL) abort();
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
  scalar_conversions(operand, operand->type);
  Instruction *operand_instr = liter_get(operand->last_instr);

  /* TEST operand with itself */
  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = operand_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *setz_instr = instr_init(OP_SETZ);
  set_op_reg(&setz_instr->dest, REG_BYTE, next_vreg());

  Instruction *movz_instr = instr_init(OP_MOVZ);
  movz_instr->src = setz_instr->dest;
  set_op_reg(&movz_instr->dest, REG_DWORD, next_vreg());
  movz_instr->persist_flags |= PERSIST_DEST_CLEAR;

  not ->first_instr = liter_copy(operand->first_instr);
  if (not ->first_instr == NULL) abort();
  int status = liter_push_back(operand->last_instr, &not ->last_instr, 3,
                               test_instr, setz_instr, movz_instr);
  if (status) abort();
  return astree_adopt(not, 1, operand);
}

ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  /* test first operand; jump on false for && and true for || */
  scalar_conversions(left, left->type);
  Instruction *left_instr = liter_get(left->last_instr);

  const char *skip_label = operator->tok_kind == TOK_AND
                               ? mk_false_label(next_branch())
                               : mk_true_label(next_branch());
  Instruction *test_left_instr = instr_init(OP_TEST);
  test_left_instr->dest = test_left_instr->src = left_instr->dest;
  test_left_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *jmp_left_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  set_op_dir(&jmp_left_instr->dest, skip_label);

  int status = liter_push_back(left->last_instr, NULL, 2, test_left_instr,
                               jmp_left_instr);
  if (status) abort();

  scalar_conversions(right, right->type);
  Instruction *right_instr = liter_get(right->last_instr);

  Instruction *test_right_instr = instr_init(OP_TEST);
  test_right_instr->dest = test_right_instr->src = right_instr->dest;
  test_right_instr->persist_flags |= PERSIST_DEST_SET;

  /* result will always be the truth value of the last evaluated expression */
  Instruction *setnz_instr = instr_init(OP_SETNZ);
  set_op_reg(&setnz_instr->dest, REG_BYTE, next_vreg());
  setnz_instr->label = skip_label;
  Instruction *movz_instr = instr_init(OP_MOVZ);
  movz_instr->src = setnz_instr->dest;
  set_op_reg(&movz_instr->dest, REG_DWORD, next_vreg());
  movz_instr->persist_flags |= PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();
  status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                           test_right_instr, setnz_instr, movz_instr);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_comparison(ASTree *operator, ASTree * left, ASTree *right) {
  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  Type *common_type =
      (type_is_pointer(left->type) || type_is_pointer(right->type))
          ? (Type *)TYPE_LONG
          : type_arithmetic_conversions(left->type, right->type);

  scalar_conversions(left, common_type);
  Instruction *left_instr = liter_get(left->last_instr);

  scalar_conversions(right, common_type);
  Instruction *right_instr = liter_get(right->last_instr);

  Instruction *cmp_instr = instr_init(OP_CMP);
  /* reverse operands; looks weird in AT&T syntax but is correct */
  cmp_instr->dest = left_instr->dest;
  cmp_instr->src = right_instr->dest;
  cmp_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  Instruction *setcc_instr =
      instr_init(opcode_from_operator(operator->tok_kind, common_type));
  set_op_reg(&setcc_instr->dest, REG_BYTE, next_vreg());

  Instruction *movz_instr = instr_init(OP_MOVZ);
  movz_instr->src = setcc_instr->dest;
  set_op_reg(&movz_instr->dest, REG_DWORD, next_vreg());
  movz_instr->persist_flags = PERSIST_DEST_CLEAR;

  int status = liter_push_back(right->last_instr, &operator->last_instr, 3,
                               cmp_instr, setcc_instr, movz_instr);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_indirection(ASTree *indirection, ASTree *operand) {
  PFDBG0('g', "Translating indirection operation.");
  indirection->first_instr = liter_copy(operand->first_instr);
  if (indirection->first_instr == NULL) abort();

  scalar_conversions(operand, TYPE_POINTER);

  /* `scalar_conversions` already converts lvalues to rvalues, so by this
   * point the destination of `operand_instr` will already be the value, not the
   * location, of the result of the expression the indirection operator is being
   * applied to. since the operand must have had pointer type, this value is
   * also a location. this value should be left as-is, since the result is an
   * lvalue, and in order to use it as an lvalue we need the location.
   */
  indirection->last_instr = liter_copy(operand->last_instr);
  if (indirection->last_instr == NULL) abort();
  return astree_adopt(indirection, 1, operand);
}

ASTree *translate_addrof(ASTree *addrof, ASTree *operand) {
  PFDBG0('g', "Translating address operation.");
  /* this function doesn't need to do anything. the operand has to be an lvalue,
   * which means the destination operand of the last instruction emitted should
   * be the location (address) of the result already.
   */
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
  scalar_conversions(pointer, TYPE_POINTER);
  Instruction *pointer_instr = liter_get(pointer->last_instr);

  scalar_conversions(index, TYPE_LONG);
  Instruction *index_instr = liter_get(index->last_instr);

  subscript->first_instr = liter_copy(pointer->first_instr);
  if (subscript->first_instr == NULL) abort();

  Instruction *lea_instr = instr_init(OP_LEA);
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  lea_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  size_t scale = type_get_width(subscript->type);
  if (scale == 1 || scale == 2 || scale == 4 || scale == 8) {
    set_op_sca(&lea_instr->src, scale, NO_DISP, pointer_instr->dest.reg.num,
               index_instr->dest.reg.num);
    int status = liter_push_back(index->last_instr, &subscript->last_instr, 1,
                                 lea_instr);
    if (status) abort();
  } else {
    set_op_sca(&lea_instr->src, SCALE_BYTE, NO_DISP,
               pointer_instr->dest.reg.num, index_instr->dest.reg.num);
    Instruction *mul_instr = instr_init(OP_IMUL);
    mul_instr->dest = index_instr->dest;
    set_op_imm(&mul_instr->src, scale, IMM_UNSIGNED);
    int status = liter_push_back(index->last_instr, &subscript->last_instr, 2,
                                 mul_instr, lea_instr);
    if (status) abort();
  }
  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *translate_reference(ASTree *reference, ASTree *struct_,
                            ASTree *member) {
  PFDBG0('g', "Translating reference operator");
  Type *record_type;
  if (reference->tok_kind == TOK_ARROW) {
    scalar_conversions(struct_, struct_->type);
    record_type = type_strip_declarator(struct_->type);
  } else {
    record_type = struct_->type;
  }

  Instruction *struct_instr = liter_get(struct_->last_instr);
  Symbol *member_symbol = type_member_name(record_type, member->lexinfo);
  assert(member_symbol);

  Instruction *lea_instr = instr_init(OP_LEA);
  set_op_ind(&lea_instr->src, member_symbol->disp, struct_instr->dest.reg.num);
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  lea_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  reference->first_instr = liter_copy(struct_->first_instr);
  if (reference->first_instr == NULL) abort();
  int status = liter_push_back(struct_->last_instr, &reference->last_instr, 1,
                               lea_instr);
  if (status) abort();
  return astree_adopt(reference, 2, struct_, member);
}

ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating postfix increment/decrement");
  post_inc_dec->first_instr = liter_copy(operand->first_instr);
  if (post_inc_dec->first_instr == NULL) abort();
  Instruction *lvalue_instr = liter_get(operand->last_instr);
  if (lvalue_instr == NULL) abort();
  scalar_conversions(operand, post_inc_dec->type);
  Instruction *operand_instr = liter_get(operand->last_instr);

  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = operand_instr->dest;
  set_op_reg(&mov_instr->dest, type_get_width(post_inc_dec->type), next_vreg());

  Instruction *inc_dec_instr = instr_init(
      opcode_from_operator(post_inc_dec->tok_kind, post_inc_dec->type));
  inc_dec_instr->dest = operand_instr->dest;

  Instruction *mov_instr_2 = instr_init(OP_MOV);
  set_op_ind(&mov_instr_2->dest, NO_DISP, lvalue_instr->dest.reg.num);
  set_op_reg(&mov_instr_2->src, type_get_width(operand->type),
             operand_instr->dest.reg.num);
  /* both the location and value of the object must persist until this point */
  mov_instr_2->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = mov_instr->dest;
  dummy_instr->persist_flags |= PERSIST_DEST_CLEAR;

  int status =
      liter_push_back(operand->last_instr, &post_inc_dec->last_instr, 4,
                      mov_instr, inc_dec_instr, mov_instr_2, dummy_instr);
  if (status) abort();
  return astree_adopt(post_inc_dec, 1, operand);
}

ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating prefix increment/decrement");
  inc_dec->first_instr = liter_copy(operand->first_instr);
  if (inc_dec->first_instr == NULL) abort();
  Instruction *lvalue_instr = liter_get(operand->last_instr);
  if (lvalue_instr == NULL) abort();
  scalar_conversions(operand, inc_dec->type);
  Instruction *operand_instr = liter_get(operand->last_instr);

  Instruction *inc_dec_instr =
      instr_init(opcode_from_operator(inc_dec->tok_kind, inc_dec->type));
  inc_dec_instr->dest = operand_instr->dest;

  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_ind(&mov_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  set_op_reg(&mov_instr->src, type_get_width(operand->type),
             operand_instr->dest.reg.num);
  /* both the location and value of the object must persist until this point */
  mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = operand_instr->dest;
  dummy_instr->persist_flags |= PERSIST_DEST_CLEAR;

  int status = liter_push_back(operand->last_instr, &inc_dec->last_instr, 3,
                               inc_dec_instr, mov_instr, dummy_instr);
  if (status) abort();
  return astree_adopt(inc_dec, 1, operand);
}

ASTree *translate_unop(ASTree *operator, ASTree * operand) {
  PFDBG0('g', "Translating unary operation");
  scalar_conversions(operand, operator->type);
  Instruction *operand_instr = liter_get(operand->last_instr);

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  operator_instr->dest = operand_instr->dest;
  operator_instr->persist_flags |= PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(operand->first_instr);
  if (operator->first_instr == NULL) abort();
  int status = liter_push_back(operand->last_instr, &operator->last_instr, 1,
                               operator_instr);
  if (status) abort();
  return astree_adopt(operator, 1, operand);
}

ASTree *translate_sizeof(ASTree *sizeof_, ASTree *operand) {
  if (operand->first_instr == NULL) {
    assert(operand->last_instr == NULL);
    return astree_adopt(sizeof_, 1, operand);
  }

  assert(operand->last_instr != NULL);
  while (liter_get(operand->first_instr) != liter_get(operand->last_instr)) {
    int status = liter_delete(operand->first_instr);
    if (status) abort();
  }

  int status = liter_delete(operand->first_instr);
  if (status) abort();
  free(operand->first_instr);
  free(operand->last_instr);
  operand->first_instr = operand->last_instr = NULL;
  return astree_adopt(sizeof_, 1, operand);
}

static void convert_int_to_offset(const Operand *int_op, const Type *ptr_type,
                                  ListIter *where, ListIter **out) {
  assert(type_is_pointer(ptr_type));
  assert(where != NULL);
  assert(int_op->all.mode == MODE_REGISTER);

  Type *element_type = type_strip_declarator(ptr_type);
  Instruction *imul_instr = instr_init(OP_IMUL);
  imul_instr->dest = *int_op;
  set_op_imm(&imul_instr->src, type_get_width(element_type), IMM_UNSIGNED);
  int status = liter_push_back(where, out, 1, imul_instr);
  if (status) abort();
}

ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  PFDBG0('g', "Translating additive operation");
  scalar_conversions(left, operator->type);
  Instruction *left_instr = liter_get(left->last_instr);

  scalar_conversions(right, operator->type);
  Instruction *right_instr = liter_get(right->last_instr);

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  /* reverse operands; looks weird in AT&T syntax but is correct */
  operator_instr->dest = left_instr->dest;
  operator_instr->src = right_instr->dest;
  operator_instr->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  /* use two-operand IMUL, since it is more convenient in this case */
  if (type_is_pointer(left->type) && !type_is_pointer(right->type)) {
    convert_int_to_offset(&right_instr->dest, left->type, right->last_instr,
                          &operator->last_instr);
    int status = liter_push_back(operator->last_instr, &operator->last_instr, 1,
                                 operator_instr);
    if (status) abort();
  } else if (!type_is_pointer(left->type) && type_is_pointer(right->type)) {
    convert_int_to_offset(&left_instr->dest, right->type, right->last_instr,
                          &operator->last_instr);
    int status = liter_push_back(operator->last_instr, &operator->last_instr, 1,
                                 operator_instr);
    if (status) abort();
  } else {
    int status = liter_push_back(right->last_instr, &operator->last_instr, 1,
                                 operator_instr);
    if (status) abort();
  }
  return astree_adopt(operator, 2, left, right);
}

/* DIV/IDIV: divide dx:ax by operand; quotient -> ax; remainder -> dx except
 * when the operand is 8 bits, in which case divide ax by operand;
 * quotient->al; remainder->ah
 *
 * MUL/IMUL: multipy ax by operand; lower bits -> ax; upper bits -> dx except
 * when the operand is al, in which case upper bits -> ah
 *
 * IMUL has 2- and 3-operand forms, but we use the 1-operand form here since it
 * lets us reuse the whole procedure to emit it
 *
 * because of integral promotion, the operands should be at least 32 bits, and
 * we don't need to worry about the case where al is extended into ax rather
 * than al:dl and the remainder is in ah
 */
/* We need to use a 3rd register to hold the right operand in case it was
 * originally held in rax or rdx, in which case it would be clobbered when
 * moving the left operand into rax or when rax is sign- or zero-extended into
 * rdx. Fortunately, the virtual register for the right operand should not be
 * used after this operation, so its contents aren't important afterwards.
 * Attempting to do so will only trip up the register allocator.
 */
static void multiply_helper(ASTree *operator, ASTree * left, ASTree *right) {
  Type *common_type;
  Instruction *lvalue_instr, *left_instr;
  if (operator->tok_kind == TOK_MULEQ ||
      operator->tok_kind == TOK_DIVEQ ||
      operator->tok_kind == TOK_REMEQ) {
    lvalue_instr = liter_get(left->last_instr);
    assert(lvalue_instr->dest.all.mode == MODE_REGISTER);
    assert(lvalue_instr->dest.reg.width == REG_QWORD);
    common_type = type_arithmetic_conversions(left->type, right->type);
    scalar_conversions(left, common_type);
    left_instr = liter_get(left->last_instr);
  } else {
    lvalue_instr = NULL;
    common_type = operator->type;
    scalar_conversions(left, common_type);
    left_instr = liter_get(left->last_instr);
  }

  assert(type_get_width(common_type) >= 4);

  scalar_conversions(right, common_type);
  Instruction *right_instr = liter_get(right->last_instr);

  /* save registers whose values may or may not be clobbered */
  Instruction *push_rax_instr = instr_init(OP_PUSH);
  set_op_reg(&push_rax_instr->dest, REG_QWORD, RAX_VREG);
  Instruction *push_rdx_instr = instr_init(OP_PUSH);
  set_op_reg(&push_rdx_instr->dest, REG_QWORD, RDX_VREG);
  Instruction *push_r10_instr = instr_init(OP_PUSH);
  set_op_reg(&push_r10_instr->dest, REG_QWORD, R10_VREG);
  Instruction *push_right_instr = instr_init(OP_PUSH);
  set_op_reg(&push_right_instr->dest, REG_QWORD, right_instr->dest.reg.num);
  push_right_instr->persist_flags |= PERSIST_DEST_SET;

  /* spill lvalue location to 3rd unspill region, if applicable */
  Instruction *spill_lvalue_instr;
  if (lvalue_instr == NULL) {
    spill_lvalue_instr = instr_init(OP_NOP);
  } else {
    spill_lvalue_instr = instr_init(OP_MOV);
    spill_lvalue_instr->src = lvalue_instr->dest;
    set_op_ind(&spill_lvalue_instr->dest, UNSPILL_REGIONS[2], RBP_VREG);
  }

  /* mov left operand into rax as implicit destination operand */
  Instruction *mov_rax_instr = instr_init(OP_MOV);
  set_op_reg(&mov_rax_instr->dest, type_get_width(common_type), RAX_VREG);
  mov_rax_instr->src = left_instr->dest;
  mov_rax_instr->persist_flags |= PERSIST_SRC_SET;

  /* extend rax into rdx to be double its width for division */
  Opcode opcode = opcode_from_operator(operator->tok_kind, common_type);
  Instruction *extend_instr;
  if (opcode != OP_DIV && opcode != OP_IDIV) {
    extend_instr = instr_init(OP_NOP);
  } else if (type_is_signed(common_type)) {
    extend_instr = type_get_width(common_type) == 8 ? instr_init(OP_CQO)
                                                    : instr_init(OP_CDQ);
  } else {
    extend_instr = instr_init(OP_MOV);
    set_op_imm(&extend_instr->src, 0, IMM_UNSIGNED);
    set_op_reg(&extend_instr->dest, type_get_width(common_type), RDX_VREG);
  }

  /* pop right operand into r10 */
  Instruction *pop_right_instr = instr_init(OP_POP);
  set_op_reg(&pop_right_instr->dest, REG_QWORD, R10_VREG);

  /* perform operation with right operand in r10 */
  Instruction *operator_instr = instr_init(opcode);
  set_op_reg(&operator_instr->dest, type_get_width(common_type), R10_VREG);

  /* if this is a compound assignment operator, store result in lvalue
   * location; otherwise, store in 3rd unspill register, which should not be
   * used by any subsequent instructions. if it is a compound assignment
   * operator, we also need to unspill the register containing the lvalue
   * location; we can use r10 since we are done with the right operand.
   */
  Instruction *unspill_lvalue_instr, *assign_instr;
  if (lvalue_instr == NULL) {
    unspill_lvalue_instr = instr_init(OP_NOP);
    assign_instr = instr_init(OP_NOP);
  } else {
    unspill_lvalue_instr = instr_init(OP_MOV);
    set_op_ind(&unspill_lvalue_instr->src, UNSPILL_REGIONS[2], RBP_VREG);
    set_op_reg(&unspill_lvalue_instr->dest, REG_QWORD, R10_VREG);

    assign_instr = instr_init(OP_MOV);
    set_op_reg(&assign_instr->src,
               type_get_width(left->type),
               operator->tok_kind == TOK_REMEQ ? RDX_VREG : RAX_VREG);
    set_op_ind(&assign_instr->dest, NO_DISP, R10_VREG);
  }

  Instruction *store_instr = instr_init(OP_MOV);
  if (operator->tok_kind == '%' || operator->tok_kind == TOK_REMEQ)
    set_op_reg(&store_instr->src, type_get_width(common_type), RDX_VREG);
  else
    set_op_reg(&store_instr->src, type_get_width(common_type), RAX_VREG);
  set_op_ind(&store_instr->dest, UNSPILL_REGIONS[2], RBP_VREG);

  /* restore clobbered registers */
  Instruction *pop_r10_instr = instr_init(OP_POP);
  set_op_reg(&pop_r10_instr->dest, REG_QWORD, R10_VREG);
  Instruction *pop_rdx_instr = instr_init(OP_POP);
  set_op_reg(&pop_rdx_instr->dest, REG_QWORD, RDX_VREG);
  Instruction *pop_rax_instr = instr_init(OP_POP);
  set_op_reg(&pop_rax_instr->dest, REG_QWORD, RAX_VREG);

  /* load result into a fresh vreg */
  Instruction *load_instr = instr_init(OP_MOV);
  load_instr->src = store_instr->dest;
  if (lvalue_instr == NULL)
    set_op_reg(&load_instr->dest, type_get_width(common_type), next_vreg());
  else
    set_op_reg(&load_instr->dest, type_get_width(left->type), next_vreg());
  load_instr->persist_flags |= PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(left->first_instr);
  assert(operator->first_instr != NULL);

  int status = liter_push_back(
      right->last_instr, &operator->last_instr, 16, push_rax_instr,
      push_rdx_instr, push_r10_instr, push_right_instr, spill_lvalue_instr,
      mov_rax_instr, extend_instr, pop_right_instr, operator_instr,
      unspill_lvalue_instr, assign_instr, store_instr, pop_r10_instr,
      pop_rdx_instr, pop_rax_instr, load_instr);
  assert(!status);
  assert(operator->last_instr != NULL);
}

ASTree *translate_multiplication(ASTree *operator, ASTree * left,
                                 ASTree *right) {
  PFDBG0('g', "Translating binary operation");
  multiply_helper(operator, left, right);
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  PFDBG0('g', "Translating binary operation");
  scalar_conversions(left, operator->type);
  Instruction *left_instr = liter_get(left->last_instr);

  scalar_conversions(right, operator->type);
  Instruction *right_instr = liter_get(right->last_instr);

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  /* reverse operands; looks weird in AT&T syntax but is correct */
  operator_instr->dest = left_instr->dest;
  operator_instr->src = right_instr->dest;
  operator_instr->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  operator->first_instr = liter_copy(left->first_instr);
  if (operator->first_instr == NULL) abort();

  int status = liter_push_back(right->last_instr, &operator->last_instr, 1,
                               operator_instr);
  if (status) abort();
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_conditional(ASTree *qmark, ASTree *condition,
                              ASTree *true_expr, ASTree *false_expr) {
  scalar_conversions(condition, condition->type);
  size_t current_branch = next_branch();
  Instruction *condition_instr = liter_get(condition->last_instr);
  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;
  Instruction *jmp_false_instr =
      instr_init(opcode_from_operator(qmark->tok_kind, qmark->type));
  set_op_dir(&jmp_false_instr->dest, mk_false_label(current_branch));
  int status = liter_push_back(condition->last_instr, NULL, 2, test_instr,
                               jmp_false_instr);
  if (status) abort();

  if (type_is_void(qmark->type)) {
    Instruction *nop_instr = instr_init(OP_NOP);
    Instruction *jmp_end_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_end_instr->dest, mk_true_label(current_branch));
    status = liter_push_back(true_expr->last_instr, NULL, 2, nop_instr,
                             jmp_end_instr);

    Instruction *false_label = liter_get(false_expr->first_instr);
    false_label->label = mk_false_label(current_branch);
    Instruction *end_label = instr_init(OP_NOP);
    end_label->label = mk_true_label(current_branch);
    status = liter_push_back(false_expr->last_instr, &qmark->last_instr, 1,
                             end_label);
  } else {
    scalar_conversions(true_expr, qmark->type);
    Instruction *true_expr_instr = liter_get(true_expr->last_instr);
    Instruction *mov_true_instr = instr_init(OP_MOV);
    mov_true_instr->src = true_expr_instr->dest;
    set_op_reg(&mov_true_instr->dest, type_get_width(qmark->type), next_vreg());
    /* clear persistence data for result vreg; set it for true expr result */
    mov_true_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    Instruction *jmp_end_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_end_instr->dest, mk_true_label(current_branch));
    status = liter_push_back(true_expr->last_instr, NULL, 2, mov_true_instr,
                             jmp_end_instr);

    scalar_conversions(false_expr, qmark->type);
    Instruction *false_label = liter_get(false_expr->first_instr);
    false_label->label = mk_false_label(current_branch);
    Instruction *false_expr_instr = liter_get(false_expr->last_instr);
    Instruction *mov_false_instr = instr_init(OP_MOV);
    mov_false_instr->src = false_expr_instr->dest;
    mov_false_instr->dest = mov_true_instr->dest;
    mov_false_instr->persist_flags |= PERSIST_SRC_SET;
    /* dummy mov so that last instruction has destination reg */
    Instruction *end_label = instr_init(OP_MOV);
    end_label->label = mk_true_label(current_branch);
    end_label->src = end_label->dest = mov_false_instr->dest;
    /* persist result vreg across bblocks */
    end_label->persist_flags |= PERSIST_DEST_SET;
    status = liter_push_back(false_expr->last_instr, &qmark->last_instr, 2,
                             mov_false_instr, end_label);
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

static void assign_aggregate(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) abort();

  Instruction *lvalue_instr = liter_get(lvalue->last_instr);
  Instruction *rvalue_instr = liter_get(rvalue->last_instr);
  assignment->last_instr = liter_next(rvalue->last_instr, 1);
  if (assignment->last_instr == NULL) abort();
  /* `bulk_mtom` should make registers persist */
  bulk_mtom(lvalue_instr->dest.reg.num, rvalue_instr->dest.reg.num,
            assignment->type, rvalue->last_instr);
  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = lvalue_instr->dest;
  dummy_instr->persist_flags |= PERSIST_DEST_CLEAR;
  /* push_front since iter is current past the last instruction */
  int status = liter_push_front(assignment->last_instr, &assignment->last_instr,
                                1, dummy_instr);
  if (status) abort();
}

static void assign_add(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  Instruction *lvalue_instr = liter_get(lvalue->last_instr);

  scalar_conversions(rvalue, lvalue->type);
  Instruction *rvalue_instr = liter_get(rvalue->last_instr);

  Instruction *assignment_instr =
      instr_init(opcode_from_operator(assignment->tok_kind, assignment->type));
  set_op_ind(&assignment_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  assignment_instr->src = rvalue_instr->dest;
  assignment_instr->persist_flags |= PERSIST_SRC_SET;

  /* load new value to register in case parent expression needs it */
  Instruction *dummy_instr = instr_init(OP_MOV);
  set_op_ind(&dummy_instr->src, NO_DISP, lvalue_instr->dest.reg.num);
  set_op_reg(&dummy_instr->dest, type_get_width(assignment->type), next_vreg());
  dummy_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) abort();

  /* TODO(Robert): copied from translate_addition */
  /* use two-operand IMUL, since it is more convenient in this case */
  if (type_is_pointer(lvalue->type) && !type_is_pointer(rvalue->type)) {
    Type *element_type = type_strip_declarator(lvalue->type);
    Instruction *mul_instr = instr_init(OP_IMUL);
    mul_instr->dest = rvalue_instr->dest;
    set_op_imm(&mul_instr->src, type_get_width(element_type), IMM_UNSIGNED);
    int status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 3,
                                 mul_instr, assignment_instr, dummy_instr);
    if (status) abort();
  } else {
    int status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 2,
                                 assignment_instr, dummy_instr);
    if (status) abort();
  }
}

static void assign_scalar(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  Instruction *lvalue_instr = liter_get(lvalue->last_instr);

  scalar_conversions(rvalue, assignment->type);
  Instruction *rvalue_instr = liter_get(rvalue->last_instr);

  Instruction *assignment_instr =
      instr_init(opcode_from_operator(assignment->tok_kind, assignment->type));
  set_op_ind(&assignment_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  assignment_instr->src = rvalue_instr->dest;
  assignment_instr->persist_flags |= PERSIST_DEST_SET | PERSIST_SRC_SET;

  Instruction *load_instr = instr_init(OP_MOV);
  load_instr->src = assignment_instr->dest;
  set_op_reg(&load_instr->dest, type_get_width(assignment->type), next_vreg());
  load_instr->persist_flags |= PERSIST_DEST_CLEAR;

  assignment->first_instr = liter_copy(lvalue->first_instr);
  if (assignment->first_instr == NULL) abort();
  int status = liter_push_back(rvalue->last_instr, &assignment->last_instr, 2,
                               assignment_instr, load_instr);
  if (status) abort();
}

/* NOTE: callers assume this function does not return errors */
ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  PFDBG0('g', "Translating assignment");
  if (type_is_union(assignment->type) || type_is_struct(assignment->type)) {
    assign_aggregate(assignment, lvalue, rvalue);
  } else if (assignment->tok_kind == TOK_ADDEQ ||
             assignment->tok_kind == TOK_SUBEQ) {
    assign_add(assignment, lvalue, rvalue);
  } else if (assignment->tok_kind == TOK_MULEQ ||
             assignment->tok_kind == TOK_DIVEQ ||
             assignment->tok_kind == TOK_REMEQ) {
    multiply_helper(assignment, lvalue, rvalue);
  } else {
    assign_scalar(assignment, lvalue, rvalue);
  }
  /* don't adopt; this function gets used for initialization */
  return assignment;
}

static void translate_agg_arg(ASTree *call, ASTree *arg) {
  size_t arg_eightbytes = type_get_eightbytes(arg->type);
  Instruction *arg_instr = liter_get(arg->last_instr);
  assert(arg_instr->dest.all.mode == MODE_INDIRECT);
  Instruction *mov_instr = instr_init(OP_MOV);
  /* use arbitrary volatile reg to store arg, since they should be saved at
   * this point; avoid argument and return regs in case the way i handle calls
   * needs to change
   */
  set_op_reg(&mov_instr->dest, REG_QWORD, R10_VREG);
  mov_instr->src = arg_instr->dest;

  int status = liter_advance(call->last_instr, -1);
  if (status) abort();
  if (arg_eightbytes <= 2 &&
      arg_eightbytes + arg_reg_index <= PARAM_REG_COUNT) {
    bulk_mtor(PARAM_REGS + arg_reg_index, mov_instr->dest.reg.num, NO_DISP,
              arg->type, call->last_instr);
    arg_reg_index += arg_eightbytes;
  } else {
    bulk_mtom(RSP_VREG, mov_instr->dest.reg.num, arg->type, call->last_instr);
    arg_stack_disp += arg_eightbytes * X64_SIZEOF_LONG;
    Instruction *sub_instr = instr_init(OP_SUB);
    set_op_reg(&sub_instr->dest, REG_QWORD, RSP_VREG);
    set_op_imm(&sub_instr->src, arg_eightbytes * 8, IMM_UNSIGNED);
    /* push after since instructions will be reversed */
    status = liter_push_back(call->last_instr, NULL, 1, sub_instr);
    if (status) abort();
  }

  /* push after since instructions will be reversed */
  status = liter_push_back(call->last_instr, NULL, 1, mov_instr);
  if (status) abort();
  status = liter_advance(call->last_instr, 1);
  if (status) abort();
}

static void translate_scalar_arg(ASTree *call, ASTree *arg) {
  Instruction *arg_instr = liter_get(arg->last_instr);
  assert(arg_instr->dest.all.mode == MODE_INDIRECT);
  assert(arg_instr->src.all.mode == MODE_REGISTER);
  assert(arg_instr->src.reg.width == REG_QWORD);

  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = arg_instr->dest;

  if (arg_reg_index < PARAM_REG_COUNT) {
    set_op_reg(&mov_instr->dest, REG_QWORD, PARAM_REGS[arg_reg_index++]);
    int status =
        liter_push_front(call->last_instr, &call->last_instr, 1, mov_instr);
    if (status) abort();
  } else {
    /* use arbitrary volatile reg to store arg, since they should be saved at
     * this point; avoid argument and return regs in case the way i handle calls
     * needs to change
     */
    set_op_reg(&mov_instr->dest, REG_QWORD, R10_VREG);
    Instruction *push_instr = instr_init(OP_PUSH);
    set_op_reg(&push_instr->dest, REG_QWORD, R10_VREG);
    int status = liter_push_front(call->last_instr, &call->last_instr, 2,
                                  mov_instr, push_instr);
    if (status) abort();
    arg_stack_disp += X64_SIZEOF_LONG;
  }
}

static void translate_args(ASTree *call) {
  /* account for hidden out param */
  int out_param = type_get_eightbytes(call->type) > 2;
  arg_reg_index = out_param ? 1 : 0;
  arg_stack_disp = 0;
  if (type_is_struct(call->type) || type_is_union(call->type)) {
    if (out_param) {
      Instruction *lea_instr = instr_init(OP_LEA);
      set_op_reg(&lea_instr->dest, REG_QWORD, RDI_VREG);
      set_op_ind(&lea_instr->src, assign_stack_space(call->type), RBP_VREG);
      int status =
          liter_push_front(call->last_instr, &call->last_instr, 1, lea_instr);
      if (status) abort();
    } else {
      (void)assign_stack_space(call->type);
    }
  }

  size_t i;
  for (i = 1; i < astree_count(call); ++i) {
    PFDBG1('g', "Translating parameter %i", i);
    ASTree *arg = astree_get(call, i);
    assert(arg->type != NULL && !type_is_array(arg->type));
    if (type_is_union(arg->type) || type_is_struct(arg->type)) {
      translate_agg_arg(call, arg);
    } else {
      translate_scalar_arg(call, arg);
    }
  }
}

static void save_call_subexprs(ASTree *call) {
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
    if (spill_regions == NULL) abort();
    for (i = old_count; i < spill_regions_count; ++i) {
      spill_regions[i] = assign_stack_space(TYPE_LONG);
    }
  }

  for (i = 0; i < astree_count(call); ++i) {
    ASTree *subexpr = astree_get(call, i);
    /* turn lvalues into rvalues and extend all scalar arguments to be eight
     * bytes wide to make loading and storing more convenient
     */
    if (type_is_scalar(subexpr->type))
      scalar_conversions(subexpr, TYPE_UNSIGNED_LONG);

    Instruction *subexpr_instr = liter_get(subexpr->last_instr);
    assert(subexpr_instr->dest.all.mode == MODE_REGISTER);
    assert(subexpr_instr->dest.reg.width == REG_QWORD);
    Instruction *spill_instr = instr_init(OP_MOV);
    spill_instr->src = subexpr_instr->dest;
    set_op_ind(&spill_instr->dest,
               spill_regions[call->spill_eightbytes - (i + 1)], RBP_VREG);
    spill_instr->persist_flags |= PERSIST_SRC_SET;
    int status = liter_push_back(subexpr->last_instr, &subexpr->last_instr, 1,
                                 spill_instr);
    if (status) abort();
  }
}

ASTree *translate_call(ASTree *call) {
  PFDBG0('g', "Translating function call");
  ASTree *fn_pointer = astree_get(call, 0);
  call->first_instr = liter_copy(fn_pointer->first_instr);
  if (call->first_instr == NULL) abort();
  save_call_subexprs(call);
  Instruction *sub_instr = instr_init(OP_SUB);
  set_op_reg(&sub_instr->dest, REG_QWORD, RSP_VREG);
  int status = llist_push_back(instructions, sub_instr);
  if (status) abort();

  /* temporary iterator for inserting args in reverse order */
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();

  /* do this after so that params are moved in reverse order */
  translate_args(call);
  save_volatile_regs(call->last_instr);
  free(call->last_instr);
  call->last_instr = NULL;

  /* set sub_instr's src op now that we know stack param space */
  assert(arg_stack_disp % X64_SIZEOF_LONG == 0);
  /* align stack to 16-byte boundary; we can use a bitand since we know the
   * stack should already be aligned to an 8-byte boundary
   */
  if (arg_stack_disp & X64_SIZEOF_LONG) {
    set_op_imm(&sub_instr->src, X64_SIZEOF_LONG, IMM_UNSIGNED);
    arg_stack_disp += X64_SIZEOF_LONG;
  } else {
    set_op_imm(&sub_instr->src, 0, IMM_UNSIGNED);
  }

  if (type_is_variadic_function(type_strip_declarator(fn_pointer->type))) {
    Instruction *zero_eax_instr = instr_init(OP_MOV);
    set_op_imm(&zero_eax_instr->src, 0, IMM_UNSIGNED);
    set_op_reg(&zero_eax_instr->dest, REG_DWORD, RAX_VREG);
    int status = llist_push_back(instructions, zero_eax_instr);
    if (status) abort();
  }

  Instruction *fn_pointer_instr = liter_get(fn_pointer->last_instr);
  assert(fn_pointer_instr->dest.all.mode == MODE_INDIRECT);
  Instruction *load_fn_instr = instr_init(OP_MOV);
  load_fn_instr->src = fn_pointer_instr->dest;
  /* use r10 since it is never used for args */
  set_op_reg(&load_fn_instr->dest, type_get_width(fn_pointer->type), R10_VREG);
  Instruction *call_instr = instr_init(OP_CALL);
  call_instr->dest = load_fn_instr->dest;
  status = llist_push_back(instructions, load_fn_instr);
  if (status) abort();
  status = llist_push_back(instructions, call_instr);
  if (status) abort();

  Instruction *rsp_reset_instr = instr_init(OP_ADD);
  set_op_reg(&rsp_reset_instr->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_reset_instr->src, arg_stack_disp, IMM_UNSIGNED);
  status = llist_push_back(instructions, rsp_reset_instr);
  if (status) abort();

  if (!type_is_void(call->type)) {
    /* store return value on the stack temporarily so that volatile registers
     * can be restored without worrying about the return value being clobbered
     */
    Instruction *store_instr = instr_init(OP_MOV);
    set_op_ind(&store_instr->dest, RETURN_VAL_DISP, RBP_VREG);
    Instruction *load_instr = instr_init(OP_MOV);
    load_instr->src = store_instr->dest;
    if (type_is_struct(call->type) || type_is_union(call->type)) {
      if (type_get_eightbytes(call->type) <= 2) {
        ListIter *temp = llist_iter_last(instructions);
        bulk_rtom(RBP_VREG, -window_size, RETURN_REGS, call->type, temp);
        free(temp);
      }
      Instruction *agg_addr_instr = instr_init(OP_LEA);
      set_op_ind(&agg_addr_instr->src, -window_size, RBP_VREG);
      /* any volatile register is fine since they will all be restored */
      set_op_reg(&agg_addr_instr->dest, REG_QWORD, RCX_VREG);
      int status = llist_push_back(instructions, agg_addr_instr);
      if (status) abort();
      store_instr->src = agg_addr_instr->dest;
      set_op_reg(&load_instr->dest, REG_QWORD, next_vreg());
      load_instr->persist_flags |= PERSIST_DEST_CLEAR;
    } else {
      set_op_reg(&store_instr->src, type_get_width(call->type), RAX_VREG);
      set_op_reg(&load_instr->dest, type_get_width(call->type), next_vreg());
      load_instr->persist_flags |= PERSIST_DEST_CLEAR;
    }
    int status = llist_push_back(instructions, store_instr);
    if (status) abort();
    restore_volatile_regs();
    status = llist_push_back(instructions, load_instr);
    if (status) abort();
  } else {
    restore_volatile_regs();
  }
  call->last_instr = llist_iter_last(instructions);
  if (call->last_instr == NULL) abort();
  return call;
}

ASTree *translate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident) {
  va_start_->first_instr = liter_copy(expr->first_instr);
  if (va_start_->first_instr == NULL) abort();
  scalar_conversions(expr, (Type *)TYPE_LONG);
  Instruction *expr_instr = liter_get(expr->last_instr);
  if (expr_instr == NULL) abort();
  size_t va_list_vreg = expr_instr->dest.reg.num;

  /* `list->gp_offset = param_reg_index * X64_SIZEOF_LONG;` */
  Instruction *load_gp_offset_instr = instr_init(OP_MOV);
  set_op_imm(&load_gp_offset_instr->src, param_reg_index * X64_SIZEOF_LONG,
             IMM_UNSIGNED);
  set_op_reg(&load_gp_offset_instr->dest, REG_DWORD, next_vreg());

  Instruction *store_gp_offset_instr = instr_init(OP_MOV);
  store_gp_offset_instr->src = load_gp_offset_instr->dest;
  set_op_ind(&store_gp_offset_instr->dest, GP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* `list->fp_offset = 304;` */
  Instruction *load_fp_offset_instr = instr_init(OP_MOV);
  set_op_imm(&load_fp_offset_instr->src, FP_OFFSET, IMM_UNSIGNED);
  set_op_reg(&load_fp_offset_instr->dest, REG_DWORD, next_vreg());

  Instruction *store_fp_offset_instr = instr_init(OP_MOV);
  store_fp_offset_instr->src = load_fp_offset_instr->dest;
  set_op_ind(&store_fp_offset_instr->dest, FP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* `list->reg_save_area = reg_save_area_disp + %rbp` */
  Instruction *reg_save_area_disp_instr = instr_init(OP_MOV);
  set_op_imm(&reg_save_area_disp_instr->src, reg_save_area_disp, IMM_SIGNED);
  set_op_reg(&reg_save_area_disp_instr->dest, REG_QWORD, next_vreg());

  Instruction *add_rbp_instr = instr_init(OP_ADD);
  set_op_reg(&add_rbp_instr->src, REG_QWORD, RBP_VREG);
  add_rbp_instr->dest = reg_save_area_disp_instr->dest;

  Instruction *store_reg_save_area_instr = instr_init(OP_MOV);
  store_reg_save_area_instr->src = add_rbp_instr->dest;
  set_op_ind(&store_reg_save_area_instr->dest, REG_SAVE_AREA_MEMBER_DISP,
             va_list_vreg);
  store_reg_save_area_instr->persist_flags |= PERSIST_DEST_SET;

  /* `list->overflow_arg_area = param_stack_disp + %rbp;` */
  Instruction *param_stack_disp_instr = instr_init(OP_MOV);
  set_op_imm(&param_stack_disp_instr->src, param_stack_disp, IMM_SIGNED);
  set_op_reg(&param_stack_disp_instr->dest, REG_QWORD, next_vreg());

  Instruction *add_rbp_instr_2 = instr_init(OP_ADD);
  set_op_reg(&add_rbp_instr_2->src, REG_QWORD, RBP_VREG);
  add_rbp_instr_2->dest = param_stack_disp_instr->dest;

  Instruction *store_overflow_arg_area_instr = instr_init(OP_MOV);
  store_overflow_arg_area_instr->src = add_rbp_instr_2->dest;
  set_op_ind(&store_overflow_arg_area_instr->dest,
             OVERFLOW_ARG_AREA_MEMBER_DISP, va_list_vreg);

  ListIter *temp = llist_iter_last(instructions);
  int status = liter_push_back(
      temp, &va_start_->last_instr, 10, load_gp_offset_instr,
      store_gp_offset_instr, load_fp_offset_instr, store_fp_offset_instr,
      reg_save_area_disp_instr, add_rbp_instr, store_reg_save_area_instr,
      param_stack_disp_instr, add_rbp_instr_2, store_overflow_arg_area_instr);
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
static void helper_va_arg_reg_param(ASTree *va_arg_, ASTree *expr,
                                    ASTree *type_name) {
  Instruction *expr_instr = liter_get(expr->last_instr);
  assert(expr_instr != NULL && expr_instr->dest.all.mode == MODE_REGISTER);
  size_t va_list_vreg = expr_instr->dest.reg.num;
  size_t eightbytes = type_get_eightbytes(astree_get(type_name, 1)->type);
  size_t result_vreg = next_vreg(), current_branch = next_branch();

  /* load gp offset member */
  Instruction *load_gp_offset_instr = instr_init(OP_MOV);
  set_op_ind(&load_gp_offset_instr->src, GP_OFFSET_MEMBER_DISP, va_list_vreg);
  set_op_reg(&load_gp_offset_instr->dest, REG_DWORD, result_vreg);
  /* first use of result_vreg; it no longer needs to persist */
  load_gp_offset_instr->persist_flags = PERSIST_DEST_CLEAR;

  /* jump if arg cannot fit into the save area */
  Instruction *cmp_gp_offset_instr = instr_init(OP_CMP);
  /* reverse operands; looks weird in AT&T syntax but is correct */
  cmp_gp_offset_instr->dest = load_gp_offset_instr->dest;
  /* if arg takes up two eightbytes, offset can't be >= 40 */
  if (eightbytes == 2)
    set_op_imm(&cmp_gp_offset_instr->src, GP_OFFSET_MAX - X64_SIZEOF_LONG,
               IMM_SIGNED);
  else
    set_op_imm(&cmp_gp_offset_instr->src, GP_OFFSET_MAX, IMM_SIGNED);

  Instruction *jmp_ge_instr = instr_init(OP_JGE);
  set_op_dir(&jmp_ge_instr->dest, mk_true_label(current_branch));

  /* add gp offset to save area disp to get location of next arg */
  Instruction *add_save_area_instr = instr_init(OP_ADD);
  set_op_ind(&add_save_area_instr->src, REG_SAVE_AREA_MEMBER_DISP,
             va_list_vreg);
  /* dword mov should zero hi 32 bits, so a qword add should behave here */
  set_op_reg(&add_save_area_instr->dest, REG_QWORD, result_vreg);

  /* TODO(Robert): give all operands/instructions a width field; currently the
   * compiler cannot emit instructions with one immediate mode operand and one
   * indirect/scaled-index mode operand because neither of them carry width
   * information.
   */
  /* load arg eightbytes */
  Instruction *load_eightbyte_instr = instr_init(OP_MOV);
  set_op_imm(&load_eightbyte_instr->src, eightbytes * X64_SIZEOF_LONG, 0);
  set_op_reg(&load_eightbyte_instr->dest, REG_DWORD, next_vreg());

  /* update gp offset member */
  Instruction *update_offset_instr = instr_init(OP_ADD);
  update_offset_instr->src = load_eightbyte_instr->dest;
  set_op_ind(&update_offset_instr->dest, GP_OFFSET_MEMBER_DISP, va_list_vreg);

  /* jump to dummy instruction */
  Instruction *jmp_false_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_false_instr->dest, mk_false_label(current_branch));

  /* load stack param area */
  Instruction *load_overflow_arg_area_instr = instr_init(OP_MOV);
  set_op_ind(&load_overflow_arg_area_instr->src, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  set_op_reg(&load_overflow_arg_area_instr->dest, REG_QWORD, result_vreg);
  load_overflow_arg_area_instr->label = mk_true_label(current_branch);

  /* update stack param save pointer */
  Instruction *load_disp_instr = instr_init(OP_MOV);
  set_op_imm(&load_disp_instr->src, eightbytes * X64_SIZEOF_LONG, IMM_UNSIGNED);
  set_op_reg(&load_disp_instr->dest, REG_QWORD, next_vreg());

  Instruction *add_disp_instr = instr_init(OP_ADD);
  add_disp_instr->src = load_disp_instr->dest;
  set_op_ind(&add_disp_instr->dest, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  /* va_list result vreg must perist until at least this point */
  add_disp_instr->persist_flags |= PERSIST_DEST_SET;

  /* dummy mov so last instruction holds result */
  Instruction *dummy_instr = instr_init(OP_MOV);
  set_op_reg(&dummy_instr->src, REG_QWORD, result_vreg);
  dummy_instr->dest = dummy_instr->src;
  dummy_instr->label = mk_false_label(current_branch);
  dummy_instr->persist_flags = PERSIST_DEST_SET;

  int status = liter_push_back(expr->last_instr, &va_arg_->last_instr, 11,
                               load_gp_offset_instr, cmp_gp_offset_instr,
                               jmp_ge_instr, add_save_area_instr,
                               load_eightbyte_instr, update_offset_instr,
                               jmp_false_instr, load_overflow_arg_area_instr,
                               load_disp_instr, add_disp_instr, dummy_instr);
  if (status) abort();
}

static void helper_va_arg_stack_param(ASTree *va_arg_, ASTree *expr,
                                      ASTree *type_name) {
  Instruction *expr_instr = liter_get(expr->last_instr);
  assert(expr_instr != NULL && expr_instr->dest.all.mode == MODE_REGISTER);
  size_t va_list_vreg = expr_instr->dest.reg.num;
  size_t eightbytes = type_get_eightbytes(astree_get(type_name, 1)->type);

  /* load location of next stack parameter */
  Instruction *load_overflow_arg_area_instr = instr_init(OP_MOV);
  set_op_ind(&load_overflow_arg_area_instr->src, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  set_op_reg(&load_overflow_arg_area_instr->dest, REG_QWORD, next_vreg());
  /* first use of result vreg; no need to persist it above this point */
  load_overflow_arg_area_instr->persist_flags |= PERSIST_DEST_CLEAR;

  /* load displacement to add to overflow pointer */
  Instruction *load_disp_instr = instr_init(OP_MOV);
  set_op_imm(&load_disp_instr->src, eightbytes * X64_SIZEOF_LONG, IMM_UNSIGNED);
  set_op_reg(&load_disp_instr->dest, REG_QWORD, next_vreg());

  /* update overflow pointer */
  Instruction *add_disp_instr = instr_init(OP_ADD);
  add_disp_instr->src = load_disp_instr->dest;
  set_op_ind(&add_disp_instr->dest, OVERFLOW_ARG_AREA_MEMBER_DISP,
             va_list_vreg);
  /* va_list_vreg must persist until at least this point */
  add_disp_instr->persist_flags |= PERSIST_DEST_SET;

  /* dummy mov so that last instruction contains result */
  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = load_overflow_arg_area_instr->dest;

  int status = liter_push_back(expr->last_instr, &va_arg_->last_instr, 4,
                               load_overflow_arg_area_instr, load_disp_instr,
                               add_disp_instr, dummy_instr);
  if (status) abort();
}

ASTree *translate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name) {
  va_arg_->first_instr = liter_copy(expr->first_instr);
  if (va_arg_->first_instr == NULL) abort();

  if (type_get_eightbytes(astree_get(type_name, 1)->type) <= 2)
    helper_va_arg_reg_param(va_arg_, expr, type_name);
  else
    helper_va_arg_stack_param(va_arg_, expr, type_name);
  assert(va_arg_->last_instr != NULL);
  assert(liter_get(va_arg_->last_instr) == llist_back(instructions));

  return astree_adopt(va_arg_, 2, expr, type_name);
}

static void translate_params(ASTree *declarator) {
  ASTree *fn_dirdecl = astree_get(declarator, astree_count(declarator) - 1);
  const Type *fn_type = declarator->type;
  Type *ret_type = type_strip_declarator(fn_type);
  /* account for hidden out param */
  param_reg_index = type_get_eightbytes(ret_type) > 2 ? 1 : 0;
  if (param_reg_index == 1) {
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, REG_QWORD, RDI_VREG);
    set_op_ind(&mov_instr->dest, HIDDEN_PARAM_DISP, RBP_VREG);
    int status = llist_push_back(instructions, mov_instr);
    if (status) abort();
  }
  /* offset to account for preserved regs and return address */
  param_stack_disp = PROLOGUE_EIGHTBYTES * X64_SIZEOF_LONG;
  size_t i, param_count = astree_count(fn_dirdecl);
  if (param_count > 0 && type_is_variadic_function(fn_type)) --param_count;
  if (param_count != 0 && astree_get(fn_dirdecl, 0)->tok_kind != TOK_VOID) {
    for (i = 0; i < param_count; ++i) {
      ASTree *param = astree_get(fn_dirdecl, i);
      ASTree *param_decl = astree_get(param, 1);
      Symbol *param_symbol = NULL;
      int in_current_scope =
          state_get_symbol(state, param_decl->lexinfo,
                           strlen(param_decl->lexinfo), &param_symbol);
#ifdef NDEBUG
      (void)in_current_scope;
#endif
      assert(in_current_scope && param_symbol);
      size_t param_symbol_eightbytes = type_get_eightbytes(param_symbol->type);
      if (param_symbol_eightbytes <= 2 &&
          param_reg_index + param_symbol_eightbytes <= PARAM_REG_COUNT) {
        param_symbol->disp = assign_stack_space(param_symbol->type);
        ListIter *temp = llist_iter_last(instructions);
        bulk_rtom(RBP_VREG, param_symbol->disp, PARAM_REGS + param_reg_index,
                  param_symbol->type, temp);
        param_reg_index += param_symbol_eightbytes;
        free(temp);
      } else {
        param_symbol->disp = param_stack_disp;
        param_stack_disp += param_symbol_eightbytes * X64_SIZEOF_LONG;
      }
    }
  }

  if (type_is_variadic_function(fn_type) && param_reg_index < PARAM_REG_COUNT) {
    reg_save_area_disp = assign_stack_space(TYPE_VA_SPILL_REGION);
    ListIter *temp = llist_iter_last(instructions);
    /* because spill region type is large enough to hold all registers, we must
     * copy all registers to the stack when the function is variadic since
     * `bulk_rtom` determines the number of registers to copy based on the size
     * of the type
     */
    bulk_rtom(RBP_VREG, reg_save_area_disp, PARAM_REGS, TYPE_VA_SPILL_REGION,
              temp);
    free(temp);
  }
}

ASTree *translate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                         ASTree *else_body) {
  scalar_conversions(condition, condition->type);
  Instruction *condition_instr = liter_get(condition->last_instr);

  ifelse->first_instr = liter_copy(condition->first_instr);
  if (ifelse->first_instr == NULL) abort();

  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(ifelse->jump_id);
  if (else_body->tok_kind != TOK_EMPTY) {
    Instruction *else_label = instr_init(OP_NONE);
    else_label->label = mk_stmt_label(ifelse->jump_id);
    int status = liter_push_front(else_body->first_instr, NULL, 1, else_label);
    if (status) abort();

    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, else_label->label);
    status = liter_push_back(condition->last_instr, NULL, 2, test_instr,
                             test_jmp_instr);
    if (status) abort();

    Instruction *jmp_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_instr->dest, end_label->label);
    status = liter_push_back(if_body->last_instr, NULL, 1, jmp_instr);
    if (status) abort();
    status = liter_push_back(else_body->last_instr, &ifelse->last_instr, 1,
                             end_label);
    if (status) abort();
  } else {
    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, end_label->label);
    int status = liter_push_back(condition->last_instr, NULL, 2, test_instr,
                                 test_jmp_instr);
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

  scalar_conversions(condition, control_type);
  Instruction *cond_instr = liter_get(condition->last_instr);
  if (cond_instr == NULL) abort();
  switch_->first_instr = liter_copy(condition->first_instr);
  if (switch_->first_instr == NULL) abort();

  /* switch prologue */
  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = cond_instr->dest;
  set_op_reg(&mov_instr->dest, type_get_width(control_type), control_vreg);
  /* clear control vreg from persistence data; persist condition expression */
  mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
  Instruction *jmp_case1_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_case1_instr->dest, mk_case_label(switch_->jump_id, 0));
  int status = liter_push_back(condition->last_instr, NULL, 2, mov_instr,
                               jmp_case1_instr);
  if (status) abort();

  /* switch epilogue */
  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(switch_->jump_id);
  Instruction *jmp_end_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_end_instr->dest, end_label->label);
  Instruction *dummy_case_label = instr_init(OP_NONE);
  dummy_case_label->label = mk_case_label(switch_->jump_id, fake_case_id);
  if (has_default_stmt) {
    Instruction *jmp_def_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_def_instr->dest, mk_def_label(switch_->jump_id));
    int status = liter_push_back(body->last_instr, &switch_->last_instr, 4,
                                 jmp_end_instr, dummy_case_label, jmp_def_instr,
                                 end_label);
    if (status) abort();
  } else {
    int status = liter_push_back(body->last_instr, &switch_->last_instr, 3,
                                 jmp_end_instr, dummy_case_label, end_label);
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

  Instruction *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(while_->jump_id);
  /* set first instr to label */
  int status = liter_push_front(condition->first_instr, &while_->first_instr, 1,
                                condition_label);
  if (status) abort();

  scalar_conversions(condition, condition->type);
  Instruction *condition_instr = liter_get(condition->last_instr);
  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->src = test_instr->dest = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(while_->jump_id);

  Instruction *test_jmp_instr = instr_init(OP_JZ);
  set_op_dir(&test_jmp_instr->dest, end_label->label);
  status = liter_push_back(condition->last_instr, NULL, 2, test_instr,
                           test_jmp_instr);
  if (status) abort();

  Instruction *cond_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_instr->dest, condition_label->label);
  /* set last instr to end label */
  status = liter_push_back(body->last_instr, &while_->last_instr, 2,
                           cond_jmp_instr, end_label);
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

  Instruction *condition_start_instr = liter_get(condition->first_instr);
  condition_start_instr->label = mk_cond_label(for_->jump_id);

  /* because of the way `liter_push_back` works, new instructions should be
   * added after the condition in reverse order
   */
  assert(reinitializer->first_instr != NULL &&
         reinitializer->last_instr != NULL);

  /* add unconditional jump to function body, skipping reinitializer */
  Instruction *body_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&body_jmp_instr->dest, mk_stmt_label(for_->jump_id));
  int status = liter_push_back(condition->last_instr, NULL, 1, body_jmp_instr);
  if (status) abort();

  /* emit loop exit jump if condition is not empty */
  if (condition->tok_kind != TOK_EMPTY) {
    scalar_conversions(condition, condition->type);
    Instruction *condition_instr = liter_get(condition->last_instr);
    Instruction *test_instr = instr_init(OP_TEST);
    test_instr->dest = test_instr->src = condition_instr->dest;
    test_instr->persist_flags |= PERSIST_DEST_SET;

    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, mk_end_label(for_->jump_id));
    status = liter_push_back(condition->last_instr, NULL, 2, test_instr,
                             test_jmp_instr);
    if (status) abort();
  }

  Instruction *reinit_start_instr = liter_get(reinitializer->first_instr);
  reinit_start_instr->label = mk_reinit_label(for_->jump_id);
  Instruction *cond_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_instr->dest, mk_cond_label(for_->jump_id));
  status = liter_push_back(reinitializer->last_instr, NULL, 1, cond_jmp_instr);
  if (status) abort();

  Instruction *body_label = instr_init(OP_NONE);
  body_label->label = mk_stmt_label(for_->jump_id);
  status = liter_push_front(body->first_instr, NULL, 1, body_label);
  if (status) abort();
  Instruction *reinit_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&reinit_jmp_instr->dest, mk_reinit_label(for_->jump_id));
  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(for_->jump_id);
  status = liter_push_back(body->last_instr, &for_->last_instr, 2,
                           reinit_jmp_instr, end_label);
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

  Instruction *body_label = instr_init(OP_NONE);
  body_label->label = mk_stmt_label(do_->jump_id);
  int status =
      liter_push_front(body->first_instr, &do_->first_instr, 1, body_label);

  Instruction *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(do_->jump_id);
  status = liter_push_front(condition->first_instr, NULL, 1, condition_label);
  if (status) abort();

  scalar_conversions(condition, condition->type);
  Instruction *condition_instr = liter_get(condition->last_instr);
  if (condition_instr == NULL) abort();

  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *test_jmp_instr = instr_init(OP_JNZ);
  set_op_dir(&test_jmp_instr->dest, body_label->label);

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(do_->jump_id);
  status = liter_push_back(condition->last_instr, &do_->last_instr, 3,
                           test_instr, test_jmp_instr, end_label);

  if (saved_break != SIZE_MAX) state_push_break_id(state, saved_break);
  if (saved_continue != SIZE_MAX) state_push_continue_id(state, saved_continue);

  return astree_adopt(do_, 2, body, condition);
}

ASTree *translate_block(ASTree *block) {
  PFDBG0('g', "Translating compound statement");
  ASTree *first_stmt;
  size_t i, stmt_count = astree_count(block);
  for (i = 0, first_stmt = astree_get(block, i);
       i < stmt_count && first_stmt != NULL && first_stmt->first_instr == NULL;
       first_stmt = astree_get(block, ++i))
    ;

  /* emit nop if block contains no instructions */
  if (first_stmt == NULL || first_stmt->first_instr == NULL) {
    Instruction *nop_instr = instr_init(OP_NOP);
    int status = llist_push_back(instructions, nop_instr);
    if (status) abort();
    block->first_instr = llist_iter_last(instructions);
    if (block->first_instr == NULL) abort();
    block->last_instr = liter_copy(block->first_instr);
    if (block->last_instr == NULL) abort();
    return block;
  }

  ASTree *last_stmt;
  for (i = 1, last_stmt = astree_get(block, stmt_count - i);
       i <= stmt_count && last_stmt != NULL && last_stmt->last_instr == NULL;
       last_stmt = astree_get(block, stmt_count - (++i)))
    ;

  assert(last_stmt != NULL && last_stmt->last_instr != NULL);

  block->first_instr = liter_copy(first_stmt->first_instr);
  if (block->first_instr == NULL) abort();
  block->last_instr = liter_copy(last_stmt->last_instr);
  if (block->last_instr == NULL) abort();
  return block;
}

static void return_scalar(ASTree *ret, ASTree *expr) {
  ret->first_instr = liter_copy(expr->first_instr);
  if (ret->first_instr == NULL) abort();
  Symbol *function_symbol = state_get_function(state);
  const Type *function_type = function_symbol->type;
  /* strip function */
  Type *return_type = type_strip_declarator(function_type);
  scalar_conversions(expr, return_type);
  Instruction *expr_instr = liter_get(expr->last_instr);

  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = expr_instr->dest;
  set_op_reg(&mov_instr->dest, type_get_width(return_type), RAX_VREG);
  mov_instr->persist_flags |= PERSIST_SRC_SET;
  int status = llist_push_back(instructions, mov_instr);
  if (status) abort();

  restore_preserved_regs();
  Instruction *ret_instr = instr_init(OP_RET);
  status = llist_push_back(instructions, ret_instr);
  if (status) abort();
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) abort();
}

static void return_aggregate(ASTree *ret, ASTree *expr) {
  ret->first_instr = liter_copy(expr->first_instr);
  if (ret->first_instr == NULL) abort();
  Instruction *expr_instr = liter_get(expr->last_instr);
  size_t expr_eightbytes = type_get_eightbytes(expr->type);

  if (expr_eightbytes <= 2) {
    ListIter *temp = llist_iter_last(instructions);
    /* `bulk_mtor` should handle persistence flags */
    bulk_mtor(RETURN_REGS, expr_instr->dest.reg.num, NO_DISP, expr->type, temp);
    free(temp);
  } else {
    Instruction *hidden_mov_instr = instr_init(OP_MOV);
    set_op_reg(&hidden_mov_instr->dest, REG_QWORD, RAX_VREG);
    set_op_ind(&hidden_mov_instr->src, HIDDEN_PARAM_DISP, RBP_VREG);
    int status = llist_push_back(instructions, hidden_mov_instr);
    if (status) abort();
    ListIter *temp = llist_iter_last(instructions);
    bulk_mtom(RAX_VREG, expr_instr->dest.reg.num, expr->type, temp);
    free(temp);
  }

  if (ret->first_instr == NULL) abort();
  restore_preserved_regs();
  Instruction *ret_instr = instr_init(OP_RET);
  int status = llist_push_back(instructions, ret_instr);
  if (status) abort();
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) abort();
}

static void return_void(ASTree *ret) {
  Instruction *nop_instr = instr_init(OP_NOP);
  int status = llist_push_back(instructions, nop_instr);
  if (status) abort();
  ret->first_instr = llist_iter_last(instructions);
  if (ret->first_instr == NULL) abort();
  restore_preserved_regs();
  Instruction *ret_instr = instr_init(OP_RET);
  status = llist_push_back(instructions, ret_instr);
  if (status) abort();
  ret->last_instr = llist_iter_last(instructions);
  if (ret->last_instr == NULL) abort();
}

ASTree *translate_return(ASTree *ret, ASTree *expr) {
  PFDBG0('g', "Translating return statement");
  if (type_is_void(expr->type)) {
    return_void(ret);
  } else if (type_is_union(expr->type) || type_is_struct(expr->type)) {
    return_aggregate(ret, expr);
  } else {
    return_scalar(ret, expr);
  }
  return astree_adopt(ret, 1, expr);
}

ASTree *translate_continue(ASTree *continue_) {
  Instruction *cond_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_instr->dest, mk_cond_label(continue_->jump_id));
  int status = llist_push_back(instructions, cond_jmp_instr);
  if (status) abort();
  continue_->first_instr = llist_iter_last(instructions);
  if (continue_->first_instr == NULL) abort();
  continue_->last_instr = liter_copy(continue_->first_instr);
  if (continue_->last_instr == NULL) abort();
  return continue_;
}

ASTree *translate_break(ASTree *break_) {
  Instruction *end_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&end_jmp_instr->dest, mk_end_label(break_->jump_id));
  int status = llist_push_back(instructions, end_jmp_instr);
  if (status) abort();
  break_->first_instr = llist_iter_last(instructions);
  if (break_->first_instr == NULL) abort();
  break_->last_instr = liter_copy(break_->first_instr);
  if (break_->last_instr == NULL) abort();
  return break_;
}

ASTree *translate_goto(ASTree *goto_, ASTree *ident) {
  Instruction *jmp_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_instr->dest, mk_local_label(ident->lexinfo));
  int status = llist_push_back(instructions, jmp_instr);
  if (status) abort();
  goto_->first_instr = llist_iter_last(instructions);
  if (goto_->first_instr == NULL) abort();
  goto_->last_instr = liter_copy(goto_->first_instr);
  if (goto_->last_instr == NULL) abort();
  return astree_adopt(goto_, 1, ident);
}

ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt) {
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = mk_local_label(ident->lexinfo);

  int status =
      liter_push_front(stmt->first_instr, &label->first_instr, 1, label_instr);
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

  scalar_conversions(expr, control_type);
  Instruction *expr_instr = liter_get(expr->last_instr);
  assert(expr_instr != NULL && expr_instr->dest.all.mode == MODE_REGISTER);

  Instruction *test_instr = instr_init(OP_TEST);
  set_op_reg(&test_instr->dest, type_get_width(control_type),
             state_get_control_reg(state));
  test_instr->src = expr_instr->dest;
  test_instr->persist_flags = PERSIST_DEST_SET | PERSIST_SRC_SET;

  Instruction *jmp_instr = instr_init(OP_JNE);
  /* NOTE: because of the way case ids are used, they do not require special
   * handling like jump, continue, and selection ids do
   */
  /* jump to next case statement if condition is false */
  set_op_dir(&jmp_instr->dest,
             mk_case_label(case_->jump_id, case_->case_id + 1));
  Instruction *fall_label = instr_init(OP_NONE);
  fall_label->label = mk_fallthru_label(case_->jump_id, case_->case_id);
  Instruction *case_label = instr_init(OP_NONE);
  case_label->label = mk_case_label(case_->jump_id, case_->case_id);
  Instruction *fall_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&fall_jmp_instr->dest, fall_label->label);
  int status = liter_push_front(stmt->first_instr, &case_->first_instr, 5,
                                fall_jmp_instr, case_label, test_instr,
                                jmp_instr, fall_label);
  if (status) abort();
  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *translate_default(ASTree *default_, ASTree *stmt) {
  Instruction *def_label = instr_init(OP_NONE);
  def_label->label = mk_def_label(default_->jump_id);

  int status =
      liter_push_front(stmt->first_instr, &default_->first_instr, 1, def_label);
  if (status) abort();
  default_->last_instr = liter_copy(stmt->last_instr);
  if (default_->last_instr == NULL) abort();
  return astree_adopt(default_, 1, stmt);
}

void translate_static_scalar_init(const Type *type, ASTree *initializer,
                                  ListIter *where) {
  assert(initializer->first_instr == NULL && initializer->last_instr == NULL);
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
  Instruction *instr = instr_init(directive);
  if (initializer->constant.label != NULL) {
    set_op_pic(&instr->dest, initializer->constant.integral.signed_value,
               initializer->constant.label);
  } else if (type_is_unsigned(type)) {
    set_op_imm(&instr->dest, initializer->constant.integral.unsigned_value, 1);
  } else {
    set_op_imm(&instr->dest, initializer->constant.integral.signed_value, 0);
  }
  int status = liter_push_back(where, &where, 1, instr);
  if (status) abort();

  if (state_get_function(state) != NULL) {
    Instruction *nop_instr = instr_init(OP_NOP);
    status = llist_push_back(instructions, nop_instr);
    if (status) abort();
    initializer->first_instr = llist_iter_last(instructions);
    if (initializer->first_instr == NULL) abort();
    initializer->last_instr = liter_copy(initializer->first_instr);
    if (initializer->last_instr == NULL) abort();
  } else {
    initializer->first_instr = liter_copy(where);
    if (initializer->first_instr == NULL) abort();
    initializer->last_instr = liter_copy(where);
    if (initializer->last_instr == NULL) abort();
  }
}

void translate_auto_scalar_init(const Type *type, ptrdiff_t disp,
                                ASTree *initializer, ListIter *where) {
  assert(initializer->first_instr == NULL && initializer->last_instr == NULL);
  assert((initializer->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT);
  Instruction *load_instr;
  if (initializer->constant.label != NULL) {
    load_instr = instr_init(OP_LEA);
    set_op_pic(&load_instr->src, initializer->constant.integral.signed_value,
               initializer->constant.label);
  } else if (type_is_unsigned(type)) {
    load_instr = instr_init(OP_MOV);
    set_op_imm(&load_instr->src, initializer->constant.integral.unsigned_value,
               0);
  } else {
    load_instr = instr_init(OP_MOV);
    set_op_imm(&load_instr->src, initializer->constant.integral.signed_value,
               1);
  }
  set_op_reg(&load_instr->dest, type_get_width(type), next_vreg());
  Instruction *store_instr = instr_init(OP_MOV);
  store_instr->src = load_instr->dest;
  set_op_ind(&store_instr->dest, disp, RBP_VREG);

  int status = liter_push_back(where, &where, 2, load_instr, store_instr);
  if (status) abort();
  initializer->first_instr = liter_prev(where, 1);
  if (initializer->first_instr == NULL) abort();
  initializer->last_instr = liter_copy(where);
  if (initializer->last_instr == NULL) abort();
}

void translate_static_literal_init(const Type *arr_type, ASTree *literal,
                                   ListIter *where) {
  assert(literal->first_instr == NULL && literal->last_instr == NULL);

  if (state_get_function(state) != NULL) {
    Instruction *nop_instr = instr_init(OP_NOP);
    int status = llist_push_back(instructions, nop_instr);
    if (status) abort();
    literal->first_instr = llist_iter_last(instructions);
    assert(literal->first_instr != NULL);
    literal->last_instr = liter_copy(literal->first_instr);
    assert(literal->last_instr != NULL);
  }

  /* TODO(Robert): use a map here. this is ugly. */
  size_t i;
  for (i = 0; i < literals_size; ++i) {
    if (literals[i].label == literal->constant.label) {
      const char *str = literals[i].literal;
      size_t arr_width = type_get_width(arr_type);
      size_t literal_length = strlen(str) - 2;
      if (type_is_deduced_array(arr_type)) {
        assert(arr_width == literal_length + 1);
        Instruction *asciz_instr = instr_init(OP_ASCIZ);
        set_op_dir(&asciz_instr->dest, str);
        int status = liter_push_back(where, &where, 1, asciz_instr);
        if (status) abort();

        if (state_get_function(state) == NULL) {
          literal->first_instr = liter_copy(where);
          assert(literal->first_instr != NULL);
          literal->last_instr = liter_copy(where);
          assert(literal->last_instr != NULL);
        }
        return;
      } else if (literal_length <= arr_width) {
        Instruction *ascii_instr = instr_init(OP_ASCII);
        set_op_dir(&ascii_instr->dest, str);
        int status = liter_push_back(where, &where, 1, ascii_instr);
        if (status) abort();
        size_t zero_count = arr_width - literal_length;
        if (zero_count > 0) static_zero_pad(zero_count, where);
        if (state_get_function(state) == NULL) {
          literal->first_instr = liter_copy(where);
          assert(literal->first_instr != NULL);
          literal->last_instr = liter_copy(where);
          assert(literal->last_instr != NULL);
        }
        return;
      } else {
        abort();
      }
    }
  }
  /* literal not found */
  abort();
}

void translate_auto_literal_init(const Type *arr_type, ptrdiff_t arr_disp,
                                 ASTree *literal, ListIter *where) {
  assert(literal->first_instr == NULL && literal->last_instr == NULL);
  Instruction *literal_lea_instr = instr_init(OP_LEA);
  set_op_pic(&literal_lea_instr->src, literal->constant.integral.signed_value,
             literal->constant.label);
  set_op_reg(&literal_lea_instr->dest, REG_QWORD, next_vreg());
  /* need to clear persistence because `bulk_mtom` always sets it */
  literal_lea_instr->persist_flags |= PERSIST_DEST_CLEAR;

  Instruction *arr_lea_instr = instr_init(OP_LEA);
  set_op_ind(&arr_lea_instr->src, arr_disp, RBP_VREG);
  set_op_reg(&arr_lea_instr->dest, REG_QWORD, next_vreg());
  /* need to clear persistence because `bulk_mtom` always sets it */
  arr_lea_instr->persist_flags |= PERSIST_DEST_CLEAR;

  int status =
      liter_push_back(where, &where, 2, literal_lea_instr, arr_lea_instr);
  if (status) abort();
  literal->first_instr = liter_prev(where, 1);
  if (literal->first_instr == NULL) abort();

  size_t arr_width = type_get_width(arr_type);
  size_t literal_width = type_get_width(literal->type);
  liter_advance(where, 1);
  ListIter *temp = liter_prev(where, 1);
  /* we need to know where the last instruction was inserted */
  bulk_mtom(arr_lea_instr->dest.reg.num, literal_lea_instr->dest.reg.num,
            (arr_width > literal_width) ? literal->type : arr_type, temp);
  if (arr_width > literal_width)
    bulk_mzero(arr_lea_instr->dest.reg.num, NO_DISP, literal_width, arr_type,
               temp);
  liter_advance(where, -1);
  free(temp);
  if (status) abort();
  literal->last_instr = liter_copy(where);
  if (literal->last_instr == NULL) abort();
}

int translate_static_prelude(ASTree *declarator, Symbol *symbol,
                             ListIter *where, int is_initialized) {
  const char *identifier =
      symbol->linkage == LINK_NONE
          ? mk_static_label(declarator->lexinfo, symbol->static_id)
          : declarator->lexinfo;

  Instruction *section_instr;
  if (type_is_const(symbol->type)) {
    section_instr = instr_init(OP_SECTION);
    set_op_dir(&section_instr->dest, ".rodata");
  } else if (is_initialized) {
    section_instr = instr_init(OP_DATA);
  } else {
    section_instr = instr_init(OP_BSS);
  }

  Instruction *align_instr = instr_init(OP_ALIGN);
  set_op_imm(&align_instr->dest, type_get_alignment(declarator->type),
             IMM_UNSIGNED);
  Instruction *type_instr = instr_init(OP_TYPE);
  set_op_dir(&type_instr->dest, identifier);
  set_op_dir(&type_instr->src, "@object");
  Instruction *size_instr = instr_init(OP_SIZE);
  set_op_dir(&size_instr->dest, identifier);
  set_op_imm(&size_instr->src, type_get_width(declarator->type), IMM_UNSIGNED);
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = identifier;

  /* this call to `liter_push_back` should mutate `where` in-place, since the
   * output parameter points to the input parameter
   */
  if (symbol->linkage == LINK_EXT) {
    Instruction *globl_instr = instr_init(OP_GLOBL);
    set_op_dir(&globl_instr->dest, declarator->lexinfo);

    return liter_push_back(where, &where, 6, globl_instr, section_instr,
                           align_instr, type_instr, size_instr, label_instr);
  } else {
    return liter_push_back(where, &where, 5, section_instr, align_instr,
                           type_instr, size_instr, label_instr);
  }
}

static void translate_static_local_init(ASTree *assignment, ASTree *declarator,
                                        ASTree *initializer) {
  (void)assignment;
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, (char *)declarator->lexinfo,
                         strlen(declarator->lexinfo), &symbol);
  assign_static_space(declarator->lexinfo, symbol);
  ListIter *temp = liter_copy(before_definition);
  (void)traverse_initializer(declarator->type, symbol->disp, initializer,
                             before_definition);

  /* wait to do this so that deduced array sizes are set */
  int status = translate_static_prelude(declarator, symbol, temp, 1);
  if (status) abort();
  free(temp);

  assert(declarator->first_instr == NULL && declarator->last_instr == NULL);
  assert(assignment->first_instr == NULL && assignment->last_instr == NULL);
  assert(initializer->first_instr != NULL && initializer->last_instr != NULL);

  assignment->first_instr = liter_copy(initializer->first_instr);
  if (assignment->first_instr == NULL) abort();
  assignment->last_instr = liter_copy(initializer->last_instr);
  if (assignment->last_instr == NULL) abort();
}

/* TODO(Robert): merge with `translate_auto_scalar_init` */
static void translate_auto_local_init(ASTree *assignment, ASTree *declarator,
                                      ASTree *initializer) {
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, (char *)declarator->lexinfo,
                         strlen(declarator->lexinfo), &symbol);
  symbol->disp = assign_stack_space(symbol->type);
  maybe_load_cexpr(initializer, NULL);

  Instruction *lea_instr = instr_init(OP_LEA);
  set_op_ind(&lea_instr->src, symbol->disp, RBP_VREG);
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  lea_instr->persist_flags |= PERSIST_DEST_CLEAR;

  /* object initialized is loaded before the initializer is computed to mirror
   * the way assignment operations look in assembly
   */
  int status = liter_push_front(initializer->first_instr,
                                &declarator->first_instr, 1, lea_instr);
  if (status) abort();
  declarator->last_instr = liter_copy(declarator->first_instr);
  if (declarator->last_instr == NULL) abort();

  (void)translate_assignment(assignment, declarator, initializer);
}

static ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                                    ASTree *declarator, ASTree *initializer) {
  PFDBG0('g', "Translating local initialization");
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope && symbol);

  if (symbol->storage == STORE_STAT) {
    translate_static_local_init(assignment, declarator, initializer);
    return declaration;
  } else if (initializer->tok_kind != TOK_INIT_LIST &&
             initializer->tok_kind != TOK_STRINGCON &&
             (initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    translate_auto_local_init(assignment, declarator, initializer);
    return declaration;
  } else {
    symbol->disp = assign_stack_space(symbol->type);

    ListIter *temp = llist_iter_last(instructions);
    (void)traverse_initializer(declarator->type, symbol->disp, initializer,
                               temp);
    free(temp);

    assert(declarator->first_instr == NULL && declarator->last_instr == NULL);
    assert(initializer->first_instr != NULL && initializer->last_instr != NULL);
    assignment->first_instr = liter_copy(initializer->first_instr);
    assert(assignment->first_instr != NULL);
    assignment->last_instr = liter_copy(initializer->last_instr);
    assert(assignment->last_instr != NULL);
    return declaration;
  }
}

static ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating local declaration");
  assert(declarator->first_instr == NULL && declarator->last_instr == NULL);
  Instruction *nop_instr = instr_init(OP_NOP);
  int status = llist_push_back(instructions, nop_instr);
  if (status) abort();
  declarator->first_instr = llist_iter_last(instructions);
  if (declarator->first_instr == NULL) abort();
  declarator->last_instr = llist_iter_last(instructions);
  if (declarator->last_instr == NULL) abort();

  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(symbol && in_current_scope);

  if (type_is_function(declarator->type) || symbol->info == SYM_INHERITOR ||
      declarator->tok_kind == TOK_TYPE_NAME || symbol->linkage == LINK_EXT ||
      symbol->linkage == LINK_TYPEDEF) {
    return declaration;
  } else if (symbol->storage == STORE_STAT) {
    assign_static_space(declarator->lexinfo, symbol);
    int status =
        translate_static_prelude(declarator, symbol, before_definition, 0);
    if (status) abort();
  } else if (symbol->storage == STORE_AUTO) {
    symbol->disp = assign_stack_space(symbol->type);
  }

  return declaration;
}

ASTree *translate_local_declarations(ASTree *block, ASTree *declarations) {
  /* skip typespec list */
  size_t i, decl_count = astree_count(declarations);
  for (i = 1; i < decl_count; ++i) {
    ASTree *declaration = astree_get(declarations, i);
    if (declaration->tok_kind == TOK_IDENT) {
      (void)translate_local_decl(declarations, declaration);
    } else if (declaration->tok_kind == '=') {
      ASTree *declarator = astree_get(declaration, 0),
             *initializer = astree_get(declaration, 1);
      translate_local_init(declarations, declaration, declarator, initializer);
    } else {
      abort();
    }
  }

  declarations->first_instr =
      liter_copy(astree_get(declarations, 1)->first_instr);
  assert(declarations->first_instr != NULL);
  declarations->last_instr =
      liter_copy(astree_get(declarations, decl_count - 1)->last_instr);
  assert(declarations->last_instr != NULL);
  return astree_adopt(block, 1, declarations);
}

static ASTree *translate_global_init(ASTree *declaration, ASTree *declarator,
                                     ASTree *initializer) {
  PFDBG0('g', "Translating global initialization");
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope && symbol);

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  ListIter *temp = liter_copy(before_definition);

  (void)traverse_initializer(declarator->type, NO_DISP, initializer,
                             before_definition);
  /* wait to do this so that deduced array sizes are set */
  int status = translate_static_prelude(declarator, symbol, temp, 1);
  free(temp);
  if (status) abort();
  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return declaration;
}

static ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating global declaration");
  assert(declarator->tok_kind == TOK_IDENT);
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, (char *)declarator->lexinfo,
                                          strlen(declarator->lexinfo), &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope && symbol);

  if (symbol->storage == STORE_EXT || type_is_function(declarator->type)) {
    return declaration;
  } else if (symbol->storage == STORE_STAT) {
    ListIter *temp = llist_iter_last(instructions);
    int status = translate_static_prelude(declarator, symbol, temp, 0);
    free(temp);
    if (status) abort();
    Instruction *zero_instr = instr_init(OP_ZERO);
    set_op_imm(&zero_instr->dest, type_get_width(declarator->type),
               IMM_UNSIGNED);
    status = llist_push_back(instructions, zero_instr);
    if (status) abort();
  } else if (symbol->storage != STORE_TYPEDEF) {
    abort();
  }

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  return declaration;
}

ASTree *translate_global_declarations(ASTree *root, ASTree *declarations) {
  if (astree_count(declarations) == 3 &&
      astree_get(declarations, 2)->tok_kind == TOK_BLOCK)
    /* function defnition; no further instructions to emit */
    return astree_adopt(root, 1, declarations);
  else if (astree_count(declarations) == 2 &&
           astree_get(declarations, 1)->tok_kind == TOK_TYPE_NAME)
    /* declares nothing; emit no instructions */
    return astree_adopt(root, 1, declarations);

  /* skip typespec list */
  size_t i, decl_count = astree_count(declarations);
  for (i = 1; i < decl_count; ++i) {
    ASTree *declaration = astree_get(declarations, i);
    if (declaration->tok_kind == TOK_IDENT) {
      (void)translate_global_decl(declarations, declaration);
    } else if (declaration->tok_kind == '=') {
      ASTree *declarator = astree_get(declaration, 0),
             *initializer = astree_get(declaration, 1);
      translate_global_init(declarations, declarator, initializer);
    } else {
      abort();
    }
  }
  return astree_adopt(root, 1, declarations);
}

ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body) {
  PFDBG0('g', "Translating function prologue");
  ++fn_count;
  window_size = INIT_WINDOW_SIZE;
  Instruction *text_instr = instr_init(OP_TEXT);
  int status = llist_push_back(instructions, text_instr);
  if (status) abort();
  declaration->first_instr = llist_iter_last(instructions);
  if (declaration->first_instr == NULL) abort();
  Symbol *symbol = NULL;
  state_get_symbol(state, declarator->lexinfo, strlen(declarator->lexinfo),
                   &symbol);
  assert(symbol != NULL);
  assert(symbol->info != SYM_HIDDEN);
  if (symbol->linkage == LINK_EXT) {
    Instruction *globl_instr = instr_init(OP_GLOBL);
    set_op_dir(&globl_instr->dest, declarator->lexinfo);
    status = llist_push_back(instructions, globl_instr);
    if (status) abort();
  }
  Instruction *type_instr = instr_init(OP_TYPE);
  set_op_dir(&type_instr->dest, declarator->lexinfo);
  set_op_dir(&type_instr->src, "@function");
  status = llist_push_back(instructions, type_instr);
  if (status) abort();
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = declarator->lexinfo;
  status = llist_push_back(instructions, label_instr);

  save_preserved_regs();

  /* save location for later rsp adjustment */
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  translate_params(declarator);
  return astree_adopt(declaration, 2, declarator, body);
}

ASTree *end_translate_fn(ASTree *declaration) {
  /* emit rsp adjustment; set to bogus value initially since we don't know how
   * many bytes the register allocator will spill yet */
  Instruction *rsp_sub_instr = instr_init(OP_SUB);
  set_op_reg(&rsp_sub_instr->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_sub_instr->src, PTRDIFF_MAX, IMM_UNSIGNED);
  int status = liter_push_back(declaration->last_instr, NULL, 1, rsp_sub_instr);
  if (status) abort();
  free(declaration->last_instr);

  restore_preserved_regs();
  Instruction *return_instr = instr_init(OP_RET);
  status = llist_push_back(instructions, return_instr);
  if (status) abort();
  ASTree *declarator = astree_get(declaration, 1);
  Instruction *size_instr = instr_init(OP_SIZE);
  set_op_dir(&size_instr->dest, declarator->lexinfo);
  set_op_dir(&size_instr->src, mk_fn_size(declarator->lexinfo));
  status = llist_push_back(instructions, size_instr);
  if (status) abort();
  declaration->last_instr = llist_iter_last(instructions);
  if (declaration->last_instr == NULL) abort();

  free(before_definition);
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();

  if (skip_liveness) goto no_live;
  liveness_sr(declaration->first_instr, declaration->last_instr);
  if (skip_allocator) goto no_alloc;
  /* this function will adjust window_size to account for spilled bytes */
  allocate_regs(declaration->first_instr, declaration->last_instr);
no_live:;
no_alloc:;

  /* align to ensure stack alignment to 16x + 8 */
  size_t window_padding = (window_size % 16 > 0) ? 16 - (window_size % 16) : 0;
  assert(window_padding <= PTRDIFF_MAX);
  window_size += window_padding;

  /* set rsp adjustment to its actual value */
  rsp_sub_instr->src.imm.val = window_size;
  return declaration;
}

int generator_print_il(FILE *out) {
  static char buffer[MAX_INSTR_LENGTH];
  size_t i;
  for (i = 0; i < llist_size(instructions); ++i) {
    Instruction *instr = llist_get(instructions, i);
    int chars_written = instr_to_str(instr, buffer);
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
    Instruction *instr = llist_get(instructions, i);
    int chars_written = instr_debug(instr, buffer);
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
  int status = llist_init(instructions, free, NULL);
  if (status) abort();
  Instruction *file_instr = instr_init(OP_FILE);
  set_op_dir(&file_instr->dest, filename);
  status = llist_push_back(instructions, file_instr);
  if (status) abort();
  before_definition = llist_iter_last(instructions);
  if (before_definition == NULL) abort();
  literals = malloc(sizeof(*literals) * literals_cap);
  static_locals = malloc(sizeof(*static_locals));
  status =
      map_init(static_locals, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  if (status) abort();
  generated_text = malloc(sizeof(*generated_text));
  status =
      map_init(generated_text, DEFAULT_MAP_SIZE, NULL, free, strncmp_wrapper);
  if (status) abort();

  static Tag tag_va_spill_region = {X64_SIZEOF_LONG * PARAM_REG_COUNT,
                                    X64_ALIGNOF_LONG,
                                    {{NULL, BLIB_LLIST_EMPTY}},
                                    TAG_STRUCT,
                                    1};
  static Type type_va_spill_region = {{TYPE_CODE_STRUCT, 0}};
  type_va_spill_region.tag.value = &tag_va_spill_region;
  TYPE_VA_SPILL_REGION = &type_va_spill_region;
}

void asmgen_free_globals(void) {
  free(before_definition);

  int status = llist_destroy(instructions);
  if (status) abort();
  free(literals);

  status = map_destroy(static_locals);
  if (status) abort();
  free(static_locals);

  status = map_destroy(generated_text);
  if (status) abort();
  free(generated_text);
}
