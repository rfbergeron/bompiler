#include "asmgen.h"

#include <limits.h>
#include <stdarg.h>
#include <string.h>

#include "arrlist.h"
#include "assert.h"
#include "astree.h"
#include "debug.h"
#include "lyutils.h"
#include "regalloc.h"
#include "scope.h"
#include "state.h"
#include "strset.h"
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
#define LABEL_BUFFER_SIZE ((size_t)1024)
#define NO_DISP 0
#define IMM_SIGNED 1
#define IMM_UNSIGNED 0
#define MAX_IDENT_LEN 31
#define MAX_INSTR_DEBUG_LENGTH 4096
#define MAX_OPERAND_DEBUG_LENGTH 1024
#define DEFAULT_MAP_SIZE 100

static const char LOCAL_FMT[] = ".L%lu$%s";
static const char STATIC_FMT[] = "%s.%lu";
static const char COND_FMT[] = ".LC%lu";
static const char END_FMT[] = ".LE%lu";
static const char STMT_FMT[] = ".LS%lu";
static const char FOR_FMT[] = ".LR%lu";
static const char TRUE_FMT[] = ".LT%lu";
static const char FALSE_FMT[] = ".LF%lu";
static const char DEF_FMT[] = ".LD%lu";
static const char CASE_FMT[] = ".LS%luC%lu";
static const char FALL_FMT[] = ".LS%luF%lu";
static const char STR_FMT[] = ".LSTR%lu";
static const char FN_SIZE_FMT[] = ".-%s";
static const ptrdiff_t PROLOGUE_EIGHTBYTES = 2;
static const ptrdiff_t FP_OFFSET = 304;
static const ptrdiff_t GP_OFFSET_MAX = 48;
static const ptrdiff_t GP_OFFSET_MEMBER_DISP = NO_DISP;
static const ptrdiff_t FP_OFFSET_MEMBER_DISP = X64_SIZEOF_INT;
static const ptrdiff_t OVERFLOW_ARG_AREA_MEMBER_DISP = 2 * X64_SIZEOF_INT;
static const ptrdiff_t REG_SAVE_AREA_MEMBER_DISP =
    2 * X64_SIZEOF_INT + X64_SIZEOF_LONG;
/* reserve 8 bytes for function call return values, 8 bytes for hidden
 * parameter storage, and 8 bytes each for the first, second and third register
 * unspill regions, repsectively
 */
static const ptrdiff_t RETURN_VAL_DISP = -8 * (PRESERVED_REG_COUNT + 1);
static const ptrdiff_t HIDDEN_PARAM_DISP = -8 * (PRESERVED_REG_COUNT + 2);
static const ptrdiff_t INIT_WINDOW_SIZE = 40;
static const ptrdiff_t VA_INIT_WINDOW_SIZE = 88;
/* locations of 8-byte regions for contents of unspilled registers */
const ptrdiff_t UNSPILL_REGIONS[] = {-8 * (PRESERVED_REG_COUNT + 5),
                                     -8 * (PRESERVED_REG_COUNT + 4),
                                     -8 * (PRESERVED_REG_COUNT + 3)};
const size_t UNSPILL_REGIONS_SIZE = ARRAY_ELEM_COUNT(UNSPILL_REGIONS);
const ptrdiff_t VA_SPILL_REGIONS[] = {
    -8 * (PRESERVED_REG_COUNT + 11), -8 * (PRESERVED_REG_COUNT + 10),
    -8 * (PRESERVED_REG_COUNT + 9),  -8 * (PRESERVED_REG_COUNT + 8),
    -8 * (PRESERVED_REG_COUNT + 7),  -8 * (PRESERVED_REG_COUNT + 6)};
static size_t param_reg_index;
static ptrdiff_t param_stack_disp;
ptrdiff_t window_size;

ARR_STAT(const char *, literal_tokens);
ARR_STAT(const char *, literal_labels);
ARR_STAT(ptrdiff_t, spill_regions);

static size_t fn_count;

extern int skip_allocator;
extern int skip_liveness;

#define SEMCHK(tree)                          \
  assert((tree)->cexpr_kind == CEXPR_FALSE && \
         astree_is_lvalue((tree)) == LVAL_FALSE)
#define TYPCHK(left, right) assert(types_equivalent(left, right, 1))
#define WIDCHK(left, right)                         \
  assert((left)->dest.all.mode == MODE_REGISTER &&  \
         (right)->dest.all.mode == MODE_REGISTER && \
         (left)->dest.reg.width == (right)->dest.reg.width)
#define REGCHK(instr) assert((instr)->dest.all.mode == MODE_REGISTER)

const char *mk_generic_label(const char *fmt, size_t unique_id) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, fmt, unique_id);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

#define mk_def_label(id) mk_generic_label(DEF_FMT, id)
#define mk_stmt_label(id) mk_generic_label(STMT_FMT, id)
#define mk_cond_label(id) mk_generic_label(COND_FMT, id)
#define mk_end_label(id) mk_generic_label(END_FMT, id)
#define mk_for_label(id) mk_generic_label(FOR_FMT, id)
#define mk_true_label(id) mk_generic_label(TRUE_FMT, id)
#define mk_false_label(id) mk_generic_label(FALSE_FMT, id)
#define mk_literal_label(id) mk_generic_label(STR_FMT, id)

const char *mk_static_label(const char *name, size_t unique_id) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, STATIC_FMT, name, unique_id);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

const char *mk_fallthru_label(size_t switch_id, size_t case_id) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, FALL_FMT, switch_id, case_id);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

const char *mk_case_label(size_t switch_id, size_t case_id) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, CASE_FMT, switch_id, case_id);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

const char *mk_local_label(const char *name) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, LOCAL_FMT, fn_count, name);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

const char *mk_fn_size(const char *name) {
  char buffer[LABEL_BUFFER_SIZE];
  sprintf(buffer, FN_SIZE_FMT, name);
  assert(strlen(buffer) < LABEL_BUFFER_SIZE);
  return gen_string_intern(buffer);
}

size_t next_vreg(void) {
  static size_t vreg_count = REAL_REG_COUNT;
  return vreg_count++;
}

size_t next_branch(void) {
  static size_t branch_count;
  return branch_count++;
}

size_t next_static_id(void) {
  static size_t static_id_count;
  return static_id_count++;
}

static void bulk_rtom(size_t dest_memreg, ptrdiff_t dest_disp,
                      const size_t *src_regs, const Type *type,
                      Instruction *where) {
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
        set_op_imm(&shr_instr->src, alignment * 8, IMM_UNSIGNED);
        (void)instr_append(where, 2, mov_instr, shr_instr);
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
      (void)instr_append(where, 1, mov_instr);
    }
  }
}

/* NOTE: any bytes not written by this function will contain garbage; this is
 * fine for aggregates but it means that using it to move scalar values will
 * result in zeroes in the high bits for 32-bit values and garbage otherwise
 */
/* NOTE: this function is used exclusively for moving arguments, parameters,
 * and return values. because the register allocator is bad and ignores
 * instructions which have already been assigned a real register, we cannot
 * use virtual registers in this function, because the source and destination
 * registers will be real registers. The `r10` volatile register is already
 * being used if this function is called, so the only safe register is `r11`.
 */
static void bulk_mtor(const size_t *dest_regs, size_t src_memreg,
                      ptrdiff_t src_disp, const Type *type,
                      Instruction *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  if (alignment < 8 && width / alignment > 1) {
    size_t eightbytes = type_get_eightbytes(type);
    size_t i;
    for (i = 0; i < eightbytes; ++i) {
      Instruction *zero_dest_instr = instr_init(OP_XOR);
      set_op_reg(&zero_dest_instr->dest, REG_QWORD, dest_regs[i]);
      zero_dest_instr->src = zero_dest_instr->dest;
      (void)instr_append(where, 1, zero_dest_instr);

      size_t j;
      for (j = 0; j < 8 && i * 8 + j < width; j += alignment) {
        size_t chunk_disp = src_disp + i * 8 + j;

        Instruction *mov_instr = instr_init(OP_MOV);
        set_op_ind(&mov_instr->src, chunk_disp, src_memreg);
        set_op_reg(&mov_instr->dest, alignment, R11_VREG);
        /* persist vregs across basic blocks */
        if (i == 0 && src_memreg >= REAL_REG_COUNT)
          mov_instr->persist_flags |= PERSIST_SRC_SET;
        (void)instr_append(where, 1, mov_instr);

        Instruction *movz_instr;
        if (alignment == X64_ALIGNOF_INT) {
          movz_instr = NULL;
        } else {
          movz_instr = instr_init(OP_MOVZ);
          movz_instr->src = mov_instr->dest;
          set_op_reg(&movz_instr->dest, REG_QWORD, R11_VREG);
          (void)instr_append(where, 1, movz_instr);
        }

        Instruction *shl_instr = instr_init(OP_SHL);
        if (alignment == X64_ALIGNOF_INT)
          set_op_reg(&shl_instr->dest, REG_QWORD, mov_instr->dest.reg.num);
        else
          shl_instr->dest = movz_instr->dest;
        set_op_imm(&shl_instr->src, j * 8, IMM_UNSIGNED);

        Instruction *bitor_instr = instr_init(OP_OR);
        bitor_instr->src = shl_instr->dest;
        set_op_reg(&bitor_instr->dest, REG_QWORD, dest_regs[i]);

        (void)instr_append(where, 2, shl_instr, bitor_instr);
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
      (void)instr_append(where, 1, mov_instr);
    }
  }
}

static void bulk_mtom(size_t dest_reg, size_t src_reg, const Type *type,
                      Instruction *where) {
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
    (void)instr_append(where, 2, mov_instr, mov_instr_2);
  }
}

void bulk_mzero(size_t dest_memreg, ptrdiff_t dest_disp, size_t skip_bytes,
                const Type *type, Instruction *where) {
  size_t alignment = type_get_alignment(type);
  size_t width = type_get_width(type);
  size_t i = skip_bytes;
  Instruction *zero_instr = instr_init(OP_MOV);
  set_op_imm(&zero_instr->src, 0, IMM_UNSIGNED);
  set_op_reg(&zero_instr->dest, alignment, next_vreg());
  (void)instr_append(where, 1, zero_instr);

  while (i < width) {
    ptrdiff_t chunk_disp = i + dest_disp;
    if ((i + dest_disp) % alignment != 0) {
      Instruction *mov_instr = instr_init(OP_MOV);
      set_op_reg(&mov_instr->src, REG_BYTE, zero_instr->dest.reg.num);
      set_op_ind(&mov_instr->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_DEST_SET;
      (void)instr_append(where, 1, mov_instr);
      ++i;
    } else {
      Instruction *mov_instr = instr_init(OP_MOV);
      mov_instr->src = zero_instr->dest;
      set_op_ind(&mov_instr->dest, chunk_disp, dest_memreg);
      if (i == skip_bytes && dest_memreg >= REAL_REG_COUNT)
        mov_instr->persist_flags |= PERSIST_DEST_SET;
      (void)instr_append(where, 1, mov_instr);
      i += alignment;
    }
  }
}

void static_zero_pad(size_t count, Instruction *where) {
  Instruction *zero_instr = instr_init(OP_ZERO);
  set_op_imm(&zero_instr->dest, count, IMM_UNSIGNED);
  (void)instr_append(where, 1, zero_instr);
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

static const char *process_literal(const char *literal) {
  size_t literal_len = strlen(literal), i;
  size_t processed_len = literal_len;
  /* increase length to account for octal sequences */
  for (i = 0; i < literal_len; ++i)
    if (literal[i] == '\\' && (literal[i + 1] == 'a' || literal[i + 1] == 'v'))
      processed_len += 2;
  char *processed = malloc((processed_len + 1) * sizeof(char));
  processed[processed_len] = '\0';

  size_t j;
  for (i = 0, j = 0; i < literal_len; ++i) {
    if (literal[i] == '\\' && literal[i + 1] == 'a') {
      /* insert octal sequence */
      processed[j++] = '\\';
      processed[j++] = '0';
      processed[j++] = '0';
      processed[j++] = '7';
      /* skip escaped character */
      ++i;
    } else if (literal[i] == '\\' && literal[i + 1] == 'v') {
      processed[j++] = '\\';
      processed[j++] = '0';
      processed[j++] = '1';
      processed[j++] = '3';
      ++i;
    } else {
      processed[j++] = literal[i];
    }
  }

  assert(processed[processed_len] == '\0');
  const char *ret = string_set_intern(processed);
  free(processed);
  return ret;
}

const char *asmgen_literal_label(const char *literal) {
  /* TODO(Robert): bad time complexity */
  size_t i;
  for (i = 0; i < ARR_LEN(literal_tokens); ++i)
    if (strcmp(ARR_GET(literal_tokens, i), literal) == 0)
      return ARR_GET(literal_labels, i);

  Instruction *section_instr = instr_init(OP_SECTION);
  set_op_dir(&section_instr->dest, ".rodata");

  /* TODO(Robert): determine when alignment needs to be set, if ever */
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = mk_literal_label(ARR_LEN(literal_labels));

  Instruction *string_instr = instr_init(OP_ASCIZ);
  set_op_dir(&string_instr->dest, process_literal(literal));

  /* append directly to the translation unit's instruction list */
  (void)instr_append(parser_root->instructions, 3, section_instr, label_instr,
                     string_instr);

  ARR_PUSH(literal_tokens, literal);
  ARR_PUSH(literal_labels, label_instr->label);
  return label_instr->label;
}

static ptrdiff_t assign_stack_space(const Type *type) {
  size_t width = type_get_width(type);
  size_t alignment = type_get_alignment(type);
  size_t padding = alignment - (window_size % alignment);
  ptrdiff_t padded_space = width + ((padding == alignment) ? 0 : padding);
  assert(padded_space >= 0);
  assert(window_size >= 0 && PTRDIFF_MAX - window_size >= padded_space);
  window_size += padded_space;
  /* account for saved preserved registers */
  return -(window_size + 8 * PRESERVED_REG_COUNT);
}

static void save_preserved_regs(Instruction *where) {
  Instruction *push_rbp_instr = instr_init(OP_PUSH);
  set_op_reg(&push_rbp_instr->dest, REG_QWORD, RBP_VREG);

  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_reg(&mov_instr->dest, REG_QWORD, RBP_VREG);
  set_op_reg(&mov_instr->src, REG_QWORD, RSP_VREG);

  (void)instr_append(where, 2, push_rbp_instr, mov_instr);

  size_t i;
  for (i = 1; i <= PRESERVED_REG_COUNT; ++i) {
    Instruction *push_instr = instr_init(OP_PUSH);
    set_op_reg(&push_instr->dest, REG_QWORD,
               PRESERVED_REGS[PRESERVED_REG_COUNT - i]);
    (void)instr_append(where, 1, push_instr);
  }
}

static void save_volatile_regs(Instruction *where) {
  size_t i;
  for (i = 1; i <= VOLATILE_REG_COUNT; ++i) {
    Instruction *push_instr = instr_init(OP_PUSH);
    set_op_reg(&push_instr->dest, REG_QWORD,
               VOLATILE_REGS[VOLATILE_REG_COUNT - i]);
    (void)instr_append(where, 1, push_instr);
  }
}

static void restore_preserved_regs(Instruction *where) {
  size_t i;
  for (i = 0; i < PRESERVED_REG_COUNT; ++i) {
    Instruction *pop_instr = instr_init(OP_POP);
    set_op_reg(&pop_instr->dest, REG_QWORD, PRESERVED_REGS[i]);
    (void)instr_append(where, 1, pop_instr);
  }

  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_reg(&mov_instr->dest, REG_QWORD, RSP_VREG);
  set_op_reg(&mov_instr->src, REG_QWORD, RBP_VREG);

  Instruction *pop_rbp_instr = instr_init(OP_POP);
  set_op_reg(&pop_rbp_instr->dest, REG_QWORD, RBP_VREG);

  (void)instr_append(where, 2, mov_instr, pop_rbp_instr);
}

static void restore_volatile_regs(Instruction *where) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
    Instruction *pop_instr = instr_init(OP_POP);
    set_op_reg(&pop_instr->dest, REG_QWORD, VOLATILE_REGS[i]);
    (void)instr_append(where, 1, pop_instr);
  }
}

ASTree *translate_empty_expr(ASTree *empty_expr) {
  assert(instr_empty(empty_expr->instructions));
  Instruction *nop_instr = instr_init(OP_NOP);
  (void)instr_append(empty_expr->instructions, 1, nop_instr);
  return empty_expr;
}

ASTree *translate_cexpr_conv(ASTree *cexpr_conv, ASTree *expr) {
  assert(expr->cexpr_kind != CEXPR_FALSE);
  assert(instr_empty(expr->instructions) || expr->tok_kind == TOK_SIZEOF);
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

  (void)instr_append(cexpr_conv->instructions, 1, load_instr);
  return astree_adopt(cexpr_conv, 1, expr);
}

static Instruction *convert_rval(const Instruction *instr, const Type *type) {
  assert(instr->dest.all.mode == MODE_REGISTER);
  assert(instr->dest.reg.width == X64_SIZEOF_LONG);

  Instruction *mov_instr = instr_init(OP_MOV);
  set_op_ind(&mov_instr->src, NO_DISP, instr->dest.reg.num);
  set_op_reg(&mov_instr->dest, type_get_width(type), next_vreg());
  mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
  return mov_instr;
}

ASTree *translate_rval_conv(ASTree *rval_conv, ASTree *expr) {
  assert(astree_is_lvalue(expr) != LVAL_FALSE);
  assert(expr->cexpr_kind == CEXPR_FALSE);
  assert(!instr_empty(expr->instructions));

  if (type_is_scalar(expr->type))
    (void)instr_append(
        rval_conv->instructions, 2, expr->instructions,
        convert_rval(instr_prev(expr->instructions), expr->type));
  else
    (void)instr_append(rval_conv->instructions, 1, expr->instructions);

  return astree_adopt(rval_conv, 1, expr);
}

ASTree *translate_ptr_conv(ASTree *ptr_conv, ASTree *expr) {
  assert(expr->cexpr_kind == CEXPR_FALSE);
  (void)instr_append(ptr_conv->instructions, 1, expr->instructions);
  return astree_adopt(ptr_conv, 1, expr);
}

/* NOTE: on x64, most operations that write to the lower 32 bits of a
 * register will zero the upper 32 bits.
 *
 * any signed int -> any wider unsigned int: movz
 * any signed int -> any wider signed int: movs
 * any unsigned int -> any wider int: movz
 * any int -> any narrower int: simple mov
 * any int -> any int of same width: nop
 * anything -> void: nop
 *
 * also, this function is poorly named; it can also "convert" aggregates to void
 */
static Instruction *convert_scalar(const Instruction *instr,
                                   const Type *to_type, const Type *from_type) {
  assert((type_is_scalar(from_type) && type_is_scalar(to_type)) ||
         type_is_void(to_type));
  assert(instr->dest.all.mode == MODE_REGISTER);

  size_t from_width = type_get_width(from_type);
  size_t to_width = type_get_width(to_type);

  if (type_is_void(to_type)) {
    return instr_init(OP_NOP);
  } else if (from_width == to_width) {
    Instruction *mov_instr = instr_init(OP_MOV);
    mov_instr->src = mov_instr->dest = instr->dest;
    mov_instr->persist_flags |= PERSIST_SRC_SET;
    return mov_instr;
  } else if (from_width > to_width) {
    /* unnecessary mov so that the width of the destination is set correctly,
     * and the whole structure describing the operand can just be copied to the
     * next instruction that needs it.
     */
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, to_width, instr->dest.reg.num);
    set_op_reg(&mov_instr->dest, to_width, next_vreg());
    mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    return mov_instr;
  } else if (type_is_signed(from_type) || type_is_enum(from_type)) {
    Instruction *movs_instr = instr_init(OP_MOVS);
    movs_instr->src = instr->dest;
    set_op_reg(&movs_instr->dest, to_width, next_vreg());
    movs_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    return movs_instr;
  } else if (to_width != X64_SIZEOF_LONG || from_width != X64_SIZEOF_INT) {
    assert(type_is_unsigned(from_type));
    Instruction *movz_instr = instr_init(OP_MOVZ);
    movz_instr->src = instr->dest;
    set_op_reg(&movz_instr->dest, to_width, next_vreg());
    movz_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    return movz_instr;
  } else {
    /* set width of destination; no need to extend */
    assert(type_is_unsigned(from_type));
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, REG_QWORD, instr->dest.reg.num);
    mov_instr->dest = mov_instr->src;
    mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    return mov_instr;
  }
}

ASTree *translate_scal_conv(ASTree *scal_conv, ASTree *expr) {
  SEMCHK(expr);
  Instruction *conversion = convert_scalar(instr_prev(expr->instructions),
                                           scal_conv->type, expr->type);
  (void)instr_append(scal_conv->instructions, 2, expr->instructions,
                     conversion);
  return astree_adopt(scal_conv, 1, expr);
}

ASTree *translate_disp_conv(ASTree *disp_conv, ASTree *expr,
                            const Type *pointer_type) {
  SEMCHK(expr);
  Instruction *conv_instr = convert_scalar(instr_prev(expr->instructions),
                                           disp_conv->type, expr->type);

  /* use two-operand imul because it is more convenient */
  Instruction *imul_instr = instr_init(OP_IMUL);
  set_op_imm(&imul_instr->src, type_get_elem_width(pointer_type), IMM_SIGNED);
  imul_instr->persist_flags = PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  imul_instr->dest = conv_instr->dest;

  (void)instr_append(disp_conv->instructions, 3, expr->instructions, conv_instr,
                     imul_instr);

  return astree_adopt(disp_conv, 1, expr);
}

/* implemented based on wikipedia pseudocode */
/* TODO(Robert): come up with some better names... */
static unsigned long mulinv(unsigned long l) {
  static const unsigned long TWO_POW_63 = 0x8000000000000000UL;
  assert(l % 2 != 0);

  unsigned long t0 = 1, r0 = l;
  unsigned long t1, r1;
  if (l > TWO_POW_63) {
    t1 = ULONG_MAX;
    r1 = 0 - l;
  } else {
    unsigned long q0 = 2 * (TWO_POW_63 / l);
    if (0 - q0 * l > l) ++q0;
    t1 = 0 - q0 * 1;
    r1 = 0 - q0 * l;
  }

  while (r1 != 0) {
    unsigned long quotient = r0 / r1;
    unsigned long tt = t0;
    t0 = t1;
    t1 = tt - quotient * t1;
    unsigned long rt = r0;
    r0 = r1;
    r1 = rt - quotient * r1;
  }

  return t0;
}

ASTree *translate_diff_conv(ASTree *diff_conv, ASTree *expr,
                            const Type *pointer_type) {
  SEMCHK(expr);
  assert(instr_empty(diff_conv->instructions));
  assert(!instr_empty(expr->instructions));

  Instruction *expr_instr = instr_prev(expr->instructions);
  assert(expr_instr->dest.all.mode == MODE_REGISTER &&
         expr_instr->dest.reg.width == REG_QWORD);

  size_t stride = type_get_elem_width(pointer_type);
  assert(stride != 0);
  int shift_amount = 0;
  while (!(stride & 0x1)) {
    stride >>= 1;
    ++shift_amount;
  }

  (void)instr_append(diff_conv->instructions, 1, expr->instructions);

  if (shift_amount != 0) {
    Instruction *sar_instr = instr_init(OP_SAR);
    sar_instr->dest = expr_instr->dest;
    set_op_imm(&sar_instr->src, shift_amount, IMM_UNSIGNED);
    (void)instr_append(diff_conv->instructions, 1, sar_instr);
  }

  if (stride == 1) return astree_adopt(diff_conv, 1, expr);

  size_t stride_inv = mulinv(stride);
  Instruction *load_inv_instr = instr_init(OP_MOV);
  set_op_imm(&load_inv_instr->src, stride_inv, IMM_UNSIGNED);
  set_op_reg(&load_inv_instr->dest, REG_QWORD, next_vreg());

  Instruction *imul_instr = instr_init(OP_IMUL);
  imul_instr->src = load_inv_instr->dest;
  imul_instr->dest = expr_instr->dest;

  (void)instr_append(diff_conv->instructions, 2, load_inv_instr, imul_instr);

  return astree_adopt(diff_conv, 1, expr);
}

ASTree *translate_ident(ASTree *ident) {
  Instruction *lea_instr = instr_init(OP_LEA);
  Symbol *symbol = NULL;
  state_get_symbol(state, ident->lexinfo, &symbol);
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
  (void)instr_append(ident->instructions, 1, lea_instr);
  return ident;
}

/* Two classes of operators whose result is a boolean:
 * - comparison: >, <, >=, <=, ==, !=
 * - logical: &&, ||, !
 *
 * logical NOT does the same thing as the conversion from an arbitrary value
 * to a boolean except instead of using SETNZ it does SETZ
 */
ASTree *translate_logical_not(ASTree * not, ASTree *operand) {
  SEMCHK(operand);
  Instruction *operand_instr = instr_prev(operand->instructions);

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

  (void)instr_append(not ->instructions, 4, operand->instructions, test_instr,
                     setz_instr, movz_instr);
  return astree_adopt(not, 1, operand);
}

ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  SEMCHK(left);
  SEMCHK(right);
  /* test first operand; jump on false for && and true for || */
  Instruction *left_instr = instr_prev(left->instructions);

  const char *skip_label = operator->tok_kind == TOK_AND
                               ? mk_false_label(next_branch())
                               : mk_true_label(next_branch());

  Instruction *test_left_instr = instr_init(OP_TEST);
  test_left_instr->dest = test_left_instr->src = left_instr->dest;
  test_left_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *jmp_left_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  set_op_dir(&jmp_left_instr->dest, skip_label);

  Instruction *right_instr = instr_prev(right->instructions);

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

  (void)instr_append(operator->instructions, 7, left->instructions,
                     test_left_instr, jmp_left_instr, right->instructions,
                     test_right_instr, setnz_instr, movz_instr);

  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_comparison(ASTree *operator, ASTree * left, ASTree *right) {
  SEMCHK(left);
  SEMCHK(right);
  TYPCHK(left->type, right->type);

  Instruction *left_instr = instr_prev(left->instructions);
  Instruction *right_instr = instr_prev(right->instructions);

  WIDCHK(left_instr, right_instr);
  Instruction *cmp_instr = instr_init(OP_CMP);
  /* reverse operands; looks weird in AT&T syntax but is correct */
  cmp_instr->dest = left_instr->dest;
  cmp_instr->src = right_instr->dest;
  cmp_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_SET;

  Instruction *setcc_instr =
      instr_init(opcode_from_operator(operator->tok_kind, right->type));
  set_op_reg(&setcc_instr->dest, REG_BYTE, next_vreg());

  Instruction *movz_instr = instr_init(OP_MOVZ);
  movz_instr->src = setcc_instr->dest;
  set_op_reg(&movz_instr->dest, REG_DWORD, next_vreg());
  movz_instr->persist_flags = PERSIST_DEST_CLEAR;

  (void)instr_append(operator->instructions, 5, left->instructions,
                     right->instructions, cmp_instr, setcc_instr, movz_instr);

  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_indirection(ASTree *indirection, ASTree *operand) {
  SEMCHK(operand);
  REGCHK((Instruction *)instr_prev(operand->instructions));
  PFDBG0('g', "Translating indirection operation.");
  /* this function doesn't need to do anything. if the operand was an lvalue,
   * the semantic analyzer should have created a semantic node which handles
   * the dereferencing of the original value. whether or not this occurred,
   * the value is then marked as an lvalue so that it may be dereferenced
   * again
   */
  (void)instr_append(indirection->instructions, 1, operand->instructions);
  return astree_adopt(indirection, 1, operand);
}

ASTree *translate_addrof(ASTree *addrof, ASTree *operand) {
  REGCHK((Instruction *)instr_prev(operand->instructions));
  PFDBG0('g', "Translating address operation.");
  /* this function doesn't need to do anything. the operand has to be an lvalue,
   * which means the destination operand of the last instruction emitted should
   * be the location (address) of the result already.
   */
  (void)instr_append(addrof->instructions, 1, operand->instructions);
  return astree_adopt(addrof, 1, operand);
}

ASTree *translate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  PFDBG0('g', "Translating pointer subscript");
  /* both the pointer and index must be in a register so that the displacement
   * and scale addressing mode can be used
   */
  SEMCHK(pointer);
  SEMCHK(index);
  Instruction *pointer_instr = instr_prev(pointer->instructions);
  Instruction *index_instr = instr_prev(index->instructions);

  WIDCHK(pointer_instr, index_instr);

  /* index should already have been scaled */
  Instruction *lea_instr = instr_init(OP_LEA);
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  set_op_sca(&lea_instr->src, SCALE_BYTE, NO_DISP, pointer_instr->dest.reg.num,
             index_instr->dest.reg.num);
  lea_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  (void)instr_append(subscript->instructions, 3, pointer->instructions,
                     index->instructions, lea_instr);
  return astree_adopt(subscript, 2, pointer, index);
}

ASTree *translate_reference(ASTree *reference, ASTree *struct_,
                            ASTree *member) {
  SEMCHK(struct_);
  PFDBG0('g', "Translating reference operator");
  Type *record_type;
  if (reference->tok_kind == TOK_ARROW) {
    record_type = type_strip_declarator(struct_->type);
  } else {
    record_type = struct_->type;
  }

  Instruction *struct_instr = instr_prev(struct_->instructions);
  Symbol *member_symbol = type_get_member_name(record_type, member->lexinfo);
  assert(member_symbol);
  REGCHK(struct_instr);

  Instruction *lea_instr = instr_init(OP_LEA);
  set_op_ind(&lea_instr->src, member_symbol->disp, struct_instr->dest.reg.num);
  set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
  lea_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;

  (void)instr_append(reference->instructions, 2, struct_->instructions,
                     lea_instr);

  return astree_adopt(reference, 2, struct_, member);
}

ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating postfix increment/decrement");
  Instruction *lvalue_instr = instr_prev(operand->instructions);
  Instruction *rval_conv_instr = convert_rval(lvalue_instr, operand->type);
  Instruction *type_conv_instr =
      convert_scalar(rval_conv_instr, post_inc_dec->type, operand->type);

  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = type_conv_instr->dest;
  set_op_reg(&mov_instr->dest, type_get_width(post_inc_dec->type), next_vreg());
  mov_instr->persist_flags |= PERSIST_DEST_CLEAR;

  Instruction *inc_dec_instr = instr_init(
      opcode_from_operator(post_inc_dec->tok_kind, post_inc_dec->type));
  inc_dec_instr->dest = type_conv_instr->dest;

  /* no need to set flags; conversion functions should do that */
  Instruction *store_instr = instr_init(OP_MOV);
  set_op_ind(&store_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  set_op_reg(&store_instr->src, type_get_width(operand->type),
             type_conv_instr->dest.reg.num);

  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = mov_instr->dest;

  (void)instr_append(post_inc_dec->instructions, 7, operand->instructions,
                     rval_conv_instr, type_conv_instr, mov_instr, inc_dec_instr,
                     store_instr, dummy_instr);
  return astree_adopt(post_inc_dec, 1, operand);
}

ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand) {
  PFDBG0('g', "Translating prefix increment/decrement");
  Instruction *lvalue_instr = instr_prev(operand->instructions);
  Instruction *rval_conv_instr = convert_rval(lvalue_instr, operand->type);
  Instruction *type_conv_instr =
      convert_scalar(rval_conv_instr, inc_dec->type, operand->type);

  /* no need to set persistence flags; conversion functions do that */
  Instruction *inc_dec_instr =
      instr_init(opcode_from_operator(inc_dec->tok_kind, inc_dec->type));
  inc_dec_instr->dest = type_conv_instr->dest;

  Instruction *store_instr = instr_init(OP_MOV);
  set_op_ind(&store_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  set_op_reg(&store_instr->src, type_get_width(operand->type),
             type_conv_instr->dest.reg.num);

  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = type_conv_instr->dest;

  (void)instr_append(inc_dec->instructions, 6, operand->instructions,
                     rval_conv_instr, type_conv_instr, inc_dec_instr,
                     store_instr, dummy_instr);
  return astree_adopt(inc_dec, 1, operand);
}

ASTree *translate_unop(ASTree *operator, ASTree * operand) {
  SEMCHK(operand);
  PFDBG0('g', "Translating unary operation");
  Instruction *operand_instr = instr_prev(operand->instructions);
  REGCHK(operand_instr);

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  operator_instr->dest = operand_instr->dest;
  operator_instr->persist_flags |= PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  (void)instr_append(operator->instructions, 2, operand->instructions,
                     operator_instr);
  return astree_adopt(operator, 1, operand);
}

ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  PFDBG0('g', "Translating additive operation");
  SEMCHK(left);
  SEMCHK(right);
  Instruction *left_instr = instr_prev(left->instructions);
  Instruction *right_instr = instr_prev(right->instructions);
  WIDCHK(left_instr, right_instr);

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  /* reverse operands; looks weird in AT&T syntax but is correct */
  operator_instr->dest = left_instr->dest;
  operator_instr->src = right_instr->dest;
  operator_instr->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  (void)instr_append(operator->instructions, 3, left->instructions,
                     right->instructions, operator_instr);
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
  Instruction *lvalue_instr, *conv_instr, *left_instr;
  if (operator->tok_kind ==
      TOK_MULEQ || operator->tok_kind == TOK_DIVEQ || operator->tok_kind ==
      TOK_REMEQ) {
    lvalue_instr = instr_prev(left->instructions);
    assert(lvalue_instr->dest.all.mode == MODE_REGISTER);
    assert(lvalue_instr->dest.reg.width == REG_QWORD);
    conv_instr = convert_rval(lvalue_instr, operator->type);
    left_instr = convert_scalar(conv_instr, right->type, operator->type);
  } else {
    lvalue_instr = conv_instr = NULL;
    /* TODO(Robert): remove this line */
    right->type = operator->type;
    left_instr = instr_prev(left->instructions);
  }

  assert(type_get_width(right->type) >= 4);

  Instruction *right_instr = instr_prev(right->instructions);
  WIDCHK(left_instr, right_instr);

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
  set_op_reg(&mov_rax_instr->dest, type_get_width(right->type), RAX_VREG);
  mov_rax_instr->src = left_instr->dest;
  mov_rax_instr->persist_flags |= PERSIST_SRC_SET;

  /* extend rax into rdx to be double its width for division */
  Opcode opcode = opcode_from_operator(operator->tok_kind, right->type);
  Instruction *extend_instr;
  if (opcode != OP_DIV && opcode != OP_IDIV) {
    extend_instr = instr_init(OP_NOP);
  } else if (type_is_signed(right->type)) {
    extend_instr = type_get_width(right->type) == 8 ? instr_init(OP_CQO)
                                                    : instr_init(OP_CDQ);
  } else {
    extend_instr = instr_init(OP_MOV);
    set_op_imm(&extend_instr->src, 0, IMM_UNSIGNED);
    set_op_reg(&extend_instr->dest, type_get_width(right->type), RDX_VREG);
  }

  /* pop right operand into r10 */
  Instruction *pop_right_instr = instr_init(OP_POP);
  set_op_reg(&pop_right_instr->dest, REG_QWORD, R10_VREG);

  /* perform operation with right operand in r10 */
  Instruction *operator_instr = instr_init(opcode);
  set_op_reg(&operator_instr->dest, type_get_width(right->type), R10_VREG);

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
    set_op_reg(&store_instr->src, type_get_width(right->type), RDX_VREG);
  else
    set_op_reg(&store_instr->src, type_get_width(right->type), RAX_VREG);
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
    set_op_reg(&load_instr->dest, type_get_width(right->type), next_vreg());
  else
    set_op_reg(&load_instr->dest, type_get_width(left->type), next_vreg());
  load_instr->persist_flags |= PERSIST_DEST_CLEAR;

  if (lvalue_instr == NULL) {
    (void)instr_append(operator->instructions, 18, left->instructions,
                       right->instructions, push_rax_instr, push_rdx_instr,
                       push_r10_instr, push_right_instr, spill_lvalue_instr,
                       mov_rax_instr, extend_instr, pop_right_instr,
                       operator_instr, unspill_lvalue_instr, assign_instr,
                       store_instr, pop_r10_instr, pop_rdx_instr, pop_rax_instr,
                       load_instr);
  } else {
    (void)instr_append(operator->instructions, 20, left->instructions,
                       conv_instr, left_instr, right->instructions,
                       push_rax_instr, push_rdx_instr, push_r10_instr,
                       push_right_instr, spill_lvalue_instr, mov_rax_instr,
                       extend_instr, pop_right_instr, operator_instr,
                       unspill_lvalue_instr, assign_instr, store_instr,
                       pop_r10_instr, pop_rdx_instr, pop_rax_instr, load_instr);
  }
}

ASTree *translate_multiplication(ASTree *operator, ASTree * left,
                                 ASTree *right) {
  SEMCHK(left);
  SEMCHK(right);
  TYPCHK(left->type, right->type);
  PFDBG0('g', "Translating binary operation");
  multiply_helper(operator, left, right);
  return astree_adopt(operator, 2, left, right);
}

static void shift_helper(ASTree *operator, ASTree * left, ASTree *right) {
  Instruction *lvalue_instr, *conv_instr, *left_instr;
  Type *promoted_type;
  if (operator->tok_kind == TOK_SHLEQ || operator->tok_kind == TOK_SHREQ) {
    lvalue_instr = instr_prev(left->instructions);
    assert(lvalue_instr->dest.all.mode == MODE_REGISTER);
    assert(lvalue_instr->dest.reg.width == REG_QWORD);
    conv_instr = convert_rval(lvalue_instr, operator->type);
    promoted_type =
        type_arithmetic_conversions(operator->type, (Type *)TYPE_INT);
    left_instr = convert_scalar(conv_instr, promoted_type, operator->type);
  } else {
    promoted_type = operator->type;
    lvalue_instr = conv_instr = NULL;
    left_instr = instr_prev(left->instructions);
  }

  PFDBG0('g', "Translating shift operation");
  Instruction *right_instr = instr_prev(right->instructions);
  REGCHK(right_instr);

  /* save registers whose values may be clobbered */
  Instruction *push_rcx_instr = instr_init(OP_PUSH);
  set_op_reg(&push_rcx_instr->dest, REG_QWORD, RCX_VREG);
  Instruction *push_r10_instr = instr_init(OP_PUSH);
  set_op_reg(&push_r10_instr->dest, REG_QWORD, R10_VREG);
  Instruction *push_right_instr = instr_init(OP_PUSH);
  set_op_reg(&push_right_instr->dest, REG_QWORD, right_instr->dest.reg.num);
  push_right_instr->persist_flags |= PERSIST_DEST_SET;

  /* spill lvalue location to 3rd unspill region, if applicable */
  Instruction *spill_lvalue_instr = NULL;
  if (lvalue_instr != NULL) {
    spill_lvalue_instr = instr_init(OP_MOV);
    spill_lvalue_instr->src = lvalue_instr->dest;
    set_op_ind(&spill_lvalue_instr->dest, UNSPILL_REGIONS[2], RBP_VREG);
  }

  /* put left operand in r10 */
  Instruction *mov_left_instr = instr_init(OP_MOV);
  mov_left_instr->src = left_instr->dest;
  set_op_reg(&mov_left_instr->dest, left_instr->dest.reg.width, R10_VREG);
  mov_left_instr->persist_flags |= PERSIST_SRC_SET;

  /* pop right operand into rcx */
  Instruction *pop_right_instr = instr_init(OP_POP);
  pop_right_instr->dest = push_rcx_instr->dest;

  /* shifts can only use `cl` as a register source operand */
  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, promoted_type));
  operator_instr->dest = mov_left_instr->dest;
  set_op_reg(&operator_instr->src, REG_BYTE, RCX_VREG);

  /* if this is a compound assignment operator, store result in lvalue
   * location; otherwise, store in 3rd unspill register, which should not be
   * used by any subsequent instructions. if it is a compound assignment
   * operator, we also need to unspill the register containing the lvalue
   * location; we can use rcx since we are done with the right operand.
   */
  Instruction *unspill_lvalue_instr = NULL, *assign_instr = NULL;
  if (lvalue_instr != NULL) {
    unspill_lvalue_instr = instr_init(OP_MOV);
    set_op_ind(&unspill_lvalue_instr->src, UNSPILL_REGIONS[2], RBP_VREG);
    set_op_reg(&unspill_lvalue_instr->dest, REG_QWORD, RCX_VREG);

    assign_instr = instr_init(OP_MOV);
    set_op_reg(&assign_instr->src, type_get_width(left->type), R10_VREG);
    set_op_ind(&assign_instr->dest, NO_DISP, RCX_VREG);
  }

  /* save result to 3rd unspill region, which should not be used */
  Instruction *store_instr = instr_init(OP_MOV);
  store_instr->src = operator_instr->dest;
  set_op_ind(&store_instr->dest, UNSPILL_REGIONS[2], RBP_VREG);

  /* restore clobbered registers */
  Instruction *pop_r10_instr = instr_init(OP_POP);
  pop_r10_instr->dest = push_r10_instr->dest;
  Instruction *pop_rcx_instr = instr_init(OP_POP);
  pop_rcx_instr->dest = push_rcx_instr->dest;

  /* load result into a fresh vreg */
  Instruction *load_instr = instr_init(OP_MOV);
  load_instr->src = store_instr->dest;
  set_op_reg(&load_instr->dest, store_instr->src.reg.width, next_vreg());
  load_instr->persist_flags |= PERSIST_DEST_CLEAR;

  if (lvalue_instr == NULL) {
    (void)instr_append(operator->instructions, 12, left->instructions,
                       right->instructions, push_rcx_instr, push_r10_instr,
                       push_right_instr, mov_left_instr, pop_right_instr,
                       operator_instr, store_instr, pop_r10_instr,
                       pop_rcx_instr, load_instr);
  } else {
    (void)instr_append(operator->instructions, 17, left->instructions,
                       conv_instr, left_instr, right->instructions,
                       push_rcx_instr, push_r10_instr, push_right_instr,
                       spill_lvalue_instr, mov_left_instr, pop_right_instr,
                       operator_instr, unspill_lvalue_instr, assign_instr,
                       store_instr, pop_r10_instr, pop_rcx_instr, load_instr);
  }
}

ASTree *translate_shift(ASTree *operator, ASTree * left, ASTree *right) {
  SEMCHK(right);
  SEMCHK(left);
  shift_helper(operator, left, right);
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right) {
  SEMCHK(left);
  SEMCHK(right);
  PFDBG0('g', "Translating binary operation");
  Instruction *left_instr = instr_prev(left->instructions);
  Instruction *right_instr = instr_prev(right->instructions);
  if (operator->tok_kind != TOK_SHR && operator->tok_kind != TOK_SHL) {
    TYPCHK(left->type, right->type);
    WIDCHK(left_instr, right_instr);
  }

  Instruction *operator_instr =
      instr_init(opcode_from_operator(operator->tok_kind, operator->type));
  /* reverse operands; looks weird in AT&T syntax but is correct */
  operator_instr->dest = left_instr->dest;
  operator_instr->src = right_instr->dest;
  operator_instr->persist_flags |=
      PERSIST_SRC_SET | PERSIST_DEST_SET | PERSIST_DEST_CLEAR;

  (void)instr_append(operator->instructions, 3, left->instructions,
                     right->instructions, operator_instr);
  return astree_adopt(operator, 2, left, right);
}

ASTree *translate_conditional(ASTree *qmark, ASTree *condition,
                              ASTree *true_expr, ASTree *false_expr) {
  SEMCHK(condition);
  SEMCHK(true_expr);
  SEMCHK(false_expr);
  TYPCHK(true_expr->type, false_expr->type);
  size_t current_branch = next_branch();
  Instruction *condition_instr = instr_prev(condition->instructions);
  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;
  Instruction *jmp_false_instr =
      instr_init(opcode_from_operator(qmark->tok_kind, qmark->type));
  set_op_dir(&jmp_false_instr->dest, mk_false_label(current_branch));
  if (type_is_void(qmark->type)) {
    Instruction *nop_instr = instr_init(OP_NOP);
    Instruction *jmp_end_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_end_instr->dest, mk_true_label(current_branch));

    Instruction *false_label = instr_next(false_expr->instructions);
    false_label->label = mk_false_label(current_branch);
    Instruction *end_label = instr_init(OP_NOP);
    end_label->label = mk_true_label(current_branch);

    (void)instr_append(qmark->instructions, 8, condition->instructions,
                       test_instr, jmp_false_instr, true_expr->instructions,
                       nop_instr, jmp_end_instr, false_expr->instructions,
                       end_label);
  } else {
    Instruction *true_expr_instr = instr_prev(true_expr->instructions);
    Instruction *mov_true_instr = instr_init(OP_MOV);
    mov_true_instr->src = true_expr_instr->dest;
    set_op_reg(&mov_true_instr->dest, type_get_width(qmark->type), next_vreg());
    /* clear persistence data for result vreg; set it for true expr result */
    mov_true_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
    Instruction *jmp_end_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_end_instr->dest, mk_true_label(current_branch));

    Instruction *false_label = instr_next(false_expr->instructions);
    false_label->label = mk_false_label(current_branch);
    Instruction *false_expr_instr = instr_prev(false_expr->instructions);
    WIDCHK(true_expr_instr, false_expr_instr);
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

    (void)instr_append(qmark->instructions, 9, condition->instructions,
                       test_instr, jmp_false_instr, true_expr->instructions,
                       mov_true_instr, jmp_end_instr, false_expr->instructions,
                       mov_false_instr, end_label);
  }

  return astree_adopt(qmark, 3, condition, true_expr, false_expr);
}

ASTree *translate_comma(ASTree *comma, ASTree *left, ASTree *right) {
  (void)instr_append(comma->instructions, 2, left->instructions,
                     right->instructions);
  return astree_adopt(comma, 2, left, right);
}

static void assign_aggregate(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  Instruction *lvalue_instr = instr_prev(lvalue->instructions);
  Instruction *rvalue_instr = instr_prev(rvalue->instructions);
  (void)instr_append(assignment->instructions, 2, lvalue->instructions,
                     rvalue->instructions);
  /* `bulk_mtom` should make registers persist */
  bulk_mtom(lvalue_instr->dest.reg.num, rvalue_instr->dest.reg.num,
            assignment->type, assignment->instructions);
  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->src = dummy_instr->dest = lvalue_instr->dest;
  dummy_instr->persist_flags |= PERSIST_DEST_CLEAR;
  (void)instr_append(assignment->instructions, 1, dummy_instr);
}

static void assign_compound(ASTree *assignment, ASTree *lvalue,
                            ASTree *rvalue) {
  SEMCHK(rvalue);
  /* no need to set persistence flags; conversion functions do that */
  Instruction *lvalue_instr = instr_prev(lvalue->instructions);
  REGCHK(lvalue_instr);
  Instruction *rval_conv_instr = convert_rval(lvalue_instr, assignment->type);
  Instruction *type_conv_instr;
  if (assignment->tok_kind == TOK_SHREQ || assignment->tok_kind == TOK_SHLEQ) {
    Type *promoted_type =
        type_arithmetic_conversions(assignment->type, (Type *)TYPE_INT);
    type_conv_instr =
        convert_scalar(rval_conv_instr, promoted_type, assignment->type);
  } else {
    type_conv_instr =
        convert_scalar(rval_conv_instr, rvalue->type, assignment->type);
  }

  Instruction *rvalue_instr = instr_prev(rvalue->instructions);
  REGCHK(rvalue_instr);

  /* perform operation at promoted width */
  Instruction *operator_instr =
      instr_init(opcode_from_operator(assignment->tok_kind, rvalue->type));
  operator_instr->src = rvalue_instr->dest;
  operator_instr->dest = type_conv_instr->dest;
  operator_instr->persist_flags = PERSIST_SRC_SET | PERSIST_DEST_SET;

  /* save result */
  Instruction *store_instr = instr_init(OP_MOV);
  set_op_reg(&store_instr->src, type_get_width(assignment->type),
             operator_instr->dest.reg.num);
  set_op_ind(&store_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  store_instr->persist_flags = PERSIST_DEST_SET;

  /* dummy mov with new value */
  Instruction *dummy_instr = instr_init(OP_MOV);
  dummy_instr->dest = dummy_instr->src = store_instr->src;

  (void)instr_append(assignment->instructions, 7, lvalue->instructions,
                     rval_conv_instr, type_conv_instr, rvalue->instructions,
                     operator_instr, store_instr, dummy_instr);
}

static void assign_scalar(ASTree *assignment, ASTree *lvalue, ASTree *rvalue) {
  SEMCHK(rvalue);
  TYPCHK(lvalue->type, rvalue->type);
  Instruction *lvalue_instr = instr_prev(lvalue->instructions);
  REGCHK(lvalue_instr);
  Instruction *rvalue_instr = instr_prev(rvalue->instructions);
  REGCHK(rvalue_instr);

  Instruction *assignment_instr =
      instr_init(opcode_from_operator(assignment->tok_kind, assignment->type));
  set_op_ind(&assignment_instr->dest, NO_DISP, lvalue_instr->dest.reg.num);
  assignment_instr->src = rvalue_instr->dest;
  assignment_instr->persist_flags |= PERSIST_DEST_SET | PERSIST_SRC_SET;

  Instruction *load_instr = instr_init(OP_MOV);
  load_instr->src = assignment_instr->dest;
  set_op_reg(&load_instr->dest, type_get_width(assignment->type), next_vreg());
  load_instr->persist_flags |= PERSIST_DEST_CLEAR;

  (void)instr_append(assignment->instructions, 4, lvalue->instructions,
                     rvalue->instructions, assignment_instr, load_instr);
}

ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue) {
  SEMCHK(rvalue);
  assert(astree_is_lvalue(lvalue) == LVAL_MODABLE);
  assert(lvalue->cexpr_kind == CEXPR_FALSE);
  PFDBG0('g', "Translating assignment");
  if (type_is_union(assignment->type) || type_is_struct(assignment->type)) {
    assign_aggregate(assignment, lvalue, rvalue);
  } else if (assignment->tok_kind == TOK_MULEQ ||
             assignment->tok_kind == TOK_DIVEQ ||
             assignment->tok_kind == TOK_REMEQ) {
    multiply_helper(assignment, lvalue, rvalue);
  } else if (assignment->tok_kind == TOK_SHLEQ ||
             assignment->tok_kind == TOK_SHREQ) {
    shift_helper(assignment, lvalue, rvalue);
  } else if (assignment->tok_kind != '=') {
    assign_compound(assignment, lvalue, rvalue);
  } else {
    assign_scalar(assignment, lvalue, rvalue);
  }

  return astree_adopt(assignment, 2, lvalue, rvalue);
}

static void load_reg_arg(ASTree *call, ASTree *arg, size_t reg_index) {
  Instruction *arg_instr = instr_prev(arg->instructions);
  if (type_is_scalar(arg->type)) {
    Instruction *load_arg_instr = instr_init(OP_MOV);
    load_arg_instr->src = arg_instr->dest;
    set_op_reg(&load_arg_instr->dest, REG_QWORD, PARAM_REGS[reg_index]);
    (void)instr_append(call->instructions, 1, load_arg_instr);
  } else {
    Instruction *load_arg_instr = instr_init(OP_MOV);
    load_arg_instr->src = arg_instr->dest;
    set_op_reg(&load_arg_instr->dest, REG_QWORD, R10_VREG);
    (void)instr_append(call->instructions, 1, load_arg_instr);
    bulk_mtor(&PARAM_REGS[reg_index], R10_VREG, NO_DISP, arg->type,
              call->instructions);
  }
}

static void load_spilled_arg(ASTree *call, ASTree *arg, size_t disp) {
  Instruction *arg_instr = instr_prev(arg->instructions);

  Instruction *load_arg_instr = instr_init(OP_MOV);
  load_arg_instr->src = arg_instr->dest;
  set_op_reg(&load_arg_instr->dest, REG_QWORD, R10_VREG);

  Instruction *stack_arg_instr = instr_init(OP_MOV);
  stack_arg_instr->src = load_arg_instr->dest;
  set_op_ind(&stack_arg_instr->dest, disp, RSP_VREG);

  (void)instr_append(call->instructions, 2, load_arg_instr, stack_arg_instr);
}

static void load_stack_arg(ASTree *call, ASTree *arg, size_t disp) {
  assert(type_is_aggregate(arg->type));
  Instruction *arg_instr = instr_prev(arg->instructions);

  Instruction *arg_loc_instr = instr_init(OP_MOV);
  arg_loc_instr->src = arg_instr->dest;
  set_op_reg(&arg_loc_instr->dest, REG_QWORD, R11_VREG);

  Instruction *stack_loc_instr = instr_init(OP_LEA);
  set_op_ind(&stack_loc_instr->src, disp, RSP_VREG);
  set_op_reg(&stack_loc_instr->dest, REG_QWORD, R10_VREG);

  (void)instr_append(call->instructions, 2, arg_loc_instr, stack_loc_instr);
  bulk_mtom(R10_VREG, R11_VREG, arg->type, call->instructions);
}

static size_t load_args(ASTree *call) {
  /* emit stack adjustment instruction; set source operand later */
  Instruction *rsp_sub_instr = instr_init(OP_SUB);
  set_op_reg(&rsp_sub_instr->dest, REG_QWORD, RSP_VREG);
  (void)instr_append(call->instructions, 1, rsp_sub_instr);

  /* account for hidden out param */
  size_t reg_index;
  if (type_get_eightbytes(call->type) > 2) {
    Instruction *hidden_arg_instr = instr_init(OP_LEA);
    set_op_ind(&hidden_arg_instr->src, assign_stack_space(call->type),
               RBP_VREG);
    set_op_reg(&hidden_arg_instr->dest, REG_QWORD, RDI_VREG);
    (void)instr_append(call->instructions, 1, hidden_arg_instr);
    reg_index = 1;
  } else {
    reg_index = 0;
  }

  size_t stack_disp = 0;

  /* move arguments to appropriate locations */
  size_t i, arg_count = astree_count(call) - 1;
  for (i = 0; i < arg_count; ++i) {
    ASTree *arg = astree_get(call, i + 1);
    size_t arg_eightbytes = type_get_eightbytes(arg->type);
    Instruction *arg_instr = instr_prev(arg->instructions);
    assert(arg_instr->dest.all.mode == MODE_INDIRECT);
    assert(arg_instr->dest.ind.num == RBP_VREG);
    if (arg_eightbytes > 2) {
      load_stack_arg(call, arg, stack_disp);
      stack_disp += arg_eightbytes * X64_SIZEOF_LONG;
    } else if (reg_index + arg_eightbytes <= PARAM_REG_COUNT) {
      load_reg_arg(call, arg, reg_index);
      reg_index += arg_eightbytes;
    } else if (type_is_aggregate(arg->type)) {
      load_stack_arg(call, arg, stack_disp);
      stack_disp += arg_eightbytes * X64_SIZEOF_LONG;
    } else {
      load_spilled_arg(call, arg, stack_disp);
      stack_disp += X64_SIZEOF_LONG;
    }
  }

  assert(stack_disp % 8 == 0);
  /* 16x+8 window size and 9 push ops align the stack; maintain that here */
  if (stack_disp & 0x8) stack_disp += 8;
  set_op_imm(&rsp_sub_instr->src, stack_disp, IMM_UNSIGNED);

  /* return size of stack adustment */
  return stack_disp;
}

static void save_call_subexprs(ASTree *call) {
  size_t i;
  for (i = 0; i < astree_count(call); ++i) {
    ASTree *subexpr = astree_get(call, i);
    if (call->spill_eightbytes < subexpr->spill_eightbytes + i + 1)
      call->spill_eightbytes = subexpr->spill_eightbytes + i + 1;
  }

  if (ARR_LEN(spill_regions) < call->spill_eightbytes)
    ARR_RESIZE(spill_regions, call->spill_eightbytes,
               assign_stack_space(TYPE_LONG));

  for (i = 0; i < astree_count(call); ++i) {
    ASTree *subexpr = astree_get(call, i);
    Instruction *spill_instr = instr_init(OP_MOV);
    set_op_ind(&spill_instr->dest,
               ARR_GET(spill_regions, call->spill_eightbytes - (i + 1)),
               RBP_VREG);
    spill_instr->persist_flags |= PERSIST_SRC_SET;
    Instruction *subexpr_instr = instr_prev(subexpr->instructions);

    if (type_is_scalar(subexpr->type)) {
      /* extend all scalar arguments to be eight bytes wide to make loading and
       * storing more convenient
       */
      Instruction *conv_instr =
          convert_scalar(subexpr_instr, TYPE_UNSIGNED_LONG, subexpr->type);
      spill_instr->src = conv_instr->dest;
      (void)instr_append(subexpr->instructions, 2, conv_instr, spill_instr);
      (void)instr_append(call->instructions, 1, subexpr->instructions);
    } else {
      assert(subexpr_instr->dest.all.mode == MODE_REGISTER);
      assert(subexpr_instr->dest.reg.width == REG_QWORD);
      spill_instr->src = subexpr_instr->dest;
      (void)instr_append(subexpr->instructions, 1, spill_instr);
      (void)instr_append(call->instructions, 1, subexpr->instructions);
    }
  }
}

static void recieve_void(ASTree *call) {
  Instruction *nop_instr = instr_init(OP_NOP);
  (void)instr_append(call->instructions, 1, nop_instr);
  restore_volatile_regs(call->instructions);
}

static void recieve_normal(ASTree *call) {
  Instruction *store_instr = instr_init(OP_MOV);
  set_op_ind(&store_instr->dest, RETURN_VAL_DISP, RBP_VREG);
  set_op_reg(&store_instr->src,
             type_get_eightbytes(call->type) > 2 ? REG_QWORD
                                                 : type_get_width(call->type),
             RAX_VREG);
  (void)instr_append(call->instructions, 1, store_instr);

  restore_volatile_regs(call->instructions);

  Instruction *load_instr = instr_init(OP_MOV);
  set_op_reg(&load_instr->dest, store_instr->src.reg.width, next_vreg());
  load_instr->src = store_instr->dest;
  (void)instr_append(call->instructions, 1, load_instr);
}

static void recieve_reg_agg(ASTree *call) {
  ptrdiff_t disp = assign_stack_space(call->type);
  bulk_rtom(RBP_VREG, disp, RETURN_REGS, call->type, call->instructions);

  restore_volatile_regs(call->instructions);

  Instruction *agg_addr_instr = instr_init(OP_LEA);
  set_op_ind(&agg_addr_instr->src, disp, RBP_VREG);
  set_op_reg(&agg_addr_instr->dest, REG_QWORD, next_vreg());
  (void)instr_append(call->instructions, 1, agg_addr_instr);
}

ASTree *translate_call(ASTree *call) {
  PFDBG0('g', "Translating function call");
  ASTree *designator = astree_get(call, 0);

  save_call_subexprs(call);
  save_volatile_regs(call->instructions);
  size_t rsp_adjustment = load_args(call);

  if (type_is_variadic_function(type_strip_declarator(designator->type))) {
    Instruction *zero_eax_instr = instr_init(OP_MOV);
    set_op_imm(&zero_eax_instr->src, 0, IMM_UNSIGNED);
    set_op_reg(&zero_eax_instr->dest, REG_BYTE, RAX_VREG);
    (void)instr_append(call->instructions, 1, zero_eax_instr);
  }

  Instruction *load_desg_instr = instr_init(OP_MOV);
  Instruction *store_desg_instr = instr_prev(designator->instructions);
  assert(store_desg_instr->dest.all.mode == MODE_INDIRECT);
  load_desg_instr->src = store_desg_instr->dest;
  /* use r10 since it is never used for args */
  set_op_reg(&load_desg_instr->dest, type_get_width(designator->type),
             R10_VREG);

  Instruction *call_instr = instr_init(OP_CALL);
  call_instr->dest = load_desg_instr->dest;

  Instruction *rsp_add_instr = instr_init(OP_ADD);
  set_op_imm(&rsp_add_instr->src, rsp_adjustment, IMM_UNSIGNED);
  set_op_reg(&rsp_add_instr->dest, REG_QWORD, RSP_VREG);

  (void)instr_append(call->instructions, 3, load_desg_instr, call_instr,
                     rsp_add_instr);

  if (type_is_void(call->type)) {
    recieve_void(call);
  } else if (type_is_aggregate(call->type) &&
             type_get_eightbytes(call->type) <= 2) {
    recieve_reg_agg(call);
  } else {
    recieve_normal(call);
  }

  return call;
}

ASTree *translate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident) {
  SEMCHK(expr);
  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);
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
  set_op_imm(&reg_save_area_disp_instr->src, VA_SPILL_REGIONS[0], IMM_SIGNED);
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

  (void)instr_append(
      va_start_->instructions, 11, expr->instructions, load_gp_offset_instr,
      store_gp_offset_instr, load_fp_offset_instr, store_fp_offset_instr,
      reg_save_area_disp_instr, add_rbp_instr, store_reg_save_area_instr,
      param_stack_disp_instr, add_rbp_instr_2, store_overflow_arg_area_instr);
  return astree_adopt(va_start_, 2, expr, ident);
}

ASTree *translate_va_end(ASTree *va_end_, ASTree *expr) {
  SEMCHK(expr);
  REGCHK((Instruction *)instr_prev(expr->instructions));
  (void)instr_append(va_end_->instructions, 1, expr->instructions);
  return astree_adopt(va_end_, 1, expr);
}

/* NOTE: since floating point arithmetic has not been implemented whatsoever,
 * the `fp_offset` field is ignored by `va_arg`.
 */
static void helper_va_arg_reg_param(ASTree *va_arg_, ASTree *expr,
                                    ASTree *type_name) {
  SEMCHK(expr);
  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);
  assert(expr_instr != NULL && expr_instr->dest.all.mode == MODE_REGISTER);
  ASTree *abs_decl = astree_get(type_name, 1);
  const Type *arg_type = abs_decl->type;
  size_t va_list_vreg = expr_instr->dest.reg.num;
  size_t eightbytes = type_get_eightbytes(arg_type);
  size_t result_vreg = next_vreg(), current_branch = next_branch();

  /* load gp offset member */
  Instruction *load_gp_offset_instr = instr_init(OP_MOV);
  set_op_ind(&load_gp_offset_instr->src, GP_OFFSET_MEMBER_DISP, va_list_vreg);
  set_op_reg(&load_gp_offset_instr->dest, REG_DWORD, next_vreg());
  load_gp_offset_instr->persist_flags |= PERSIST_DEST_CLEAR;

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
  set_op_reg(&add_save_area_instr->dest, REG_QWORD,
             load_gp_offset_instr->dest.reg.num);
  add_save_area_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *load_reg_save_instr = instr_init(OP_MOV);
  set_op_reg(&load_reg_save_instr->dest, REG_QWORD, result_vreg);
  /* first use of result_vreg; clear persistence */
  load_reg_save_instr->persist_flags |= PERSIST_DEST_CLEAR;
  if (type_is_scalar(arg_type)) {
    /* load value from register save area into result vreg */
    set_op_ind(&load_reg_save_instr->src, NO_DISP,
               add_save_area_instr->dest.reg.num);
  } else {
    /* argument is a struct/union; return its location */
    load_reg_save_instr->src = add_save_area_instr->dest;
  }

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
  set_op_reg(&load_overflow_arg_area_instr->dest, REG_QWORD, next_vreg());
  load_overflow_arg_area_instr->label = mk_true_label(current_branch);

  Instruction *load_stack_save_instr = instr_init(OP_MOV);
  if (type_is_scalar(arg_type)) {
    /* load value from stack save area into result vreg */
    set_op_reg(&load_stack_save_instr->dest, type_get_width(arg_type),
               result_vreg);
    set_op_ind(&load_stack_save_instr->src, NO_DISP,
               load_overflow_arg_area_instr->dest.reg.num);
  } else {
    /* argument is a struct/union; return its location */
    set_op_reg(&load_stack_save_instr->dest, REG_QWORD, result_vreg);
    load_stack_save_instr->src = load_overflow_arg_area_instr->dest;
  }

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
  dummy_instr->src = dummy_instr->dest = load_stack_save_instr->dest;
  dummy_instr->label = mk_false_label(current_branch);
  dummy_instr->persist_flags = PERSIST_DEST_SET;

  (void)instr_append(
      va_arg_->instructions, 15, expr->instructions, abs_decl->instructions,
      load_gp_offset_instr, cmp_gp_offset_instr, jmp_ge_instr,
      add_save_area_instr, load_reg_save_instr, load_eightbyte_instr,
      update_offset_instr, jmp_false_instr, load_overflow_arg_area_instr,
      load_stack_save_instr, load_disp_instr, add_disp_instr, dummy_instr);
}

static void helper_va_arg_stack_param(ASTree *va_arg_, ASTree *expr,
                                      ASTree *type_name) {
  SEMCHK(expr);
  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);
  assert(expr_instr != NULL && expr_instr->dest.all.mode == MODE_REGISTER);
  size_t va_list_vreg = expr_instr->dest.reg.num;
  ASTree *abs_decl = astree_get(type_name, 1);
  size_t eightbytes = type_get_eightbytes(abs_decl->type);

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

  (void)instr_append(va_arg_->instructions, 6, expr->instructions,
                     abs_decl->instructions, load_overflow_arg_area_instr,
                     load_disp_instr, add_disp_instr, dummy_instr);
}

ASTree *translate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name) {
  if (type_get_eightbytes(astree_get(type_name, 1)->type) <= 2)
    helper_va_arg_reg_param(va_arg_, expr, type_name);
  else
    helper_va_arg_stack_param(va_arg_, expr, type_name);
  return astree_adopt(va_arg_, 2, expr, type_name);
}

static void va_translate_params(ASTree *declarator, ASTree *body) {
  Type *ret_type = type_strip_declarator(declarator->type);

  /* account for hidden out param */
  if (type_get_eightbytes(ret_type) > 2) {
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, REG_QWORD, RDI_VREG);
    set_op_ind(&mov_instr->dest, HIDDEN_PARAM_DISP, RBP_VREG);
    /* prepend parameter mov instructions to body so rsp adjustment instruction
     * is visible when we emit the function epilogue
     */
    (void)instr_append(body->instructions, 1, mov_instr);
    param_reg_index = 1;
  } else {
    param_reg_index = 0;
  }

  /* because spill region type is large enough to hold all registers, we must
   * copy all registers to the stack when the function is variadic since
   * `bulk_rtom` determines the number of registers to copy based on the size
   * of the type
   */
  bulk_rtom(RBP_VREG, VA_SPILL_REGIONS[0], PARAM_REGS, TYPE_VA_SPILL_REGION,
            body->instructions);

  ASTree *fn_dirdecl = astree_get(declarator, 0);
  assert(fn_dirdecl->tok_kind == TOK_PARAM_LIST);
  /* offset to account for return address and saved rbp */
  param_stack_disp = PROLOGUE_EIGHTBYTES * X64_SIZEOF_LONG;
  window_size = VA_INIT_WINDOW_SIZE;
  size_t i, param_count = type_get_param_count(declarator->type);
  for (i = 0; i < param_count; ++i) {
    ASTree *param = astree_get(fn_dirdecl, i);
    ASTree *param_decl = astree_get(param, 1);
    Symbol *param_symbol = NULL;
    int in_current_scope =
        state_get_symbol(state, param_decl->lexinfo, &param_symbol);
#ifdef NDEBUG
    (void)in_current_scope;
#endif
    assert(in_current_scope && param_symbol);
    size_t param_symbol_eightbytes = type_get_eightbytes(param_symbol->type);
    if (param_symbol_eightbytes <= 2 &&
        param_reg_index + param_symbol_eightbytes <= PARAM_REG_COUNT) {
      param_symbol->disp = VA_SPILL_REGIONS[param_reg_index];
      param_reg_index += param_symbol_eightbytes;
    } else {
      param_symbol->disp = param_stack_disp;
      param_stack_disp += param_symbol_eightbytes * X64_SIZEOF_LONG;
    }
  }
}

static void translate_params(ASTree *declarator, ASTree *body) {
  Type *ret_type = type_strip_declarator(declarator->type);

  /* account for hidden out param */
  if (type_get_eightbytes(ret_type) > 2) {
    Instruction *mov_instr = instr_init(OP_MOV);
    set_op_reg(&mov_instr->src, REG_QWORD, RDI_VREG);
    set_op_ind(&mov_instr->dest, HIDDEN_PARAM_DISP, RBP_VREG);
    /* prepend parameter mov instructions to body so rsp adjustment instruction
     * is visible when we emit the function epilogue
     */
    (void)instr_append(body->instructions, 1, mov_instr);
    param_reg_index = 1;
  } else {
    param_reg_index = 0;
  }

  ASTree *fn_dirdecl = astree_get(declarator, 0);
  assert(fn_dirdecl->tok_kind == TOK_PARAM_LIST);
  /* offset to account for return address and saved rbp */
  param_stack_disp = PROLOGUE_EIGHTBYTES * X64_SIZEOF_LONG;
  window_size = INIT_WINDOW_SIZE;
  size_t i, param_count = type_get_param_count(declarator->type);
  for (i = 0; i < param_count; ++i) {
    ASTree *param = astree_get(fn_dirdecl, i);
    ASTree *param_decl = astree_get(param, 1);
    Symbol *param_symbol = NULL;
    int in_current_scope =
        state_get_symbol(state, param_decl->lexinfo, &param_symbol);
#ifdef NDEBUG
    (void)in_current_scope;
#endif
    assert(in_current_scope && param_symbol);
    size_t param_symbol_eightbytes = type_get_eightbytes(param_symbol->type);
    if (param_symbol_eightbytes <= 2 &&
        param_reg_index + param_symbol_eightbytes <= PARAM_REG_COUNT) {
      param_symbol->disp = assign_stack_space(param_symbol->type);
      bulk_rtom(RBP_VREG, param_symbol->disp, PARAM_REGS + param_reg_index,
                param_symbol->type, body->instructions);
      param_reg_index += param_symbol_eightbytes;
    } else {
      param_symbol->disp = param_stack_disp;
      param_stack_disp += param_symbol_eightbytes * X64_SIZEOF_LONG;
    }
  }
}

ASTree *translate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                         ASTree *else_body) {
  SEMCHK(condition);
  Instruction *condition_instr = instr_prev(condition->instructions);
  REGCHK(condition_instr);

  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(ifelse->jump_id);
  if (else_body->tok_kind != TOK_EMPTY) {
    Instruction *else_label = instr_init(OP_NONE);
    else_label->label = mk_stmt_label(ifelse->jump_id);

    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, else_label->label);

    Instruction *jmp_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_instr->dest, end_label->label);

    (void)instr_append(ifelse->instructions, 8, condition->instructions,
                       test_instr, test_jmp_instr, if_body->instructions,
                       jmp_instr, else_label, else_body->instructions,
                       end_label);
  } else {
    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, end_label->label);

    (void)instr_append(ifelse->instructions, 5, condition->instructions,
                       test_instr, test_jmp_instr, if_body->instructions,
                       end_label);
  }
  return astree_adopt(ifelse, 3, condition, if_body, else_body);
}

ASTree *translate_switch(ASTree *switch_, ASTree *condition, ASTree *body,
                         int has_default) {
  SEMCHK(condition);

  /* switch prologue */
  Instruction *jmp_case1_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_case1_instr->dest, mk_case_label(switch_->jump_id, 0));

  /* switch epilogue */
  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(switch_->jump_id);

  Instruction *jmp_end_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_end_instr->dest, end_label->label);

  Instruction *dummy_case_label = instr_init(OP_NONE);
  dummy_case_label->label = mk_case_label(switch_->jump_id, switch_->case_id);

  if (has_default) {
    Instruction *jmp_def_instr = instr_init(OP_JMP);
    set_op_dir(&jmp_def_instr->dest, mk_def_label(switch_->jump_id));

    (void)instr_append(switch_->instructions, 7, condition->instructions,
                       jmp_case1_instr, body->instructions, jmp_end_instr,
                       dummy_case_label, jmp_def_instr, end_label);
  } else {
    (void)instr_append(switch_->instructions, 6, condition->instructions,
                       jmp_case1_instr, body->instructions, jmp_end_instr,
                       dummy_case_label, end_label);
  }

  return astree_adopt(switch_, 2, condition, body);
}

ASTree *translate_switch_expr(ASTree *expr) {
  Instruction *expr_instr = instr_prev(expr->instructions);
  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = expr_instr->dest;
  size_t control_vreg = state_get_control_reg(state);
  set_op_reg(&mov_instr->dest, type_get_width(expr->type), control_vreg);
  /* clear control vreg from persistence data; persist condition expression */
  mov_instr->persist_flags |= PERSIST_SRC_SET | PERSIST_DEST_CLEAR;
  (void)instr_append(expr->instructions, 1, mov_instr);
  return expr;
}

ASTree *translate_while(ASTree *while_, ASTree *condition, ASTree *body) {
  SEMCHK(condition);
  Instruction *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(while_->jump_id);

  Instruction *condition_instr = instr_prev(condition->instructions);
  REGCHK(condition_instr);

  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->src = test_instr->dest = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(while_->jump_id);

  Instruction *test_jmp_instr = instr_init(OP_JZ);
  set_op_dir(&test_jmp_instr->dest, end_label->label);

  Instruction *cond_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_instr->dest, condition_label->label);

  (void)instr_append(while_->instructions, 7, condition_label,
                     condition->instructions, test_instr, test_jmp_instr,
                     body->instructions, cond_jmp_instr, end_label);

  return astree_adopt(while_, 2, condition, body);
}

ASTree *translate_for(ASTree *for_, ASTree *initializer, ASTree *condition,
                      ASTree *reinitializer, ASTree *body) {
  SEMCHK(initializer);
  SEMCHK(condition);
  SEMCHK(reinitializer);
  assert(!instr_empty(initializer->instructions));
  assert(!instr_empty(condition->instructions));
  assert(!instr_empty(reinitializer->instructions));

  /* unfortunately, due to the name being used and the way `for`-loops are
   * structured, the "condition" label is attached to the reinitializer
   * expression, which then jumps to the actual condition, which does not have
   * a condition label, but has a special `for`-loop label
   */
  Instruction *reinit_start_instr = instr_next(reinitializer->instructions);
  assert(reinit_start_instr->label == NULL);
  reinit_start_instr->label = mk_cond_label(for_->jump_id);

  Instruction *cond_start_instr = instr_next(condition->instructions);
  assert(cond_start_instr->label == NULL);
  cond_start_instr->label = mk_for_label(for_->jump_id);

  Instruction *cond_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&cond_jmp_instr->dest, cond_start_instr->label);

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(for_->jump_id);

  /* emit loop exit jump if condition is not empty */
  if (condition->tok_kind != TOK_EMPTY) {
    Instruction *condition_instr = instr_prev(condition->instructions);
    REGCHK(condition_instr);

    Instruction *test_instr = instr_init(OP_TEST);
    test_instr->dest = test_instr->src = condition_instr->dest;
    test_instr->persist_flags |= PERSIST_DEST_SET;

    Instruction *test_jmp_instr = instr_init(OP_JZ);
    set_op_dir(&test_jmp_instr->dest, mk_end_label(for_->jump_id));

    (void)instr_append(for_->instructions, 8, initializer->instructions,
                       condition->instructions, test_instr, test_jmp_instr,
                       body->instructions, reinitializer->instructions,
                       cond_jmp_instr, end_label);
  } else {
    (void)instr_append(for_->instructions, 6, initializer->instructions,
                       condition->instructions, body->instructions,
                       reinitializer->instructions, cond_jmp_instr, end_label);
  }

  return astree_adopt(for_, 4, initializer, condition, reinitializer, body);
}

ASTree *translate_do(ASTree *do_, ASTree *body, ASTree *condition) {
  SEMCHK(condition);
  Instruction *body_label = instr_init(OP_NONE);
  body_label->label = mk_stmt_label(do_->jump_id);

  Instruction *condition_label = instr_init(OP_NONE);
  condition_label->label = mk_cond_label(do_->jump_id);

  Instruction *condition_instr = instr_prev(condition->instructions);
  REGCHK(condition_instr);

  Instruction *test_instr = instr_init(OP_TEST);
  test_instr->dest = test_instr->src = condition_instr->dest;
  test_instr->persist_flags |= PERSIST_DEST_SET;

  Instruction *test_jmp_instr = instr_init(OP_JNZ);
  set_op_dir(&test_jmp_instr->dest, body_label->label);

  Instruction *end_label = instr_init(OP_NONE);
  end_label->label = mk_end_label(do_->jump_id);

  (void)instr_append(do_->instructions, 7, body_label, body->instructions,
                     condition_label, condition->instructions, test_instr,
                     test_jmp_instr, end_label);

  return astree_adopt(do_, 2, body, condition);
}

ASTree *translate_block(ASTree *block) {
  PFDBG0('g', "Translating compound statement");

  /* emit nop if block contains no instructions */
  if (instr_empty(block->instructions))
    (void)instr_append(block->instructions, 1, instr_init(OP_NOP));

  return block;
}

static void return_scalar(ASTree *ret, ASTree *expr) {
  SEMCHK(expr);
  Symbol *function_symbol = state_get_function(state);
  const Type *function_type = function_symbol->type;
  /* strip function */
  Type *return_type = type_strip_declarator(function_type);
  TYPCHK(expr->type, return_type);

  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);

  Instruction *mov_instr = instr_init(OP_MOV);
  mov_instr->src = expr_instr->dest;
  set_op_reg(&mov_instr->dest, type_get_width(return_type), RAX_VREG);
  mov_instr->persist_flags |= PERSIST_SRC_SET;

  (void)instr_append(ret->instructions, 2, expr->instructions, mov_instr);

  restore_preserved_regs(ret->instructions);

  Instruction *ret_instr = instr_init(OP_RET);

  (void)instr_append(ret->instructions, 1, ret_instr);
}

static void return_aggregate(ASTree *ret, ASTree *expr) {
  SEMCHK(expr);
  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);
  size_t expr_eightbytes = type_get_eightbytes(expr->type);

  if (expr_eightbytes <= 2) {
    /* mov location in case it is in rax or rdx */
    Instruction *mov_loc_r10_instr = instr_init(OP_MOV);
    mov_loc_r10_instr->src = expr_instr->dest;
    set_op_reg(&mov_loc_r10_instr->dest, REG_QWORD, R10_VREG);
    mov_loc_r10_instr->persist_flags |= PERSIST_SRC_SET;

    (void)instr_append(ret->instructions, 2, expr->instructions,
                       mov_loc_r10_instr);

    bulk_mtor(RETURN_REGS, R10_VREG, NO_DISP, expr->type, ret->instructions);
  } else {
    Instruction *hidden_mov_instr = instr_init(OP_MOV);
    set_op_reg(&hidden_mov_instr->dest, REG_QWORD, next_vreg());
    set_op_ind(&hidden_mov_instr->src, HIDDEN_PARAM_DISP, RBP_VREG);
    hidden_mov_instr->persist_flags |= PERSIST_DEST_CLEAR;

    (void)instr_append(ret->instructions, 2, expr->instructions,
                       hidden_mov_instr);

    bulk_mtom(hidden_mov_instr->dest.reg.num, expr_instr->dest.reg.num,
              expr->type, ret->instructions);

    Instruction *mov_rax_instr = instr_init(OP_MOV);
    set_op_ind(&mov_rax_instr->src, HIDDEN_PARAM_DISP, RBP_VREG);
    set_op_reg(&mov_rax_instr->dest, REG_QWORD, RAX_VREG);

    (void)instr_append(ret->instructions, 1, mov_rax_instr);
  }

  restore_preserved_regs(ret->instructions);

  Instruction *ret_instr = instr_init(OP_RET);

  (void)instr_append(ret->instructions, 1, ret_instr);
}

static void return_void(ASTree *ret) {
  Instruction *nop_instr = instr_init(OP_NOP);

  (void)instr_append(ret->instructions, 1, nop_instr);

  restore_preserved_regs(ret->instructions);

  Instruction *ret_instr = instr_init(OP_RET);

  (void)instr_append(ret->instructions, 1, ret_instr);
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

  (void)instr_append(continue_->instructions, 1, cond_jmp_instr);

  return continue_;
}

ASTree *translate_break(ASTree *break_) {
  Instruction *end_jmp_instr = instr_init(OP_JMP);
  set_op_dir(&end_jmp_instr->dest, mk_end_label(break_->jump_id));

  (void)instr_append(break_->instructions, 1, end_jmp_instr);

  return break_;
}

ASTree *translate_goto(ASTree *goto_, ASTree *ident) {
  Instruction *jmp_instr = instr_init(OP_JMP);
  set_op_dir(&jmp_instr->dest, mk_local_label(ident->lexinfo));

  (void)instr_append(goto_->instructions, 1, jmp_instr);

  return astree_adopt(goto_, 1, ident);
}

ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt) {
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = mk_local_label(ident->lexinfo);

  (void)instr_append(label->instructions, 2, label_instr, stmt->instructions);

  return astree_adopt(label, 2, ident, stmt);
}

ASTree *translate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  SEMCHK(expr);

  const Type *control_type = state_get_control_type(state);
  if (control_type == NULL) abort();
  TYPCHK(expr->type, control_type);

  Instruction *expr_instr = instr_prev(expr->instructions);
  REGCHK(expr_instr);
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

  (void)instr_append(case_->instructions, 6, fall_jmp_instr, case_label,
                     test_instr, jmp_instr, fall_label, stmt->instructions);

  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *translate_default(ASTree *default_, ASTree *stmt) {
  Instruction *def_label = instr_init(OP_NONE);
  def_label->label = mk_def_label(default_->jump_id);

  (void)instr_append(default_->instructions, 2, def_label, stmt->instructions);

  return astree_adopt(default_, 1, stmt);
}

void translate_static_scalar_init(const Type *type, ASTree *initializer) {
  assert(instr_empty(initializer->instructions));
  Opcode directive;
  switch (type_get_width(type)) {
    case X64_SIZEOF_LONG:
      directive = OP_QUAD;
      break;
    case X64_SIZEOF_INT:
      directive = OP_LONG;
      break;
    case X64_SIZEOF_SHRT:
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

  (void)instr_append(initializer->instructions, 1, instr);
}

void translate_auto_scalar_init(const Type *type, ptrdiff_t disp,
                                ASTree *initializer) {
  assert(instr_empty(initializer->instructions));
  assert(initializer->cexpr_kind >= CEXPR_INIT);
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

  (void)instr_append(initializer->instructions, 2, load_instr, store_instr);
}

void translate_static_literal_init(const Type *arr_type, ASTree *literal) {
  assert(instr_empty(literal->instructions));

  /* TODO(Robert): use a map here. this is ugly. */
  size_t i;
  for (i = 0; i < ARR_LEN(literal_labels); ++i) {
    if (ARR_GET(literal_labels, i) == literal->constant.label) {
      const char *str = ARR_GET(literal_tokens, i);
      size_t arr_width = type_get_width(arr_type);
      size_t literal_length = type_get_width(literal->type);
      assert(literal_length - 1 <= arr_width);

      Instruction *ascii_instr = arr_width == literal_length
                                     ? instr_init(OP_ASCIZ)
                                     : instr_init(OP_ASCII);
      set_op_dir(&ascii_instr->dest, str);

      (void)instr_append(literal->instructions, 1, ascii_instr);

      if (arr_width > literal_length)
        /* add one for null terminator since we used `.ascii` above */
        static_zero_pad(arr_width - literal_length + 1, literal->instructions);
      return;
    }
  }
  /* literal not found */
  abort();
}

void translate_auto_literal_init(const Type *arr_type, ptrdiff_t arr_disp,
                                 ASTree *literal) {
  assert(instr_empty(literal->instructions));

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

  (void)instr_append(literal->instructions, 2, literal_lea_instr,
                     arr_lea_instr);

  size_t arr_width = type_get_width(arr_type);
  size_t literal_width = type_get_width(literal->type);
  bulk_mtom(arr_lea_instr->dest.reg.num, literal_lea_instr->dest.reg.num,
            (arr_width > literal_width) ? literal->type : arr_type,
            literal->instructions);
  if (arr_width > literal_width)
    bulk_mzero(arr_lea_instr->dest.reg.num, NO_DISP, literal_width, arr_type,
               literal->instructions);
}

static void translate_static_decl(ASTree *declarator, Symbol *symbol) {
  assert(symbol->storage == STORE_STAT);
  assert(symbol->info != SYM_HIDDEN && symbol->info != SYM_INHERITOR);
  if (symbol->instructions != NULL) return;
  symbol->instructions = declarator->instructions;
  const char *identifier =
      symbol->linkage == LINK_NONE
          ? mk_static_label(declarator->lexinfo, symbol->static_id)
          : declarator->lexinfo;

  Instruction *section_instr;
  if (type_is_const(symbol->type)) {
    section_instr = instr_init(OP_SECTION);
    set_op_dir(&section_instr->dest, ".rodata");
  } else if (symbol->info == SYM_DEFINED) {
    section_instr = instr_init(OP_DATA);
  } else {
    section_instr = instr_init(OP_BSS);
  }

  Instruction *align_instr = instr_init(OP_ALIGN);
  set_op_sym(&align_instr->dest, symbol);
  Instruction *type_instr = instr_init(OP_TYPE);
  set_op_dir(&type_instr->dest, identifier);
  set_op_dir(&type_instr->src, "@object");
  Instruction *size_instr = instr_init(OP_SIZE);
  set_op_dir(&size_instr->dest, identifier);
  set_op_sym(&size_instr->src, symbol);
  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = identifier;

  if (symbol->linkage == LINK_EXT) {
    Instruction *globl_instr = instr_init(OP_GLOBL);
    set_op_dir(&globl_instr->dest, declarator->lexinfo);
    assert(state_get_function(state) == NULL);

    (void)instr_append(declarator->instructions, 6, globl_instr, section_instr,
                       align_instr, type_instr, size_instr, label_instr);
  } else { /* symbol->linkage == LINK_INT || symbol->linkage == LINK_NONE */
    assert(symbol->linkage == LINK_INT || symbol->linkage == LINK_NONE);
    (void)instr_append(declarator->instructions, 5, section_instr, align_instr,
                       type_instr, size_instr, label_instr);
  }
}

ASTree *translate_prepare_init(ASTree *declaration, ASTree *declarator) {
  assert(instr_empty(declarator->instructions));
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo, &symbol);
  if (symbol == NULL)
    in_current_scope = state_get_member(state, declarator->lexinfo, &symbol);

#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(symbol && in_current_scope);

  if (type_is_function(declarator->type) || symbol->info == SYM_INHERITOR ||
      declarator->tok_kind == TOK_TYPE_NAME || symbol->storage == STORE_EXT ||
      symbol->linkage == LINK_TYPEDEF) {
    return astree_adopt(declaration, 1, declarator);
  } else if (symbol->storage == STORE_AUTO) {
    assert(symbol->linkage == LINK_NONE);
    symbol->disp = assign_stack_space(symbol->type);
    return astree_adopt(declaration, 1, declarator);
  } else {
    assert(symbol->storage == STORE_STAT);
    /* assign static id if this symbol has not appeared previously */
    if (symbol->linkage == LINK_NONE && symbol->instructions == NULL)
      symbol->static_id = next_static_id();
    translate_static_decl(declarator, symbol);
    return astree_adopt(declaration, 1, declarator);
  }
}

ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                             ASTree *declarator, ASTree *initializer) {
  assert(instr_empty(assignment->instructions));
  assert(!instr_empty(initializer->instructions));
  PFDBG0('g', "Translating local initialization");
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo, &symbol);
#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope && symbol);
  assert(symbol->linkage == LINK_NONE);
  assert(symbol->info == SYM_DEFINED);

  if (symbol->storage == STORE_STAT) {
    assert(!instr_empty(declarator->instructions));
    /* append directives directly to root node */
    (void)instr_append(parser_root->instructions, 2, declarator->instructions,
                       initializer->instructions);
    return astree_adopt(declaration, 1,
                        astree_adopt(assignment, 2, declarator, initializer));
  } else if (initializer->tok_kind != TOK_INIT_LIST &&
             initializer->tok_kind != TOK_STRINGCON &&
             initializer->cexpr_kind == CEXPR_FALSE) {
    assert(instr_empty(declarator->instructions));
    assert(symbol->instructions == NULL);
    Instruction *lea_instr = instr_init(OP_LEA);
    set_op_ind(&lea_instr->src, symbol->disp, RBP_VREG);
    set_op_reg(&lea_instr->dest, REG_QWORD, next_vreg());
    lea_instr->persist_flags |= PERSIST_DEST_CLEAR;

    (void)instr_append(declarator->instructions, 1, lea_instr);
    (void)translate_assignment(assignment, declarator, initializer);
    (void)instr_append(declaration->instructions, 1, assignment->instructions);
    /* `translate_assignment` performs adoption for subtree */
    return astree_adopt(declaration, 1, assignment);
  } else {
    assert(initializer->cexpr_kind != CEXPR_MAYBE);
    assert(instr_empty(declarator->instructions));
    assert(symbol->instructions == NULL);

    (void)instr_append(assignment->instructions, 1, initializer->instructions);
    (void)instr_append(declaration->instructions, 1, assignment->instructions);
    return astree_adopt(declaration, 1,
                        astree_adopt(assignment, 2, declarator, initializer));
  }
}

ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating local declaration");
  assert(instr_empty(declarator->instructions));

  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo, &symbol);
  if (symbol == NULL)
    in_current_scope = state_get_member(state, declarator->lexinfo, &symbol);

#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(symbol && in_current_scope);

  if (type_is_function(declarator->type) || symbol->info == SYM_INHERITOR ||
      declarator->tok_kind == TOK_TYPE_NAME || symbol->storage == STORE_EXT ||
      symbol->linkage == LINK_TYPEDEF) {
    return astree_adopt(declaration, 1, declarator);
  } else if (symbol->storage == STORE_STAT) {
    symbol->static_id = next_static_id();
    translate_static_decl(declarator, symbol);
    Instruction *zero_instr = instr_init(OP_ZERO);
    set_op_sym(&zero_instr->dest, symbol);
    /* append directives directly to root node */
    (void)instr_append(parser_root->instructions, 2, declarator->instructions,
                       zero_instr);
    return astree_adopt(declaration, 1, declarator);
  } else { /* symbol->storage == STORE_AUTO */
    assert(symbol->storage == STORE_AUTO);
    symbol->disp = assign_stack_space(symbol->type);
    return astree_adopt(declaration, 1, declarator);
  }
}

ASTree *translate_block_content(ASTree *block, ASTree *content) {
  (void)instr_append(block->instructions, 1, content->instructions);
  return astree_adopt(block, 1, content);
}

ASTree *translate_stmt_expr(ASTree *stmt_expr) {
  if (instr_empty(stmt_expr->instructions))
    (void)instr_append(stmt_expr->instructions, 1, instr_init(OP_NOP));
  return stmt_expr;
}

ASTree *translate_global_init(ASTree *declaration, ASTree *assignment,
                              ASTree *declarator, ASTree *initializer) {
  PFDBG0('g', "Translating global initialization");
  assert(!instr_empty(initializer->instructions));
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, declarator->lexinfo, &symbol);

  assert(symbol->linkage == LINK_EXT || symbol->linkage == LINK_INT);
  assert(symbol->storage == STORE_STAT);
  assert(symbol->instructions != NULL);

  if (instr_prev(symbol->instructions)->opcode == OP_ZERO) {
    /* previous declaration; rewrite it */
    instr_prev(symbol->instructions)->opcode = OP_ERASED;
    Instruction *section_instr = instr_next(symbol->instructions);
    if (section_instr->opcode == OP_GLOBL)
      section_instr = instr_next(section_instr);
    if (section_instr->opcode == OP_BSS)
      section_instr->opcode = OP_DATA;
    else
      assert(section_instr->opcode == OP_SECTION);
    (void)instr_append(symbol->instructions, 1, initializer->instructions);
  } else {
    assert(!instr_empty(declarator->instructions));
    (void)instr_append(assignment->instructions, 2, declarator->instructions,
                       initializer->instructions);
    (void)instr_append(declaration->instructions, 1, assignment->instructions);
  }

  return astree_adopt(declaration, 1,
                      astree_adopt(assignment, 2, declarator, initializer));
}

ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator) {
  PFDBG0('g', "Translating global declaration");
  assert(declarator->tok_kind == TOK_IDENT);
  Symbol *symbol = NULL;
  int in_current_scope = state_get_symbol(state, declarator->lexinfo, &symbol);
  if (symbol == NULL)
    in_current_scope = state_get_member(state, declarator->lexinfo, &symbol);

#ifdef NDEBUG
  (void)in_current_scope;
#endif
  assert(in_current_scope && symbol);

  if (symbol->storage == STORE_STAT && !type_is_function(declarator->type) &&
      symbol->info != SYM_DEFINED) {
    translate_static_decl(declarator, symbol);
    if (instr_prev(symbol->instructions)->opcode != OP_ZERO) {
      Instruction *zero_instr = instr_init(OP_ZERO);
      set_op_sym(&zero_instr->dest, symbol);
      (void)instr_append(declarator->instructions, 1, zero_instr);
    }
    (void)instr_append(declaration->instructions, 1, declarator->instructions);
  }

  return astree_adopt(declaration, 1, declarator);
}

ASTree *end_translate_declaration(ASTree *declaration) {
  if (instr_empty(declaration->instructions) &&
      scope_get_kind(state_peek_scope(state)) != SCOPE_FILE &&
      scope_get_kind(state_peek_scope(state)) != SCOPE_MEMBER)
    (void)instr_append(declaration->instructions, 1, instr_init(OP_NOP));
  return declaration;
}

ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body) {
  PFDBG0('g', "Translating function prologue");
  ++fn_count;
  ARR_RESIZE(spill_regions, 0, 0);

  Instruction *text_instr = instr_init(OP_TEXT);

  Instruction *type_instr = instr_init(OP_TYPE);
  set_op_dir(&type_instr->dest, declarator->lexinfo);
  set_op_dir(&type_instr->src, "@function");

  Instruction *label_instr = instr_init(OP_NONE);
  label_instr->label = declarator->lexinfo;

  Symbol *symbol = NULL;
  state_get_symbol(state, declarator->lexinfo, &symbol);
  assert(symbol != NULL);
  assert(symbol->info != SYM_HIDDEN);

  /* most function preamble instructions are attached to the declarator */
  if (symbol->linkage == LINK_EXT) {
    Instruction *globl_instr = instr_init(OP_GLOBL);
    set_op_dir(&globl_instr->dest, declarator->lexinfo);

    (void)instr_append(declarator->instructions, 4, text_instr, globl_instr,
                       type_instr, label_instr);
  } else {
    (void)instr_append(declarator->instructions, 3, text_instr, type_instr,
                       label_instr);
  }

  save_preserved_regs(declarator->instructions);

  /* emit rsp adjustment; set to bogus value initially since we don't know how
   * many bytes the register allocator will spill yet */
  Instruction *rsp_sub_instr = instr_init(OP_SUB);
  set_op_reg(&rsp_sub_instr->dest, REG_QWORD, RSP_VREG);
  set_op_imm(&rsp_sub_instr->src, PTRDIFF_MAX, IMM_UNSIGNED);

  (void)instr_append(declarator->instructions, 1, rsp_sub_instr);

  if (type_is_variadic_function(declarator->type))
    va_translate_params(declarator, body);
  else
    translate_params(declarator, body);
  return astree_adopt(declaration, 2, declarator, body);
}

ASTree *end_translate_fn(ASTree *declaration) {
  ASTree *body = astree_get(declaration, 2);
  assert(body->tok_kind == TOK_BLOCK);
  restore_preserved_regs(body->instructions);

  Instruction *return_instr = instr_init(OP_RET);
  (void)instr_append(body->instructions, 1, return_instr);

  ASTree *declarator = astree_get(declaration, 1);
  assert(declarator->tok_kind == TOK_IDENT);

  Instruction *size_instr = instr_init(OP_SIZE);
  set_op_dir(&size_instr->dest, declarator->lexinfo);
  set_op_dir(&size_instr->src, mk_fn_size(declarator->lexinfo));
  (void)instr_append(body->instructions, 1, size_instr);

  (void)instr_append(declaration->instructions, 2, declarator->instructions,
                     body->instructions);

  if (skip_liveness) goto no_live;
  liveness_sr(declaration->instructions);
  if (skip_allocator) goto no_alloc;
  /* this function will adjust window_size to account for spilled bytes */
  allocate_regs(declaration->instructions);
no_live:;
no_alloc:;

  /* align to ensure stack alignment to 16x + 8 */
  if (window_size & 0xf) window_size += 16 - (window_size & 0xf);

  /* set rsp adjustment to its actual value */
  Instruction *rsp_sub_instr = instr_prev(declarator->instructions);
  assert(rsp_sub_instr->opcode == OP_SUB);
  assert(rsp_sub_instr->dest.all.mode == MODE_REGISTER);
  assert(rsp_sub_instr->dest.reg.num == RSP_VREG);
  rsp_sub_instr->src.imm.val = window_size;
  return declaration;
}

ASTree *translate_topdecl(ASTree *unit, ASTree *topdecl) {
  (void)instr_append(unit->instructions, 1, topdecl->instructions);
  return astree_adopt(unit, 1, topdecl);
}

ASTree *translate_unit(ASTree *unit) {
  assert(unit->tok_kind == TOK_ROOT);
  Instruction *file_instr = instr_init(OP_FILE);
  set_op_dir(&file_instr->dest, unit->lexinfo);
  (void)instr_append(unit->instructions, 1, file_instr);
  return unit;
}

int generator_print_il(FILE *out) {
  static char buffer[MAX_INSTR_LENGTH];
  Instruction *instr = instr_next(parser_root->instructions);
  while (instr != parser_root->instructions) {
    int chars_written = instr_to_str(instr, buffer);
    if (chars_written < 0)
      return chars_written;
    else if (chars_written > 0)
      chars_written = fprintf(out, "%s\n", buffer);
    if (chars_written < 0) return chars_written;
    instr = instr_next(instr);
  }
  return 0;
}

int generator_debug_il(FILE *out) {
  static char buffer[MAX_INSTR_DEBUG_LENGTH];
  Instruction *instr = instr_next(parser_root->instructions);
  while (instr != parser_root->instructions) {
    int chars_written = instr_debug(instr, buffer);
    if (chars_written < 0) return chars_written;
    chars_written = fprintf(out, "%s\n", buffer);
    if (chars_written < 0) return chars_written;
    instr = instr_next(instr);
  }
  return 0;
}

void asmgen_init_globals(void) {
  ARR_INIT(literal_tokens, 16);
  ARR_INIT(literal_labels, 16);
  ARR_INIT(spill_regions, 2);
}

void asmgen_free_globals(void) {
  ARR_DESTROY(literal_tokens);
  ARR_DESTROY(literal_labels);
  ARR_DESTROY(spill_regions);
}
