#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "astree.h"
#include "badllist.h"
#include "lyutils.h"

/* macros used to generate information about opcodes */
#define FOREACH_OPCODE(GENERATOR)            \
  GENERATOR(INVALID, OPTYPE_INVALID, 0, 0)   \
  GENERATOR(NONE, OPTYPE_NULLARY, 0, 0)      \
  /* arithmetic */                           \
  GENERATOR(ADD, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(SUB, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(MUL, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(DIV, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(INC, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(DEC, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(NEG, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(IMUL, OPTYPE_CONTEXTUAL, 1, 0)   \
  GENERATOR(IDIV, OPTYPE_UNARY, 1, 0)        \
  /* compare and test */                     \
  GENERATOR(TEST, OPTYPE_BINARY, 1, 0)       \
  GENERATOR(CMP, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(SETE, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETNE, OPTYPE_UNARY, 0, 1)       \
  GENERATOR(SETG, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETGE, OPTYPE_UNARY, 0, 1)       \
  GENERATOR(SETL, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETLE, OPTYPE_UNARY, 0, 1)       \
  GENERATOR(SETA, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETAE, OPTYPE_UNARY, 0, 1)       \
  GENERATOR(SETB, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETBE, OPTYPE_UNARY, 0, 1)       \
  GENERATOR(SETZ, OPTYPE_UNARY, 0, 1)        \
  GENERATOR(SETNZ, OPTYPE_UNARY, 0, 1)       \
  /* jump */                                 \
  GENERATOR(JMP, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JE, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JNE, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JG, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JGE, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JL, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JLE, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JA, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JAE, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JB, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JBE, OPTYPE_UNARY, 0, 0)         \
  GENERATOR(JZ, OPTYPE_UNARY, 0, 0)          \
  GENERATOR(JNZ, OPTYPE_UNARY, 0, 0)         \
  /* logical and bitwise */                  \
  GENERATOR(NOT, OPTYPE_UNARY, 1, 0)         \
  GENERATOR(OR, OPTYPE_BINARY, 1, 0)         \
  GENERATOR(AND, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(LEA, OPTYPE_BINARY, 1, 1)        \
  GENERATOR(XOR, OPTYPE_BINARY, 1, 0)        \
  /* shifts */                               \
  GENERATOR(SHL, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(SAL, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(SHR, OPTYPE_BINARY, 1, 0)        \
  GENERATOR(SAR, OPTYPE_BINARY, 1, 0)        \
  /* code movement */                        \
  GENERATOR(MOV, OPTYPE_BINARY, 1, 1)        \
  GENERATOR(MOVZ, OPTYPE_BINARY, 1, 1)       \
  GENERATOR(MOVS, OPTYPE_BINARY, 1, 1)       \
  GENERATOR(PUSH, OPTYPE_UNARY, 1, 0)        \
  GENERATOR(POP, OPTYPE_UNARY, 1, 1)         \
  GENERATOR(CALL, OPTYPE_UNARY, 0, 0)        \
  GENERATOR(LEAVE, OPTYPE_NULLARY, 0, 0)     \
  GENERATOR(RET, OPTYPE_NULLARY, 0, 0)       \
  GENERATOR(NOP, OPTYPE_NULLARY, 0, 0)       \
  /* directives */                           \
  GENERATOR(GLOBL, OPTYPE_DIRECTIVE, 0, 0)   \
  GENERATOR(ZERO, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(BYTE, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(VALUE, OPTYPE_DIRECTIVE, 0, 0)   \
  GENERATOR(LONG, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(QUAD, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(ALIGN, OPTYPE_DIRECTIVE, 0, 0)   \
  GENERATOR(SIZE, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(TYPE, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(ASCII, OPTYPE_DIRECTIVE, 0, 0)   \
  GENERATOR(ASCIZ, OPTYPE_DIRECTIVE, 0, 0)   \
  GENERATOR(SECTION, OPTYPE_DIRECTIVE, 0, 0) \
  GENERATOR(BSS, OPTYPE_DIRECTIVE, 0, 0)     \
  GENERATOR(TEXT, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(DATA, OPTYPE_DIRECTIVE, 0, 0)    \
  GENERATOR(FILE, OPTYPE_DIRECTIVE, 0, 0)

#define GENERATE_ENUM(CODE, TYPE, BOOL, WRITES) OP_##CODE,
typedef enum opcode { FOREACH_OPCODE(GENERATE_ENUM) OPCODE_COUNT } Opcode;
#undef GENERATE_ENUM

typedef enum optype {
  OPTYPE_INVALID = -1,
  OPTYPE_NULLARY,
  OPTYPE_UNARY,
  OPTYPE_BINARY,
  OPTYPE_CONTEXTUAL,
  OPTYPE_DIRECTIVE
} OpType;
typedef enum address_mode {
  MODE_NONE,
  MODE_REGISTER,
  MODE_IMMEDIATE,
  MODE_DIRECT,
  MODE_INDIRECT,
  MODE_SCALE,
  MODE_PIC
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
typedef enum persist_flag {
  PERSIST_NONE = 0,
  PERSIST_SRC_SET = 1 << 0,
  PERSIST_DEST_SET = 1 << 1,
  PERSIST_SRC_CLEAR = 1 << 2,
  PERSIST_DEST_CLEAR = 1 << 3
} PersistFlag;

typedef union operand {
  struct opall {
    AddressMode mode;
  } all;
  struct opreg {
    AddressMode mode;
    RegWidth width;
    size_t num;
    struct instruction_data *next_use;
  } reg;
  struct opimm {
    AddressMode mode;
    int is_signed;
    uintmax_t val;
  } imm;
  struct opdir {
    AddressMode mode;
    const char *lab;
    intmax_t disp;
  } dir;
  struct oppic {
    AddressMode mode;
    const char *lab;
    SymbolValue *symval;
    intmax_t disp;
    struct instruction_data *next_use;
  } pic;
  struct opind {
    AddressMode mode;
    size_t num;
    intmax_t disp;
    struct instruction_data *next_use;
  } ind;
  struct opsca {
    AddressMode mode;
    IndexScale scale;
    size_t base;
    intmax_t disp;
    size_t index;
    struct instruction_data *base_next_use;
    struct instruction_data *index_next_use;
  } sca;
} Operand;

typedef struct instruction_data {
  Opcode opcode;
  unsigned int persist_flags;
  const char *label;
  const char *comment;
  Operand dest;
  Operand src;
} InstructionData;

extern const size_t RSP_VREG;
extern const size_t RBP_VREG;
extern const size_t REAL_REG_COUNT;
extern const size_t VOLATILE_REGS[];
extern const size_t VOLATILE_REG_COUNT;
extern ptrdiff_t window_size;

const char *mk_static_label(const char *name, size_t unique_id);
const char *mk_fnptr_text(const char *name);
InstructionData *instr_init(Opcode opcode);
void set_op_reg(Operand *operand, RegWidth width, size_t num);
void set_op_imm(Operand *operand, uintmax_t val, int is_signed);
void set_op_dir(Operand *operand, const char *label);
void set_op_pic(Operand *operand, intmax_t disp, const char *label,
                SymbolValue *symval);
void set_op_ind(Operand *operand, intmax_t disp, size_t num);
void set_op_sca(Operand *operand, IndexScale scale, intmax_t disp, size_t base,
                size_t index);
void maybe_load_cexpr(ASTree *expr, ListIter *where);
size_t asmgen_literal_label(const char *literal, const char **out);
int bulk_mzero(size_t dest_memreg, ptrdiff_t dest_disp, size_t skip_bytes,
               const Type *type, ListIter *where);
int static_zero_pad(size_t count, ListIter *where);
OpType optype_from_opcode(Opcode opcode);
ASTree *translate_empty_expr(ASTree *empty_expr);
ASTree *translate_ident(ASTree *ident);
ASTree *translate_cast(ASTree *cast, ASTree *expr);
ASTree *translate_logical_not(ASTree * not, ASTree *operand);
ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_comparison(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_indirection(ASTree *indirection, ASTree *operand);
ASTree *translate_addrof(ASTree *addrof, ASTree *operand);
ASTree *translate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index);
ASTree *translate_reference(ASTree *reference, ASTree *struct_, ASTree *member);
ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand);
ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand);
ASTree *translate_unop(ASTree *operator, ASTree * operand);
ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_multiplication(ASTree *operator, ASTree * left,
                                 ASTree *right);
ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_conditional(ASTree *qmark, ASTree *condition,
                              ASTree *true_expr, ASTree *false_expr);
ASTree *translate_comma(ASTree *comma, ASTree *left, ASTree *right);
ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue);
ASTree *translate_call(ASTree *call);
ASTree *translate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident);
ASTree *translate_va_end(ASTree *va_end_, ASTree *expr);
ASTree *translate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name);
ASTree *translate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                         ASTree *else_body);
ASTree *translate_switch(ASTree *switch_, ASTree *condition, ASTree *body);
ASTree *translate_while(ASTree *while_, ASTree *condition, ASTree *body);
ASTree *translate_for(ASTree *for_, ASTree *initializer, ASTree *condition,
                      ASTree *reinitializer, ASTree *body);
ASTree *translate_do(ASTree *do_, ASTree *body, ASTree *condition);
ASTree *translate_block(ASTree *block);
ASTree *translate_return(ASTree *ret, ASTree *expr);
ASTree *translate_continue(ASTree *continue_);
ASTree *translate_break(ASTree *break_);
ASTree *translate_goto(ASTree *goto_, ASTree *ident);
ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt);
ASTree *translate_case(ASTree *case_, ASTree *expr, ASTree *stmt);
ASTree *translate_default(ASTree *default_, ASTree *stmt);
ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                             ASTree *declarator, ASTree *initializer);
ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator);
ASTree *translate_global_init(ASTree *declaration, ASTree *assignment,
                              ASTree *declarator, ASTree *initializer);
ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator);
ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body);
ASTree *end_translate_fn(ASTree *declaration);
ASTree *translate_static_scalar_init(const Type *type, ASTree *initializer,
                                     ListIter *where);
ASTree *translate_auto_scalar_init(const Type *type, ptrdiff_t disp,
                                   ASTree *initializer, ListIter *where);
ASTree *translate_static_literal_init(const Type *type, ASTree *literal,
                                      ListIter *where);
ASTree *translate_auto_literal_init(const Type *type, ptrdiff_t disp,
                                    ASTree *literal, ListIter *where);
int generator_print_il(FILE *out);
int generator_debug_il(FILE *out);
void asmgen_init_globals(const char *filename);
void asmgen_free_globals(void);

#endif
