#ifndef __INSTR_H__
#define __INSTR_H__

#include <stddef.h>

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
  GENERATOR(CBW, OPTYPE_NULLARY, 0, 0)       \
  GENERATOR(CWD, OPTYPE_NULLARY, 0, 0)       \
  GENERATOR(CDQ, OPTYPE_NULLARY, 0, 0)       \
  GENERATOR(CQO, OPTYPE_NULLARY, 0, 0)       \
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

#define RAX_VREG 0UL
#define RCX_VREG 1UL
#define RDX_VREG 2UL
#define RBX_VREG 3UL
#define RSP_VREG 4UL
#define RBP_VREG 5UL
#define RSI_VREG 6UL
#define RDI_VREG 7UL
#define R8_VREG 8UL
#define R9_VREG 9UL
#define R10_VREG 10UL
#define R11_VREG 11UL
#define R12_VREG 12UL
#define R13_VREG 13UL
#define R14_VREG 14UL
#define R15_VREG 15UL

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

typedef struct instruction Instruction;
typedef union operand {
  struct opall {
    AddressMode mode;
  } all;
  struct opreg {
    AddressMode mode;
    RegWidth width;
    size_t num;
    Instruction *next_use;
  } reg;
  struct opimm {
    AddressMode mode;
    int is_signed;
    unsigned long val;
  } imm;
  struct opdir {
    AddressMode mode;
    const char *lab;
    long disp;
  } dir;
  struct oppic {
    AddressMode mode;
    const char *lab;
    long disp;
  } pic;
  struct opind {
    AddressMode mode;
    size_t num;
    long disp;
    Instruction *next_use;
  } ind;
  struct opsca {
    AddressMode mode;
    IndexScale scale;
    size_t base;
    long disp;
    size_t index;
    Instruction *base_next_use;
    Instruction *index_next_use;
  } sca;
} Operand;

struct instruction {
  Opcode opcode;
  unsigned int persist_flags;
  const char *label;
  const char *comment;
  Operand dest;
  Operand src;
};

/* unfortunately, we can't use objects to hold array sizes because then they
 * can't be used in initializer lists, but when we use macros we can't use
 * arrays with deduced length, since the type will be incomplete in other
 * translation units
 */
#define ARRAY_ELEM_COUNT(array) (sizeof(array) / sizeof((array)[0]))
extern const size_t PARAM_REGS[6];
#define PARAM_REG_COUNT ARRAY_ELEM_COUNT(PARAM_REGS)
extern const size_t RETURN_REGS[2];
#define RETURN_REG_COUNT ARRAY_ELEM_COUNT(RETURN_REGS)
extern const size_t PRESERVED_REGS[5];
#define PRESERVED_REG_COUNT ARRAY_ELEM_COUNT(PRESERVED_REGS)
extern const size_t VOLATILE_REGS[9];
#define VOLATILE_REG_COUNT ARRAY_ELEM_COUNT(VOLATILE_REGS)
/* rsp and rbp are special purpose, and not included in preserved registers */
#define REAL_REG_COUNT (VOLATILE_REG_COUNT + PRESERVED_REG_COUNT + 2)

OpType optype_from_opcode(Opcode opcode);
int opcode_needs_width(Opcode opcode);
Instruction *instr_init(Opcode opcode);
void set_op_reg(Operand *operand, RegWidth width, size_t num);
void set_op_imm(Operand *operand, unsigned long val, int is_signed);
void set_op_dir(Operand *operand, const char *label);
void set_op_pic(Operand *operand, long disp, const char *label);
void set_op_ind(Operand *operand, long disp, size_t num);
void set_op_sca(Operand *operand, IndexScale scale, long disp, size_t base,
                size_t index);
int instr_debug(Instruction *instr, char *str);
int instr_to_str(Instruction *instr, char *str);
#endif
