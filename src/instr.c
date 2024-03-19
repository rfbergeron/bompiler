#include "instr.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
/* Base and index are registers; scale is limited to {1, 2, 4, 8}, and
 * displacement is a signed 32-bit integer.
 */
/* register order: rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8-r15 */
/* argument registers, in order: rdi, rsi, rdx, rcx, r8, r9 */
/* return registers: rax, rdx
 * preserved registers: rbx, rsp, rbp, r12-r15
 * other registers: r10, r11
 */
const size_t PARAM_REGS[] = {RDI_VREG, RSI_VREG, RDX_VREG,
                             RCX_VREG, R8_VREG,  R9_VREG};
const size_t RETURN_REGS[] = {RAX_VREG, RDX_VREG};
const size_t PRESERVED_REGS[] = {RBX_VREG, RSP_VREG, RBP_VREG, R12_VREG,
                                 R13_VREG, R14_VREG, R15_VREG};
const size_t VOLATILE_REGS[] = {RAX_VREG, RDX_VREG, RCX_VREG,
                                RSI_VREG, RDI_VREG, R8_VREG,
                                R9_VREG,  R10_VREG, R11_VREG};

/* number of eightbytes occupied by the function prologue on the stack */
static const char VREG_REG_TABLE[][5] = {
    GENERATE_T1_REGS(A),   GENERATE_T1_REGS(C),   GENERATE_T1_REGS(D),
    GENERATE_T1_REGS(B),   GENERATE_T2_REGS(SP),  GENERATE_T2_REGS(BP),
    GENERATE_T2_REGS(SI),  GENERATE_T2_REGS(DI),  GENERATE_T3_REGS(R8),
    GENERATE_T3_REGS(R9),  GENERATE_T3_REGS(R10), GENERATE_T3_REGS(R11),
    GENERATE_T3_REGS(R12), GENERATE_T3_REGS(R13), GENERATE_T3_REGS(R14),
    GENERATE_T3_REGS(R15)};

Instruction *instr_init(Opcode opcode) {
  Instruction *ret = calloc(1, sizeof(Instruction));
  ret->opcode = opcode;
  return ret;
}

void set_op_reg(Operand *operand, RegWidth width, size_t num) {
  operand->reg.mode = MODE_REGISTER;
  operand->reg.width = width;
  operand->reg.num = num;
  operand->reg.next_use = NULL;
}

void set_op_imm(Operand *operand, unsigned long val, int is_signed) {
  operand->imm.mode = MODE_IMMEDIATE;
  operand->imm.val = val;
  operand->imm.is_signed = is_signed;
}

void set_op_dir(Operand *operand, const char *label) {
  operand->dir.mode = MODE_DIRECT;
  operand->dir.lab = label;
}

void set_op_pic(Operand *operand, long disp, const char *label) {
  operand->pic.mode = MODE_PIC;
  operand->pic.disp = disp;
  operand->pic.lab = label;
}

void set_op_ind(Operand *operand, long disp, size_t num) {
  operand->ind.mode = MODE_INDIRECT;
  operand->ind.disp = disp;
  operand->ind.num = num;
  operand->ind.next_use = NULL;
}

void set_op_sca(Operand *operand, IndexScale scale, long disp, size_t base,
                size_t index) {
  operand->sca.mode = MODE_SCALE;
  operand->sca.scale = scale;
  operand->sca.disp = disp;
  operand->sca.base = base;
  operand->sca.index = index;
  operand->sca.base_next_use = NULL;
  operand->sca.index_next_use = NULL;
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

static int operand_to_str(Operand *operand, char *str) {
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

static int opcode_to_str(Instruction *instr, char *str) {
  switch (optype_from_opcode(instr->opcode)) {
    case OPTYPE_CONTEXTUAL:
      /* fallthrough */
    case OPTYPE_BINARY:
      if (instr->opcode == OP_MOVS || instr->opcode == OP_MOVZ) {
        assert(instr->src.all.mode == MODE_REGISTER &&
               instr->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c%c", OPCODES[instr->opcode],
                       WIDTH_TO_CHAR[instr->src.reg.width],
                       WIDTH_TO_CHAR[instr->dest.reg.width]);
      } else if (instr->src.all.mode == MODE_REGISTER) {
        assert(instr->opcode == OP_SHL || instr->opcode == OP_SHR ||
               instr->opcode == OP_SAL || instr->opcode == OP_SAR ||
               instr->opcode == OP_MOV ||
               instr->dest.all.mode != MODE_REGISTER ||
               instr->src.reg.width == instr->dest.reg.width);
        return sprintf(str, "%s%c", OPCODES[instr->opcode],
                       WIDTH_TO_CHAR[instr->src.reg.width]);
      } else {
        assert(instr->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[instr->opcode],
                       WIDTH_TO_CHAR[instr->dest.reg.width]);
      }
    case OPTYPE_UNARY:
      if (opcode_needs_width(instr->opcode)) {
        assert(instr->dest.all.mode == MODE_REGISTER);
        return sprintf(str, "%s%c", OPCODES[instr->opcode],
                       WIDTH_TO_CHAR[instr->dest.reg.width]);
      } else {
        return sprintf(str, "%s", OPCODES[instr->opcode]);
      }
    case OPTYPE_NULLARY:
      return sprintf(str, "%s", OPCODES[instr->opcode]);
    case OPTYPE_INVALID:
      /* fallthrough */
    case OPTYPE_DIRECTIVE:
      /* fallthrough */
    default:
      abort();
  }
}

static int bin_to_str(Instruction *instr, char *str) {
  static char opcode_str[MAX_OPCODE_LENGTH], dest_str[MAX_OPERAND_LENGTH],
      src_str[MAX_OPERAND_LENGTH];
  int chars_written = opcode_to_str(instr, opcode_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&instr->dest, dest_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&instr->src, src_str);
  if (chars_written < 0) return chars_written;

  return sprintf(str, "%s %s, %s", opcode_str, src_str, dest_str);
}

static int un_to_str(Instruction *instr, char *str) {
  static char opcode_str[MAX_OPCODE_LENGTH], dest_str[MAX_OPERAND_LENGTH];
  int chars_written = opcode_to_str(instr, opcode_str);
  if (chars_written < 0) return chars_written;

  chars_written = operand_to_str(&instr->dest, dest_str);
  if (chars_written < 0) return chars_written;

  if (instr->opcode == OP_CALL && instr->dest.all.mode == MODE_REGISTER)
    return sprintf(str, "%s *%s", opcode_str, dest_str);
  else
    return sprintf(str, "%s %s", opcode_str, dest_str);
}

static int dir_to_str(Instruction *instr, char *str) {
  switch (instr->opcode) {
    case OP_FILE:
      assert(instr->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s \"%s\"", OPCODES[OP_FILE], instr->dest.dir.lab);
    case OP_GLOBL:
      /* fallthrough */
    case OP_SECTION:
      assert(instr->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s", OPCODES[instr->opcode],
                     instr->dest.dir.lab);
    case OP_ZERO:
      /* fallthrough */
    case OP_ALIGN:
      assert(instr->dest.all.mode == MODE_IMMEDIATE);
      return sprintf(str, ".%s %lu", OPCODES[instr->opcode],
                     instr->dest.imm.val);
    case OP_BYTE:
      /* fallthrough */
    case OP_VALUE:
      /* fallthrough */
    case OP_LONG:
      /* fallthrough */
    case OP_QUAD:
      assert(instr->dest.all.mode == MODE_IMMEDIATE ||
             instr->dest.all.mode == MODE_PIC);
      if (instr->dest.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %li", OPCODES[instr->opcode],
                       instr->dest.imm.val);
      else
        return sprintf(str, ".%s %s%+li", OPCODES[instr->opcode],
                       instr->dest.pic.lab, instr->dest.pic.disp);
    case OP_SIZE:
      assert(instr->dest.all.mode == MODE_DIRECT);
      assert(instr->src.all.mode == MODE_DIRECT ||
             instr->src.all.mode == MODE_IMMEDIATE);
      if (instr->src.all.mode == MODE_IMMEDIATE)
        return sprintf(str, ".%s %s, %lu", OPCODES[OP_SIZE],
                       instr->dest.dir.lab, instr->src.imm.val);
      else
        return sprintf(str, ".%s %s, %s", OPCODES[OP_SIZE], instr->dest.dir.lab,
                       instr->src.dir.lab);
    case OP_TYPE:
      assert(instr->dest.all.mode == MODE_DIRECT);
      assert(instr->src.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s, %s", OPCODES[OP_TYPE], instr->dest.dir.lab,
                     instr->src.dir.lab);
    case OP_ASCIZ:
    case OP_ASCII:
      assert(instr->dest.all.mode == MODE_DIRECT);
      return sprintf(str, ".%s %s", OPCODES[instr->opcode],
                     instr->dest.dir.lab);
    case OP_BSS:
      /* fallthrough */
    case OP_TEXT:
      /* fallthrough */
    case OP_DATA:
      return sprintf(str, ".%s", OPCODES[instr->opcode]);
    default:
      abort();
  }
}

static int operand_debug(Operand *operand, char *str) {
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
                     operand->imm.val, (long)operand->imm.val,
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

int instr_debug(Instruction *instr, char *str) {
  static char src_buf[MAX_OPERAND_DEBUG_LENGTH],
      dest_buf[MAX_OPERAND_DEBUG_LENGTH];
  int status = operand_debug(&instr->src, src_buf);
  if (status < 0) return status;
  status = operand_debug(&instr->dest, dest_buf);
  if (status < 0) return status;
  return sprintf(str,
                 "Instruction %p {\n"
                 "\tLabel: %p \"%s\"\n"
                 "\tOpcode: %i %s\n"
                 "\tSource Operand%s"
                 "\tDestination Operand%s}",
                 (void *)instr, (void *)instr->label, instr->label,
                 instr->opcode, OPCODES[instr->opcode], src_buf, dest_buf);
}

int instr_to_str(Instruction *instr, char *str) {
  int ret = 0;
  if (instr->label != NULL) {
    int pad_count;
    if ((instr->opcode == OP_INVALID || instr->opcode == OP_NONE) &&
        !instr->comment)
      pad_count = 0;
    else if (strlen(instr->label) > 6)
      pad_count = 1;
    else
      pad_count = 7 - (int)strlen(instr->label);
    int chars_written =
        sprintf(str + ret, "%s:%*s", instr->label, pad_count, "");
    if (chars_written < 0) return chars_written;
    ret += chars_written;
  } else {
    str[ret++] = '\t';
    str[ret] = '\0';
  }
  switch (optype_from_opcode(instr->opcode)) {
    int chars_written;
    case OPTYPE_DIRECTIVE:
      chars_written = dir_to_str(instr, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_CONTEXTUAL:
      chars_written =
          (instr->src.all.mode == MODE_NONE ? un_to_str : bin_to_str)(
              instr, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_BINARY:
      chars_written = bin_to_str(instr, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_UNARY:
      chars_written = un_to_str(instr, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_NULLARY:
      if (instr->opcode == OP_NONE) break;
      chars_written = opcode_to_str(instr, str + ret);
      if (chars_written < 0) return chars_written;
      ret += chars_written;
      break;
    case OPTYPE_INVALID:
      /* fallthrough */
    default:
      abort();
  }
  if (instr->comment != NULL) {
    int chars_written =
        sprintf(str + ret, ret > 0 ? " # %s" : "# %s", instr->comment);
    if (chars_written < 0)
      return chars_written;
    else
      ret += chars_written;
  }
  return ret;
}
