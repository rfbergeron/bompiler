#include "regalloc.h"

#include <assert.h>
/* why on earth is SIZE_MAX in stdint.h and not limits.h? */
#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "bblock.h"
#include "instr.h"
#include "symtable.h"
#include "taboe_impl.h"

static struct reg_desc {
  size_t vreg_num;
  Instruction *next_use;
} *reg_descs[REAL_REG_COUNT];
typedef struct reg_desc RegDesc;
static size_t reg_descs_sizes[REAL_REG_COUNT];
static size_t reg_descs_caps[REAL_REG_COUNT];
/* locations of 8-byte region for contents of unspilled registers */
extern const ptrdiff_t UNSPILL_REGIONS[];
extern const size_t UNSPILL_REGIONS_SIZE;
extern ptrdiff_t window_size;

static ptrdiff_t *vreg_descs;
static size_t vreg_descs_size;
static size_t vreg_descs_cap;
static const char *LEADER_COMMENT = "basic block leader";

static int in_function;
static unsigned short regs_clobbered;
static unsigned short used_volatile;

/* hash by Thomas Wang https://gist.github.com/badboy/6267743 */
static unsigned long hash64shift(unsigned long key) {
  key = (~key) + (key << 21); /* key = (key << 21) - key - 1; */
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); /* key * 265 */
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); /* key * 21 */
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}

static size_t comp_size_t(size_t x, size_t y) { return y - x; }

#define DEFAULT_SIZE (sizeof(unsigned long) * CHAR_BIT)
TABOE_TDEF(LiveTable)
TABOE_TYPE(LiveTable, size_t, Instruction *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, LiveTable, live_table, size_t, Instruction *,
           unsigned long, hash64shift, comp_size_t)
static LiveTable *liveness_table;
static LiveTable *persistence_table;

/* TODO(Robert): this function is more complicated than it needs to be. while it
 * might make more since on a RISC architecture with 3-operand instructions, on
 * x86 most instructions which overwrite dest also use the contents of dest in
 * performing their operation; eg ADD does overwrite dest, but it also uses the
 * value in dest to perform the addition, so the register would still be live
 * at the beginning of the instruction. very few instructions write to dest in
 * a purely destructive manner; namely the SETcc, MOV, and LEA instructions.
 */
static int instr_writes_dest(Instruction *instr) {
#define GENERATE_WRITES(CODE, TYPE, BOOL, WRITES) WRITES,
  static const int OPCODE_WRITE_TABLE[] = {FOREACH_OPCODE(GENERATE_WRITES)};
#undef GENERATE_WRITES
  if (instr->opcode == OP_MOV && instr->src.all.mode == MODE_REGISTER &&
      instr->dest.all.mode == MODE_REGISTER &&
      instr->dest.reg.num == instr->src.reg.num) {
    /* dummy mov from a register to itself; vreg is still live */
    assert(instr->dest.reg.width == instr->src.reg.width);
    return 0;
  } else {
    return OPCODE_WRITE_TABLE[instr->opcode];
  }
}

static void liveness_helper(size_t *vreg_num, Instruction **next_use_out,
                            int clear_persist, int set_persist) {
  /* new use info must be in `next_use_out` */
  Instruction *new_next_use = *next_use_out;

  int exists = live_table_get(liveness_table, *vreg_num, next_use_out);
  if (!exists)
    exists = live_table_get(persistence_table, *vreg_num, next_use_out);
  if (!exists) *next_use_out = NULL;

  /* PERSIST_*_SET takes precedence over PERSIST_*_CLEAR */
  if (set_persist) {
    live_table_put(persistence_table, *vreg_num, new_next_use);
  } else if (clear_persist) {
    live_table_put(persistence_table, *vreg_num, NULL);
  }

  live_table_put(liveness_table, *vreg_num, new_next_use);
}

static void update_liveness(Instruction *instr, Operand *operand) {
  switch (operand->all.mode) {
    case MODE_NONE:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
    case MODE_PIC:
    case MODE_SYMBOL:
      break;
    case MODE_REGISTER:
      if (operand->reg.num < REAL_REG_COUNT) break;
      /* set next use to NULL if operand is dest and instruction overwrites
       * dest without making use of the existing value
       */
      operand->reg.next_use =
          operand == &instr->dest && instr_writes_dest(instr) ? NULL : instr;
      liveness_helper(&operand->reg.num, &operand->reg.next_use,
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_CLEAR)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_CLEAR)),
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_SET)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_SET)));
      break;
    case MODE_INDIRECT:
      if (operand->ind.num < REAL_REG_COUNT) break;
      operand->ind.next_use = instr;
      liveness_helper(&operand->ind.num, &operand->ind.next_use,
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_CLEAR)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_CLEAR)),
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_SET)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_SET)));
      break;
    case MODE_SCALE:
      if (operand->sca.base < REAL_REG_COUNT) goto skip_base;
      operand->sca.base_next_use = instr;
      liveness_helper(&operand->sca.base, &operand->sca.base_next_use,
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_CLEAR)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_CLEAR)),
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_SET)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_SET)));
    skip_base:
      if (operand->sca.index < REAL_REG_COUNT) break;
      operand->sca.index_next_use = instr;
      liveness_helper(&operand->sca.index, &operand->sca.index_next_use,
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_CLEAR)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_CLEAR)),
                      (operand == &instr->dest &&
                       (instr->persist_flags & PERSIST_DEST_SET)) ||
                          (operand == &instr->src &&
                           (instr->persist_flags & PERSIST_SRC_SET)));
      break;
    default:
      abort();
  }
}

static void liveness_bblock(BBlock *block) {
  Instruction *end = instr_prev(bblock_get_leader(block));
  Instruction *current =
      instr_prev(bblock_get_leader(bblock_get_seq_follower(block)));

  while (current != end) {
    update_liveness(current, &current->dest);
    update_liveness(current, &current->src);
    current = instr_prev(current);
  }
}

void liveness_sr(Instruction *instructions) {
  persistence_table = live_table_init(DEFAULT_SIZE);
  liveness_table = live_table_init(DEFAULT_SIZE);
  BBlock **bblocks;
  size_t bblocks_size = bblock_partition(instructions, &bblocks);
  assert(bblocks_size > 0);

  size_t i;
  /* skip first and last bblock, which contain only directives */
  for (i = 2; i < bblocks_size; ++i) {
    BBlock *block = bblocks[bblocks_size - i];
    liveness_bblock(block);
    bblock_get_leader(block)->comment = LEADER_COMMENT;
    live_table_clear(liveness_table);
  }

  for (i = 0; i < bblocks_size; ++i) bblock_destroy(bblocks[i]);
  free(bblocks);
  live_table_destroy(persistence_table);
  live_table_destroy(liveness_table);
}

static void assign_or_spill(size_t *vreg_num, size_t vreg_width,
                            unsigned short *used_volatile) {
  if (*vreg_num < REAL_REG_COUNT) {
    *used_volatile |= 1 << *vreg_num;
    return;
  }

  size_t vreg_index = *vreg_num - REAL_REG_COUNT;
  if (vreg_index >= vreg_descs_size) {
    if (vreg_descs_cap <= vreg_index) {
      while (vreg_descs_cap <= vreg_index) vreg_descs_cap <<= 1;
      vreg_descs = realloc(vreg_descs, sizeof(ptrdiff_t) * vreg_descs_cap);
      if (vreg_descs == NULL) abort();
    }
    size_t i;
    for (i = vreg_descs_size; i <= vreg_index; ++i)
      vreg_descs[i] = REAL_REG_COUNT;
    vreg_descs_size = vreg_index + 1;
  }

  if (vreg_descs[vreg_index] == (ptrdiff_t)REAL_REG_COUNT) {
    size_t i, next_available = REAL_REG_COUNT;
    for (i = 0; i < REAL_REG_COUNT; ++i) {
      /* the last entry in each descriptor should be the only live variable,
       * if any of them are live at all
       */
      if ((reg_descs_sizes[i] == 0 ||
           reg_descs[i][reg_descs_sizes[i] - 1].next_use == NULL) &&
          (*used_volatile & 1 << i) == 0) {
        next_available = i;
        break;
      }
    }

    if (next_available < REAL_REG_COUNT) {
      /* resize register descriptor if necessary */
      if (reg_descs_sizes[next_available] == reg_descs_caps[next_available]) {
        reg_descs[next_available] =
            realloc(reg_descs[next_available],
                    sizeof(RegDesc) * (reg_descs_caps[next_available] <<= 1));
      }
      /* set vreg number */
      reg_descs[next_available][reg_descs_sizes[next_available]].vreg_num =
          *vreg_num;
      ++reg_descs_sizes[next_available];
      /* set vreg descriptor */
      vreg_descs[vreg_index] = next_available;
    } else {
      /* spill */
      ptrdiff_t padding = window_size % vreg_width == 0
                              ? 0
                              : vreg_width - window_size % vreg_width;
      window_size += padding + vreg_width;
      vreg_descs[vreg_index] = -window_size;
    }
  } else {
    ptrdiff_t location = vreg_descs[vreg_index];
    if (location >= 0)
      assert(location < (ptrdiff_t)REAL_REG_COUNT &&
             reg_descs[location][reg_descs_sizes[location] - 1].next_use !=
                 NULL);
  }

  if (vreg_descs[vreg_index] >= 0)
    *used_volatile |= 1 << vreg_descs[vreg_index];
}

static void select_reg(Instruction *instr, size_t *vreg_num, size_t vreg_width,
                       Instruction *next_use, unsigned short *used_volatile) {
  assert(instr->opcode != OP_SENTINEL);
  if (*vreg_num < REAL_REG_COUNT) return;
  ptrdiff_t location = vreg_descs[*vreg_num - REAL_REG_COUNT];
  if (location >= 0) {
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           *vreg_num);
    reg_descs[location][reg_descs_sizes[location] - 1].next_use = next_use;
    *vreg_num = location;
  } else {
    /* choose unspill reg */
    size_t unspill_reg = REAL_REG_COUNT, i;
    size_t unspill_index = 0;
    for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
      if ((*used_volatile & 1 << VOLATILE_REGS[i]) == 0) {
        size_t j;
        for (j = 0; j < sizeof(unsigned short) * CHAR_BIT; ++j)
          if ((*used_volatile & 1 << j) != 0) ++unspill_index;
        *used_volatile |= 1 << VOLATILE_REGS[i];
        unspill_reg = VOLATILE_REGS[i];
        break;
      }
    }

    assert(unspill_index < UNSPILL_REGIONS_SIZE);
    assert(unspill_reg != REAL_REG_COUNT);
    /* save contents of unspill reg */
    Instruction *store_unspill_instr = instr_init(OP_MOV);
    set_op_reg(&store_unspill_instr->src, REG_QWORD, unspill_reg);
    set_op_ind(&store_unspill_instr->dest, UNSPILL_REGIONS[unspill_index],
               RBP_VREG);
    /* load spilled vreg into unspill reg */
    Instruction *load_spill_instr = instr_init(OP_MOV);
    set_op_ind(&load_spill_instr->src, location, RBP_VREG);
    set_op_reg(&load_spill_instr->dest, vreg_width, unspill_reg);
    (void)instr_prepend(instr, 2, store_unspill_instr, load_spill_instr);

    /* spill register again */
    Instruction *store_spill_instr = instr_init(OP_MOV);
    set_op_reg(&store_spill_instr->src, vreg_width, unspill_reg);
    set_op_ind(&store_spill_instr->dest, location, RBP_VREG);
    /* load original value back into unspill register */
    Instruction *load_unspill_instr = instr_init(OP_MOV);
    set_op_ind(&load_unspill_instr->src, UNSPILL_REGIONS[unspill_index],
               RBP_VREG);
    set_op_reg(&load_unspill_instr->dest, REG_QWORD, unspill_reg);
    (void)instr_append(instr, 2, store_spill_instr, load_unspill_instr);

    /* set vreg to unspill reg */
    *vreg_num = unspill_reg;
  }
}

static void reg_thunk(Instruction *instr, Operand *operand,
                      unsigned short *used_volatile) {
  switch (operand->all.mode) {
    default:
      abort();
    case MODE_NONE:
    case MODE_PIC:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
    case MODE_SYMBOL:
      return;
    case MODE_REGISTER:
      select_reg(instr, &operand->reg.num, operand->reg.width,
                 operand->reg.next_use, used_volatile);
      return;
    case MODE_INDIRECT:
      select_reg(instr, &operand->ind.num, REG_QWORD, operand->ind.next_use,
                 used_volatile);
      return;
    case MODE_SCALE:
      select_reg(instr, &operand->sca.base, REG_QWORD,
                 operand->sca.base_next_use, used_volatile);
      select_reg(instr, &operand->sca.index, REG_QWORD,
                 operand->sca.index_next_use, used_volatile);
      return;
  }
}

static void assign_thunk(Operand *operand, unsigned short *used_volatile) {
  switch (operand->all.mode) {
    default:
      abort();
    case MODE_NONE:
    case MODE_PIC:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
    case MODE_SYMBOL:
      return;
    case MODE_REGISTER:
      assign_or_spill(&operand->reg.num, operand->reg.width, used_volatile);
      return;
    case MODE_INDIRECT:
      assign_or_spill(&operand->ind.num, REG_QWORD, used_volatile);
      return;
    case MODE_SCALE:
      assign_or_spill(&operand->sca.base, REG_QWORD, used_volatile);
      assign_or_spill(&operand->sca.index, REG_QWORD, used_volatile);
      return;
  }
}

static void src_thunk_in_fn(Instruction *instr, unsigned short *used_volatile) {
  /* normal register assignment */
  if (instr->src.all.mode != MODE_REGISTER ||
      instr->src.reg.num < REAL_REG_COUNT) {
    reg_thunk(instr, &instr->src, used_volatile);
    return;
  }

  size_t vreg_index = instr->src.reg.num - REAL_REG_COUNT;
  ptrdiff_t location = vreg_descs[vreg_index];
  if (vreg_descs_size <= vreg_index || location == (ptrdiff_t)REAL_REG_COUNT) {
    /* bulk_mtom vreg -> use rax no matter what */
    instr->src.reg.num = 0;
  } else if (location < 0) {
    /* unspill in function call */
    if (instr->dest.all.mode == MODE_REGISTER) {
      /* change operand to indirect mode if argument is passed in register */
      set_op_ind(&instr->src, location, RBP_VREG);
    } else {
      /* use rax to move to another stack location and mark it as clobbered */
      Instruction *unspill_instr = instr_init(OP_MOV);
      set_op_ind(&unspill_instr->src, location, RBP_VREG);
      set_op_reg(&unspill_instr->dest, instr->src.reg.width, 0);
      (void)instr_prepend(instr, 1, unspill_instr);
      instr->src = unspill_instr->dest;
      regs_clobbered |= 1 << 0;
    }
  } else if (regs_clobbered & 1 << location) {
    /* value already clobbered -> load from stack, rsp-relative */
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           instr->src.reg.num);
    ptrdiff_t volatile_index = 0;
    while ((size_t)volatile_index < VOLATILE_REG_COUNT &&
           VOLATILE_REGS[volatile_index] != (size_t)location)
      ++volatile_index;
    assert((size_t)volatile_index < VOLATILE_REG_COUNT);
    if (instr->dest.all.mode == MODE_REGISTER) {
      /* change operand to indirect mode if argument is passed in register */
      set_op_ind(&instr->src, volatile_index * X64_SIZEOF_LONG, RSP_VREG);
    } else {
      /* otherwise, use rax to move to another stack location and mark it as
       * clobbered
       */
      Instruction *unspill_instr = instr_init(OP_MOV);
      set_op_ind(&unspill_instr->src, volatile_index * X64_SIZEOF_LONG,
                 RSP_VREG);
      set_op_reg(&unspill_instr->dest, instr->src.reg.width, 0);
      (void)instr_prepend(instr, 1, unspill_instr);
      instr->src = unspill_instr->dest;
      regs_clobbered |= 1 << 0;
    }
    /* remember to set next use information */
    reg_descs[location][reg_descs_sizes[location] - 1].next_use =
        instr->src.reg.next_use;
  } else {
    /* replace register normally */
    reg_thunk(instr, &instr->src, used_volatile);
    return;
  }
}

static void dest_thunk_in_fn(Instruction *instr,
                             unsigned short *used_volatile) {
  if (instr->dest.all.mode != MODE_REGISTER) {
    reg_thunk(instr, &instr->dest, used_volatile);
  } else if (instr->dest.reg.num < REAL_REG_COUNT) {
    /* setting argument register; mark it as clobbered */
    if (instr->opcode != OP_PUSH) regs_clobbered |= 1 << instr->dest.reg.num;
  } else if (instr->dest.reg.num - REAL_REG_COUNT >= vreg_descs_size ||
             vreg_descs[instr->dest.reg.num - REAL_REG_COUNT] ==
                 (ptrdiff_t)REAL_REG_COUNT) {
    /* unmapped virtual register; use rax */
    instr->dest.reg.num = 0;
    regs_clobbered |= 1 << 0;
  } else if (vreg_descs[instr->dest.reg.num - REAL_REG_COUNT] < 0) {
    /* spilled mapped virtual register; we should be at the call instruction */
    /* replace with an indirect mode operand */
    assert(instr->opcode == OP_CALL);
    set_op_ind(&instr->dest, vreg_descs[instr->dest.reg.num - REAL_REG_COUNT],
               RBP_VREG);
  } else if (regs_clobbered &
             1 << vreg_descs[instr->dest.reg.num - REAL_REG_COUNT]) {
    /* clobbered mapped virtual register; should be at call */
    /* replace with rsp-relative indirect mode operand */
    ptrdiff_t location = vreg_descs[instr->dest.reg.num - REAL_REG_COUNT];
    assert(instr->opcode == OP_CALL);
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           instr->dest.reg.num);
    ptrdiff_t volatile_index = 0;
    while ((size_t)volatile_index < VOLATILE_REG_COUNT &&
           VOLATILE_REGS[volatile_index] != (size_t)location)
      ++volatile_index;
    assert((size_t)volatile_index < VOLATILE_REG_COUNT);
    set_op_ind(&instr->dest, volatile_index * X64_SIZEOF_LONG, RSP_VREG);
    /* remember to set next use information */
    reg_descs[location][reg_descs_sizes[location] - 1].next_use =
        instr->src.reg.next_use;
  } else {
    reg_thunk(instr, &instr->dest, used_volatile);
  }
}

static void select_regs(Instruction *instr) {
  used_volatile = 0;
  if (in_function) {
    src_thunk_in_fn(instr, &used_volatile);
    dest_thunk_in_fn(instr, &used_volatile);
  } else {
    assign_thunk(&instr->src, &used_volatile);
    assign_thunk(&instr->dest, &used_volatile);
    reg_thunk(instr, &instr->src, &used_volatile);
    reg_thunk(instr, &instr->dest, &used_volatile);
  }

  if (instr->opcode == OP_CALL) {
    in_function = 0;
  } else if (instr->opcode == OP_PUSH &&
             instr->dest.all.mode == MODE_REGISTER &&
             instr->dest.reg.num == R11_VREG) {
    in_function = 1;
    regs_clobbered = 0;
  }
}

void allocate_regs(Instruction *instructions) {
  static const size_t INIT_VREG_DESCS_CAP = 8;
  size_t i;
  for (i = 0; i < REAL_REG_COUNT; ++i) {
    static const size_t INIT_REG_DESCS_CAP = 2;
    reg_descs_sizes[i] = 0;
    reg_descs_caps[i] = INIT_REG_DESCS_CAP;
    reg_descs[i] = malloc(sizeof(**reg_descs) * reg_descs_caps[i]);
  }
  /* dummy entries for rsp and rbp since those aren't really general purpose */
  ++reg_descs_sizes[RSP_VREG];
  reg_descs[RSP_VREG][1].vreg_num = SIZE_MAX;
  reg_descs[RSP_VREG][1].next_use = instructions;
  ++reg_descs_sizes[RBP_VREG];
  reg_descs[RBP_VREG][1].vreg_num = SIZE_MAX;
  reg_descs[RBP_VREG][1].next_use = instructions;
  vreg_descs_size = 0;
  vreg_descs_cap = INIT_VREG_DESCS_CAP;
  vreg_descs = malloc(sizeof(ptrdiff_t) * vreg_descs_cap);
  Instruction *current = instr_next(instructions);

  /* save window_size before spilling registers so that we know how many bytes
   * have been spilled for debugging purposes
   */
  /* ptrdiff_t old_window_size = window_size; */

  /* we should be able to run through directives as though they were
   * instructions and nothing should happen to them, since their operands
   * should always be in immediate or direct mode
   */
  while (current != instructions) {
    select_regs(current);
    current = instr_next(current);
  }

  for (i = 0; i < REAL_REG_COUNT; ++i) free(reg_descs[i]);
  free(vreg_descs);
}
