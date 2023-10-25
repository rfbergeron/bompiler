#include "regalloc.h"

#include <assert.h>

#include "asmgen.h"
#include "badalist.h"
#include "badmap.h"
#include "string.h"
#include "symtable.h"

typedef struct basic_block {
  ListIter *leader;
  struct basic_block **followers;
  size_t followers_size;
  size_t followers_cap;
} BBlock;

static struct reg_desc {
  size_t vreg_num;
  InstructionData *next_use;
} *reg_descs[16];
typedef struct reg_desc RegDesc;
static size_t reg_descs_sizes[16];
static size_t reg_descs_caps[16];
/* location of 24-byte region for contents of unspilled registers */
static ptrdiff_t unspill_region = -32;

static ptrdiff_t *vreg_descs;
static size_t vreg_descs_size;
static size_t vreg_descs_cap;
static const char *LEADER_COMMENT = "basic block leader";
static const char *GROUP_COMMENT =
    "basic block leader; group with previous block";

static Map group_save_table;
static int in_function;
static unsigned short regs_clobbered;
static unsigned short used_volatile;

int instr_is_jump(InstructionData *data) {
  switch (data->opcode) {
    case OP_JMP:
    case OP_JE:
    case OP_JNE:
    case OP_JG:
    case OP_JGE:
    case OP_JL:
    case OP_JLE:
    case OP_JA:
    case OP_JAE:
    case OP_JB:
    case OP_JBE:
    case OP_JZ:
    case OP_JNZ:
      return 1;
    default:
      return 0;
  }
}

int instr_is_mov(InstructionData *data) {
  switch (data->opcode) {
    case OP_MOV:
    case OP_MOVS:
    case OP_MOVZ:
      return 1;
    default:
      return 0;
  }
}

int instr_is_setcc(InstructionData *data) {
  switch (data->opcode) {
    case OP_SETA:
    case OP_SETAE:
    case OP_SETB:
    case OP_SETBE:
    case OP_SETL:
    case OP_SETLE:
    case OP_SETG:
    case OP_SETGE:
    case OP_SETZ:
    case OP_SETNZ:
    case OP_SETE:
    case OP_SETNE:
      return 1;
    default:
      return 0;
  }
}

int instr_is_directive(InstructionData *data) {
  return optype_from_opcode(data->opcode) == OPTYPE_DIRECTIVE;
}

int instr_has_label(InstructionData *data) { return data->label != NULL; }

int reg_is_volatile(size_t reg_num) {
  size_t i;
  for (i = 0; i < VOLATILE_REG_COUNT; ++i)
    if (reg_num == VOLATILE_REGS[i]) return 1;
  return 0;
}

int bblock_in_group(BBlock *block) {
  InstructionData *data = liter_get(block->leader);
  ListIter *leader_prev = liter_prev(block->leader, 1);
  InstructionData *data_prev = liter_get(leader_prev);
  free(leader_prev);
  return (data != NULL && data->label != NULL &&
          ((strncmp(data->label, ".LT", 3) == 0) ||
           (strncmp(data->label, ".LF", 3) == 0))) ||
         (data_prev != NULL && data_prev->dest.all.mode == MODE_DIRECT &&
          (strncmp(data_prev->dest.dir.lab, ".LF", 3) == 0 ||
           strncmp(data_prev->dest.dir.lab, ".LT", 3) == 0));
}

BBlock *bblock_init(ListIter *leader) {
  if (leader == NULL) return NULL;
  BBlock *block = malloc(sizeof(BBlock));
  if (block == NULL) return NULL;
  block->leader = leader;
  block->followers = malloc(sizeof(BBlock *));
  if (block->followers == NULL) return free(block->leader), free(block), NULL;
  block->followers_cap = 1;
  block->followers_size = 0;
  return block;
}

void bblock_destroy(BBlock *block) {
  if (block == NULL) return;
  free(block->leader);
  free(block->followers);
  free(block);
}

int bblock_add_follower(BBlock *bblock, BBlock *follower) {
  if (bblock == NULL || follower == NULL) return -1;
  if (bblock->followers_size == bblock->followers_cap) {
    BBlock **new_followers = realloc(
        bblock->followers, sizeof(BBlock *) * (bblock->followers_cap <<= 1));
    if (new_followers == NULL) return -1;
    bblock->followers = new_followers;
  }
  bblock->followers[bblock->followers_size++] = follower;
  return 0;
}

int bblock_partition(ListIter *first, ListIter *last, ArrayList *out) {
  ListIter *exit_leader = liter_prev(last, 1);
  if (exit_leader == NULL) abort();
  while (instr_is_directive(liter_get(exit_leader)))
    if (liter_advance(exit_leader, -1) != 0) abort();
  int status = liter_advance(exit_leader, 1);
  if (status) abort();
  BBlock *exit_block = bblock_init(exit_leader);
  if (exit_block == NULL) abort();

  BBlock *enter_block = bblock_init(liter_copy(first));
  if (enter_block == NULL) abort();
  status = alist_push(out, enter_block);
  if (status) abort();
  /* TODO(Robert): iterator comparison functions */
  ListIter *current = liter_copy(first);
  if (current == NULL) abort();
  while (current->node != exit_leader->node &&
         instr_is_directive(liter_get(current)))
    if (liter_advance(current, 1) != 0) abort();

  while (current->node != exit_leader->node) {
    BBlock *block = bblock_init(liter_copy(current));
    int status = bblock_add_follower(alist_peek(out), block);
    if (status) abort();
    status = alist_push(out, block);
    if (status) abort();
    for (;;) {
      int status = liter_advance(current, 1);
      if (status) {
        abort();
      } else if (current->node == exit_leader->node ||
                 instr_has_label(liter_get(current))) {
        break;
      } else if (instr_is_jump(liter_get(current))) {
        int status = liter_advance(current, 1);
        if (status) abort();
        break;
      }
    }
  }

  free(current);
  status = bblock_add_follower(alist_peek(out), exit_block);
  if (status) abort();
  status = alist_push(out, exit_block);
  if (status) abort();
  return 0;
}

void update_liveness(InstructionData *data, Operand *operand, Map *temp_lives) {
  switch (operand->all.mode) {
    int status;
    case MODE_NONE:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
      break;
    case MODE_REGISTER:
      if (operand->reg.num < REAL_REG_COUNT) break;
      operand->reg.next_use =
          map_get(temp_lives, &operand->reg.num, sizeof(operand->reg.num));
      InstructionData *new_data =
          ((instr_is_mov(data) || instr_is_setcc(data)) &&
           operand == &data->dest &&
           (data->src.all.mode != MODE_REGISTER ||
            data->src.reg.num != operand->reg.num) &&
           map_get(&group_save_table, &operand->reg.num,
                   sizeof(operand->reg.num)) == NULL)
              ? NULL
              : data;
      if (map_insert(temp_lives, &operand->reg.num, sizeof(operand->reg.num),
                     new_data))
        abort();
      break;
    case MODE_INDIRECT:
      if (operand->ind.num < REAL_REG_COUNT) break;
      operand->ind.next_use =
          map_get(temp_lives, &operand->ind.num, sizeof(operand->ind.num));
      if (map_insert(temp_lives, &operand->ind.num, sizeof(operand->ind.num),
                     data))
        abort();
      break;
    case MODE_PIC:
      operand->pic.next_use = operand->pic.symval->next_use;
      operand->pic.symval->next_use = data;
      break;
    case MODE_SCALE:
      if (operand->sca.base < REAL_REG_COUNT) goto skip_base;
      operand->sca.base_next_use =
          map_get(temp_lives, &operand->sca.base, sizeof(operand->sca.base));
      if (map_insert(temp_lives, &operand->sca.base, sizeof(operand->sca.base),
                     data))
        abort();
    skip_base:
      if (operand->sca.index < REAL_REG_COUNT) break;
      operand->sca.index_next_use =
          map_get(temp_lives, &operand->sca.index, sizeof(operand->sca.index));
      if (map_insert(temp_lives, &operand->sca.index,
                     sizeof(operand->sca.index), data))
        abort();
      break;
    default:
      abort();
  }
}

int liveness_bblock(BBlock *block, Map *temp_lives) {
  ListIter *first = block->leader;
  ListIter *last = block->followers[0]->leader;
  ListIter *current = liter_prev(last, 1);
  if (current == NULL) abort();
  while (current->node != first->node->prev) {
    InstructionData *data = liter_get(current);
    if (data == NULL) abort();
    update_liveness(data, &data->dest, temp_lives);
    update_liveness(data, &data->src, temp_lives);
    if (liter_advance(current, -1)) abort();
  }
  free(current);
  return 0;
}

/* very simple compare function for badlib map */
static int compare_regnums(size_t *r1, size_t *r2) {
  if (r1 == NULL || r2 == NULL)
    return r1 == r2;
  else
    return *r1 == *r2;
}

int liveness_sr(ListIter *first, ListIter *last) {
  Map liveness_table;
  int status = map_init(&liveness_table, DEFAULT_MAP_SIZE, NULL, NULL,
                        (BlibComparator)compare_regnums);
  if (status) abort();
  status = map_init(&group_save_table, DEFAULT_MAP_SIZE, NULL, NULL,
                    (BlibComparator)compare_regnums);
  if (status) abort();
  ArrayList bblocks;
  status = alist_init(&bblocks, 0);
  if (status) abort();
  status = bblock_partition(first, last, &bblocks);
  if (status) abort();

  size_t i, bblocks_size = alist_size(&bblocks);
  /* skip first and last bblock, which contain only directives */
  for (i = 2; i < bblocks_size; ++i) {
    BBlock *block = alist_get(&bblocks, bblocks_size - i);
    int status = liveness_bblock(block, &liveness_table);
    if (status) abort();
    InstructionData *leader_data = liter_get(block->leader);
    if (bblock_in_group(block)) {
      leader_data->comment = GROUP_COMMENT;
      /* mark dummy vreg so that it is not cleared */
      if (leader_data->label != NULL &&
          (strncmp(leader_data->label, ".LT", 3) == 0 ||
           strncmp(leader_data->label, ".LF", 3) == 0) &&
          leader_data->opcode == OP_MOV &&
          leader_data->dest.all.mode == MODE_REGISTER &&
          leader_data->src.all.mode == MODE_REGISTER &&
          leader_data->dest.reg.num == leader_data->src.reg.num) {
        assert(map_get(&group_save_table, &leader_data->src.reg.num,
                       sizeof(leader_data->src.reg.num)) == NULL);
        int status = map_insert(&group_save_table, &leader_data->src.reg.num,
                                sizeof(leader_data->src.reg.num),
                                &leader_data->src.reg.num);
        if (status) abort();
      }
    } else {
      leader_data->comment = LEADER_COMMENT;
      if (map_clear(&liveness_table)) abort();
      if (map_clear(&group_save_table)) abort();
    }
  }

  status = map_destroy(&liveness_table);
  if (status) abort();
  status = map_destroy(&group_save_table);
  status = alist_destroy(&bblocks, (BlibDestroyer)bblock_destroy);
  if (status) abort();
  return 0;
}

int assign_or_spill(size_t *vreg_num, size_t vreg_width,
                    unsigned short *used_volatile) {
  if (*vreg_num < REAL_REG_COUNT) {
    *used_volatile |= 1 << *vreg_num;
    return 0;
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
  return 0;
}

int select_reg(ListIter *where, size_t *vreg_num, size_t vreg_width,
               InstructionData *next_use, unsigned short *used_volatile) {
  if (*vreg_num < REAL_REG_COUNT) return 0;
  ptrdiff_t location = vreg_descs[*vreg_num - REAL_REG_COUNT];
  if (location >= 0) {
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           *vreg_num);
    reg_descs[location][reg_descs_sizes[location] - 1].next_use = next_use;
    *vreg_num = location;
  } else {
    /* choose unspill reg */
    size_t unspill_reg = REAL_REG_COUNT, i;
    ptrdiff_t unspill_disp = 0;
    for (i = 0; i < VOLATILE_REG_COUNT; ++i) {
      if ((*used_volatile & 1 << VOLATILE_REGS[i]) == 0) {
        size_t j;
        for (j = 0; j < sizeof(unsigned short) * CHAR_BIT; ++j)
          if ((*used_volatile & 1 << j) != 0) unspill_disp += 8;
        assert(unspill_disp <= 16);
        *used_volatile |= 1 << VOLATILE_REGS[i];
        unspill_reg = VOLATILE_REGS[i];
        break;
      }
    }

    assert(unspill_disp <= 16);
    assert(unspill_reg != REAL_REG_COUNT);
    /* save contents of unspill reg */
    InstructionData *store_unspill_data = instr_init(OP_MOV);
    set_op_reg(&store_unspill_data->src, REG_QWORD, unspill_reg);
    set_op_ind(&store_unspill_data->dest, unspill_region + unspill_disp,
               RBP_VREG);
    /* load spilled vreg into unspill reg */
    InstructionData *load_spill_data = instr_init(OP_MOV);
    set_op_ind(&load_spill_data->src, location, RBP_VREG);
    set_op_reg(&load_spill_data->dest, vreg_width, unspill_reg);
    int status =
        liter_push_front(where, NULL, 2, store_unspill_data, load_spill_data);
    if (status) abort();
    /* spill register again */
    InstructionData *store_spill_data = instr_init(OP_MOV);
    set_op_reg(&store_spill_data->src, vreg_width, unspill_reg);
    set_op_ind(&store_spill_data->dest, location, RBP_VREG);
    /* load original value back into unspill register */
    InstructionData *load_unspill_data = instr_init(OP_MOV);
    set_op_ind(&load_unspill_data->src, unspill_region + unspill_disp,
               RBP_VREG);
    set_op_reg(&load_unspill_data->dest, REG_QWORD, unspill_reg);
    status =
        liter_push_back(where, NULL, 2, store_spill_data, load_unspill_data);
    if (status) abort();
    /* set vreg to unspill reg */
    *vreg_num = unspill_reg;
  }
  return 0;
}

int reg_thunk(ListIter *where, Operand *operand,
              unsigned short *used_volatile) {
  switch (operand->all.mode) {
    default:
      abort();
    case MODE_NONE:
    case MODE_PIC:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
      return 0;
    case MODE_REGISTER:
      return select_reg(where, &operand->reg.num, operand->reg.width,
                        operand->reg.next_use, used_volatile);
    case MODE_INDIRECT:
      return select_reg(where, &operand->ind.num, REG_QWORD,
                        operand->ind.next_use, used_volatile);
    case MODE_SCALE:
      return select_reg(where, &operand->sca.base, REG_QWORD,
                        operand->sca.base_next_use, used_volatile) ||
             select_reg(where, &operand->sca.index, REG_QWORD,
                        operand->sca.index_next_use, used_volatile);
  }
}

int assign_thunk(Operand *operand, unsigned short *used_volatile) {
  switch (operand->all.mode) {
    default:
      abort();
    case MODE_NONE:
    case MODE_PIC:
    case MODE_DIRECT:
    case MODE_IMMEDIATE:
      return 0;
    case MODE_REGISTER:
      return assign_or_spill(&operand->reg.num, operand->reg.width,
                             used_volatile);
    case MODE_INDIRECT:
      return assign_or_spill(&operand->ind.num, REG_QWORD, used_volatile);
    case MODE_SCALE:
      return assign_or_spill(&operand->sca.base, REG_QWORD, used_volatile) ||
             assign_or_spill(&operand->sca.index, REG_QWORD, used_volatile);
  }
}

int src_thunk_in_fn(ListIter *where, InstructionData *data,
                    unsigned short *used_volatile) {
  /* normal register assignment */
  if (data->src.all.mode != MODE_REGISTER || data->src.reg.num < REAL_REG_COUNT)
    return reg_thunk(where, &data->src, used_volatile);

  size_t vreg_index = data->src.reg.num - REAL_REG_COUNT;
  ptrdiff_t location = vreg_descs[vreg_index];
  if (vreg_descs_size <= vreg_index || location == (ptrdiff_t)REAL_REG_COUNT) {
    /* bulk_mtom vreg -> use rax no matter what */
    data->src.reg.num = 0;
  } else if (location < 0) {
    /* unspill in function call */
    if (data->dest.all.mode == MODE_REGISTER) {
      /* change operand to indirect mode if argument is passed in register */
      set_op_ind(&data->src, location, RBP_VREG);
    } else {
      /* use rax to move to another stack location and mark it as clobbered */
      InstructionData *unspill_data = instr_init(OP_MOV);
      set_op_ind(&unspill_data->src, location, RBP_VREG);
      set_op_reg(&unspill_data->dest, data->src.reg.width, 0);
      int status = liter_push_front(where, NULL, 1, unspill_data);
      if (status) abort();
      data->src = unspill_data->dest;
      regs_clobbered |= 1 << 0;
    }
  } else if (regs_clobbered & 1 << location) {
    /* value already clobbered -> load from stack, rsp-relative */
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           data->src.reg.num);
    ptrdiff_t volatile_index = 0;
    while ((size_t)volatile_index < VOLATILE_REG_COUNT &&
           VOLATILE_REGS[volatile_index] != (size_t)location)
      ++volatile_index;
    assert((size_t)volatile_index < VOLATILE_REG_COUNT);
    if (data->dest.all.mode == MODE_REGISTER) {
      /* change operand to indirect mode if argument is passed in register */
      set_op_ind(&data->src, volatile_index * 8, RSP_VREG);
    } else {
      /* otherwise, use rax to move to another stack location and mark it as
       * clobbered
       */
      InstructionData *unspill_data = instr_init(OP_MOV);
      set_op_ind(&unspill_data->src, volatile_index * 8, RSP_VREG);
      set_op_reg(&unspill_data->dest, data->src.reg.width, 0);
      int status = liter_push_front(where, NULL, 1, unspill_data);
      if (status) abort();
      data->src = unspill_data->dest;
      regs_clobbered |= 1 << 0;
    }
    /* remember to set next use information */
    reg_descs[location][reg_descs_sizes[location] - 1].next_use =
        data->src.reg.next_use;
  } else {
    /* replace register normally */
    return reg_thunk(where, &data->src, used_volatile);
  }
  return 0;
}

int dest_thunk_in_fn(ListIter *where, InstructionData *data,
                     unsigned short *used_volatile) {
  if (data->dest.all.mode != MODE_REGISTER) {
    return reg_thunk(where, &data->dest, used_volatile);
  } else if (data->dest.reg.num < REAL_REG_COUNT) {
    /* setting argument register; mark it as clobbered */
    if (data->opcode != OP_PUSH) regs_clobbered |= 1 << data->dest.reg.num;
    return 0;
  } else if (data->dest.reg.num - REAL_REG_COUNT >= vreg_descs_size ||
             vreg_descs[data->dest.reg.num - REAL_REG_COUNT] ==
                 (ptrdiff_t)REAL_REG_COUNT) {
    /* unmapped virtual register; use rax */
    data->dest.reg.num = 0;
    regs_clobbered |= 1 << 0;
    return 0;
  } else if (vreg_descs[data->dest.reg.num - REAL_REG_COUNT] < 0) {
    /* spilled mapped virtual register; we should be at the call instruction */
    /* replace with an indirect mode operand */
    assert(data->opcode == OP_CALL);
    set_op_ind(&data->dest, vreg_descs[data->dest.reg.num - REAL_REG_COUNT],
               RBP_VREG);
    return 0;
  } else if (regs_clobbered &
             1 << vreg_descs[data->dest.reg.num - REAL_REG_COUNT]) {
    /* clobbered mapped virtual register; should be at call */
    /* replace with rsp-relative indirect mode operand */
    ptrdiff_t location = vreg_descs[data->dest.reg.num - REAL_REG_COUNT];
    assert(data->opcode == OP_CALL);
    assert(reg_descs[location][reg_descs_sizes[location] - 1].vreg_num ==
           data->dest.reg.num);
    ptrdiff_t volatile_index = 0;
    while ((size_t)volatile_index < VOLATILE_REG_COUNT &&
           VOLATILE_REGS[volatile_index] != (size_t)location)
      ++volatile_index;
    assert((size_t)volatile_index < VOLATILE_REG_COUNT);
    set_op_ind(&data->dest, volatile_index * 8, RSP_VREG);
    /* remember to set next use information */
    reg_descs[location][reg_descs_sizes[location] - 1].next_use =
        data->src.reg.next_use;
    return 0;
  } else {
    return reg_thunk(where, &data->dest, used_volatile);
  }
}

int select_regs(ListIter *where) {
  InstructionData *data = liter_get(where);
  used_volatile = 0;
  int status = in_function ? src_thunk_in_fn(where, data, &used_volatile) ||
                                 dest_thunk_in_fn(where, data, &used_volatile)
                           : assign_thunk(&data->src, &used_volatile) ||
                                 assign_thunk(&data->dest, &used_volatile) ||
                                 reg_thunk(where, &data->src, &used_volatile) ||
                                 reg_thunk(where, &data->dest, &used_volatile);
  if (status) abort();
  if (data->opcode == OP_CALL) {
    in_function = 0;
  } else if (data->opcode == OP_PUSH && data->dest.all.mode == MODE_REGISTER &&
             data->dest.reg.num == 11) {
    in_function = 1;
    regs_clobbered = 0;
  }
  return 0;
}

int allocate_regs(ListIter *first, ListIter *last) {
  size_t i;
  for (i = 0; i < 16; ++i) {
    reg_descs_sizes[i] = 0;
    reg_descs_caps[i] = 2;
    reg_descs[i] = malloc(sizeof(**reg_descs) * reg_descs_caps[i]);
  }
  /* dummy entries for rsp and rbp since those aren't really general purpose */
  ++reg_descs_sizes[RSP_VREG];
  reg_descs[RSP_VREG][1].vreg_num = SIZE_MAX;
  reg_descs[RSP_VREG][1].next_use = liter_get(first);
  ++reg_descs_sizes[RBP_VREG];
  reg_descs[RBP_VREG][1].vreg_num = SIZE_MAX;
  reg_descs[RBP_VREG][1].next_use = liter_get(first);
  vreg_descs_size = 0;
  vreg_descs_cap = 8;
  vreg_descs = malloc(sizeof(ptrdiff_t) * vreg_descs_cap);
  ListIter *current = liter_copy(first);

  /* save window_size before spilling registers so that we know how many bytes
   * have been spilled for debugging purposes
   */
  ptrdiff_t old_window_size = window_size;

  /* we should be able to run through directives as though they were
   * instructions and nothing should happen to them, since their operands
   * should always be in immediate or direct mode
   */
  while (current->node != last->node) {
    int status = select_regs(current);
    if (status) abort();
    status = liter_advance(current, 1);
    if (status) abort();
  }

  free(current);
  for (i = 0; i < 16; ++i) free(reg_descs[i]);
  free(vreg_descs);
  return 0;
}
