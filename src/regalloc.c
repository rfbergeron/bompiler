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

/* The liveness table will be a badlib `Map` keyed with `Variable*`s and whose
 * values are `ListIter*`s corresponding to the next instruction they are used
 * at.
 */

static const char *LEADER_COMMENT = "basic block leader";
static const char *GROUP_COMMENT =
    "basic block leader; group with previous block";

static Map group_save_table;

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
