#include "bblock.h"

#include <assert.h>
#include <stdlib.h>

#include "instr.h"

BBlock *bblock_init(ListIter *leader) {
  if (leader == NULL) return NULL;
  BBlock *bblock = malloc(sizeof(BBlock));
  bblock->leader = leader;
  bblock->seq_follower = NULL;
  bblock->jump_follower = NULL;
  return bblock;
}

void bblock_destroy(BBlock *bblock) {
  if (bblock == NULL) return;
  free(bblock->leader);
  free(bblock);
}

ListIter *bblock_get_leader(BBlock *bblock) {
  return bblock == NULL ? NULL : bblock->leader;
}

BBlock *bblock_get_seq_follower(BBlock *bblock) {
  return bblock == NULL ? NULL : bblock->seq_follower;
}

void bblock_set_seq_follower(BBlock *bblock, BBlock *follower) {
  bblock->seq_follower = follower;
}

BBlock *bblock_get_jump_follower(BBlock *bblock) {
  return bblock == NULL ? NULL : bblock->jump_follower;
}

void bblock_set_jump_follower(BBlock *bblock, BBlock *follower) {
  bblock->jump_follower = follower;
}

static int instr_is_directive(Instruction *instr) {
  return optype_from_opcode(instr->opcode) == OPTYPE_DIRECTIVE;
}

static int instr_is_jump(Instruction *instr) {
  switch (instr->opcode) {
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

static int instr_has_label(Instruction *instr) { return instr->label != NULL; }

size_t bblock_partition(ListIter *first, ListIter *last, BBlock ***out) {
  static const size_t BBLOCKS_START_CAP = 4;
  ListIter *exit_leader = liter_prev(last, 1);
  if (exit_leader == NULL) abort();
  while (instr_is_directive(liter_get(exit_leader)))
    if (liter_advance(exit_leader, -1) != 0) abort();
  int status = liter_advance(exit_leader, 1);
  if (status) abort();
  BBlock *exit_block = bblock_init(exit_leader);
  if (exit_block == NULL) abort();

  size_t bblocks_cap = BBLOCKS_START_CAP;
  size_t bblocks_size = 0;
  BBlock **bblocks = malloc(bblocks_cap * sizeof(BBlock *));

  BBlock *enter_block = bblock_init(liter_copy(first));
  if (enter_block == NULL) abort();
  if (bblocks_size >= bblocks_cap)
    bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
  bblocks[bblocks_size++] = enter_block;
  /* TODO(Robert): iterator comparison functions */
  ListIter *current = liter_copy(first);
  if (current == NULL) abort();
  while (current->node != exit_leader->node &&
         instr_is_directive(liter_get(current)))
    if (liter_advance(current, 1) != 0) abort();

  while (current->node != exit_leader->node) {
    BBlock *block = bblock_init(liter_copy(current));
    bblock_set_seq_follower(bblocks[bblocks_size - 1], block);
    if (bblocks_size >= bblocks_cap)
      bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
    bblocks[bblocks_size++] = block;
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
  bblock_set_seq_follower(bblocks[bblocks_size - 1], exit_block);
  if (bblocks_size >= bblocks_cap)
    bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
  bblocks[bblocks_size++] = exit_block;
  *out = bblocks;
  return bblocks_size;
}
