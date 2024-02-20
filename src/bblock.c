#include "bblock.h"

#include <assert.h>
#include <stdlib.h>

#include "instr.h"

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

static void bblock_add_follower(BBlock *bblock, BBlock *follower) {
  assert(bblock != NULL && follower != NULL);
  if (bblock->followers_size == bblock->followers_cap) {
    bblock->followers = realloc(
        bblock->followers, sizeof(BBlock *) * (bblock->followers_cap <<= 1));
  }
  bblock->followers[bblock->followers_size++] = follower;
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

void bblock_partition(ListIter *first, ListIter *last, ArrayList *out) {
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
    bblock_add_follower(alist_peek(out), block);
    int status = alist_push(out, block);
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
  bblock_add_follower(alist_peek(out), exit_block);
  status = alist_push(out, exit_block);
  if (status) abort();
}
