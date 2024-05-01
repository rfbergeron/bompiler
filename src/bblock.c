#include "bblock.h"

#include <assert.h>
#include <stdlib.h>

#include "instr.h"

BBlock *bblock_init(Instruction *leader) {
  if (leader == NULL) return NULL;
  BBlock *bblock = malloc(sizeof(BBlock));
  bblock->leader = leader;
  bblock->seq_follower = NULL;
  bblock->jump_follower = NULL;
  return bblock;
}

void bblock_destroy(BBlock *bblock) {
  if (bblock == NULL) return;
  free(bblock);
}

Instruction *bblock_get_leader(BBlock *bblock) {
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

size_t bblock_partition(Instruction *instructions, BBlock ***out) {
  static const size_t BBLOCKS_START_CAP = 4;
  Instruction *exit_leader = instructions;
  while (instr_is_directive(instr_prev(exit_leader)))
    exit_leader = instr_prev(exit_leader);
  if (!instr_is_directive(exit_leader)) abort();

  BBlock *exit_block = bblock_init(exit_leader);
  if (exit_block == NULL) abort();

  size_t bblocks_cap = BBLOCKS_START_CAP;
  size_t bblocks_size = 0;
  BBlock **bblocks = malloc(bblocks_cap * sizeof(BBlock *));

  BBlock *enter_block = bblock_init(instr_next(instructions));
  if (enter_block == NULL) abort();
  if (bblocks_size >= bblocks_cap)
    bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
  bblocks[bblocks_size++] = enter_block;

  Instruction *current = instr_next(instructions);
  do current = instr_next(current);
  while (instr_is_directive(current));
  if (current->opcode == OP_SENTINEL) abort();

  while (current != exit_block->leader) {
    BBlock *block = bblock_init(current);
    bblock_set_seq_follower(bblocks[bblocks_size - 1], block);
    if (bblocks_size >= bblocks_cap)
      bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
    bblocks[bblocks_size++] = block;

    for (;;) {
      current = instr_next(current);
      if (current == exit_leader || instr_has_label(current)) {
        break;
      } else if (instr_is_jump(current)) {
        current = instr_next(current);
        break;
      }
    }
  }

  bblock_set_seq_follower(bblocks[bblocks_size - 1], exit_block);
  if (bblocks_size >= bblocks_cap)
    bblocks = realloc(bblocks, (bblocks_cap <<= 1) * sizeof(BBlock *));
  bblocks[bblocks_size++] = exit_block;
  *out = bblocks;
  return bblocks_size;
}
