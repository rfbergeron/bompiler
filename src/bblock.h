#ifndef __BBLOCK_H__
#define __BBLOCK_H__
#include "instr.h"

typedef struct basic_block BBlock;
struct basic_block {
  Instruction *leader;
  BBlock *seq_follower;
  BBlock *jump_follower;
};

BBlock *bblock_init(Instruction *leader);
void bblock_destroy(BBlock *bblock);
Instruction *bblock_get_leader(BBlock *bblock);
BBlock *bblock_get_seq_follower(BBlock *bblock);
void bblock_set_seq_follower(BBlock *bblock, BBlock *follower);
BBlock *bblock_get_jump_follower(BBlock *bblock);
void bblock_set_jump_follower(BBlock *bblock, BBlock *follower);
size_t bblock_partition(Instruction *instructions, BBlock ***out);
#endif
