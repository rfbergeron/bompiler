#ifndef __BBLOCK_H__
#define __BBLOCK_H__
#include "badllist.h"

typedef struct basic_block BBlock;
struct basic_block {
  ListIter *leader;
  BBlock *seq_follower;
  BBlock *jump_follower;
};

BBlock *bblock_init(ListIter *leader);
void bblock_destroy(BBlock *bblock);
ListIter *bblock_get_leader(BBlock *bblock);
BBlock *bblock_get_seq_follower(BBlock *bblock);
void bblock_set_seq_follower(BBlock *bblock, BBlock *follower);
BBlock *bblock_get_jump_follower(BBlock *bblock);
void bblock_set_jump_follower(BBlock *bblock, BBlock *follower);
size_t bblock_partition(ListIter *first, ListIter *last, BBlock ***out);
#endif
