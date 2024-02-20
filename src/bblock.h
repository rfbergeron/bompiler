#ifndef __BBLOCK_H__
#define __BBLOCK_H__
#include "badalist.h"
#include "badllist.h"

typedef struct basic_block BBlock;
struct basic_block {
  ListIter *leader;
  BBlock **followers;
  size_t followers_size;
  size_t followers_cap;
};

BBlock *bblock_init(ListIter *leader);
void bblock_destroy(BBlock *block);
void bblock_partition(ListIter *first, ListIter *last, ArrayList *out);
#endif
