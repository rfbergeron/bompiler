#ifndef __REGALLOC_H__
#define __REGALLOC_H__
#include "badllist.h"
#include "symtable.h"

int liveness_sr(ListIter *first, ListIter *last);
int allocate_regs(ListIter *first, ListIter *last);

#endif
