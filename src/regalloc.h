#ifndef __REGALLOC_H__
#define __REGALLOC_H__
#include "badllist.h"
#include "symtable.h"

void liveness_sr(ListIter *first, ListIter *last);
void allocate_regs(ListIter *first, ListIter *last);

#endif
