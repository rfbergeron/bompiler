#ifndef __REGALLOC_H__
#define __REGALLOC_H__
#include "instr.h"

void liveness_sr(Instruction *instructions);
void allocate_regs(Instruction *instructions);

#endif
