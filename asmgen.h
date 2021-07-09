#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "astree.h"
#include "attributes.h"
#include "lyutils.h"

void asmgen_init_globals();
void asmgen_free_globals();
int translate_file(ASTree *root);
int write_asm(FILE *out);

#endif
