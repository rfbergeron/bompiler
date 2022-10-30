#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "astree.h"
#include "attributes.h"
#include "lyutils.h"

int generator_print_il(FILE *out);
void asmgen_init_globals(const char *filename);
void asmgen_free_globals(void);

#endif
