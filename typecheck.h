#ifndef __TYPECHECK_H__
#define __TYPECHECK_H__

#include "astree.h"

void type_checker_init_globals();
void type_checker_free_globals();
int type_checker_make_table(ASTree *root);
void type_checker_dump_symbols(FILE *out);

#endif
