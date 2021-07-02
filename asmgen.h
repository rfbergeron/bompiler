#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "astree.h"
#include "attributes.h"
#include "lyutils.h"

void intlang_init_globals();
void intlang_free_globals();
int intlang_generate();

#endif
