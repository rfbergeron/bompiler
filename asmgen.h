#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "attributes.h"
#include "astree.h"
#include "lyutils.h"

void intlang_init_globals();
void intlang_free_globals();
int intlang_generate();

#endif
