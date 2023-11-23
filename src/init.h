#ifndef __INITLISTS_H__
#define __INITLISTS_H__
#include "astree.h"
#include "badllist.h"
#include "bcc_types.h"

ASTree *traverse_initializer(Type *type, ptrdiff_t disp, ASTree *initializer,
                             ListIter *where);
#endif
