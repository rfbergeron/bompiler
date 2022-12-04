#ifndef __INITLISTS_H__
#define __INITLISTS_H__
#include "astree.h"
#include "attributes.h"
#include "badllist.h"

ASTree *traverse_initializer(const TypeSpec *type, ptrdiff_t disp,
                             ASTree *initializer, ListIter *where);
#endif
