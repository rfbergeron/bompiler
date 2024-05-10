#ifndef __TCHK_INIT_H__
#define __TCHK_INIT_H__
#include "astree.h"

ASTree *define_symbol(ASTree *decl_list, ASTree *equal_sign,
                      ASTree *initializer);
#endif
