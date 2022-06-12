#ifndef __BCC_ERR_H__
#define __BCC_ERR_H__
#include "astree.h"
#define UNWRAP(node) \
  (node->symbol == TOK_TYPE_ERROR ? astree_get(node, 0) : node)
ASTree *create_type_error(ASTree *tree, int errcode);
ASTree *create_terr(ASTree *tree, int errcode, size_t info_count, ...);
ASTree *propogate_err(ASTree *parent, ASTree *child);
ASTree *propogate_err_v(ASTree *parent, size_t count, ...);
ASTree *propogate_err_a(ASTree *parent, size_t count, ASTree **children);
void create_error_symbol(SymbolValue *symval, int errcode);
#endif
