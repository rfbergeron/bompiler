#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "attributes.h"
#include "badllist.h"
#include "badmap.h"
#include "debug.h"
#include "symtable.h"

typedef struct astree {
  const TypeSpec *type;      /* type info */
  const char *lexinfo;       /* lexical information */
  SymbolTable *symbol_table; /* symbol table for scope, if applicable */
  Location loc;              /* source location */
  LinkedList children;       /* children of this n-way node */
  int symbol;                /* token code */
  unsigned int attributes;   /* node-specific attributes */
} ASTree;

#define EMPTY_EXPR_VALUE \
  { &SPEC_EMPTY, ";", NULL, LOC_EMPTY, BLIB_LLIST_EMPTY, ';', ATTR_NONE }
#define UNWRAP(node) \
  (node->symbol == TOK_TYPE_ERROR ? astree_get(node, 0) : node)
extern ASTree EMPTY_EXPR;

ASTree *astree_init(int symbol, const Location location, const char *lexinfo);
int astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, const size_t count, ...);
ASTree *astree_replace(ASTree *parent, const size_t index, ASTree *child);
ASTree *astree_get(ASTree *parent, const size_t index);
ASTree *astree_remove(ASTree *parent, const size_t index);
ASTree *astree_create_errnode(ASTree *child, int errcode, size_t info_count,
                              ...);
ASTree *astree_propogate_errnode(ASTree *parent, ASTree *child);
ASTree *astree_propogate_errnode_v(ASTree *parent, size_t count, ...);
ASTree *astree_propogate_errnode_a(ASTree *parent, size_t count,
                                   ASTree **children);
size_t astree_count(ASTree *parent);
int astree_to_string(ASTree *astree, char *buffer, size_t size);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out);
#endif
