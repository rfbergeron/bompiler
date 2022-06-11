#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
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

extern ASTree EMPTY_EXPR;

ASTree *astree_init(int symbol, const Location location, const char *lexinfo);
int astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, const size_t count, ...);
ASTree *astree_replace(ASTree *parent, const size_t index, ASTree *child);
ASTree *astree_get(ASTree *parent, const size_t index);
ASTree *astree_remove(ASTree *parent, const size_t index);
size_t astree_count(ASTree *parent);
int astree_to_string(ASTree *astree, char *buffer, size_t size);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out);
#endif
