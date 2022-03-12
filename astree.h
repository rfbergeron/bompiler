#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "symtable.h"

typedef struct astree {
  struct astree *next_sibling; /* for adopting long lists of siblings */
  struct astree *firstborn;    /* head of the list of siblings */
  const TypeSpec *type;        /* type info */
  const char *lexinfo;         /* lexical information */
  SymbolTable *symbol_table;   /* symbol table for scope, if applicable */
  Location loc;                /* source location */
  LinkedList children;         /* children of this n-way node */
  int symbol;                  /* token code */
  unsigned int attributes;     /* node-specific attributes */
} ASTree;

ASTree *astree_init(int symbol, const Location location, const char *lexinfo);
int astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3);
ASTree *astree_adopt_sym(ASTree *parent, int symbol, ASTree *child1,
                         ASTree *child2);
ASTree *astree_descend(ASTree *parent, ASTree *descendant);
ASTree *astree_inject(ASTree *old_node, ASTree *new_node);
ASTree *astree_twin(ASTree *sibling1, ASTree *sibling2);
ASTree *astree_first(ASTree *parent);
ASTree *astree_second(ASTree *parent);
ASTree *astree_third(ASTree *parent);
ASTree *astree_get(ASTree *parent, const size_t index);
size_t astree_count(ASTree *parent);
int astree_to_string(ASTree *astree, char *buffer, size_t size);
int astree_print_tree(ASTree *tree, FILE *out, int depth);
int astree_print_symbols(ASTree *tree, FILE *out);

#endif
