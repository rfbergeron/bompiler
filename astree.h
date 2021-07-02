#ifndef __ASTREE_H__
#define __ASTREE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "debug.h"

typedef struct astree {
  LinkedList *children;        /* children of this n-way node */
  struct astree *next_sibling; /* for adopting long lists of siblings */
  struct astree *firstborn;    /* head of the list of siblings */
  Location loc;                /* source location */
  int symbol;                  /* token code */
  unsigned int attributes;     /* node-specific attributes */
  struct typespec type;        /* type info */
  const char *lexinfo;         /* lexical information */
  Map *symbol_table;           /* symbol table for scope, if applicable */
} ASTree;

void location_print(FILE *out, const Location location_);

int astree_init(ASTree *tree, int symbol_, const Location location,
                const char *lexinfo);
int astree_destroy(ASTree *tree);
ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3);
ASTree *astree_adopt_sym(ASTree *parent, int symbol, ASTree *child1,
                         ASTree *child2);
ASTree *astree_descend(ASTree *parent, ASTree *descendant);
ASTree *astree_twin(ASTree *sibling1, ASTree *sibling2);
ASTree *astree_first(ASTree *parent);
ASTree *astree_second(ASTree *parent);
ASTree *astree_third(ASTree *parent);
void astree_dump(ASTree *tree, FILE *out);
void astree_dump_tree(ASTree *tree, FILE *out, int depth);
void astree_print_tree(ASTree *tree, FILE *out, int depth);
void astree_to_string(ASTree *astree, char *buffer, size_t size);
void location_to_string(Location location, char *buffer, size_t size);

#endif
