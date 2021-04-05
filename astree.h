#ifndef __ASTREE_H__
#define __ASTREE_H__

#include <stdio.h>
#include <string.h>

#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"

typedef struct location Location;
struct location {
  size_t filenr;
  size_t linenr;
  size_t blocknr;
  size_t offset;
};

typedef struct ASTree ASTree;

struct ASTree {
  int symbol;              /* token code */
  const char *lexinfo;     /* lexical information */
  Location loc;            /* source location */
  struct typespec type;    /* type info */
  unsigned int attributes; /* node-specific attributes */
  LinkedList *children;    /* children of this n-way node */
  ASTree *next_sibling;    /* for adopting long lists of siblings */
  ASTree *firstborn;       /* head of the list of siblings */
};

void location_print(FILE *out, const Location location_);

ASTree *astree_init(int symbol_, const Location location, const char *lexinfo);
void astree_destroy(ASTree *tree);
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
