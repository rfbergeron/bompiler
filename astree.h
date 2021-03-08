#ifndef __ASTREE_H__
#define __ASTREE_H__

#include <stdio.h>
#include <string.h>

#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"
//#include "lyutils.h"

typedef struct {
  size_t filenr;
  size_t linenr;
  size_t blocknr;
  size_t offset;
} Location;

typedef struct ASTree ASTree;

struct ASTree {
  int symbol;                 // token code
  const char *lexinfo;        // lexical information
  Location loc;               // source location
  Location decl_loc;          // for identies declaration location
  struct typespec type;       // type info
  unsigned int attributes;    // node-specific attributes
  LinkedList *children;       // children of this n-way node
  ASTree *next_sibling;       // for adopting long lists of siblings
  ASTree *firstborn;          // head of the list of siblings
};

void location_print(FILE *out, const Location location_);

ASTree *astree_init(int symbol_, const Location location, const char *lexinfo);
void astree_destroy(ASTree *astree_);
void astree_vdestroy(size_t count, ...);
ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3);
ASTree *astree_adopt_sym(ASTree *parent, int symbol, ASTree *child1,
                         ASTree *child2);
ASTree *astree_buddy_up(ASTree *astree_, ASTree *sibling);
ASTree *astree_first(ASTree *parent);
ASTree *astree_second(ASTree *parent);
ASTree *astree_third(ASTree *parent);
void astree_dump(ASTree *astree_, FILE *out);
void astree_dump_tree(ASTree *astree_, FILE *out, int depth);
void astree_print_tree(ASTree *astree_, FILE *out, int depth);
void astree_to_string(ASTree *astree, char *buffer, size_t size);
void location_to_string(Location location, char *buffer, size_t size);

#endif
