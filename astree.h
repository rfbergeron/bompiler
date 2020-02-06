#ifndef __ASTREE_H__
#define __ASTREE_H__

#include <stdio.h>
#include <string.h>

#include "auxlib.h"
#include "lyutils.h"
#include "klib/kvec.h"

typedef struct {
    size_t filenr;
    size_t linenr;
    size_t offset;
} Location;

typedef struct ASTree;

typedef kvec_t(ASTree *) ASTVector;

typedef struct {
    int symbol;                  // token code
    Location loc;         // source location
    Location decl_loc;    // for identies declaration location
    const char **lexinfo;        // pointer to lexical information
    ASTVector *children;  // children of this n-way node
    ASTree *next_sibling; // for adopting long lists of siblings
    ASTree *firstborn;    // head of the list of siblings
    size_t blocknr;              // block number this node occurs in
    int attributes[16];          // type attributes
    const char **type_id;        // structure type
} ASTree;

void location_print (FILE *out, const Location location_);

ASTree *astree_init (int symbol_,
                            const Location loc_,
                            const char *lexinfo);
void astree_free (ASTree *astree_);
ASTree *astree_adopt (ASTree *parent,
                             ASTree *child1,
                             ASTree *child2,
                             ASTree *child3);
ASTree *astree_adopt_sym (ASTree *parent,
                                 int symbol,
                                 ASTree *child1,
                                 ASTree *child2);
ASTree *astree_buddy_up (ASTree *astree_, ASTree *sibling);
void astree_dump (ASTree *astree_, FILE *out);
void astree_dump_tree (ASTree *astree_, FILE *out, int depth);
void astree_print_tree (ASTree *astree_, FILE *out, int depth);
char *astree_to_string (ASTree *astree_);
char *location_to_string (Location location_);

void astree_destroy (size_t count, ...);

#endif
