#ifndef __ASTREE_H__
#    define __ASTREE_H__

#    include <stdio.h>
#    include <string.h>

#    include "auxlib.h"
#    include "lyutils.h"
#    include "map.h"
#    include "vector.h"

struct location {
    size_t fileno;
    size_t lineno;
    size_t offset;
};

struct astree {
    int symbol;                 // token code
    struct location loc;        // source location
    struct location decl_loc;   // for identies declaration location
    const char **lexinfo;       // pointer to lexical information
    struct vector *children;    // children of this n-way node
    struct astree *next_sibling;    // for adopting long lists of siblings
    struct astree *firstborn;   // head of the list of siblings
    size_t block_nr;            // block number this node occurs in
    int *attributes;            // type attributes
    const char **type_id;       // structure type
};

void location_print (FILE * out, const struct location location_);

struct astree *astree_init (int symbol, const struct location,
                            const char *lexinfo);
void astree_free (struct astree *astree_);
struct astree *astree_adopt (struct astree *parent, struct astree *child1,
                             struct astree *child2, struct astree *child3);
struct astree *astee_adopt_sym (struct astree *parent, int symbol,
                                struct astree *child1, struct astree *child2);
struct astree *astree_buddy_up (struct astree *astree_, struct astree *sibling);
void astree_dump_node (struct astree *astree_, FILE * out);
void astree_dump_tree (struct astree *astree_, FILE * out, int depth);
void astree_print (const struct astree *astree_, FILE * out);

// it might be a good idea to combine this and astree_free into a single
// function somehow
void destroy (struct astree *tree1, struct astree *tree2, struct astree *tree3);

#endif
