#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#include "string_set.h"
#include "lyutils.h"
#include "astree.h"
#include "vector.h"
#include "map.h"
//#include "symtable.h"

struct astree *astree_init (int symbol_, const struct location loc_,
                            const char *info) {
    struct astree *ret = (struct astree *) calloc (sizeof (struct astree *), 1);

    ret->symbol = symbol_;
    ret->loc = loc_;
    ret->lexinfo = string_set_intern (info);
    ret->firstborn = ret;
    ret->next_sibling = NULL;
    // remember, attributes for nodes which adopt a different symbol
    // must have the appropriate attributes set in adopt_symbol
    switch (symbol_) {
        case '+':
        case '-':
        case '/':
        case '*':
        case '%':
        case TOK_EQ:
        case TOK_NE:
        case TOK_LT:
        case TOK_LE:
        case TOK_GT:
        case TOK_GE:
        case TOK_NOT:
        case TOK_POS:
        case TOK_NEG:
            //attributes.set((size_t)attr::INT);
        case '=':
        case TOK_ALLOC:
        case TOK_CALL:
            //attributes.set((size_t)attr::VREG);
            break;
        case TOK_ARROW:
        case TOK_INDEX:
            //attributes.set((size_t)attr::LVAL);
            //attributes.set((size_t)attr::VADDR);
            break;
        case TOK_NULLPTR:
            //attributes.set((size_t)attr::NULLPTR_T);
            //attributes.set((size_t)attr::CONST);
            break;
        case TOK_INTCON:
        case TOK_CHARCON:
            //attributes.set((size_t)attr::CONST);
            //attributes.set((size_t)attr::INT);
            break;
        case TOK_STRINGCON:
            //attributes.set((size_t)attr::CONST);
            //attributes.set((size_t)attr::STRING);
            break;
    }
    return ret;
}

void astree_free (struct astree *astree_) {
    while (!vector_empty (astree_->children)) {
        struct astree *child = vector_pop (astree_->children);

        astree_free (child);
    }
    if (yydebug) {
        // print tree contents to stderr
    }
    free (astree_);
}

struct astree *astree_adopt (struct astree *parent, struct astree *child1,
                             struct astree *child2, struct astree *child3) {
    if (child1 != NULL) {
        struct astree *current_sibling = child1->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child2 != NULL) {
        struct astree *current_sibling = child2->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child3 != NULL) {
        struct astree *current_sibling = child3->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    return parent;
}

struct astree *astree_adopt_sym (struct astree *parent, int symbol_,
                                 struct astree *child1, struct astree *child2) {
    parent->symbol = symbol_;
    if (symbol_ == TOK_LT || symbol_ == TOK_GT) {
        //attributes.set((size_t)attr::INT);
        //attributes.set((size_t)attr::VREG);
    } else if (symbol_ == TOK_INDEX) {
        //attributes.set((size_t)attr::VADDR);
        //attributes.set((size_t)attr::LVAL);
    } else if (symbol_ == TOK_CALL) {
        //attributes.set((size_t)attr::VREG);
    }
    return astree_adopt (parent, child1, child2, NULL);
}

struct astree *astree_buddy_up (struct astree *sibling1,
                                struct astree *sibling2) {
    assert (sibling1 != NULL);
    // if sib is null don't bother doing anything
    if (sibling2 == NULL)
        return sibling1;
    // if it is the head of the list, this node points to itself
    sibling2->firstborn = sibling1->firstborn;
    // want to append to the end of the "list"
    sibling1->next_sibling = sibling2;
    /*DEBUGH('y', "  buddying up " << parser::get_tname(symbol)
       << " with " << parser::get_tname(sibling->symbol)
       << "; oldest sib: " << parser::get_tname(firstborn->symbol)
       << " " << *(firstborn->lexinfo)); */
    return sibling2;
}

/*astree* astree::first() {
    return children[0];
}

astree* astree::second() {
    return children[1];
}

astree* astree::third() {
    return children[2];
}*/

void astree_dump_tree (struct astree *tree, FILE * out, int depth) {
    // Dump formatted tree: current pointer, current token, followed
    // by pointers to children, on one line. Then call recursively on
    // children with increased depth (identation)
}

void astree_dump (struct astree *tree) {
    // print pointer value and tree contents without any special formatting,
    // followed by the pointer values of this node's children
}

char *astree_to_string (struct astree *tree) {
    // print token name, lexinfo in quotes, the location, block number,
    // attributes, and the typeid if this is a struct
    const char *tname = parser_get_tname (tree->symbol);
    char *locstring = location_to_string (tree->loc);

    if (strlen (tname) > 4)
        tname += 4;
    int bufsize = snprintf (NULL, 0, "%s \"%s\" %s {%d} attrs", tname,
                            *(tree->lexinfo), locstring, tree->block_no);
    char *buffer = (char *) malloc (sizeof (char) * (bufsize + 1));

    snprintf (buffer, bufsize, "%s \"%s\" %s {%d} attrs", tname,
              *(tree->lexinfo), locstring, tree->block_no);
    free (locstring);
    return buffer;
}

void astree_print_tree (struct astree *tree, FILE * out, int depth) {
    // print out the whole tree
    size_t numspaces = depth * 3;
    char *indent = (char *) malloc ((sizeof (char) * numspaces) + 1);

    memset (indent, ' ', numspaces);
    *(indent + numspaces) = 0;
    char *nodestr = astree_to_string (tree);

    fprintf (out, "%s%s\n", indent, nodestr);
    free (nodestr);
    free (indent);
    for (size_t i = 0; i < tree->children->size; ++i) {
        astree_print_tree ((struct astree *) vector_get (tree->children, i),
                           out, depth + 1);
    }
}

char *location_to_string (struct location location_) {
    int bufsize = snprintf (NULL, 0, "{%d, %d, %d}", location_.fileno,
                            location_.lineno, location_.offset);
    char *buffer = (char *) malloc (sizeof (char) * (bufsize + 1));

    *(buffer + bufsize) = 0;
    snprintf (buffer, bufsize, "{%d, %d, %d}", location_.fileno,
              location_.lineno, location_.offset);
    return buffer;
}

void destroy (struct astree *tree1, struct astree *tree2, struct astree *tree3) {
    //DEBUGH('y', "  DESTROYING");
    if (tree1 != NULL)
        astree_free (tree1);
    if (tree2 != NULL)
        astree_free (tree2);
    if (tree3 != NULL)
        astree_free (tree3);
}
