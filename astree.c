#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"
#include "lyutils.h"
#include "map.h"
#include "string_set.h"
#include "vector.h"
//#include "symtable.h"

struct astree *astree_init (int symbol_,
                            const struct location loc_,
                            const char *info) {
    DEBUGS ('t',
            "Initializing new astree node with code: %s",
            parser_get_tname (symbol_));
    struct astree *ret = (struct astree *) malloc (sizeof (struct astree));

    ret->symbol = symbol_;
    ret->loc = loc_;
    ret->lexinfo = string_set_intern (info);
    ret->children = vector_init (10);
    ret->next_sibling = NULL;
    ret->firstborn = ret;
    ret->blocknr = 0;

    DEBUGS ('t', "Assigning node attributes.");
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
            // attributes.set((size_t)attr::INT);
        case '=':
        case TOK_ALLOC:
        case TOK_CALL:
            // attributes.set((size_t)attr::VREG);
            break;
        case TOK_ARROW:
        case TOK_INDEX:
            // attributes.set((size_t)attr::LVAL);
            // attributes.set((size_t)attr::VADDR);
            break;
        case TOK_NULLPTR:
            // attributes.set((size_t)attr::NULLPTR_T);
            // attributes.set((size_t)attr::CONST);
            break;
        case TOK_INTCON:
        case TOK_CHARCON:
            // attributes.set((size_t)attr::CONST);
            // attributes.set((size_t)attr::INT);
            break;
        case TOK_STRINGCON:
            // attributes.set((size_t)attr::CONST);
            // attributes.set((size_t)attr::STRING);
            break;
    }
    return ret;
}

void astree_free (struct astree *astree_) {
    assert (astree_ != NULL);
    DEBUGS ('t',
            "Freeing an astree with sym %d, %s.",
            astree_->symbol,
            parser_get_tname (astree_->symbol));
    while (!vector_empty (astree_->children)) {
        struct astree *child = vector_pop (astree_->children);

        astree_free (child);
    }
    if (yydebug) {
        // print tree contents to stderr
    }
    if (astree_->children != NULL) vector_free (astree_->children);
    free (astree_);
}

struct astree *astree_adopt (struct astree *parent,
                             struct astree *child1,
                             struct astree *child2,
                             struct astree *child3) {
    if (child1 != NULL) {
        DEBUGS ('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
                                          parser_get_tname(child1->symbol));
        struct astree *current_sibling = child1->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child2 != NULL) {
        DEBUGS ('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
                                          parser_get_tname(child2->symbol));
        struct astree *current_sibling = child2->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child3 != NULL) {
        DEBUGS ('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
                                          parser_get_tname(child3->symbol));
        struct astree *current_sibling = child3->firstborn;

        do {
            vector_push (parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    return parent;
}

struct astree *astree_adopt_sym (struct astree *parent,
                                 int symbol_,
                                 struct astree *child1,
                                 struct astree *child2) {
    parent->symbol = symbol_;
    if (symbol_ == TOK_LT || symbol_ == TOK_GT) {
        // attributes.set((size_t)attr::INT);
        // attributes.set((size_t)attr::VREG);
    } else if (symbol_ == TOK_INDEX) {
        // attributes.set((size_t)attr::VADDR);
        // attributes.set((size_t)attr::LVAL);
    } else if (symbol_ == TOK_CALL) {
        // attributes.set((size_t)attr::VREG);
    }
    return astree_adopt (parent, child1, child2, NULL);
}

struct astree *astree_buddy_up (struct astree *sibling1,
                                struct astree *sibling2) {
    assert (sibling1 != NULL);
    // if sib is null don't bother doing anything
    if (sibling2 == NULL) return sibling1;
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

void astree_dump_tree (struct astree *tree, FILE *out, int depth) {
    // Dump formatted tree: current pointer, current token, followed
    // by pointers to children, on one line. Then call recursively on
    // children with increased depth (identation)
}

void astree_dump (struct astree *tree, FILE *out) {
    // print pointer value and tree contents without any special formatting,
    // followed by the pointer values of this node's children
    if (tree == NULL) return;
    DEBUGS ('t', "Dumping astree node.");
    char *nodestr = astree_to_string (tree);

    fprintf (out, "%p->%s", tree, nodestr);
    free (nodestr);
}

char *astree_to_string (struct astree *tree) {
    // print token name, lexinfo in quotes, the location, block number,
    // attributes, and the typeid if this is a struct
    if (tree == NULL) {
        return NULL;
    }

    const char *tname = parser_get_tname (tree->symbol);
    char *locstring = location_to_string (tree->loc);

    if (strlen (tname) > 4) tname += 4;
    int bufsize = snprintf (NULL,
                            0,
                            "%s \"%s\" %s {%d} attrs",
                            tname,
                            *(tree->lexinfo),
                            locstring,
                            tree->blocknr);
    char *buffer = (char *) malloc (sizeof (char) * (bufsize + 1));

    snprintf (buffer,
              bufsize,
              "%s \"%s\" %s {%d} attrs",
              tname,
              *(tree->lexinfo),
              locstring,
              tree->blocknr);
    free (locstring);
    return buffer;
}

void astree_print_tree (struct astree *tree, FILE *out, int depth) {
    // print out the whole tree
    DEBUGS ('t',
            "Tree info: token: %s, lexinfo: %s",
            parser_get_tname (tree->symbol),
            *(tree->lexinfo));
    size_t numspaces = depth * 3;
    char *indent = (char *) malloc ((sizeof (char) * numspaces) + 1);

    memset (indent, ' ', numspaces);
    *(indent + numspaces) = 0;
    char *nodestr = astree_to_string (tree);

    fprintf (out, "%s%s\n", indent, nodestr);
    free (nodestr);
    free (indent);
    DEBUGS ('t',
            "       %s has %u children",
            parser_get_tname (tree->symbol),
            tree->children->stack_size);
    DEBUGS ('t', "Printing addresses of children.");
    for (size_t i = 0; i < tree->children->stack_size; ++i) {
        DEBUGS ('t', "   %p", vector_get (tree->children, i));
    }

    for (size_t i = 0; i < tree->children->stack_size; ++i) {
        struct astree *child = (struct astree *) vector_get (tree->children, i);

        if (child != NULL) {
            DEBUGS ('t',
                    "       %s has a child; printing.",
                    parser_get_tname (tree->symbol));
            astree_print_tree (child, out, depth + 1);
        }
    }
}

char *location_to_string (struct location location_) {
    int bufsize = snprintf (NULL,
                            0,
                            "{%d, %d, %d}",
                            location_.filenr,
                            location_.linenr,
                            location_.offset) +
                  1;
    char *buffer = (char *) malloc (sizeof (char) * (bufsize));

    snprintf (buffer,
              bufsize,
              "{%d, %d, %d}",
              location_.filenr,
              location_.linenr,
              location_.offset);
    return buffer;
}

void astree_destroy (size_t count, ...) {
    va_list args;

    va_start (args, count);
    for (size_t i = 0; i < count; ++i) {
        struct astree *tree = va_arg (args, struct astree *);

        if (tree != NULL) {
            DEBUGS ('t',
                    "  DESTROYING: %d, %s",
                    tree->symbol,
                    parser_get_tname (tree->symbol));
            astree_free (tree);
        }
    }
    va_end (args);
}
