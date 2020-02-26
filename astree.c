#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"
#include "lyutils.h"
#include "strset.h"
#define LINESIZE 1024

ASTree *astree_first (ASTree *parent) { return kv_A (parent->children, 0); }

ASTree *astree_second (ASTree *parent) { return kv_A (parent->children, 1); }

ASTree *astree_third (ASTree *parent) { return kv_A (parent->children, 2); }

ASTree *astree_init (int symbol_, const Location location, const char *info) {
    DEBUGS ('t',
            "Initializing new astree node with code: %s",
            parser_get_tname (symbol_));
    ASTree *ret = malloc (sizeof (*ret));

    ret->symbol = symbol_;
    ret->loc = location;
    ret->lexinfo = string_set_intern (info);
    kv_init (ret->children);
    ret->next_sibling = NULL;
    ret->firstborn = ret;
    ret->blocknr = 0;
    memset(ret->attributes, 0, NUM_ATTRIBUTES);
    ret->first = &astree_first;
    ret->second = &astree_second;
    ret->third = &astree_third;

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
            ret->attributes[ATTR_INT] = 1;
        case '=':
        case TOK_ALLOC:
        case TOK_CALL:
            ret->attributes[ATTR_VREG] = 1;
            break;
        case TOK_ARROW:
        case TOK_INDEX:
            ret->attributes[ATTR_LVAL] = 1;
            ret->attributes[ATTR_VADDR] = 1;
            break;
        case TOK_NULLPTR:
            ret->attributes[ATTR_NULL] = 1;
            ret->attributes[ATTR_CONST] = 1;
            break;
        case TOK_INTCON:
        case TOK_CHARCON:
            ret->attributes[ATTR_CONST] = 1;
            ret->attributes[ATTR_INT] = 1;
            break;
        case TOK_STRINGCON:
            ret->attributes[ATTR_CONST] = 1;
            ret->attributes[ATTR_STRING] = 1;
            break;
    }
    return ret;
}

void astree_destroy (ASTree *astree) {
    if (astree == NULL) return;
    DEBUGS ('t',
            "Freeing an astree with sym %d, %s.",
            astree->symbol,
            parser_get_tname (astree->symbol));
    while (kv_size (astree->children) != 0) {
        ASTree *child = kv_pop (astree->children);

        astree_destroy (child);
    }
    if (yydebug) {
        // print tree contents to stderr
    }
    kv_destroy (astree->children);
    free (astree);
}

void astree_annihilate (size_t count, ...) {
    va_list args;

    va_start (args, count);
    for (size_t i = 0; i < count; ++i) {
        ASTree *tree = va_arg (args, ASTree *);

        if (tree != NULL) {
            DEBUGS ('t',
                    "  ANNIHILATING: %d, %s",
                    tree->symbol,
                    parser_get_tname (tree->symbol));
            astree_destroy (tree);
        }
    }
    va_end (args);
}

ASTree *astree_adopt (ASTree *parent,
                      ASTree *child1,
                      ASTree *child2,
                      ASTree *child3) {
    if (child1 != NULL) {
        DEBUGS ('t',
                "Tree %s adopts %s",
                parser_get_tname (parent->symbol),
                parser_get_tname (child1->symbol));
        ASTree *current_sibling = child1->firstborn;

        do {
            kv_push (ASTree *, parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child2 != NULL) {
        DEBUGS ('t',
                "Tree %s adopts %s",
                parser_get_tname (parent->symbol),
                parser_get_tname (child2->symbol));
        ASTree *current_sibling = child2->firstborn;

        do {
            kv_push (ASTree *, parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    if (child3 != NULL) {
        DEBUGS ('t',
                "Tree %s adopts %s",
                parser_get_tname (parent->symbol),
                parser_get_tname (child3->symbol));
        ASTree *current_sibling = child3->firstborn;

        do {
            kv_push (ASTree *, parent->children, current_sibling);
            current_sibling = current_sibling->next_sibling;
        } while (current_sibling != NULL);
    }
    return parent;
}

ASTree *astree_adopt_sym (ASTree *parent,
                          int symbol_,
                          ASTree *child1,
                          ASTree *child2) {
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

ASTree *astree_buddy_up (ASTree *sibling1, ASTree *sibling2) {
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

void astree_dump_tree (ASTree *tree, FILE *out, int depth) {
    // Dump formatted tree: current pointer, current token, followed
    // by pointers to children, on one line. Then call recursively on
    // children with increased depth (identation)
}

void astree_dump (ASTree *tree, FILE *out) {
    // print pointer value and tree contents without any special formatting,
    // followed by the pointer values of this node's children
    if (tree == NULL) return;
    DEBUGS ('t', "Dumping astree node.");
    char nodestr[LINESIZE];
    astree_to_string (tree, nodestr, LINESIZE);

    fprintf (out, "%p->%s", tree, nodestr);
}

void location_to_string (Location location, char *buffer, size_t size) {
    int bufsize = snprintf (buffer,
                            size,
                            "{%d, %d, %d}",
                            location.filenr,
                            location.linenr,
                            location.offset) +
                  1;
}

void attributes_to_string (int *attributes, char *buffer, size_t size) {
    size_t offset = 0;
    buffer[0] = 0;
    for (size_t i = 0, offset = 0; i < NUM_ATTRIBUTES; ++i) {
        if(attributes[i]) {
            DEBUGS('a', "Printing attribute %s", attr_map[i]);
            snprintf (buffer + offset,
                                              size - offset,
                                              "%s ", attr_map[i]);
            offset = strlen (buffer);
        }
    }
}

void astree_to_string (ASTree *tree, char *buffer, size_t size) {
    // print token name, lexinfo in quotes, the location, block number,
    // attributes, and the typeid if this is a struct
    if (tree == NULL) return;

    const char *tname = parser_get_tname (tree->symbol);
    char locstr[LINESIZE];
    location_to_string (tree->loc, locstr, LINESIZE);
    char attrstr[LINESIZE];
    attributes_to_string (tree->attributes, attrstr, LINESIZE);

    if (strlen (tname) > 4) tname += 4;
    snprintf (buffer,
              size,
              "%s \"%s\" %s {%d} %s",
              tname,
              tree->lexinfo,
              locstr,
              tree->blocknr,
              attrstr);
}

void astree_print_tree (ASTree *tree, FILE *out, int depth) {
    // print out the whole tree
    DEBUGS ('t',
            "Tree info: token: %s, lexinfo: %s, children: %u",
            parser_get_tname (tree->symbol),
            tree->lexinfo,
            kv_size (tree->children));

    size_t numspaces = depth * 3;
    char indent[LINESIZE];
    memset (indent, ' ', numspaces);
    indent[numspaces] = 0;
    char nodestr[LINESIZE];
    astree_to_string (tree, nodestr, LINESIZE);
    fprintf (out, "%s%s\n", indent, nodestr);

    for (size_t i = 0; i < kv_size (tree->children); ++i) {
        DEBUGS ('t', "    %p", kv_A (tree->children, i));
    }

    for (size_t i = 0; i < kv_size (tree->children); ++i) {
        ASTree *child = (ASTree *) kv_A (tree->children, i);

        if (child != NULL) {
            astree_print_tree (child, out, depth + 1);
        }
    }
}
