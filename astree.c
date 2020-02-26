#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"
#include "lyutils.h"
#include "string_set.h"
//#include "symtable.h"

ASTree *astree_first (ASTree *parent) { return kv_A (parent->children, 0); }

ASTree *astree_second (ASTree *parent) { return kv_A (parent->children, 1); }

ASTree *astree_third (ASTree *parent) { return kv_A (parent->children, 2); }

ASTree *astree_init (int symbol_, const Location loc_, const char *info) {
    DEBUGS ('t',
            "Initializing new astree node with code: %s",
            parser_get_tname (symbol_));
    ASTree *ret = (ASTree *) malloc (sizeof (ASTree));

    ret->symbol = symbol_;
    ret->loc = loc_;
    ret->lexinfo = string_set_intern (info);
    kv_init (ret->children);
    ret->next_sibling = NULL;
    ret->firstborn = ret;
    ret->blocknr = 0;
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

void astree_free (ASTree *astree_) {
    assert (astree_ != NULL);
    DEBUGS ('t',
            "Freeing an astree with sym %d, %s.",
            astree_->symbol,
            parser_get_tname (astree_->symbol));
    while (kv_size (astree_->children) != 0) {
        ASTree *child = kv_pop (astree_->children);

        astree_free (child);
    }
    if (yydebug) {
        // print tree contents to stderr
    }
    kv_destroy (astree_->children);
    free (astree_);
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
    char *nodestr = astree_to_string (tree);

    fprintf (out, "%p->%s", tree, nodestr);
    free (nodestr);
}

char *astree_to_string (ASTree *tree) {
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
                            tree->lexinfo,
                            locstring,
                            tree->blocknr);
    char *buffer = (char *) malloc (sizeof (char) * (bufsize + 1));

    snprintf (buffer,
              bufsize,
              "%s \"%s\" %s {%d} attrs",
              tname,
              tree->lexinfo,
              locstring,
              tree->blocknr);
    free (locstring);
    return buffer;
}

void astree_print_tree (ASTree *tree, FILE *out, int depth) {
    // print out the whole tree
    DEBUGS ('t',
            "Tree info: token: %s, lexinfo: %s, children: %u",
            parser_get_tname (tree->symbol),
            tree->lexinfo,
            kv_size (tree->children));

    size_t numspaces = depth * 3;
    char indent[1024];
    memset (indent, ' ', numspaces);
    indent[numspaces] = 0;
    char *nodestr = astree_to_string (tree);
    fprintf (out, "%s%s\n", indent, nodestr);
    free (nodestr);

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

char *location_to_string (Location location_) {
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
        ASTree *tree = va_arg (args, ASTree *);

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
