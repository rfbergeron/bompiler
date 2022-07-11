#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "attributes.h"
#include "badllist.h"
#include "debug.h"
#include "lyutils.h"
#include "strset.h"
#include "symtable.h"
#include "yyparse.h"
#define LINESIZE 1024

extern int skip_type_check;

ASTree EMPTY_EXPR = {&SPEC_EMPTY,      ";", NULL,     LOC_EMPTY,
                     BLIB_LLIST_EMPTY, ';', ATTR_NONE};

ASTree *astree_init(int symbol, const Location location, const char *info) {
  DEBUGS('t', "Initializing new astree node with code: %s, lexinfo: '%s'",
         parser_get_tname(symbol), info);

  ASTree *tree = malloc(sizeof(ASTree));
  tree->symbol = symbol;
  tree->lexinfo = string_set_intern(info);
  tree->loc = location;
  tree->type = &SPEC_EMPTY;
  tree->attributes = ATTR_NONE;
  /* casting function pointers is undefined behavior but it seems like most
   * compiler implementations make it work, and here we're just changing the
   * type of a pointer argument */
  llist_init(&tree->children, (void (*)(void *))(astree_destroy), NULL);
  tree->symbol_table = NULL;
  return tree;
}

int astree_destroy(ASTree *tree) {
  if (tree == NULL) {
    fprintf(stderr, "WARNING: null pointer supplied to astree_destroy.\n");
    return 1;
  } else if (tree == &EMPTY_EXPR) {
    return 0;
  }

  DEBUGS('t', "Freeing an astree with symbol: %s, lexinfo: %s",
         parser_get_tname(tree->symbol), tree->lexinfo);
  if (yydebug) {
    /* print tree contents to stderr */
  }

  /* llist should handle destruction of children */
  int status = llist_destroy(&tree->children);
  if (status) {
    fprintf(stderr, "your datas structures library sucks\n");
    abort();
  }

  /* free symbol table if present */
  if (tree->symbol_table != NULL) {
    int status = symbol_table_destroy(tree->symbol_table);
    if (status) {
      fprintf(stderr, "unable to destroy symbol table\n");
      abort();
    }
  }

  /* free one-off TypeSpec objects */
  if (!skip_type_check && tree->type != &SPEC_EMPTY) {
    switch (tree->symbol) {
      case TOK_ADDROF:
        /* free pointer auxspec that was added */
        auxspec_destroy(llist_pop_front((LinkedList *)&tree->type->auxspecs));
      case TOK_SUBSCRIPT:
      case TOK_INDIRECTION:
      case TOK_CALL:
      case TOK_TYPE_ERROR:
        typespec_destroy((TypeSpec *)tree->type);
        free((TypeSpec *)tree->type);
        break;
    }
  }

  free(tree);
  return 0;
}

ASTree *astree_adopt(ASTree *parent, const size_t count, ...) {
  /*
   * only one of the following may be true:
   * - the parent's symbol is TOK_TYPE_ERROR
   * - the number of children post adoption is greater than 1
   */
  assert((parent->symbol != TOK_TYPE_ERROR) ||
         (astree_count(parent) + count == 1));
  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = va_arg(args, ASTree *);
    assert((child == &EMPTY_EXPR) ||
           (llist_find(&parent->children, child) == -1));
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child->symbol));
    int status = llist_push_back(&parent->children, child);
    if (status) {
      fprintf(stderr, "ERROR: your data structures library sucks.\n");
      abort();
    }
  }
  va_end(args);
  return parent;
}

ASTree *astree_replace(ASTree *parent, const size_t index, ASTree *child) {
  if (index >= astree_count(parent)) return NULL;
  ASTree *old_child = llist_extract(&parent->children, index);
  /* TODO(Robert): check status and indicate errors */
  int status = llist_insert(&parent->children, child, index);
  return old_child;
}

ASTree *astree_get(ASTree *parent, const size_t index) {
  return llist_get(&parent->children, index);
}

ASTree *astree_remove(ASTree *parent, const size_t index) {
  return llist_extract(&parent->children, index);
}

size_t astree_count(ASTree *parent) { return llist_size(&parent->children); }

int astree_to_string(ASTree *tree, char *buffer, size_t size) {
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return -1;

  const char *tname = parser_get_tname(tree->symbol);
  char locstr[LINESIZE];
  int characters_printed = location_to_string(&tree->loc, locstr, LINESIZE);
  if (characters_printed < 0) return characters_printed;
  char attrstr[LINESIZE];
  characters_printed =
      attributes_to_string(tree->attributes, attrstr, LINESIZE);
  if (characters_printed < 0) return characters_printed;
  char typestr[LINESIZE];
  characters_printed = type_to_string(tree->type, typestr, LINESIZE);
  if (characters_printed < 0) return characters_printed;

  if (strlen(tname) > 4) tname += 4;
  return snprintf(buffer, size, "%s \"%s\" {%s} {%s} {%s}", tname,
                  tree->lexinfo, locstr, typestr, attrstr);
}

int astree_print_tree(ASTree *tree, FILE *out, int depth) {
  /* print out the whole tree */
  DEBUGS('t', "Tree info: token: %s, lexinfo: %s, children: %u",
         parser_get_tname(tree->symbol), tree->lexinfo,
         llist_size(&tree->children));

  size_t numspaces = depth * 2;
  char indent[LINESIZE];
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  char nodestr[LINESIZE];
  int characters_printed = astree_to_string(tree, nodestr, LINESIZE);
  if (characters_printed < 0) return characters_printed;
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < llist_size(&tree->children); ++i) {
    DEBUGS('t', "    %p", llist_get(&tree->children, i));
  }

  for (i = 0; i < llist_size(&tree->children); ++i) {
    ASTree *child = (ASTree *)llist_get(&tree->children, i);

    if (child != NULL) {
      int status = astree_print_tree(child, out, depth + 1);
      if (status) return status;
    }
  }
  return 0;
}

int astree_print_symbols(ASTree *tree, FILE *out) {
  if (tree->symbol_table != NULL) {
    LinkedList symnames = (LinkedList)BLIB_LLIST_EMPTY;
    int status = llist_init(&symnames, NULL, NULL);
    if (status) return status;
    status = map_keys(&tree->symbol_table->primary_namespace, &symnames);
    if (status) return status;
    DEBUGS('a', "Printing %zu symbols",
           map_size(&tree->symbol_table->primary_namespace));
    const char *tname = parser_get_tname(tree->symbol);
    char locstr[LINESIZE];
    location_to_string(&tree->loc, locstr, LINESIZE);
    if (strlen(tname) > 4) tname += 4;

    fprintf(out, "%s \"%s\" {%s}\n", parser_get_tname(tree->symbol),
            tree->lexinfo, locstr);
    size_t i;
    for (i = 0; i < llist_size(&symnames); ++i) {
      const char *symname = llist_get(&symnames, i);
      SymbolValue *symval = map_get(&tree->symbol_table->primary_namespace,
                                    (char *)symname, strlen(symname));
      char symval_str[LINESIZE];
      int characters_printed = symbol_value_print(symval, symval_str, LINESIZE);
      if (characters_printed < 0) {
        fprintf(stderr,
                "ERROR: failed to print symbol table associated with "
                "astree node, symbol: %s, lexinfo: %s\n",
                parser_get_tname(tree->symbol), tree->lexinfo);
        return status;
      }
      fprintf(out, "  %s: %s\n", symname, symval_str);
    }
    llist_destroy(&symnames);
  }
  size_t i;
  for (i = 0; i < astree_count(tree); ++i) {
    astree_print_symbols(astree_get(tree, i), out);
  }
  return 0;
}
