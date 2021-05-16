#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"
#include "lyutils.h"
#include "strset.h"
#define LINESIZE 1024

ASTree *astree_first(ASTree *parent) { return llist_get(parent->children, 0); }

ASTree *astree_second(ASTree *parent) { return llist_get(parent->children, 1); }

ASTree *astree_third(ASTree *parent) { return llist_get(parent->children, 2); }

ASTree *astree_init(int symbol_, const Location location, const char *info) {
  DEBUGS('t', "Initializing new astree node with code: %s, lexinfo: '%s'",
         parser_get_tname(symbol_), info);
  ASTree *ret = malloc(sizeof(*ret));

  ret->symbol = symbol_;
  ret->lexinfo = string_set_intern(info);
  ret->loc = location;
  ret->type = SPEC_EMPTY;
  ret->attributes = 0;
  /* casting function pointers is undefined behavior but it seems like most
   * compiler implementations make it work, and here we're just changing the
   * type of a pointer argument */
  ret->children = malloc(sizeof(*(ret->children)));
  llist_init(ret->children, (void (*)(void *))(astree_destroy), NULL);
  ret->next_sibling = NULL;
  ret->firstborn = ret;

  /* remember, attributes for nodes which adopt a different symbol
   * must have the appropriate attributes set in adopt_symbol
   */
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
    case '!':
    case TOK_POS:
    case TOK_NEG:
      /* ret->attributes[ATTR_INT] = 1; */
    case '=':
    case TOK_CALL:
      /* ret->attributes[ATTR_VREG] = 1; */
      break;
    case TOK_ARROW:
    case TOK_INDEX:
      /* ret->attributes[ATTR_LVAL] = 1; */
      /* ret->attributes[ATTR_VADDR] = 1; */
      break;
    case TOK_INTCON:
    case TOK_CHARCON:
      /* ret->attributes[ATTR_CONST] = 1; */
      /* ret->attributes[ATTR_INT] = 1; */
      break;
    case TOK_STRINGCON:
      /* ret->attributes[ATTR_CONST] = 1; */
      /* ret->attributes[ATTR_STRING] = 1; */
      break;
  }
  return ret;
}

void astree_destroy(ASTree *astree) {
  if (astree == NULL) return;
  DEBUGS('t', "Freeing an astree with sym %d, %s.", astree->symbol,
         parser_get_tname(astree->symbol));
  llist_destroy(astree->children);
  if (yydebug) {
    /* print tree contents to stderr */
  }
  free(astree);
}

ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3) {
  if (child1 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child1->symbol));
    ASTree *current_sibling = child1->firstborn;

    do {
      llist_push_back(parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child2 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child2->symbol));
    ASTree *current_sibling = child2->firstborn;

    do {
      llist_push_back(parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child3 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child3->symbol));
    ASTree *current_sibling = child3->firstborn;

    do {
      llist_push_back(parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  return parent;
}

ASTree *astree_adopt_sym(ASTree *parent, int symbol_, ASTree *child1,
                         ASTree *child2) {
  parent->symbol = symbol_;
  if (symbol_ == TOK_LT || symbol_ == TOK_GT) {
    /* attributes.set((size_t)attr::INT); */
    /* attributes.set((size_t)attr::VREG); */
  } else if (symbol_ == TOK_INDEX) {
    /* attributes.set((size_t)attr::VADDR); */
    /* attributes.set((size_t)attr::LVAL); */
  } else if (symbol_ == TOK_CALL) {
    /* attributes.set((size_t)attr::VREG); */
  }
  return astree_adopt(parent, child1, child2, NULL);
}

ASTree *astree_twin(ASTree *sibling1, ASTree *sibling2) {
  assert(sibling1 != NULL || sibling2 != NULL);
  /* if sib is null don't bother doing anything */
  if (sibling1 == NULL) return sibling2;
  if (sibling2 == NULL) return sibling1;
  /* if it is the head of the list, this node points to itself */
  sibling2->firstborn = sibling1->firstborn;
  /* want to append to the end of the "list" */
  sibling1->next_sibling = sibling2;
  return sibling2;
}

ASTree *astree_descend(ASTree *parent, ASTree *descendant) {
  ASTree *current = parent;
  while (astree_first(current)) {
    current = astree_first(current);
  }
  astree_adopt(current, descendant, NULL, NULL);
  return parent;
}

void astree_dump_tree(ASTree *tree, FILE *out, int depth) {
  /* Dump formatted tree: current pointer, current token, followed
   * by pointers to children, on one line. Then call recursively on
   * children with increased depth (identation)
   */
}

void astree_dump(ASTree *tree, FILE *out) {
  /* print pointer value and tree contents without any special formatting,
   * followed by the pointer values of this node's children
   */
  if (tree == NULL) return;
  DEBUGS('t', "Dumping astree node.");
  char nodestr[LINESIZE];
  astree_to_string(tree, nodestr, LINESIZE);

  fprintf(out, "%p->%s", tree, nodestr);
}

void location_to_string(Location location, char *buffer, size_t size) {
  int bufsize = snprintf(buffer, size, "%lu, %lu, %lu, %lu", location.filenr,
                         location.linenr, location.offset, location.blocknr) +
                1;
}

void astree_to_string(ASTree *tree, char *buffer, size_t size) {
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return;

  const char *tname = parser_get_tname(tree->symbol);
  char locstr[LINESIZE];
  location_to_string(tree->loc, locstr, LINESIZE);
  char attrstr[LINESIZE];
  attributes_to_string(tree->attributes, attrstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_string(&(tree->type), typestr, LINESIZE);

  if (strlen(tname) > 4) tname += 4;
  snprintf(buffer, size, "%s \"%s\" {%s} {%s} {%s}", tname, tree->lexinfo,
           locstr, typestr, attrstr);
}

void astree_print_tree(ASTree *tree, FILE *out, int depth) {
  /* print out the whole tree */
  DEBUGS('t', "Tree info: token: %s, lexinfo: %s, children: %u",
         parser_get_tname(tree->symbol), tree->lexinfo,
         llist_size(tree->children));

  size_t numspaces = depth * 3;
  char indent[LINESIZE];
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  char nodestr[LINESIZE];
  astree_to_string(tree, nodestr, LINESIZE);
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < llist_size(tree->children); ++i) {
    DEBUGS('t', "    %p", llist_get(tree->children, i));
  }

  for (i = 0; i < llist_size(tree->children); ++i) {
    ASTree *child = (ASTree *)llist_get(tree->children, i);

    if (child != NULL) {
      astree_print_tree(child, out, depth + 1);
    }
  }
}
