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
#include "yyparse.h"
#define LINESIZE 1024

ASTree *astree_first(ASTree *parent) { return llist_get(&parent->children, 0); }

ASTree *astree_second(ASTree *parent) {
  return llist_get(&parent->children, 1);
}

ASTree *astree_third(ASTree *parent) { return llist_get(&parent->children, 2); }

ASTree *astree_get(ASTree *parent, const size_t index) {
  return llist_get(&parent->children, index);
}

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
  tree->next_sibling = NULL;
  tree->firstborn = tree;
  tree->symbol_table = (Map)BLIB_MAP_EMPTY;

  /* remember, attributes for nodes which adopt a different symbol
   * must have the appropriate attributes set in adopt_symbol
   */
  switch (symbol) {
    case '+':
    case '-':
    case '/':
    case '*':
    case '%':
    case '<':
    case '>':
    case TOK_EQ:
    case TOK_NE:
    case TOK_LE:
    case TOK_GE:
    case '!':
    case TOK_POS:
    case TOK_NEG:
      /* tree->attributes[ATTR_INT] = 1; */
    case '=':
    case TOK_CALL:
      /* tree->attributes[ATTR_VREG] = 1; */
      break;
    case TOK_ARROW:
    case TOK_SUBSCRIPT:
      /* tree->attributes[ATTR_LVAL] = 1; */
      /* tree->attributes[ATTR_VADDR] = 1; */
      break;
    case TOK_INTCON:
    case TOK_CHARCON:
      /* tree->attributes[ATTR_CONST] = 1; */
      /* tree->attributes[ATTR_INT] = 1; */
      break;
    case TOK_STRINGCON:
      /* tree->attributes[ATTR_CONST] = 1; */
      /* tree->attributes[ATTR_STRING] = 1; */
      break;
  }

  return tree;
}

int astree_destroy(ASTree *tree) {
  if (tree == NULL) return 1;
  DEBUGS('t', "Freeing an astree with sym %d, %s.", tree->symbol,
         parser_get_tname(tree->symbol));
  if (yydebug) {
    /* print tree contents to stderr */
  }

  /* llist should handle destruction of children */
  llist_destroy(&tree->children);

  /* free symbol entries associated with scope */
  map_destroy(&tree->symbol_table);

  /* free one-off TypeSpec objects */
  switch (tree->symbol) {
    case TOK_ADDROF:
      /* free pointer auxspec that was added */
      auxspec_destroy(llist_pop_front((LinkedList *)&tree->type->auxspecs));
    case TOK_CAST:
    case TOK_SUBSCRIPT:
    case TOK_INDIRECTION:
      typespec_destroy((TypeSpec *)tree->type);
      break;
  }

  free(tree);
  return 0;
}

ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3) {
  if (child1 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child1->symbol));
    ASTree *current_sibling = child1->firstborn;

    do {
      llist_push_back(&parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child2 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child2->symbol));
    ASTree *current_sibling = child2->firstborn;

    do {
      llist_push_back(&parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child3 != NULL) {
    DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child3->symbol));
    ASTree *current_sibling = child3->firstborn;

    do {
      llist_push_back(&parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  return parent;
}

ASTree *astree_adopt_sym(ASTree *parent, int symbol_, ASTree *child1,
                         ASTree *child2) {
  parent->symbol = symbol_;
  if (symbol_ == '<' || symbol_ == '>') {
    /* attributes.set((size_t)attr::INT); */
    /* attributes.set((size_t)attr::VREG); */
  } else if (symbol_ == TOK_SUBSCRIPT) {
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
  ASTree *current = sibling1;
  while (current->next_sibling) current = current->next_sibling;
  current->next_sibling = sibling2;
  /* return the head of the list */
  return sibling1;
}

ASTree *astree_descend(ASTree *parent, ASTree *descendant) {
  ASTree *current = parent;
  while (astree_first(current)) {
    current = astree_first(current);
  }
  astree_adopt(current, descendant, NULL, NULL);
  return parent;
}

size_t astree_count(ASTree *parent) { return llist_size(&parent->children); }

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

void astree_to_string(ASTree *tree, char *buffer, size_t size) {
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return;

  const char *tname = parser_get_tname(tree->symbol);
  char locstr[LINESIZE];
  location_to_string(&tree->loc, locstr, LINESIZE);
  char attrstr[LINESIZE];
  attributes_to_string(tree->attributes, attrstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_string(tree->type, typestr, LINESIZE);

  if (strlen(tname) > 4) tname += 4;
  snprintf(buffer, size, "%s \"%s\" {%s} {%s} {%s}", tname, tree->lexinfo,
           locstr, typestr, attrstr);
}

void astree_print_tree(ASTree *tree, FILE *out, int depth) {
  /* print out the whole tree */
  DEBUGS('t', "Tree info: token: %s, lexinfo: %s, children: %u",
         parser_get_tname(tree->symbol), tree->lexinfo,
         llist_size(&tree->children));

  size_t numspaces = depth * 3;
  char indent[LINESIZE];
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  char nodestr[LINESIZE];
  astree_to_string(tree, nodestr, LINESIZE);
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < llist_size(&tree->children); ++i) {
    DEBUGS('t', "    %p", llist_get(&tree->children, i));
  }

  for (i = 0; i < llist_size(&tree->children); ++i) {
    ASTree *child = (ASTree *)llist_get(&tree->children, i);

    if (child != NULL) {
      astree_print_tree(child, out, depth + 1);
    }
  }
}
