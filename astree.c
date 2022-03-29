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
#include "symtable.h"
#include "yyparse.h"
#define LINESIZE 1024

extern int skip_type_check;

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
  tree->symbol_table = NULL;

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

  /* free one-off TypeSpec objects */
  if (!skip_type_check) {
    switch (tree->symbol) {
      case TOK_ADDROF:
        /* free pointer auxspec that was added */
        auxspec_destroy(llist_pop_front((LinkedList *)&tree->type->auxspecs));
      case TOK_CAST:
      case TOK_SUBSCRIPT:
      case TOK_INDIRECTION:
      case TOK_CALL:
        typespec_destroy((TypeSpec *)tree->type);
        free((TypeSpec *)tree->type);
        break;
    }
  }

  free(tree);
  return 0;
}

ASTree *astree_adopt(ASTree *parent, ASTree *child1, ASTree *child2,
                     ASTree *child3) {
  if (child1 != NULL) {
    ASTree *current_sibling = child1->firstborn;

    do {
      DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
             parser_get_tname(current_sibling->symbol));
      llist_push_back(&parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child2 != NULL) {
    ASTree *current_sibling = child2->firstborn;

    do {
      DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
             parser_get_tname(current_sibling->symbol));
      llist_push_back(&parent->children, current_sibling);
      current_sibling = current_sibling->next_sibling;
    } while (current_sibling != NULL);
  }
  if (child3 != NULL) {
    ASTree *current_sibling = child3->firstborn;

    do {
      DEBUGS('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
             parser_get_tname(current_sibling->symbol));
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

ASTree *astree_inject(ASTree *old_node, ASTree *new_node) {
  ASTree temp = *old_node;
  *old_node = *new_node;
  *new_node = temp;
  new_node->firstborn = old_node->firstborn;
  new_node->next_sibling = old_node->next_sibling;
  old_node->firstborn = temp.firstborn;
  old_node->next_sibling = temp.next_sibling;
  return astree_adopt(old_node, new_node, NULL, NULL);
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

ASTree *extract_ident(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_ENUM:
    case TOK_CALL:
      return astree_first(tree);
    case TOK_IDENT:
    case TOK_INTCON:
    case TOK_CHARCON:
    case TOK_STRINGCON:
      return tree;
    case TOK_DECLARATOR:
      if (astree_first(tree)->symbol == TOK_DECLARATOR) {
        return extract_ident(astree_first(tree));
      } else {
        size_t i;
        for (i = 0; i < astree_count(tree); ++i) {
          ASTree *direct_decl = astree_get(tree, i);
          if (direct_decl->symbol == TOK_IDENT) {
            return direct_decl;
          }
        }
      }
      /* do not break; fall through and return error when no ident */
    case TOK_DECLARATION:
    default:
      fprintf(stderr, "ERROR: unable to get identifier for tree node %s\n",
              parser_get_tname(tree->symbol));
      return NULL;
  }
}

const TypeSpec *extract_type(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_FUNCTION:
    case TOK_DECLARATOR:
    case TOK_CALL:
      return extract_ident(tree)->type;
    case TOK_IDENT:
    default:
      return tree->type;
  }
}

const Location *extract_loc(ASTree *tree) {
  switch (tree->symbol) {
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_FUNCTION:
    case TOK_DECLARATOR:
    case TOK_CALL:
      return &extract_ident(tree)->loc;
    case TOK_IDENT:
    default:
      return &tree->loc;
  }
}
