#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "badalist.h"
#include "badllist.h"
#include "bcc_err.h"
#include "debug.h"
#include "evaluate.h"
#include "lyutils.h"
#include "strset.h"
#include "symtable.h"
#include "yyparse.h"
#define LINESIZE 1024

#ifdef UNIT_TESTING
extern void *_test_malloc(const size_t size, const char *file, const int line);
#define malloc(size) _test_malloc(size, __FILE__, __LINE__)
extern void _test_free(void *ptr, const char *file, const int line);
#define free(ptr) _test_free(ptr, __FILE__, __LINE__)
extern void mock_assert(const int result, const char *const expression,
                        const char *const file, const int line);
#undef assert
#define assert(expression) \
  mock_assert((int)(expression), #expression, __FILE__, __LINE__);
#else
static const char attr_map[][16] = {"LVAL", "DEFAULT", "CONST", "INITIALIZER",
                                    "ADDRESS"};
#endif

extern int skip_type_check;

ASTree EMPTY_EXPR = EMPTY_EXPR_VALUE;

ASTree *astree_init(int symbol, const Location location, const char *info) {
  PFDBG2('t', "Initializing new astree node with code: %s, lexinfo: '%s'",
         parser_get_tname(symbol), info);

  ASTree *tree = malloc(sizeof(ASTree));
  tree->symbol = symbol;
  tree->lexinfo = string_set_intern(info);
  tree->loc = location;
  tree->type = NULL;
  tree->attributes = ATTR_NONE;
  tree->first_instr = NULL;
  tree->last_instr = NULL;
  tree->spill_eightbytes = 0;
  /* casting function pointers is undefined behavior but it seems like most
   * compiler implementations make it work, and here we're just changing the
   * type of a pointer argument */
  llist_init(&tree->children, (void (*)(void *))(astree_destroy), NULL);
  tree->symbol_table = NULL;
  tree->constant.integral.signed_value = 0L;
  tree->constant.label = NULL;
  tree->constant.symval = NULL;
  return tree;
}

void astree_destroy(ASTree *tree) {
  if (tree == NULL || tree == &EMPTY_EXPR) return;

  PFDBG2('t', "Freeing an astree with symbol: %s, lexinfo: %s",
         parser_get_tname(tree->symbol), tree->lexinfo);
  if (yydebug) {
    /* print tree contents to stderr */
  }

  /* free one-off TypeSpec objects */
  /* TODO(Robert): Type: this is obviously not good enough any more because of
   * how the representation of types has changed. make changes as necessary.
   */
  if (!skip_type_check && tree->type != NULL) {
    switch (tree->symbol) {
      default:
        break;
      /*
      size_t i;
      int owns_type;
      default:
        for (i = 0, owns_type = 1; i < llist_size(&tree->children) && owns_type;
             ++i) {
          ASTree *child = llist_get(&tree->children, i);
          if (tree->type == child->type) owns_type = 0;
        }
        if (owns_type) type_destroy(tree->type);
        break;
      */
      case TOK_INTCON:
        /* fallthrough */
      case TOK_CHARCON:
        /* fallthrough */
      case TOK_SUBSCRIPT:
        /* fallthrough */
      case TOK_INDIRECTION:
        /* fallthrough */
      case TOK_CALL:
        break;
      case '?':
        if (!type_is_pointer(tree->type)) break;
        /* fallthrough */
      case TOK_AUTO_CONV:
        /* fallthrough */
      case TOK_ADDROF:
        assert(type_is_pointer(tree->type));
        tree->type->pointer.next = NULL;
        /* fallthrough */
      case TOK_TYPE_NAME:
        /* fallthrough */
      case TOK_TYPE_ERROR:
        type_destroy(tree->type);
        break;
    }
  }

  /* llist should handle destruction of children */
  int status = llist_destroy(&tree->children);
  if (status) {
    fprintf(stderr, "your datas structures library sucks\n");
    abort();
  }

  /* free symbol table if present */
  symbol_table_destroy(tree->symbol_table);

  /* free instruction iterators */
  free(tree->first_instr);
  free(tree->last_instr);

  free(tree);
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
    assert(child != NULL);
    assert((child == &EMPTY_EXPR) ||
           (llist_find(&parent->children, child) == (size_t)-1L));
    PFDBG2('t', "Tree %s adopts %s", parser_get_tname(parent->symbol),
           parser_get_tname(child->symbol));
    int status = llist_push_back(&parent->children, child);
    if (status) {
      fprintf(stderr, "ERROR: your data structures library sucks.\n");
      abort();
    }
    /* propogate size of spill area for nested calls */
    if (parent->spill_eightbytes < child->spill_eightbytes)
      parent->spill_eightbytes = child->spill_eightbytes;
  }
  va_end(args);
  return parent;
}

ASTree *astree_replace(ASTree *parent, const size_t index, ASTree *child) {
  assert(child != NULL);
  if (index >= astree_count(parent)) return NULL;
  ASTree *old_child = llist_extract(&parent->children, index);
  int status = llist_insert(&parent->children, child, index);
  if (status) abort();
  return old_child;
}

ASTree *astree_get(ASTree *parent, const size_t index) {
  return llist_get(&parent->children, index);
}

ASTree *astree_remove(ASTree *parent, const size_t index) {
  return llist_extract(&parent->children, index);
}

ASTree *astree_create_errnode(ASTree *child, ErrorCode code, size_t info_size,
                              ...) {
  va_list info_ptrs;
  va_start(info_ptrs, info_size);
  CompileError *error = compile_error_init_v(code, info_size, info_ptrs);
  va_end(info_ptrs);

  if (child->symbol == TOK_TYPE_ERROR) {
    type_append_error(child->type, error);
    return child;
  } else {
    ASTree *errnode = astree_init(TOK_TYPE_ERROR, child->loc, "_terr");
    errnode->type = type_init_error(error);
    return astree_adopt(errnode, 1, child);
  }
}

/* TODO(Robert): consider giving this function the ability to correctly
 * propogate error nodes for more complex tree structures, like enums
 */
ASTree *astree_propogate_errnode(ASTree *parent, ASTree *child) {
  if (child->symbol != TOK_TYPE_ERROR) {
    (void)astree_adopt(UNWRAP(parent), 1, child);
    return parent;
  } else if (parent->symbol != TOK_TYPE_ERROR) {
    ASTree *real_child = astree_replace(child, 0, parent);
    (void)astree_adopt(parent, 1, real_child);
    return child;
  } else {
    (void)type_merge_errors(parent->type, child->type);
    (void)astree_adopt(UNWRAP(parent), 1, astree_remove(child, 0));
    astree_destroy(child);
    return parent;
  }
}

ASTree *astree_propogate_errnode_v(ASTree *parent, size_t count, ...) {
  va_list children;
  va_start(children, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = va_arg(children, ASTree *);
    parent = astree_propogate_errnode(parent, child);
  }
  va_end(children);
  return parent;
}

ASTree *astree_propogate_errnode_a(ASTree *parent, size_t count,
                                   ASTree **children) {
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = children[i];
    parent = astree_propogate_errnode(parent, child);
  }
  return parent;
}

size_t astree_count(ASTree *parent) { return llist_size(&parent->children); }

int astree_is_const_zero(const ASTree *tree) {
  return (tree->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT &&
         type_is_integral(tree->type) &&
         (type_is_signed(tree->type) || type_is_enum(tree->type)
              ? tree->constant.integral.signed_value == 0
              : tree->constant.integral.unsigned_value == 0);
}

#ifndef UNIT_TESTING
static int attributes_to_string(const unsigned int attributes, char *buf) {
  if (attributes == ATTR_NONE) {
    buf[0] = 0;
    return 0;
  }

  size_t i, buf_index = 0;
  for (i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes & (1 << i))
      buf_index += sprintf(buf + buf_index, "%s ", attr_map[i]);
  }

  if (buf_index > 0) buf[buf_index - 1] = 0;
  return 0;
}

int astree_to_string(ASTree *tree, char *buffer) {
  static char locstr[LINESIZE], attrstr[LINESIZE], typestr[LINESIZE];
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return -1;

  const char *tname = parser_get_tname(tree->symbol);
  int characters_printed = location_to_string(&tree->loc, locstr);
  if (characters_printed < 0) return characters_printed;
  characters_printed = attributes_to_string(tree->attributes, attrstr);
  if (characters_printed < 0) return characters_printed;
  characters_printed = type_to_str(tree->type, typestr);
  if (characters_printed < 0) return characters_printed;

  if (strlen(tname) > 4) tname += 4;
  if ((tree->attributes & ATTR_MASK_CONST) >= ATTR_CONST_INIT) {
    if (tree->constant.label != NULL) {
      return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s} { %s%+li }", tname,
                     tree->lexinfo, locstr, typestr, attrstr,
                     tree->constant.label,
                     tree->constant.integral.signed_value);
    } else if (type_is_signed(tree->type)) {
      return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s} { %li }", tname,
                     tree->lexinfo, locstr, typestr, attrstr,
                     tree->constant.integral.signed_value);
    } else {
      return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s} { %lu }", tname,
                     tree->lexinfo, locstr, typestr, attrstr,
                     tree->constant.integral.unsigned_value);
    }
  } else {
    return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s}", tname, tree->lexinfo,
                   locstr, typestr, attrstr);
  }
}

int astree_print_tree(ASTree *tree, FILE *out, int depth) {
  static char indent[LINESIZE], nodestr[LINESIZE];
  /* print out the whole tree */
  PFDBG3('t', "Tree info: token: %s, lexinfo: %s, children: %u",
         parser_get_tname(tree->symbol), tree->lexinfo,
         llist_size(&tree->children));

  size_t numspaces = depth * 2;
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  int characters_printed = astree_to_string(tree, nodestr);
  if (characters_printed < 0) return characters_printed;
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < llist_size(&tree->children); ++i) {
    PFDBG1('t', "    %p", llist_get(&tree->children, i));
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

static int location_ge(Location *loc1, Location *loc2) {
  return loc1->filenr > loc2->filenr ||
         (loc1->filenr == loc2->filenr && loc1->linenr > loc2->linenr) ||
         (loc1->filenr == loc2->filenr && loc1->linenr == loc2->linenr &&
          loc1->offset >= loc2->offset);
}

static int compare_symval_pairs(MapPair *pair1, MapPair *pair2) {
  SymbolValue *symval1 = pair1->value;
  SymbolValue *symval2 = pair2->value;
  return location_ge(&symval1->loc, &symval2->loc);
}

static const char *table_type_to_str(TableType type) {
  switch (type) {
    case FUNCTION_TABLE:
      return "FUNCTION SCOPE";
    case BLOCK_TABLE:
      return "BLOCK SCOPE";
    case MEMBER_TABLE:
      /* currently unused */
      return "MEMBER NAMESPACE";
    case TRANS_UNIT_TABLE:
      /* currently unused */
      return "TRANSLATION UNIT SCOPE";
    default:
      abort();
  }
}

static int print_sym_child_helper(ASTree *tree, FILE *out, int depth) {
  ASTree *block = NULL;
  switch (tree->symbol) {
    case TOK_ROOT:
    case TOK_BLOCK:
      block = tree;
      break;
    case TOK_SWITCH:
      /* fallthrough */
    case TOK_WHILE:
      /* fallthrough */
    case TOK_FOR:
      /* fallthrough */
    case TOK_IF:
      /* fallthrough */
    case TOK_CASE:
      /* fallthrough */
    case TOK_LABEL:
      block = astree_get(tree, 1);
      break;
    case TOK_DO:
      /* fallthrough */
    case TOK_DEFAULT:
      block = astree_get(tree, 0);
      break;
    case TOK_DECLARATION:
      if (astree_count(tree) == 3) {
        block = astree_get(tree, 2);
        /* set tree to declarator/identifier for nicer output */
        tree = astree_get(tree, 1);
      }
      break;
  }
  if (block != NULL && block->symbol == TOK_BLOCK) {
    static char locstr[LINESIZE];
    const char *tname = parser_get_tname(tree->symbol);
    int characters_printed = location_to_string(&tree->loc, locstr);
    if (characters_printed < 0) return characters_printed;
    if (strlen(tname) > 4) tname += 4;
    int padding_plus_tname = strlen(tname) + depth * 2;
    characters_printed = fprintf(
        out, "%*s \"%s\" {%s} {%s}:\n", padding_plus_tname, tname,
        tree->lexinfo, locstr, table_type_to_str(block->symbol_table->type));
    if (characters_printed < 0) return characters_printed;
    int status = astree_print_symbols(block, out, depth + 1);
    return status < 0 ? status : 0;
  } else {
    return 0;
  }
}

static int print_sym_pair_helper(MapPair *pair, FILE *out, int depth) {
  static char symval_str[LINESIZE];
  const char *symname = pair->key;
  int padding_plus_symname = strlen(symname) + depth * 2;
  SymbolValue *symval = pair->value;
  int characters_printed = symbol_value_print(symval, symval_str);
  if (characters_printed < 0) return characters_printed;
  return fprintf(out, "%*s: %s\n", padding_plus_symname, symname, symval_str);
}

int astree_print_symbols(ASTree *block, FILE *out, int depth) {
  int ret = 0;
  Map *primary_namespace = block->symbol_table->primary_namespace;
  ArrayList symval_pairs;
  int status = alist_init(&symval_pairs, map_size(primary_namespace));
  if (status) abort();
  status = map_pairs(primary_namespace, &symval_pairs);
  if (status) abort();
  alist_ssort(&symval_pairs, (BlibComparator)compare_symval_pairs);

  size_t pair_index = 0, child_index = 0;
  while (pair_index < alist_size(&symval_pairs) &&
         child_index < astree_count(block)) {
    MapPair *pair = alist_get(&symval_pairs, pair_index);
    ASTree *child = astree_get(block, child_index);
    int characters_printed;
    if (location_ge(&child->loc, &((SymbolValue *)pair->value)->loc)) {
      characters_printed = print_sym_pair_helper(pair, out, depth);
      ++pair_index;
    } else {
      characters_printed = print_sym_child_helper(child, out, depth);
      ++child_index;
    }
    if (characters_printed < 0) goto fail;
  }
  while (pair_index < alist_size(&symval_pairs)) {
    MapPair *pair = alist_get(&symval_pairs, pair_index);
    int characters_printed = print_sym_pair_helper(pair, out, depth);
    ++pair_index;
    if (characters_printed < 0) goto fail;
  }
  while (child_index < astree_count(block)) {
    ASTree *child = astree_get(block, child_index);
    int characters_printed = print_sym_child_helper(child, out, depth);
    ++child_index;
    if (characters_printed < 0) goto fail;
  }
  goto cleanup;

fail:
  ret = -1;
  fprintf(stderr,
          "ERROR: failed to print symbol table associated with "
          "astree node, symbol: %s, lexinfo: %s\n",
          parser_get_tname(block->symbol), block->lexinfo);
cleanup:
  status = alist_destroy(&symval_pairs, free);
  if (status) abort();
  return ret;
}
#endif
