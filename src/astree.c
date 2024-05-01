#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "badalist.h"
#include "badllist.h"
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

/* extern int skip_type_check; */
const char *SCOPE_KIND_STRS[] = {"MEMBER SCOPE", "BLOCK SCOPE",
                                 "FUNCTION SCOPE", "FILE SCOPE"};

/* TODO(Robert): this is bad and slower than it needs to be. */
static ASTree **purgatory;
static size_t purgatory_cap = 16;

static void exit_purgatory(const ASTree *tree) {
  size_t i;
  for (i = 0; i < purgatory_cap; ++i)
    if (purgatory[i] == tree) purgatory[i] = NULL;
}

void astree_init_globals(void) {
  purgatory = calloc(purgatory_cap, sizeof(*purgatory));
}

void astree_destroy_globals(void) {
  size_t i;
  /* iterate in reverse so that root is destroyed last */
  for (i = 1; i <= purgatory_cap; ++i)
    if (purgatory[purgatory_cap - i] != NULL)
      astree_destroy(purgatory[purgatory_cap - i]);
  free(purgatory);
}

ASTree *astree_init(int tok_kind, const Location location, const char *info) {
  PFDBG2('t', "Initializing new astree node with code: %s, lexinfo: '%s'",
         parser_get_tname(tok_kind), info);

  size_t i;
  for (i = 0; i < purgatory_cap; ++i)
    if (purgatory[i] == NULL) break;

  if (i == purgatory_cap) {
    purgatory = realloc(purgatory, (purgatory_cap <<= 1) * sizeof(*purgatory));
    size_t j;
    for (j = i; j < purgatory_cap; ++j) purgatory[j] = NULL;
  }

  ASTree *tree = purgatory[i] = malloc(sizeof(ASTree));
  tree->tok_kind = tok_kind;
  tree->lexinfo = string_set_intern(info);
  tree->loc = location;
  tree->type = NULL;
  tree->attributes = ATTR_NONE;
  tree->instructions = instr_init(OP_SENTINEL);
  tree->spill_eightbytes = 0;
  tree->children_len = 0;
  tree->children_cap = 1;
  tree->children = malloc(tree->children_cap * sizeof(*tree->children));
  tree->scope = NULL;
  tree->constant.integral.signed_value = 0L;
  tree->constant.label = NULL;
  return tree;
}

void astree_destroy(ASTree *tree) {
  if (tree == NULL) return;

  PFDBG2('t', "Freeing an astree with symbol: %s, lexinfo: %s",
         parser_get_tname(tree->tok_kind), tree->lexinfo);

  /* free one-off TypeSpec objects */
  switch (tree->tok_kind) {
    default:
      break;
    case TOK_STRUCT:
      /* fallthrough */
    case TOK_UNION:
      /* fallthrough */
    case TOK_ENUM:
      /* fallthrough */
    case TOK_SPEC_LIST:
      free(tree->type);
      break;
    case '?':
      if (tree->type == NULL || !type_is_pointer(tree->type)) break;
      /* fallthrough */
    case TOK_PTR_CONV:
      /* fallthrough */
    case TOK_ADDROF:
      if (tree->type == NULL) break;
      assert(type_is_pointer(tree->type));
      tree->type->pointer.next = NULL;
      /* fallthrough */
    case TOK_TYPE_NAME:
      /* fallthrough */
    case TOK_STRINGCON:
      type_destroy(tree->type);
      break;
  }

  size_t i;
  for (i = 0; i < tree->children_len; ++i) astree_destroy(tree->children[i]);
  free(tree->children);

  /* free symbol table if present */
  scope_destroy(tree->scope);

  /* free instructions */
  instr_destroy(tree->instructions);

  exit_purgatory(tree);
  free(tree);
}

ASTree *astree_adopt(ASTree *parent, const size_t count, ...) {
  if (parent->children_len + count > parent->children_cap) {
    do parent->children_cap *= 2;
    while (parent->children_len + count > parent->children_cap);
    parent->children = realloc(
        parent->children, parent->children_cap * sizeof(*parent->children));
  }

  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = va_arg(args, ASTree *);
    assert(child != NULL);
    PFDBG2('t', "Tree %s adopts %s", parser_get_tname(parent->tok_kind),
           parser_get_tname(child->tok_kind));
    parent->children[parent->children_len++] = child;

    /* propogate size of spill area for nested calls */
    if (parent->spill_eightbytes < child->spill_eightbytes)
      parent->spill_eightbytes = child->spill_eightbytes;
    exit_purgatory(child);
  }
  va_end(args);
  return parent;
}

ASTree *astree_get(const ASTree *parent, const size_t index) {
  assert(index < parent->children_len);
  return parent->children[index];
}

ASTree *astree_disown(ASTree *parent) {
  assert(parent->children_len > 0);
  return parent->children[--parent->children_len];
}

size_t astree_count(const ASTree *parent) { return parent->children_len; }

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

int astree_to_string(const ASTree *tree, char *buffer) {
  static char locstr[LINESIZE], attrstr[LINESIZE], typestr[LINESIZE];
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return -1;

  const char *tname = parser_get_tname(tree->tok_kind);
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
  } else if (tree->tok_kind == TOK_CALL) {
    return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s} {SPILL: %lu}", tname,
                   tree->lexinfo, locstr, typestr, attrstr,
                   tree->spill_eightbytes * 8);
  } else {
    return sprintf(buffer, "%s \"%s\" {%s} {%s} {%s}", tname, tree->lexinfo,
                   locstr, typestr, attrstr);
  }
}

int astree_print_tree(ASTree *tree, FILE *out, int depth) {
  static char indent[LINESIZE], nodestr[LINESIZE];
  /* print out the whole tree */
  PFDBG3('t', "Tree info: token: %s, lexinfo: %s, children: %u",
         parser_get_tname(tree->tok_kind), tree->lexinfo, tree->children_len);

  size_t numspaces = depth * 2;
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  int characters_printed = astree_to_string(tree, nodestr);
  if (characters_printed < 0) return characters_printed;
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < tree->children_len; ++i) {
    PFDBG1('t', "    %p", tree->children[i]);
  }

  for (i = 0; i < tree->children_len; ++i) {
    assert(tree->children[i] != NULL);
    int status = astree_print_tree(tree->children[i], out, depth + 1);
    if (status) return status;
  }
  return 0;
}

static int location_ge(const Location *loc1, const Location *loc2) {
  return loc1->filenr > loc2->filenr ||
         (loc1->filenr == loc2->filenr && loc1->linenr > loc2->linenr) ||
         (loc1->filenr == loc2->filenr && loc1->linenr == loc2->linenr &&
          loc1->offset >= loc2->offset);
}

static int print_sym_child_helper(ASTree *tree, FILE *out, int depth) {
  ASTree *scope_node = NULL;
  switch (tree->tok_kind) {
    case TOK_ROOT:
    case TOK_BLOCK:
      scope_node = tree;
      break;
    case TOK_FOR:
      scope_node = astree_get(tree, 3);
      break;
    case TOK_SWITCH:
      /* fallthrough */
    case TOK_WHILE:
      /* fallthrough */
    case TOK_IF:
      /* fallthrough */
    case TOK_CASE:
      /* fallthrough */
    case TOK_LABEL:
      scope_node = astree_get(tree, 1);
      break;
    case TOK_DO:
      /* fallthrough */
    case TOK_DEFAULT:
      scope_node = astree_get(tree, 0);
      break;
    case TOK_DECLARATION:
      if (astree_count(tree) == 3) {
        scope_node = astree_get(tree, 2);
        /* set tree to declarator/identifier for nicer output */
        tree = astree_get(tree, 1);
      }
      break;
  }
  if (scope_node != NULL && scope_node->tok_kind == TOK_BLOCK) {
    static char locstr[LINESIZE];
    const char *tname = parser_get_tname(tree->tok_kind);
    int characters_printed = location_to_string(&tree->loc, locstr);
    if (characters_printed < 0) return characters_printed;
    if (strlen(tname) > 4) tname += 4;
    int padding_plus_tname = strlen(tname) + depth * 2;
    characters_printed =
        fprintf(out, "%*s \"%s\" {%s} {%s}:\n", padding_plus_tname, tname,
                tree->lexinfo, locstr,
                SCOPE_KIND_STRS[scope_get_kind(scope_node->scope)]);
    if (characters_printed < 0) return characters_printed;
    int status = astree_print_symbols(scope_node, out, depth + 1);
    if (tree->tok_kind == TOK_IF &&
        astree_get(tree, 2)->tok_kind == TOK_BLOCK && status >= 0)
      status = astree_print_symbols(astree_get(tree, 2), out, depth + 1);
    return status < 0 ? status : 0;
  } else {
    return 0;
  }
}

static int print_sym_helper(const char *ident, const Symbol *symbol, FILE *out,
                            int depth) {
  static char symbol_str[LINESIZE];
  int padding_plus_ident = strlen(ident) + depth * 2;
  int characters_printed = symbol_print(symbol, symbol_str);
  if (characters_printed < 0) return characters_printed;
  return fprintf(out, "%*s: %s\n", padding_plus_ident, ident, symbol_str);
}

int astree_print_symbols(ASTree *tree, FILE *out, int depth) {
  int ret = 0;

  size_t symbol_count = scope_symbol_count(tree->scope);
  size_t child_count = astree_count(tree);
  size_t symbol_index = 0, child_index = 0;
  while (symbol_index < symbol_count && child_index < child_count) {
    const char *ident;
    Symbol *symbol;
    scope_symbol_at(tree->scope, symbol_index, &ident, &symbol);
    ASTree *child = astree_get(tree, child_index);
    int characters_printed;
    if (location_ge(&child->loc, symbol->loc)) {
      characters_printed = print_sym_helper(ident, symbol, out, depth);
      ++symbol_index;
    } else {
      characters_printed = print_sym_child_helper(child, out, depth);
      ++child_index;
    }
    if (characters_printed < 0) goto fail;
  }
  while (symbol_index < symbol_count) {
    const char *ident;
    Symbol *symbol;
    scope_symbol_at(tree->scope, symbol_index, &ident, &symbol);
    int characters_printed = print_sym_helper(ident, symbol, out, depth);
    ++symbol_index;
    if (characters_printed < 0) goto fail;
  }
  while (child_index < astree_count(tree)) {
    ASTree *child = astree_get(tree, child_index);
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
          parser_get_tname(tree->tok_kind), tree->lexinfo);
cleanup:
  return ret;
}
#endif
