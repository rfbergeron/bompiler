#include "astree.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "evaluate.h"
#include "lyutils.h"
#include "state.h"
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
static const char *const cexpr_map[] = {"FALSE", "MAYBE", "INITIALIZER",
                                        "INTEGRAL"};
static const char *const lval_map[] = {"FALSE", "TRUE", "MODIFIABLE", "N/A"};
#endif

/* extern int skip_type_check; */
const char *SCOPE_KIND_STRS[] = {"MEMBER SCOPE", "BLOCK SCOPE",
                                 "FUNCTION SCOPE", "FILE SCOPE"};

/* TODO(Robert): this is bad and slower than it needs to be. */
ARR_STAT(ASTree *, purgatory);

static void exit_purgatory(const ASTree *tree) {
  size_t i;
  for (i = 0; i < ARR_LEN(purgatory); ++i)
    if (ARR_GET(purgatory, i) == tree) ARR_PUT(purgatory, i, NULL);
}

void astree_init_globals(void) { ARR_INIT(purgatory, 16); }

void astree_destroy_globals(void) {
  while (!ARR_EMPTY(purgatory)) {
    astree_destroy(ARR_PEEK(purgatory));
    ARR_POP(purgatory);
  }
  ARR_DESTROY(purgatory);
}

ASTree *astree_init(int tok_kind, const Location location, const char *info) {
  PFDBG2('t', "Initializing new astree node with code: %s, lexinfo: '%s'",
         parser_get_tname(tok_kind), info);

  size_t i;
  for (i = 0; i < ARR_LEN(purgatory); ++i)
    if (ARR_GET(purgatory, i) == NULL) break;

  ASTree *tree = malloc(sizeof(ASTree));
  if (i == ARR_LEN(purgatory))
    ARR_PUSH(purgatory, tree);
  else
    ARR_PUT(purgatory, i, tree);

  tree->tok_kind = tok_kind;
  tree->lexinfo = string_set_intern(info);
  tree->loc = location;
  tree->type = NULL;
  tree->cexpr_kind = CEXPR_FALSE;
  tree->instructions = instr_init(OP_SENTINEL);
  tree->spill_eightbytes = 0;
  ARR_INIT(tree->children, 1);
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
      type_destroy(tree->type);
      break;
    case '?':
      if (tree->type == NULL || !type_is_pointer(tree->type)) break;
      /* fallthrough */
    case TOK_PTR_CONV:
      /* fallthrough */
    case TOK_ADDROF:
      if (tree->type == NULL) break;
      assert(type_is_pointer(tree->type));
      (void)type_detach(tree->type);
      /* fallthrough */
    case TOK_TYPE_NAME:
      /* fallthrough */
    case TOK_STRINGCON:
      type_destroy(tree->type);
      break;
  }

  while (!ARR_EMPTY(tree->children)) {
    astree_destroy(ARR_PEEK(tree->children));
    ARR_POP(tree->children);
  }
  ARR_DESTROY(tree->children);

  /* free symbol table if present */
  scope_destroy(tree->scope);

  /* free instructions */
  instr_destroy(tree->instructions);

  exit_purgatory(tree);
  free(tree);
}

ASTree *astree_adopt(ASTree *parent, const size_t count, ...) {
  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = va_arg(args, ASTree *);
    assert(child != NULL);
    PFDBG2('t', "Tree %s adopts %s", parser_get_tname(parent->tok_kind),
           parser_get_tname(child->tok_kind));
    ARR_PUSH(parent->children, child);

    /* propogate size of spill area for nested calls */
    if (parent->spill_eightbytes < child->spill_eightbytes)
      parent->spill_eightbytes = child->spill_eightbytes;
    exit_purgatory(child);
  }
  va_end(args);
  return parent;
}

ASTree *astree_get(const ASTree *parent, const size_t index) {
  assert(index < ARR_LEN(parent->children));
  return ARR_GET(parent->children, index);
}

ASTree *astree_disown(ASTree *parent) {
  ASTree *disowned = ARR_PEEK(parent->children);
  ARR_POP(parent->children);
  return disowned;
}

size_t astree_count(const ASTree *parent) { return ARR_LEN(parent->children); }

int astree_is_const_zero(const ASTree *tree) {
  return tree->cexpr_kind >= CEXPR_INIT && type_is_integral(tree->type) &&
         (type_is_signed(tree->type) || type_is_enum(tree->type)
              ? tree->constant.integral.signed_value == 0
              : tree->constant.integral.unsigned_value == 0);
}

LValKind astree_is_lvalue(const ASTree *tree) {
  if (tree->type == NULL) {
    return LVAL_FALSE;
  } else if (tree->cexpr_kind >= CEXPR_INIT) {
    return LVAL_FALSE;
  } else if (tree->tok_kind == TOK_CEXPR_CONV) {
    return astree_is_lvalue(astree_get(tree, 0));
  } else if (tree->tok_kind != TOK_IDENT && tree->tok_kind != TOK_INDIRECTION &&
             tree->tok_kind != TOK_SUBSCRIPT && tree->tok_kind != TOK_IDENT &&
             tree->tok_kind != TOK_ARROW && tree->tok_kind != '.') {
    return LVAL_FALSE;
  } else if (type_is_function(tree->type)) {
    return LVAL_FALSE;
  } else if (type_is_array(tree->type) || type_is_incomplete(tree->type) ||
             type_is_const(tree->type)) {
    return LVAL_TRUE;
  } else {
    return LVAL_MODABLE;
  }
}

#ifndef UNIT_TESTING
int astree_to_string(const ASTree *tree, char *buffer) {
  static char locstr[LINESIZE], attrstr[LINESIZE], typestr[LINESIZE];
  /* print token name, lexinfo in quotes, the location, block number,
   * attributes, and the typeid if this is a struct
   */
  if (tree == NULL) return -1;

  const char *tname = parser_get_tname(tree->tok_kind);
  int characters_printed = location_to_string(&tree->loc, locstr);
  if (characters_printed < 0) return characters_printed;
  characters_printed =
      sprintf(attrstr, "CEXPR: %s LVAL: %s", cexpr_map[tree->cexpr_kind],
              lval_map[astree_is_lvalue(tree)]);

  if (characters_printed < 0) return characters_printed;
  characters_printed = type_to_str(tree->type, typestr);
  if (characters_printed < 0) return characters_printed;

  if (strlen(tname) > 4) tname += 4;
  if (tree->cexpr_kind >= CEXPR_INIT) {
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
         parser_get_tname(tree->tok_kind), tree->lexinfo,
         ARR_LEN(tree->children));

  size_t numspaces = depth * 2;
  memset(indent, ' ', numspaces);
  indent[numspaces] = 0;
  int characters_printed = astree_to_string(tree, nodestr);
  if (characters_printed < 0) return characters_printed;
  fprintf(out, "%s%s\n", indent, nodestr);

  size_t i;
  for (i = 0; i < ARR_LEN(tree->children); ++i) {
    PFDBG1('t', "    %p", ARR_GET(tree->children, i));
  }

  for (i = 0; i < ARR_LEN(tree->children); ++i) {
    assert(ARR_GET(tree->children, i) != NULL);
    int status = astree_print_tree(ARR_GET(tree->children, i), out, depth + 1);
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
