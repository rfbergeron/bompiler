#include <assert.h>
#include <stdarg.h>

#include "astree.h"
#include "bcc_err.h"
#include "evaluate.h"
#include "lyutils.h"
#include "state.h"
#include "strset.h"
#include "symtable.h"
#include "tchk_decl.h"
#include "tchk_expr.h"
#include "tchk_stmt.h"

/* dummy defines so that ccls doesn't complain when this file is not in the
 * source file that bison outputs
 */
#ifndef YYBISON
extern const char *const yytname[];
extern size_t YYTRANSLATE(int symbol);
#define BCC_YYSTATIC
#else
#define BCC_YYSTATIC static
#endif

static const char *VA_LIST_TYPEDEF_NAME = "__builtin_va_list";
static const char *VA_LIST_STRUCT_NAME = "__builtin_0_0_0_struct";
static const char *VA_LIST_MEMBER_NAMES[] = {
    "gp_offset", "fp_offset", "overflow_arg_area", "reg_save_area"};
const Type *TYPE_VA_LIST_POINTER;
Type *TYPE_VA_LIST_POINTER_INTERNAL;

const char *parser_get_tname(int symbol) {
  return yytname[YYTRANSLATE(symbol)];
}

BCC_YYSTATIC void make_va_list_type(void) {
  Tag *builtin_tag = tag_init(TAG_STRUCT);
  state_insert_tag(state, VA_LIST_STRUCT_NAME, strlen(VA_LIST_STRUCT_NAME),
                   builtin_tag);
  state_push_table(state, builtin_tag->data.members.by_name);
  builtin_tag->alignment = X64_ALIGNOF_LONG;
  builtin_tag->width = 2 * X64_SIZEOF_LONG + 2 * X64_SIZEOF_INT;
  builtin_tag->is_defined = 1;
  LinkedList *member_list = &builtin_tag->data.members.in_order;
  size_t i;
  ptrdiff_t displacement;
  for (i = 0, displacement = 0; i < 4; ++i) {
    Symbol *member_symbol = symbol_init(&LOC_EMPTY);
    member_symbol->disp = displacement;
    if (i < 2) {
      displacement += X64_SIZEOF_INT;
      member_symbol->type = type_init_base(SPEC_FLAGS_UINT);
    } else {
      displacement += X64_SIZEOF_LONG;
      member_symbol->type = type_init_pointer(QUAL_FLAG_NONE);
      Type *void_type = type_init_base(SPEC_FLAG_VOID);
      member_symbol->type = type_append(member_symbol->type, void_type, 0);
    }
    int status = llist_push_back(member_list, member_symbol);
    if (status) abort();
    const char *symname = VA_LIST_MEMBER_NAMES[i];
    state_insert_symbol(state, symname, strlen(symname), member_symbol);
    if (status) abort();
  }

  state_pop_table(state);
  Symbol *builtin_symbol = symbol_init(&LOC_EMPTY);
  builtin_symbol->storage = STORE_TYPEDEF;
  builtin_symbol->linkage = LINK_TYPEDEF;
  builtin_symbol->info = SYM_DEFINED;
  builtin_symbol->type = type_init_array(1, 0);
  Type *struct_type =
      type_init_tag(STOR_FLAG_TYPEDEF, VA_LIST_STRUCT_NAME, builtin_tag);
  (void)type_append(builtin_symbol->type, struct_type, 0);
  state_insert_symbol(state, VA_LIST_TYPEDEF_NAME, strlen(VA_LIST_TYPEDEF_NAME),
                      builtin_symbol);
  /* copy type info */
  Type *va_list_type_temp = type_copy(builtin_symbol->type, 1);
  /* convert to pointer and free array */
  TYPE_VA_LIST_POINTER_INTERNAL = type_pointer_conversions(va_list_type_temp);
  va_list_type_temp->array.next = NULL;
  type_destroy(va_list_type_temp);
  assert(type_is_pointer(TYPE_VA_LIST_POINTER_INTERNAL));
  TYPE_VA_LIST_POINTER = TYPE_VA_LIST_POINTER_INTERNAL;
}

BCC_YYSTATIC ASTree *parser_make_root(void) {
  PFDBG1('p', "Initializing AST, root token code: %d", TOK_ROOT);
  PFDBG1('p', "Translation of token code: %s", parser_get_tname(TOK_ROOT));
  Location root_loc = LOC_EMPTY_VALUE;
  /* assign filenr separately since its not an initializer constant */
  root_loc.filenr = lexer_get_filenr();
  return astree_init(TOK_ROOT, root_loc, "_root");
}

BCC_YYSTATIC ASTree *parser_make_empty(const Location loc) {
  return validate_empty_expr(astree_init(TOK_EMPTY, loc, "__empty"));
}

void parser_init_globals(void) {
  parser_root = parser_make_root();
  PFDBG0('t', "Making symbol table");
  parser_root->symbol_table = symbol_table_init(TABLE_TRANS_UNIT);
  state_push_table(state, parser_root->symbol_table);
  make_va_list_type();
}

void parser_destroy_globals(void) {
  type_destroy(TYPE_VA_LIST_POINTER_INTERNAL);
}

BCC_YYSTATIC ASTree *parser_new_sym(ASTree *tree, int new_symbol) {
  tree->tok_kind = new_symbol;
  return tree;
}

BCC_YYSTATIC ASTree *parser_make_joiner(ASTree *stringcon) {
  return astree_adopt(astree_init(TOK_STRINGCON, stringcon->loc, "_joiner"), 1,
                      stringcon);
}

BCC_YYSTATIC ASTree *parser_join_strings(ASTree *joiner) {
  assert(strcmp("_joiner", joiner->lexinfo) == 0 && astree_count(joiner) > 0);
  if (astree_count(joiner) == 1) {
    ASTree *stringcon = astree_remove(joiner, 0);
    astree_destroy(joiner);
    return stringcon;
  }

  size_t joined_len, i, literal_count = astree_count(joiner);
  for (joined_len = 0, i = 0; i < literal_count; ++i)
    /* omit double quotes from length calculation */
    joined_len += strlen(astree_get(joiner, i)->lexinfo) - 2;
  /* add one for null byte */
  char *joined_string = malloc(sizeof(char) * (joined_len + 1)), *endptr;
  joined_string[joined_len] = '\0';

  for (i = 0, endptr = joined_string; i < literal_count; ++i) {
    const char *literal = astree_get(joiner, i)->lexinfo;
    size_t literal_len = strlen(literal);
    /* omit double quotes from copy */
    (void)memcpy(endptr, literal + 1, literal_len - 2);
    endptr += literal_len - 2;
  }

  assert(joined_string[joined_len] == '\0');

  joiner->lexinfo = string_set_intern(joined_string);
  free(joined_string);

  return joiner;
}

BCC_YYSTATIC ASTree *parser_make_decl_specs(ASTree *first_specifier) {
  ASTree *decl_specs =
      astree_init(TOK_SPEC_LIST, first_specifier->loc, "_decl_specs");
  decl_specs->type = type_init_none(0);
  return validate_decl_spec(decl_specs, first_specifier);
}

BCC_YYSTATIC ASTree *parser_make_declaration(ASTree *decl_specs) {
  ASTree *declaration = astree_init(TOK_DECLARATION, LOC_EMPTY, "_declaration");
  decl_specs = finalize_decl_specs(decl_specs);
  return astree_adopt(declaration, 1, decl_specs);
}

BCC_YYSTATIC ASTree *parser_make_param_list(ASTree *left_paren,
                                            ASTree *decl_specs,
                                            ASTree *declarator) {
  /* all error handling done in validate_param */
  ASTree *param_list =
      validate_param_list(parser_new_sym(left_paren, TOK_PARAM_LIST));
  /* create temporary type object on the heap to hold the parameter auxspec */
  ASTree *declaration = parser_make_declaration(decl_specs);
  return validate_param(param_list, declaration, declarator);
}

/* For the sake of code reusability, the tree structure of a type name is the
 * same as that of a full declaration, with the exception being that in the
 * place of the node with the TOK_IDENT symbol, there is instead a node with the
 * TOK_TYPE_NAME symbol. This way, the code for processing declaration can also
 * be used to validate type names, with a little tweaking.
 */
BCC_YYSTATIC ASTree *parser_make_type_name(void) {
  return astree_init(TOK_TYPE_NAME, lexer_get_loc(), "_type_name");
}

BCC_YYSTATIC ASTree *parser_make_cast(ASTree *left_paren, ASTree *decl_specs,
                                      ASTree *type_name, ASTree *expr) {
  ASTree *cast = parser_new_sym(left_paren, TOK_CAST);
  ASTree *declaration = parser_make_declaration(decl_specs);
  return validate_cast(
      cast, finalize_declaration(declare_symbol(declaration, type_name)), expr);
}

BCC_YYSTATIC ASTree *parser_make_label(ASTree *ident) {
  return astree_init(TOK_LABEL, ident->loc, "_label");
}

BCC_YYSTATIC ASTree *parser_make_attributes_list(void) {
  return astree_init(TOK_ATTR_LIST, lexer_get_loc(), "_attr_list");
}

BCC_YYSTATIC ASTree *parse_pretty_function(ASTree *pretty_function) {
  assert(pretty_function->tok_kind == TOK_PRTY_FN);
  const char *fn_name = state_get_function_name(state);
  assert(fn_name != NULL);
  /* add 1 for terminating nul and 2 for double quotes */
  size_t fn_str_size = strlen(fn_name) + 3;
  char *fn_str = malloc(sizeof(char) * fn_str_size);
  fn_str[0] = '\"';
  fn_str[1] = '\0';
  strcat(fn_str, fn_name);
  assert(strlen(fn_str) == fn_str_size - 2);
  fn_str[fn_str_size - 2] = '\"';
  fn_str[fn_str_size - 1] = '\0';
  /* put quoted function name into the string set */
  ASTree *fn_str_node = astree_init(TOK_STRINGCON, pretty_function->loc,
                                    string_set_intern(fn_str));
  /* free quoted function name since strset duplicates it */
  free(fn_str);
  astree_destroy(pretty_function);
  return fn_str_node;
}

BCC_YYSTATIC ASTree *parse_sizeof(ASTree *sizeof_, ASTree *decl_specs,
                                  ASTree *declarator) {
  ASTree *declaration = parser_make_declaration(decl_specs);
  return validate_sizeof(
      sizeof_, finalize_declaration(declare_symbol(declaration, declarator)));
}

BCC_YYSTATIC ASTree *parse_va_arg(ASTree *va_arg_, ASTree *expr,
                                  ASTree *decl_specs, ASTree *declarator) {
  ASTree *declaration = parser_make_declaration(decl_specs);
  return validate_va_arg(
      va_arg_, expr,
      finalize_declaration(declare_symbol(declaration, declarator)));
}

BCC_YYSTATIC void parser_cleanup(size_t count, ...) {
  PFDBG1('p', "Cleaning up %lu astree nodes", count);
  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *tree = va_arg(args, ASTree *);
    astree_destroy(tree);
  }
  va_end(args);
}
