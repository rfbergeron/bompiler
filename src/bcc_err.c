#include "bcc_err.h"

#include <assert.h>

/* this macro expects that `chars_written` was previously declared */
#ifdef NDEBUG
#define CHECK_TO_STR(expr, max_len) ((void)(chars_written = (expr)))
#else
#define CHECK_TO_STR(expr, max_len)  \
  do {                               \
    chars_written = (expr);          \
    assert(chars_written >= 0);      \
    assert(chars_written < max_len); \
  } while (0)
#endif

#define BUFFER_SIZE 1024
static char type_buffer1[BUFFER_SIZE];
static char type_buffer2[BUFFER_SIZE];
static char ast_buffer1[BUFFER_SIZE];
static char ast_buffer2[BUFFER_SIZE];
static char old_sym_buffer[BUFFER_SIZE];
static char new_sym_buffer[BUFFER_SIZE];
static char tag_buffer[BUFFER_SIZE];

int semerr_const_too_large(const ASTree *constant, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: Constant value `%s` too large for type `%s`\n",
                 lexer_filename(constant->loc.filenr), constant->loc.linenr,
                 constant->loc.offset, constant->lexinfo, type_buffer1);
}

int semerr_excess_init(const ASTree *initializer, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(initializer, ast_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: initializer {%s} has excess elements for type `%s`\n",
      lexer_filename(initializer->loc.filenr), initializer->loc.linenr,
      initializer->loc.offset, ast_buffer1, type_buffer1);
}

int semerr_expected_init(const ASTree *initializer) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(initializer, ast_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr, "%s:%lu.%lu: initializer {%s} is not an initializer constant\n",
      lexer_filename(initializer->loc.filenr), initializer->loc.linenr,
      initializer->loc.offset, ast_buffer1);
}

int semerr_compat_init(const ASTree *initializer, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(initializer, ast_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr, "%s:%lu.%lu: initializer {%s} is incompatible with type `%s`\n",
      lexer_filename(initializer->loc.filenr), initializer->loc.linenr,
      initializer->loc.offset, ast_buffer1, type_buffer1);
}

int semerr_incompatible_spec(const ASTree *decl_specs,
                             const ASTree *decl_spec) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(decl_specs->type, type_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(astree_to_string(decl_spec, ast_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: declaration specifier {%s} incompatible with type `%s`\n",
      lexer_filename(decl_specs->loc.filenr), decl_specs->loc.linenr,
      decl_specs->loc.offset, ast_buffer1, type_buffer1);
}

int semerr_symbol_not_found(const ASTree *identifier) {
  semantic_error = 1;
  return fprintf(stderr, "%s:%lu.%lu: unable to resolve symbol `%s`\n",
                 lexer_filename(identifier->loc.filenr), identifier->loc.linenr,
                 identifier->loc.offset, identifier->lexinfo);
}

int semerr_expected_typedef_name(const ASTree *identifier,
                                 const Symbol *symbol) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(symbol_print(symbol, old_sym_buffer), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: expected identifier `%s` to be a typedef name; "
                 "existing symbol: {%s}\n",
                 lexer_filename(identifier->loc.filenr), identifier->loc.linenr,
                 identifier->loc.offset, identifier->lexinfo, old_sym_buffer);
}

int semerr_invalid_type(const ASTree *tree) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(tree->type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: invalid type `%s`\n",
                 lexer_filename(tree->loc.filenr), tree->loc.linenr,
                 tree->loc.offset, type_buffer1);
}

int semerr_incomplete_type(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: unexpected incomplete type `%s`\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, type_buffer1);
}

int semerr_invalid_linkage(const ASTree *declarator, const Symbol *symbol) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(symbol_print(symbol, new_sym_buffer), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: symbol {%s} has invalid linkage\n",
                 lexer_filename(declarator->loc.filenr), declarator->loc.linenr,
                 declarator->loc.offset, new_sym_buffer);
}

int semerr_incompatible_linkage(const ASTree *declarator,
                                const Symbol *old_symbol,
                                const Symbol *new_symbol) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(symbol_print(new_symbol, new_sym_buffer), BUFFER_SIZE);
  CHECK_TO_STR(symbol_print(old_symbol, old_sym_buffer), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: incompatible linkage for symbol `%s`; old "
                 "symbol: {%s}; new symbol: {%s}\n",
                 lexer_filename(declarator->loc.filenr), declarator->loc.linenr,
                 declarator->loc.offset, declarator->lexinfo, old_sym_buffer,
                 new_sym_buffer);
}

int semerr_redefine_symbol(const ASTree *declarator, const Symbol *symbol) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(symbol_print(symbol, old_sym_buffer), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: redefinition of symbol `%s`; previous definition: {%s}\n",
      lexer_filename(declarator->loc.filenr), declarator->loc.linenr,
      declarator->loc.offset, declarator->lexinfo, old_sym_buffer);
}

int semerr_define_extern(const ASTree *declarator) {
  semantic_error = 1;
  return fprintf(stderr,
                 "%s:%lu.%lu: attempt to initialize symbol `%s`, which has "
                 "external storage\n",
                 lexer_filename(declarator->loc.filenr), declarator->loc.linenr,
                 declarator->loc.offset, declarator->lexinfo);
}

int semerr_invalid_arr_size(const ASTree *array, const ASTree *expr) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(array, ast_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(astree_to_string(expr, ast_buffer2), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: invalid size {%s} for array {%s}\n",
                 lexer_filename(expr->loc.filenr), expr->loc.linenr,
                 expr->loc.offset, ast_buffer2, ast_buffer1);
}

int semerr_expected_ident(const ASTree *function, const ASTree *param) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(param, ast_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: in definition of function `%s`: expected "
                 "identifier for param {%s}\n",
                 lexer_filename(param->loc.filenr), param->loc.linenr,
                 param->loc.offset, function->lexinfo, ast_buffer1);
}

int semerr_label_not_found(const ASTree *label) {
  semantic_error = 1;
  return fprintf(stderr, "%s:%lu.%lu: unable to resolve label `%s`\n",
                 lexer_filename(label->loc.filenr), label->loc.linenr,
                 label->loc.offset, label->lexinfo);
}

int semerr_redefine_tag(const ASTree *tag_spec, const ASTree *tag_id,
                        const Tag *existing) {
  semantic_error = 1;
  const char *tag_kind_str;
  switch (tag_spec->tok_kind) {
    case TOK_STRUCT:
      tag_kind_str = "struct";
      break;
    case TOK_UNION:
      tag_kind_str = "union";
      break;
    case TOK_ENUM:
      tag_kind_str = "enum";
      break;
    default:
      abort();
  }

  /* TODO(Robert): remove `size` parameter from `tag_print` */
  (void)tag_print(existing, tag_buffer, BUFFER_SIZE);

  return fprintf(
      stderr,
      "%s:%lu.%lu: redefinition of tag `%s` as %s; previous definition: {%s}\n",
      lexer_filename(tag_spec->loc.filenr), tag_spec->loc.linenr,
      tag_spec->loc.offset, tag_id->lexinfo, tag_kind_str, tag_buffer);
}

int semerr_enum_not_found(const ASTree *enum_spec, const ASTree *enum_id) {
  semantic_error = 1;
  return fprintf(stderr, "%s:%lu.%lu: enum type `%s` not found\n",
                 lexer_filename(enum_spec->loc.filenr), enum_spec->loc.linenr,
                 enum_spec->loc.offset, enum_id->lexinfo);
}

int semerr_expected_const(const ASTree *where, const ASTree *expr) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(where, ast_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(astree_to_string(expr, ast_buffer2), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: node {%s} expected constant expression at node {%s}\n",
      lexer_filename(expr->loc.filenr), expr->loc.linenr, expr->loc.offset,
      ast_buffer1, ast_buffer2);
}

int semerr_incompatible_types(const ASTree *where, const Type *dest,
                              const Type *src) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(dest, type_buffer1), BUFFER_SIZE);
  CHECK_TO_STR(type_to_str(src, type_buffer2), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: type `%s` incompatible with type `%s`\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, type_buffer2, type_buffer1);
}

int semerr_expected_retval(const ASTree *ret, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: expected return value of type `%s`\n",
                 lexer_filename(ret->loc.filenr), ret->loc.linenr,
                 ret->loc.offset, type_buffer1);
}

int semerr_expected_scalar(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr, "%s:%lu.%lu: expected scalar type but instead found type `%s`\n",
      lexer_filename(where->loc.filenr), where->loc.linenr, where->loc.offset,
      type_buffer1);
}

int semerr_expected_integral(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: expected integral type but instead found type `%s`\n",
      lexer_filename(where->loc.filenr), where->loc.linenr, where->loc.offset,
      type_buffer1);
}

int semerr_redefine_label(const ASTree *identifier, const ASTree *old_label) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(old_label, ast_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: redefinition of label `%s`; previous definition: {%s}\n",
      lexer_filename(identifier->loc.filenr), identifier->loc.linenr,
      identifier->loc.offset, identifier->lexinfo, ast_buffer1);
}

int semerr_unexpected_stmt(const ASTree *stmt) {
  semantic_error = 1;
  return fprintf(stderr, "%s:%lu.%lu: unexpected %s statement\n",
                 lexer_filename(stmt->loc.filenr), stmt->loc.linenr,
                 stmt->loc.offset, stmt->lexinfo);
}

int semerr_expected_intconst(const ASTree *where, const ASTree *expr) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(expr, ast_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: expected expression {%s} to be an integral constant\n",
      lexer_filename(where->loc.filenr), where->loc.linenr, where->loc.offset,
      ast_buffer1);
}

int semerr_insufficient_args(const ASTree *call) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(astree_get(call, 0)->type, type_buffer1),
               BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: insufficient arguments for call to function with "
                 "type `%s`\n",
                 lexer_filename(call->loc.filenr), call->loc.linenr,
                 call->loc.offset, type_buffer1);
}

int semerr_excess_args(const ASTree *call, const ASTree *arg) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(astree_get(call, 0)->type, type_buffer1),
               BUFFER_SIZE);
  CHECK_TO_STR(astree_to_string(arg, ast_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: excess argument {%s} for call to function with type `%s`\n",
      lexer_filename(arg->loc.filenr), arg->loc.linenr, arg->loc.offset,
      ast_buffer1, type_buffer1);
}

int semerr_expected_fn_ptr(const ASTree *call, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: expected function designator but found "
                 "expression with type `%s`\n",
                 lexer_filename(call->loc.filenr), call->loc.linenr,
                 call->loc.offset, type_buffer1);
}

int semerr_expected_arithmetic(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: expected arithmetic type but found type `%s`\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, type_buffer1);
}

int semerr_expected_lvalue(const ASTree *where, const ASTree *offender) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(offender, ast_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: expression {%s} must be an lvalue\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, ast_buffer1);
}

int semerr_expected_pointer(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr, "%s:%lu.%lu: expected pointer type but instead found type `%s`\n",
      lexer_filename(where->loc.filenr), where->loc.linenr, where->loc.offset,
      type_buffer1);
}

int semerr_sizeof_fn(const ASTree *sizeof_, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: attempted to take size of function type `%s`\n",
                 lexer_filename(sizeof_->loc.filenr), sizeof_->loc.linenr,
                 sizeof_->loc.offset, type_buffer1);
}

int semerr_sizeof_incomplete(const ASTree *sizeof_, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: attempted to take size of incomplete type `%s`\n",
                 lexer_filename(sizeof_->loc.filenr), sizeof_->loc.linenr,
                 sizeof_->loc.offset, type_buffer1);
}

int semerr_expected_record_ptr(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr,
                 "%s:%lu.%lu: expected pointer to struct or union type but "
                 "instead found type `%s`\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, type_buffer1);
}

int semerr_expected_record(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(
      stderr,
      "%s:%lu.%lu: expected struct or union type but instead found type `%s`\n",
      lexer_filename(where->loc.filenr), where->loc.linenr, where->loc.offset,
      type_buffer1);
}

int semerr_not_assignable(const ASTree *where, const Type *type) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(type_to_str(type, type_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: type `%s` is not assignable\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, type_buffer1);
}
