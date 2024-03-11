#include "bcc_err.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>

#include "astree.h"
#include "bcc_types.h"
#include "symtable.h"

#define LINESIZE 1024
/* this macro expects that `chars_written` was previously declared */
#define CHECKED_PRINT(expr)                      \
  do {                                           \
    chars_written = expr;                        \
    if (chars_written < 0) return chars_written; \
  } while (0)
#ifdef NDEBUG
#define CHECK_PRINTF(expr, max_len) ((void)(chars_written = (expr)))
#else
#define CHECK_TO_STR(expr, max_len)  \
  do {                               \
    chars_written = (expr);          \
    assert(chars_written >= 0);      \
    assert(chars_written < max_len); \
  } while (0)
#endif

CompileError *compile_error_init(ErrorCode code, size_t info_size, ...) {
  CompileError *error = malloc(sizeof(CompileError));
  error->code = code;
  error->info_size = info_size;
  error->info = malloc(info_size * sizeof(void *));

  va_list info_ptrs;
  va_start(info_ptrs, info_size);
  size_t i;
  for (i = 0; i < info_size; ++i) {
    void *info_ptr = va_arg(info_ptrs, void *);
    error->info[i] = info_ptr;
  }
  va_end(info_ptrs);

  return error;
}

CompileError *compile_error_init_v(ErrorCode code, size_t info_size,
                                   va_list info_ptrs) {
  CompileError *error = malloc(sizeof(CompileError));
  error->code = code;
  error->info_size = info_size;
  error->info = malloc(info_size * sizeof(void *));

  size_t i;
  for (i = 0; i < info_size; ++i) error->info[i] = va_arg(info_ptrs, void *);

  return error;
}

void compile_error_destroy(CompileError *error) {
  if (error == NULL) return;
  free(error->info);
  free(error);
}

static int expected_err_to_string(const CompileError *error,
                                  const char *expected_str, char *buf) {
  static char expecting_buf[LINESIZE];
  ASTree *expecting_node = error->info[0];
  int chars_written;
  CHECKED_PRINT(astree_to_string(expecting_node, expecting_buf));

  static char buf1[LINESIZE], buf2[LINESIZE];
  if (error->info_size > 1) {
    ASTree *child1 = error->info[1];
    char *child1_buf = buf1;
    CHECKED_PRINT(type_to_str(child1->type, child1_buf));
    if (error->info_size > 2) {
      ASTree *child2 = error->info[2];
      char *child2_buf = buf2;
      CHECKED_PRINT(type_to_str(child2->type, child2_buf));
      /* TODO(Robert): check size without snprintf */
      return sprintf(
          buf, "Semantic error: node %s expected %s types, but found %s and %s",
          expecting_buf, expected_str, child1_buf, child2_buf);
    } else {
      return sprintf(buf,
                     "Semantic error: node %s expected %s type, but found %s",
                     expecting_buf, expected_str, child1_buf);
    }
  } else {
    char *type_buf = buf1;
    CHECKED_PRINT(type_to_str(expecting_node->type, type_buf));
    return sprintf(buf,
                   "Semantic error at node %s: expected type %s, but found %s",
                   expecting_buf, expected_str, type_buf);
  }
}

static int tag_err_to_string(const CompileError *error, char *buf,
                             size_t size) {
  (void)size; /* unused because no snprintf until C99 */
  ASTree *tag_node = error->info[0];
  static char tag_node_buf[LINESIZE];
  int chars_written;
  CHECKED_PRINT(astree_to_string(tag_node, tag_node_buf));
  ASTree *tag_name_node = astree_get(tag_node, 0);
  /* TagValue *tagval = error->info[1]; */
  /* TODO(Robert: to_string procedure for tagvalues */
  /* TODO(Robert): check size without snprintf */
  return sprintf(buf,
                 "Semantic error: identifier %s previously "
                 "declared as a tag type incompatible with node %s",
                 tag_name_node->lexinfo, tag_node_buf);
}

static int error_to_string(const CompileError *error, char *buf, size_t size) {
  int chars_written;
  static char buf1[LINESIZE], buf2[LINESIZE], buf3[LINESIZE];
  switch (error->code) {
    default:
      abort();
    case BCC_TERR_FAILURE:
      return sprintf(buf, "Error: unspecified error ");
    case BCC_TERR_SUCCESS:
      return sprintf(buf,
                     "Error: error occurred, but error code indicates success");
    case BCC_TERR_LIBRARY_FAILURE:
      return sprintf(buf,
                     "Library failure: unspecified data structure failure");
    case BCC_TERR_INCOMPLETE_TYPE: {
      ASTree *affected_node = error->info[0];
      char *affected_node_buf = buf1;
      CHECKED_PRINT(astree_to_string(affected_node, affected_node_buf));
      Type *type = error->info[1];
      char *spec_buf = buf2;
      CHECKED_PRINT(type_to_str(type, spec_buf));
      return sprintf(buf,
                     "Semantic error: node %s expected complete "
                     "type, but found %s",
                     affected_node_buf, spec_buf);
    }
    case BCC_TERR_INCOMPLETE_SPEC: {
      ASTree *spec_list = error->info[0];
      char *spec_list_buf = buf1;
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf));
      return sprintf(buf,
                     "Semantic error: specifier list at %s has "
                     "incomplete type that cannot be completed",
                     spec_list_buf);
    }
    case BCC_TERR_INCOMPATIBLE_TYPES: {
      ASTree *affected_node = error->info[0];
      char *affected_node_buf = buf1;
      CHECKED_PRINT(astree_to_string(affected_node, affected_node_buf));
      Type *type1 = error->info[1];
      char *spec1_buf = buf2;
      CHECKED_PRINT(type_to_str(type1, spec1_buf));
      Type *type2 = error->info[2];
      char *spec2_buf = buf3;
      CHECKED_PRINT(type_to_str(type2, spec2_buf));
      return sprintf(buf,
                     "Semantic error: at node %s, types %s and "
                     "%s are incompatible",
                     affected_node_buf, spec1_buf, spec2_buf);
    }
    case BCC_TERR_INCOMPATIBLE_SPEC: {
      ASTree *spec_list = error->info[0];
      char *spec_list_buf = buf1;
      CHECKED_PRINT(type_to_str(spec_list->type, spec_list_buf));
      ASTree *spec = error->info[1];
      return sprintf(buf,
                     "Semantic error: specifier %s incompatible "
                     "with specifier list %s",
                     spec->lexinfo, spec_list_buf);
    }
    case BCC_TERR_INCOMPATIBLE_DECL: {
      ASTree *declarator = error->info[0];
      char *declarator_buf = buf1;
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf));
      ASTree *dirdecl = error->info[1];
      char *dirdecl_buf = buf2;
      CHECKED_PRINT(astree_to_string(dirdecl, dirdecl_buf));
      return sprintf(buf,
                     "Semantic error: direct declarator %s incompatible "
                     "with declarator %s",
                     dirdecl_buf, declarator_buf);
    }
    case BCC_TERR_EXPECTED_TAG: {
      ASTree *operator= error->info[0];
      char *operator_buf = buf1;
      CHECKED_PRINT(astree_to_string(operator, operator_buf));
      Type *type = error->info[1];
      char *spec_buf = buf2;
      CHECKED_PRINT(type_to_str(type, spec_buf));
      return sprintf(buf,
                     "Semantic error: operator %s expected an "
                     "operand with tag type but found %s",
                     operator_buf, spec_buf);
    }
    case BCC_TERR_EXPECTED_TAG_PTR: {
      ASTree *operator= error->info[0];
      char *operator_buf = buf1;
      CHECKED_PRINT(astree_to_string(operator, operator_buf));
      Type *type = error->info[1];
      char *spec_buf = buf2;
      CHECKED_PRINT(type_to_str(type, spec_buf));
      return sprintf(
          buf,
          "Semantic error: operator %s expected an "
          "operand with type pointer to struct or union but found %s",
          operator_buf, spec_buf);
    }
    case BCC_TERR_EXPECTED_STRUCT:
      /* fallthrough */
    case BCC_TERR_EXPECTED_UNION:
      /* fallthrough */
    case BCC_TERR_EXPECTED_ENUM:
      return tag_err_to_string(error, buf, size);
    case BCC_TERR_EXPECTED_FUNCTION: {
      ASTree *declarator = error->info[0];
      char *declarator_buf = buf1;
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf));
      return sprintf(
          buf,
          "Semantic error: function definition expected declarator of "
          "function type, but found %s",
          declarator_buf);
    }
    case BCC_TERR_EXPECTED_FN_PTR:
      return expected_err_to_string(error, "function pointer", buf);
    case BCC_TERR_EXPECTED_TYPEID: {
      ASTree *spec_list = error->info[0];
      ASTree *ident = error->info[1];
      char *spec_list_buf = buf1;
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf));
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s does not "
          "refer to a type name",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_EXPECTED_DECLARATOR: {
      ASTree *declarator = error->info[0];
      char *decl_buf = buf1;
      CHECKED_PRINT(astree_to_string(declarator, decl_buf));
      ASTree *abs_declarator = error->info[1];
      char *abs_decl_buf = buf2;
      CHECKED_PRINT(astree_to_string(abs_declarator, abs_decl_buf));
      return sprintf(buf,
                     "Semantic error: function declaration %s contains "
                     "type name %s as parameter\n",
                     decl_buf, abs_decl_buf);
    }
    case BCC_TERR_EXPECTED_CONST:
      return expected_err_to_string(error, "initializer constant", buf);
    case BCC_TERR_EXPECTED_INTEGER:
      return expected_err_to_string(error, "integer", buf);
    case BCC_TERR_EXPECTED_INTCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "integer constant", buf);
    case BCC_TERR_EXPECTED_ARITHMETIC:
      return expected_err_to_string(error, "arithmetic", buf);
    case BCC_TERR_EXPECTED_ARITHCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "arithmetic constant", buf);
    case BCC_TERR_EXPECTED_SCALAR:
      return expected_err_to_string(error, "scalar", buf);
    case BCC_TERR_EXPECTED_SCALCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "scalar constant", buf);
    case BCC_TERR_EXPECTED_POINTER:
      return expected_err_to_string(error, "pointer", buf);
    case BCC_TERR_EXPECTED_RETURN:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXPECTED_RETVAL:
      /* TODO(Robert): not usable as implemented */
      return sprintf(buf, "Semantic error: expected return value expression");
    case BCC_TERR_EXPECTED_LVAL:
      return expected_err_to_string(error, "lvalue", buf);
    case BCC_TERR_EXPECTED_NONZERO:
      return expected_err_to_string(error, "nonzero integer", buf);
    case BCC_TERR_UNEXPECTED_LIST: {
      ASTree *dest = error->info[0];
      char *dest_buf = buf1;
      CHECKED_PRINT(astree_to_string(dest, dest_buf));
      ASTree *src = error->info[1];
      char *src_buf = buf2;
      CHECKED_PRINT(astree_to_string(src, src_buf));
      return sprintf(
          buf,
          "Semantic error: destination %s cannot be initialized with list %s",
          dest_buf, src_buf);
    }
    case BCC_TERR_UNEXPECTED_TOKEN: {
      ASTree *tree = error->info[0];
      char *tree_buf = buf1;
      CHECKED_PRINT(astree_to_string(tree, tree_buf));
      return sprintf(buf, "Semantic error: node %s has unexpected token",
                     tree_buf);
    }
    case BCC_TERR_UNEXPECTED_BODY:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_UNEXPECTED_RETURN:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXCESS_INITIALIZERS: {
      ASTree *declarator = error->info[0];
      char *declarator_buf = buf1;
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf));
      return sprintf(buf, "Semantic error: excess initializers for node %s",
                     declarator_buf);
    }
    case BCC_TERR_EXCESS_PARAMS: {
      ASTree *call = error->info[0];
      char *call_buf = buf1;
      CHECKED_PRINT(astree_to_string(call, call_buf));
      return sprintf(buf, "Semantic error: excess parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_INSUFF_PARAMS: {
      ASTree *call = error->info[0];
      char *call_buf = buf1;
      CHECKED_PRINT(astree_to_string(call, call_buf));
      return sprintf(buf, "Semantic error: insufficient parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_SYM_NOT_FOUND: {
      ASTree *ident = error->info[0];
      char *ident_buf = buf1;
      CHECKED_PRINT(astree_to_string(ident, ident_buf));
      return sprintf(
          buf, "Semantic error: identifier referred to by node %s not found",
          ident_buf);
    }
    case BCC_TERR_TYPEID_NOT_FOUND: {
      ASTree *spec_list = error->info[0];
      ASTree *ident = error->info[1];
      char *spec_list_buf = buf1;
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf));
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s cannot "
          "be resolved",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_TAG_NOT_FOUND: {
      ASTree *tag_node = error->info[0];
      char *tag_node_buf = buf1;
      CHECKED_PRINT(astree_to_string(tag_node, tag_node_buf));
      const char *name = astree_get(tag_node, 0)->lexinfo;
      return sprintf(buf, "Semantic error: tag %s not found at node %s", name,
                     tag_node_buf);
    }
    case BCC_TERR_LABEL_NOT_FOUND: {
      ASTree *ident_node = error->info[0];
      char *ident_node_buf = buf1;
      CHECKED_PRINT(astree_to_string(ident_node, ident_node_buf));
      const char *name = ident_node->lexinfo;
      return sprintf(buf,
                     "Semantic error: label %s not found; first referred "
                     "to here: %s",
                     name, ident_node_buf);
    }
    case BCC_TERR_REDEFINITION: {
      /* TODO(Robert): differentiate between redefinition of tags, symbols,
       * labels, etc. so we can print more detailed information */
      /* declarators and identifiers are the same so we can just do this */
      ASTree *ident = error->info[0];
      char *ident_buf = buf1;
      CHECKED_PRINT(astree_to_string(ident, ident_buf));
      return sprintf(buf, "Semantic error: redefinition of symbol at node %s",
                     ident_buf);
    }
    case BCC_TERR_CONST_TOO_SMALL: {
      ASTree *constant = error->info[0];
      char *constant_buf = buf1;
      CHECKED_PRINT(astree_to_string(constant, constant_buf));
      return sprintf(buf, "Semantic error: constant at node %s too small",
                     constant_buf);
    }
    case BCC_TERR_CONST_TOO_LARGE: {
      ASTree *constant = error->info[0];
      char *constant_buf = buf1;
      CHECKED_PRINT(astree_to_string(constant, constant_buf));
      return sprintf(buf, "Semantic error: constant at node %s too large",
                     constant_buf);
    }
  }
}

int print_errors(const Type *type, FILE *out) {
  assert(type->any.code == TYPE_CODE_ERROR);
  size_t i;
  for (i = 0; i < type->error.errors_size; ++i) {
    CompileError *error = type->error.errors[i];
    size_t buf_size = error->info_size * LINESIZE * sizeof(char);
    char *buf = malloc(buf_size);

    int chars_written = error_to_string(error, buf, buf_size);
    if (chars_written < 0)
      return free(buf), chars_written;
    else if ((size_t)chars_written >= buf_size)
      abort();

    chars_written = fprintf(out, "%s\n", buf);
    if (chars_written < 0) return free(buf), chars_written;
    free(buf);
  }
  return 0;
}

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

int semerr_incomplete_type(const ASTree *expr) {
  semantic_error = 1;
  int chars_written;
  CHECK_TO_STR(astree_to_string(expr, ast_buffer1), BUFFER_SIZE);
  return fprintf(stderr, "%s:%lu.%lu: expression {%s} has incomplete type\n",
                 lexer_filename(expr->loc.filenr), expr->loc.linenr,
                 expr->loc.offset, ast_buffer1);
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
  return fprintf(stderr,
                 "%s:%lu.%lu: expected {%s} to be an integral constant\n",
                 lexer_filename(where->loc.filenr), where->loc.linenr,
                 where->loc.offset, ast_buffer1);
}
