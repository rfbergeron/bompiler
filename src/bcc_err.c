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
#define CHECKED_PRINT(expr)             \
  do {                                  \
    chars_written = expr;               \
    if (chars_written < 0)              \
      return chars_written;             \
    else if (chars_written >= LINESIZE) \
      abort();                          \
  } while (0)

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
                                  const char *expected_str, char *buf,
                                  size_t size) {
  (void)size; /* unused because no snprintf until C99 */
  ASTree *expecting_node = error->info[0];
  char expecting_buf[LINESIZE];
  int chars_written;
  CHECKED_PRINT(astree_to_string(expecting_node, expecting_buf, LINESIZE));

  if (error->info_size > 1) {
    ASTree *child1 = error->info[1];
    char child1_buf[LINESIZE];
    CHECKED_PRINT(type_to_str(child1->type, child1_buf, LINESIZE));
    if (error->info_size > 2) {
      ASTree *child2 = error->info[2];
      char child2_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(child2->type, child2_buf, LINESIZE));
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
    char type_buf[LINESIZE];
    CHECKED_PRINT(type_to_str(expecting_node->type, type_buf, LINESIZE));
    return sprintf(buf,
                   "Semantic error at node %s: expected type %s, but found %s",
                   expecting_buf, expected_str, type_buf);
  }
}

static int tag_err_to_string(const CompileError *error, char *buf,
                             size_t size) {
  (void)size; /* unused because no snprintf until C99 */
  ASTree *tag_node = error->info[0];
  char tag_node_buf[LINESIZE];
  int chars_written;
  CHECKED_PRINT(astree_to_string(tag_node, tag_node_buf, LINESIZE));
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
      char affected_node_buf[LINESIZE];
      CHECKED_PRINT(
          astree_to_string(affected_node, affected_node_buf, LINESIZE));
      Type *type = error->info[1];
      char spec_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(type, spec_buf, size));
      return sprintf(buf,
                     "Semantic error: node %s expected complete "
                     "type, but found %s",
                     affected_node_buf, spec_buf);
    }
    case BCC_TERR_INCOMPLETE_SPEC: {
      ASTree *spec_list = error->info[0];
      char spec_list_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf, LINESIZE));
      return sprintf(buf,
                     "Semantic error: specifier list at %s has "
                     "incomplete type that cannot be completed",
                     spec_list_buf);
    }
    case BCC_TERR_INCOMPATIBLE_TYPES: {
      ASTree *affected_node = error->info[0];
      char affected_node_buf[LINESIZE];
      CHECKED_PRINT(
          astree_to_string(affected_node, affected_node_buf, LINESIZE));
      Type *type1 = error->info[1];
      char spec1_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(type1, spec1_buf, LINESIZE));
      Type *type2 = error->info[2];
      char spec2_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(type2, spec2_buf, LINESIZE));
      return sprintf(buf,
                     "Semantic error: at node %s, types %s and "
                     "%s are incompatible",
                     affected_node_buf, spec1_buf, spec2_buf);
    }
    case BCC_TERR_INCOMPATIBLE_SPEC: {
      ASTree *spec_list = error->info[0];
      char spec_list_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(spec_list->type, spec_list_buf, LINESIZE));
      ASTree *spec = error->info[1];
      return sprintf(buf,
                     "Semantic error: specifier %s incompatible "
                     "with specifier list %s",
                     spec->lexinfo, spec_list_buf);
    }
    case BCC_TERR_INCOMPATIBLE_DECL: {
      ASTree *declarator = error->info[0];
      char declarator_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf, LINESIZE));
      ASTree *dirdecl = error->info[1];
      char dirdecl_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(dirdecl, dirdecl_buf, LINESIZE));
      return sprintf(buf,
                     "Semantic error: direct declarator %s incompatible "
                     "with declarator %s",
                     dirdecl_buf, declarator_buf);
    }
    case BCC_TERR_EXPECTED_TAG: {
      ASTree *operator= error->info[0];
      char operator_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(operator, operator_buf, LINESIZE));
      Type *type = error->info[1];
      char spec_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(type, spec_buf, LINESIZE));
      return sprintf(buf,
                     "Semantic error: operator %s expected an "
                     "operand with tag type but found %s",
                     operator_buf, spec_buf);
    }
    case BCC_TERR_EXPECTED_TAG_PTR: {
      ASTree *operator= error->info[0];
      char operator_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(operator, operator_buf, LINESIZE));
      Type *type = error->info[1];
      char spec_buf[LINESIZE];
      CHECKED_PRINT(type_to_str(type, spec_buf, LINESIZE));
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
      char declarator_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf, LINESIZE));
      return sprintf(
          buf,
          "Semantic error: function definition expected declarator of "
          "function type, but found %s",
          declarator_buf);
    }
    case BCC_TERR_EXPECTED_FN_PTR:
      return expected_err_to_string(error, "function pointer", buf, size);
    case BCC_TERR_EXPECTED_TYPEID: {
      ASTree *spec_list = error->info[0];
      ASTree *ident = error->info[1];
      char spec_list_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf, LINESIZE));
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s does not "
          "refer to a type name",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_EXPECTED_DECLARATOR: {
      ASTree *declarator = error->info[0];
      char decl_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(declarator, decl_buf, LINESIZE));
      ASTree *abs_declarator = error->info[1];
      char abs_decl_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(abs_declarator, abs_decl_buf, LINESIZE));
      return sprintf(buf,
                     "Semantic error: function declaration %s contains "
                     "type name %s as parameter\n",
                     decl_buf, abs_decl_buf);
    }
    case BCC_TERR_EXPECTED_CONST:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXPECTED_INTEGER:
      return expected_err_to_string(error, "integer", buf, size);
    case BCC_TERR_EXPECTED_INTCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "integer constant", buf, size);
    case BCC_TERR_EXPECTED_ARITHMETIC:
      return expected_err_to_string(error, "arithmetic", buf, size);
    case BCC_TERR_EXPECTED_ARITHCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "arithmetic constant", buf, size);
    case BCC_TERR_EXPECTED_SCALAR:
      return expected_err_to_string(error, "scalar", buf, size);
    case BCC_TERR_EXPECTED_SCALCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(error, "scalar constant", buf, size);
    case BCC_TERR_EXPECTED_POINTER:
      return expected_err_to_string(error, "pointer", buf, size);
    case BCC_TERR_EXPECTED_RETURN:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXPECTED_RETVAL:
      /* TODO(Robert): not usable as implemented */
      return sprintf(buf, "Semantic error: expected return value expression");
    case BCC_TERR_EXPECTED_LVAL:
      return expected_err_to_string(error, "lvalue", buf, size);
    case BCC_TERR_EXPECTED_NONZERO:
      return expected_err_to_string(error, "nonzero integer", buf, size);
    case BCC_TERR_UNEXPECTED_LIST: {
      ASTree *dest = error->info[0];
      char dest_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(dest, dest_buf, size));
      ASTree *src = error->info[1];
      char src_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(src, src_buf, size));
      return sprintf(
          buf,
          "Semantic error: destination %s cannot be initialized with list %s",
          dest_buf, src_buf);
    }
    case BCC_TERR_UNEXPECTED_TOKEN: {
      ASTree *tree = error->info[0];
      char tree_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(tree, tree_buf, LINESIZE));
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
      char declarator_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(declarator, declarator_buf, LINESIZE));
      return sprintf(buf, "Semantic error: excess initializers for node %s",
                     declarator_buf);
    }
    case BCC_TERR_EXCESS_PARAMS: {
      ASTree *call = error->info[0];
      char call_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(call, call_buf, LINESIZE));
      return sprintf(buf, "Semantic error: excess parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_INSUFF_PARAMS: {
      ASTree *call = error->info[0];
      char call_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(call, call_buf, LINESIZE));
      return sprintf(buf, "Semantic error: insufficient parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_SYM_NOT_FOUND: {
      ASTree *ident = error->info[0];
      char ident_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(ident, ident_buf, LINESIZE));
      return sprintf(
          buf, "Semantic error: identifier referred to by node %s not found",
          ident_buf);
    }
    case BCC_TERR_TYPEID_NOT_FOUND: {
      ASTree *spec_list = error->info[0];
      ASTree *ident = error->info[1];
      char spec_list_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(spec_list, spec_list_buf, LINESIZE));
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s cannot "
          "be resolved",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_TAG_NOT_FOUND: {
      ASTree *tag_node = error->info[0];
      char tag_node_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(tag_node, tag_node_buf, LINESIZE));
      const char *name = astree_get(tag_node, 0)->lexinfo;
      return sprintf(buf, "Semantic error: tag %s not found at node %s", name,
                     tag_node_buf);
    }
    case BCC_TERR_LABEL_NOT_FOUND: {
      ASTree *ident_node = error->info[0];
      char ident_node_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(ident_node, ident_node_buf, LINESIZE));
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
      char ident_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(ident, ident_buf, LINESIZE));
      return sprintf(buf, "Semantic error: redefinition of symbol at node %s",
                     ident_buf);
    }
    case BCC_TERR_CONST_TOO_SMALL: {
      ASTree *constant = error->info[0];
      char constant_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(constant, constant_buf, LINESIZE));
      return sprintf(buf, "Semantic error: constant at node %s too small",
                     constant_buf);
    }
    case BCC_TERR_CONST_TOO_LARGE: {
      ASTree *constant = error->info[0];
      char constant_buf[LINESIZE];
      CHECKED_PRINT(astree_to_string(constant, constant_buf, LINESIZE));
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
