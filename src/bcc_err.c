#include "bcc_err.h"

#include "astree.h"
#include "attributes.h"
#include "stdarg.h"
#include "stddef.h"
#include "stdlib.h"
#include "symtable.h"

#define LINESIZE 1024

AuxSpec *create_erraux(int errcode, size_t info_count, ...) {
  AuxSpec *erraux = calloc(1, sizeof(*erraux));
  erraux->aux = AUX_ERROR;
  erraux->data.err.code = errcode;
  erraux->data.err.info_count = info_count;
  erraux->data.err.info = malloc(info_count * sizeof(void *));
  va_list info_ptrs;
  va_start(info_ptrs, info_count);
  size_t i;
  for (i = 0; i < info_count; ++i) {
    void *info_ptr = va_arg(info_ptrs, void *);
    erraux->data.err.info[i] = info_ptr;
  }
  va_end(info_ptrs);
  return erraux;
}

AuxSpec *create_erraux_v(int errcode, size_t info_count, va_list info_ptrs) {
  AuxSpec *erraux = calloc(1, sizeof(*erraux));
  erraux->aux = AUX_ERROR;
  erraux->data.err.code = errcode;
  erraux->data.err.info_count = info_count;
  erraux->data.err.info = malloc(info_count * sizeof(void *));
  size_t i;
  for (i = 0; i < info_count; ++i) {
    void *info_ptr = va_arg(info_ptrs, void *);
    erraux->data.err.info[i] = info_ptr;
  }
  return erraux;
}

int expected_err_to_string(AuxSpec *erraux, const char *expected_str, char *buf,
                           size_t size) {
  ASTree *parent = erraux->data.err.info[0];
  char parent_buf[LINESIZE];
  int chars_written = astree_to_string(parent, parent_buf, LINESIZE);
  ASTree *child1 = erraux->data.err.info[1];
  char child1_buf[LINESIZE];
  chars_written = type_to_string(child1->type, child1_buf, LINESIZE);
  if (erraux->data.err.info_count == 3) {
    ASTree *child2 = erraux->data.err.info[2];
    char child2_buf[LINESIZE];
    chars_written = type_to_string(child2->type, child2_buf, LINESIZE);
    /* TODO(Robert): check size without snprintf */
    return sprintf(
        buf, "Semantic error: node %s expected %s types, but found %s and %s",
        parent_buf, expected_str, child1_buf, child2_buf);
  } else {
    return sprintf(buf,
                   "Semantic error: node %s expected %s type, but found %s",
                   parent_buf, expected_str, child1_buf);
  }
}

int tag_err_to_string(AuxSpec *erraux, char *buf, size_t size) {
  ASTree *tag_node = erraux->data.err.info[0];
  char tag_node_buf[LINESIZE];
  int chars_written = astree_to_string(tag_node, tag_node_buf, LINESIZE);
  ASTree *tag_name_node = astree_get(tag_node, 0);
  TagValue *tagval = erraux->data.err.info[1];
  /* TODO(Robert: to_string procedure for tagvalues */
  /* TODO(Robert): check size without snprintf */
  return sprintf(buf,
                 "Semantic error: identifier %s previously "
                 "declared as a tag type incompatible with node %s",
                 tag_name_node->lexinfo, tag_node_buf);
}

int erraux_to_string(AuxSpec *erraux, char *buf, size_t size) {
  /* TODO(Robert): check size without snprintf */
  switch (erraux->data.err.code) {
    case BCC_TERR_FAILURE:
      return sprintf(buf, "Error: unspecified error ");
    case BCC_TERR_SUCCESS:
      return sprintf(buf,
                     "Error: error occurred, but error code indicates success");
    case BCC_TERR_LIBRARY_FAILURE:
      return sprintf(buf,
                     "Library failure: unspecified data structure failure");
    case BCC_TERR_INCOMPLETE_TYPE: {
      ASTree *affected_node = erraux->data.err.info[0];
      char affected_node_buf[LINESIZE];
      int chars_written =
          astree_to_string(affected_node, affected_node_buf, LINESIZE);
      TypeSpec *spec = erraux->data.err.info[1];
      char spec_buf[LINESIZE];
      chars_written = type_to_string(spec, spec_buf, size);
      return sprintf(buf,
                     "Semantic error: node %s expected complete "
                     "type, but found %s",
                     affected_node_buf, spec_buf);
    }
    case BCC_TERR_INCOMPLETE_SPEC: {
      ASTree *spec_list = erraux->data.err.info[0];
      char spec_list_buf[LINESIZE];
      int chars_written = astree_to_string(spec_list, spec_list_buf, LINESIZE);
      return sprintf(buf,
                     "Semantic error: specifier list at %s has "
                     "incomplete type that cannot be completed",
                     spec_list_buf);
    }
    case BCC_TERR_INCOMPATIBLE_TYPES: {
      ASTree *affected_node = erraux->data.err.info[0];
      char affected_node_buf[LINESIZE];
      int chars_written =
          astree_to_string(affected_node, affected_node_buf, LINESIZE);
      TypeSpec *spec1 = erraux->data.err.info[1];
      char spec1_buf[LINESIZE];
      chars_written = type_to_string(spec1, spec1_buf, LINESIZE);
      TypeSpec *spec2 = erraux->data.err.info[2];
      char spec2_buf[LINESIZE];
      chars_written = type_to_string(spec2, spec2_buf, LINESIZE);
      return sprintf(buf,
                     "Semantic error: at node %s, types %s and "
                     "%s are incompatible",
                     affected_node_buf, spec1_buf, spec2_buf);
    }
    case BCC_TERR_INCOMPATIBLE_SPEC: {
      ASTree *spec_list = erraux->data.err.info[0];
      char spec_list_buf[LINESIZE];
      int chars_written =
          type_to_string(spec_list->type, spec_list_buf, LINESIZE);
      ASTree *spec = erraux->data.err.info[1];
      return sprintf(buf,
                     "Semantic error: specifier %s incompatible "
                     "with specifier list %s",
                     spec->lexinfo, spec_list_buf);
    }
    case BCC_TERR_EXPECTED_TAG: {
      ASTree *operator= erraux->data.err.info[0];
      char operator_buf[LINESIZE];
      int chars_written = astree_to_string(operator, operator_buf, LINESIZE);
      TypeSpec *spec = erraux->data.err.info[1];
      char spec_buf[LINESIZE];
      chars_written = type_to_string(spec, spec_buf, LINESIZE);
      return sprintf(buf,
                     "Semantic error: operator %s expected an "
                     "operand with tag type but found %s",
                     operator_buf, spec_buf);
    }
    case BCC_TERR_EXPECTED_TAG_PTR: {
      ASTree *operator= erraux->data.err.info[0];
      char operator_buf[LINESIZE];
      int chars_written = astree_to_string(operator, operator_buf, LINESIZE);
      TypeSpec *spec = erraux->data.err.info[1];
      char spec_buf[LINESIZE];
      chars_written = type_to_string(spec, spec_buf, LINESIZE);
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
      return tag_err_to_string(erraux, buf, size);
    case BCC_TERR_EXPECTED_FUNCTION: {
      ASTree *declarator = erraux->data.err.info[0];
      char declarator_buf[LINESIZE];
      int chars_written =
          astree_to_string(declarator, declarator_buf, LINESIZE);
      return sprintf(
          buf,
          "Semantic error: function definition expected declarator of "
          "function type, but found %s",
          declarator_buf);
    }
    case BCC_TERR_EXPECTED_FN_PTR:
      return expected_err_to_string(erraux, "function pointer", buf, size);
    case BCC_TERR_EXPECTED_TYPEID: {
      ASTree *spec_list = erraux->data.err.info[0];
      ASTree *ident = erraux->data.err.info[1];
      char spec_list_buf[LINESIZE];
      int chars_written = astree_to_string(spec_list, spec_list_buf, LINESIZE);
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s does not "
          "refer to a type name",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_EXPECTED_CONST:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXPECTED_INTEGER:
      return expected_err_to_string(erraux, "integer", buf, size);
    case BCC_TERR_EXPECTED_INTCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(erraux, "integer constant", buf, size);
    case BCC_TERR_EXPECTED_ARITHMETIC:
      return expected_err_to_string(erraux, "arithmetic", buf, size);
    case BCC_TERR_EXPECTED_ARITHCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(erraux, "arithmetic constant", buf, size);
    case BCC_TERR_EXPECTED_SCALAR:
      return expected_err_to_string(erraux, "scalar", buf, size);
    case BCC_TERR_EXPECTED_SCALCONST:
      /* TODO(Robert): create two separate errors */
      return expected_err_to_string(erraux, "scalar constant", buf, size);
    case BCC_TERR_EXPECTED_POINTER:
      return expected_err_to_string(erraux, "pointer", buf, size);
    case BCC_TERR_EXPECTED_RETURN:
      /* TODO(Robert): unused */
      return 0;
    case BCC_TERR_EXPECTED_RETVAL:
      /* TODO(Robert): not usable as implemented */
      return sprintf(buf, "Semantic error: expected return value expression");
    case BCC_TERR_UNEXPECTED_LIST: {
      ASTree *dest = erraux->data.err.info[0];
      char dest_buf[LINESIZE];
      int chars_written = astree_to_string(dest, dest_buf, size);
      ASTree *src = erraux->data.err.info[1];
      char src_buf[LINESIZE];
      chars_written = astree_to_string(src, src_buf, size);
      return sprintf(
          buf,
          "Semantic error: destination %s cannot be initialized with list %s",
          dest_buf, src_buf);
    }
    case BCC_TERR_UNEXPECTED_TOKEN: {
      ASTree *tree = erraux->data.err.info[0];
      char tree_buf[LINESIZE];
      int chars_written = astree_to_string(tree, tree_buf, LINESIZE);
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
      ASTree *declarator = erraux->data.err.info[0];
      char declarator_buf[LINESIZE];
      int chars_written =
          astree_to_string(declarator, declarator_buf, LINESIZE);
      return sprintf(buf, "Semantic error: excess initializers for node %s",
                     declarator_buf);
    }
    case BCC_TERR_EXCESS_PARAMS: {
      ASTree *call = erraux->data.err.info[0];
      char call_buf[LINESIZE];
      int chars_written = astree_to_string(call, call_buf, LINESIZE);
      return sprintf(buf, "Semantic error: excess parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_INSUFF_PARAMS: {
      ASTree *call = erraux->data.err.info[0];
      char call_buf[LINESIZE];
      int chars_written = astree_to_string(call, call_buf, LINESIZE);
      return sprintf(buf, "Semantic error: insufficient parameters in call %s",
                     call_buf);
    }
    case BCC_TERR_SYM_NOT_FOUND: {
      ASTree *ident = erraux->data.err.info[0];
      char ident_buf[LINESIZE];
      int chars_written = astree_to_string(ident, ident_buf, LINESIZE);
      return sprintf(
          buf, "Semantic error: identifier referred to by node %s not found",
          ident_buf);
    }
    case BCC_TERR_TYPEID_NOT_FOUND: {
      ASTree *spec_list = erraux->data.err.info[0];
      ASTree *ident = erraux->data.err.info[1];
      char spec_list_buf[LINESIZE];
      int chars_written = astree_to_string(spec_list, spec_list_buf, LINESIZE);
      return sprintf(
          buf,
          "Semantic error: identifier \"%s\" in specifier list %s cannot "
          "be resolved",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_TAG_NOT_FOUND: {
      ASTree *tag_node = erraux->data.err.info[0];
      char tag_node_buf[LINESIZE];
      int chars_written = astree_to_string(tag_node, tag_node_buf, LINESIZE);
      const char *name = astree_get(tag_node, 0)->lexinfo;
      return sprintf(buf, "Semantic error: tag %s not found at node %s", name,
                     tag_node_buf);
    }
    case BCC_TERR_REDEFINITION: {
      /* TODO(Robert): differentiate between redefinition of tags, symbols,
       * labels, etc. so we can print more detailed information */
      /* declarators and identifiers are the same so we can just do this */
      ASTree *ident = erraux->data.err.info[0];
      char ident_buf[LINESIZE];
      int chars_written = astree_to_string(ident, ident_buf, LINESIZE);
      return sprintf(buf, "Semantic error: redefinition of symbol at node %s",
                     ident_buf);
    }
    case BCC_TERR_CONST_TOO_SMALL: {
      ASTree *constant = erraux->data.err.info[0];
      char constant_buf[LINESIZE];
      int chars_written = astree_to_string(constant, constant_buf, LINESIZE);
      return sprintf(buf, "Semantic error: constant at node %s too small",
                     constant_buf);
    }
    case BCC_TERR_CONST_TOO_LARGE: {
      ASTree *constant = erraux->data.err.info[0];
      char constant_buf[LINESIZE];
      int chars_written = astree_to_string(constant, constant_buf, LINESIZE);
      return sprintf(buf, "Semantic error: constant at node %s too large",
                     constant_buf);
    }
  }
}

int errspec_to_string(TypeSpec *errspec, char *buf, size_t size) {
  int ret = 0;
  size_t i, erraux_count = llist_size(&errspec->auxspecs);
  for (i = 0; i < erraux_count; ++i) {
    AuxSpec *erraux = llist_get(&errspec->auxspecs, i);
    int chars_written = erraux_to_string(erraux, buf + ret, size - ret);
    if (chars_written < 0) return chars_written;
    ret += chars_written;
  }
  return ret;
}

int print_errs(TypeSpec *errspec, FILE *out) {
  size_t i, erraux_count = llist_size(&errspec->auxspecs);
  for (i = 0; i < erraux_count; ++i) {
    AuxSpec *erraux = llist_get(&errspec->auxspecs, i);
    size_t info_count = erraux->data.err.info_count;
    char *erraux_buf = malloc(info_count * LINESIZE * sizeof(char));
    int chars_written =
        erraux_to_string(erraux, erraux_buf, info_count * LINESIZE);
    if (chars_written < 0) {
      free(erraux_buf);
      return chars_written;
    }
    chars_written = fprintf(out, "%s\n", erraux_buf);
    if (chars_written < 0) {
      free(erraux_buf);
      return chars_written;
    }
  }
  return 0;
}