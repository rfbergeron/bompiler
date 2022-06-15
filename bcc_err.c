#include "bcc_err.h"

#include "astree.h"
#include "attributes.h"
#include "lyutils.h"
#include "stddef.h"
#include "stdlib.h"
#include "symtable.h"

#define LINESIZE 1024

ASTree *create_type_error(ASTree *tree, int errcode) {
  AuxSpec *erraux = calloc(1, sizeof(*erraux));
  erraux->aux = AUX_ERROR;
  erraux->data.err.code = errcode;
  if (tree->symbol == TOK_TYPE_ERROR) {
    int status = llist_push_back((LinkedList *)&tree->type->auxspecs, erraux);
    if (status) abort();
    return tree;
  } else {
    ASTree *errnode = astree_init(TOK_TYPE_ERROR, tree->loc, "_terr");
    TypeSpec *errtype = calloc(1, sizeof(TypeSpec));
    errnode->type = errtype;
    int status = typespec_init(errtype);
    if (status) abort();
    errtype->base = TYPE_ERROR;
    status = llist_push_back(&errtype->auxspecs, erraux);
    if (status) abort();
    return astree_adopt(errnode, 1, tree);
  }
}

ASTree *create_terr(ASTree *tree, int errcode, size_t info_count, ...) {
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
  if (tree->symbol == TOK_TYPE_ERROR) {
    int status = llist_push_back((LinkedList *)&tree->type->auxspecs, erraux);
    if (status) abort();
    return tree;
  } else {
    ASTree *errnode = astree_init(TOK_TYPE_ERROR, tree->loc, "_terr");
    TypeSpec *errtype = calloc(1, sizeof(TypeSpec));
    errnode->type = errtype;
    int status = typespec_init(errtype);
    if (status) abort();
    errtype->base = TYPE_ERROR;
    status = llist_push_back(&errtype->auxspecs, erraux);
    if (status) abort();
    return astree_adopt(errnode, 1, tree);
  }
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
    return snprintf(
        buf, size,
        "Semantic error: node %s expected %s types, but found %s and %s",
        parent_buf, expected_str, child1_buf, child2_buf);
  } else {
    return snprintf(buf, size,
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
  return snprintf(buf, size,
                  "Semantic error: identifier %s previously "
                  "declared as a tag type incompatible with node %s",
                  tag_name_node->lexinfo, tag_node_buf);
}

int erraux_to_string(AuxSpec *erraux, char *buf, size_t size) {
  switch (erraux->data.err.code) {
    case BCC_TERR_FAILURE:
    case BCC_TERR_SUCCESS:
    case BCC_TERR_LIBRARY_FAILURE:
    case BCC_TERR_INCOMPLETE_TYPE:
    case BCC_TERR_INCOMPATIBLE_TYPES:
    case BCC_TERR_INCOMPATIBLE_SPEC:
      return 0;
    case BCC_TERR_EXPECTED_TAG: {
      ASTree *operator= erraux->data.err.info[0];
      char operator_buf[LINESIZE];
      int chars_written = astree_to_string(operator, operator_buf, LINESIZE);
      TypeSpec *spec = erraux->data.err.info[1];
      char spec_buf[LINESIZE];
      chars_written = type_to_string(spec, spec_buf, LINESIZE);
      return snprintf(buf, size,
                      "Semantic error: operator %s expected an "
                      "operand with tag type but found %s",
                      operator_buf, spec_buf);
    }
    case BCC_TERR_EXPECTED_STRUCT:
    case BCC_TERR_EXPECTED_UNION:
    case BCC_TERR_EXPECTED_ENUM:
      return tag_err_to_string(erraux, buf, size);
    case BCC_TERR_EXPECTED_FUNCTION:
      if (erraux->data.err.info_count == 2) {
        return expected_err_to_string(erraux, "function pointer", buf, size);
      } else {
        ASTree *declarator = erraux->data.err.info[0];
        char declarator_buf[LINESIZE];
        int chars_written =
            astree_to_string(declarator, declarator_buf, LINESIZE);
        return snprintf(
            buf, size,
            "Semantic error: function definition expected declarator of "
            "function type, but found %s",
            declarator_buf);
      }
    case BCC_TERR_EXPECTED_TYPEID: {
      ASTree *spec_list = erraux->data.err.info[0];
      ASTree *ident = erraux->data.err.info[1];
      char spec_list_buf[LINESIZE];
      int chars_written = astree_to_string(spec_list, spec_list_buf, LINESIZE);
      return snprintf(
          buf, size,
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
      return 0;
    case BCC_TERR_UNEXPECTED_LIST:
    case BCC_TERR_UNEXPECTED_TOKEN:
    case BCC_TERR_UNEXPECTED_BODY:
    case BCC_TERR_UNEXPECTED_RETURN:
    case BCC_TERR_EXCESS_INITIALIZERS:
    case BCC_TERR_EXCESS_PARAMS:
    case BCC_TERR_INSUFF_PARAMS:
    case BCC_TERR_SYM_NOT_FOUND:
    case BCC_TERR_TYPEID_NOT_FOUND: {
      ASTree *spec_list = erraux->data.err.info[0];
      ASTree *ident = erraux->data.err.info[1];
      char spec_list_buf[LINESIZE];
      int chars_written = astree_to_string(spec_list, spec_list_buf, LINESIZE);
      return snprintf(
          buf, size,
          "Semantic error: identifier \"%s\" in specifier list %s cannot "
          "be resolved",
          ident->lexinfo, spec_list_buf);
    }
    case BCC_TERR_TAG_NOT_FOUND:
    case BCC_TERR_REDEFINITION:
    case BCC_TERR_CONST_TOO_SMALL:
    case BCC_TERR_CONST_TOO_LARGE:
      return 0;
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

ASTree *propogate_err(ASTree *parent, ASTree *child) {
  if (child->symbol != TOK_TYPE_ERROR) {
    (void)astree_adopt(UNWRAP(parent), 1, child);
    return parent;
  } else if (parent->symbol != TOK_TYPE_ERROR) {
    ASTree *real_child = astree_replace(child, 0, parent);
    (void)astree_adopt(parent, 1, real_child);
    return child;
  } else {
    TypeSpec *parent_errs = (TypeSpec *)parent->type;
    TypeSpec *child_errs = (TypeSpec *)child->type;
    int status = typespec_append_auxspecs(parent_errs, child_errs);
    /* TODO(Robert): be a man */
    if (status) abort();
    (void)astree_adopt(UNWRAP(parent), 1, astree_remove(child, 0));
    status = astree_destroy(child);
    if (status) abort();
    return parent;
  }
}

ASTree *propogate_err_v(ASTree *parent, size_t count, ...) {
  va_list children;
  va_start(children, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = va_arg(children, ASTree *);
    parent = propogate_err(parent, child);
  }
  va_end(children);
  return parent;
}

ASTree *propogate_err_a(ASTree *parent, size_t count, ASTree **children) {
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *child = children[i];
    parent = propogate_err(parent, child);
  }
  return parent;
}

void create_error_symbol(SymbolValue *symval, int errcode) {}