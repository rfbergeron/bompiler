#include "bcc_err.h"

#include "astree.h"
#include "attributes.h"
#include "lyutils.h"
#include "stddef.h"
#include "stdlib.h"
#include "symtable.h"

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
