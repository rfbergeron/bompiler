#include "init.h"

#include "asmgen.h"
#include "assert.h"
#include "tchk_decl.h"

#define MIN(x, y) ((y) < (x) ? (y) : (x))

/* TODO(Robert): perform type checking for all initializers here. this should
 * be as simple as adding a call to `types_assignable` in `init_scalar`
 */
/* TODO(Robert): do not allow string literals or initializer lists to exceed
 * capacity for fixed size arrays, either here or in tchk_decl.c
 */
/* TODO(Robert): check for excess initializers in init lists */
/* TODO(Robert): We need a way to determine if braces have been elided from an
 * initializer. This is necessary for trying to determine if too many
 * initializers have been provided.
 */
static ASTree *init_agg_member(Type *member_type, ptrdiff_t disp,
                               ASTree *init_list, size_t *init_index,
                               ListIter *where);

static ASTree *set_init_list_iterators(ASTree *init_list) {
  assert(init_list->tok_kind == TOK_INIT_LIST);
  init_list->first_instr = liter_copy(astree_get(init_list, 0)->first_instr);
  if (init_list->first_instr == NULL) abort();
  init_list->last_instr = liter_copy(
      astree_get(init_list, astree_count(init_list) - 1)->last_instr);
  if (init_list->last_instr == NULL) abort();
  return init_list;
}

static ASTree *init_scalar(const Type *type, ptrdiff_t disp,
                           ASTree *initializer, ListIter *where) {
  if (initializer->tok_kind == TOK_TYPE_ERROR) {
    return initializer;
  } else if (initializer->tok_kind == TOK_INIT_LIST) {
    ASTree *real_initializer = astree_get(initializer, 0);
    ASTree *errnode = init_scalar(type, disp, real_initializer, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, initializer);
    } else if (astree_count(initializer) > 1) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else if (((type_is_char_array(type) || type_is_pointer(type)) &&
              initializer->tok_kind == TOK_STRINGCON) ||
             types_assignable(type, initializer->type,
                              astree_is_const_zero(initializer))) {
    if (disp >= 0) {
      if ((initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT)
        return astree_create_errnode(initializer, BCC_TERR_EXPECTED_CONST, 1,
                                     initializer);
      else
        return translate_static_scalar_init(type, initializer, where);
    } else {
      return translate_auto_scalar_init(type, disp, initializer, where);
    }
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, type, initializer->type);
  }
}

static ASTree *init_literal(Type *arr_type, ptrdiff_t arr_disp, ASTree *literal,
                            ListIter *where) {
  assert((literal->attributes & ATTR_MASK_CONST) == ATTR_CONST_INIT);
  /* subtract 2 for quotes */
  size_t literal_length = strlen(literal->lexinfo) - 2;
  if (!type_is_deduced_array(arr_type) &&
      type_get_width(arr_type) < literal_length)
    return astree_create_errnode(literal, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                 literal);
  else if (type_is_deduced_array(arr_type) &&
           type_get_width(arr_type) <= literal_length)
    /* add 1 for nul terminator */
    arr_type->array.length = literal_length + 1;

  if (arr_disp >= 0) {
    return translate_static_literal_init(arr_type, literal, where);
  } else {
    return translate_auto_literal_init(arr_type, arr_disp, literal, where);
  }
}

ASTree *init_array(Type *arr_type, ptrdiff_t arr_disp, ASTree *init_list,
                   size_t *init_index, ListIter *where) {
  Type *elem_type = type_strip_declarator(arr_type);
  size_t elem_width = type_get_width(elem_type);
  size_t elem_index;
  size_t init_count =
      MIN(type_member_count(arr_type), astree_count(init_list) - *init_index);

  for (elem_index = 0; elem_index < init_count; ++elem_index) {
    ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
    ASTree *errnode =
        init_agg_member(elem_type, elem_disp, init_list, init_index, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, init_list);
    }
  }

  if (type_member_count(arr_type) <= init_count) {
    return init_list;
  } else if (type_is_deduced_array(arr_type)) {
    arr_type->array.length = init_count;
  } else if (arr_disp >= 0) {
    size_t zero_count =
        (type_member_count(arr_type) - init_count) * type_get_width(elem_type);
    static_zero_pad(zero_count, where);
  } else {
    size_t zero_count =
        (type_member_count(arr_type) - init_count) * type_get_width(elem_type);
    int status = liter_advance(where, 1);
    if (status) abort();
    ListIter *temp = liter_prev(where, 1);
    if (temp == NULL) abort();
    bulk_mzero(RBP_VREG, arr_disp, type_get_width(arr_type) - zero_count,
               arr_type, temp);
    free(temp);
    status = liter_advance(where, -1);
    if (status) abort();
  }

  return init_list;
}

ASTree *init_union(const Type *union_type, ptrdiff_t union_disp,
                   ASTree *init_list, size_t *init_index, ListIter *where) {
  Symbol *member_symbol = type_member_index(union_type, 0);
  Type *member_type = member_symbol->type;
  ASTree *errnode =
      init_agg_member(member_type, union_disp, init_list, init_index, where);
  if (errnode->tok_kind == TOK_TYPE_ERROR) {
    (void)astree_remove(errnode, 0);
    return astree_adopt(errnode, 1, init_list);
  }
  size_t zero_count = type_get_width(union_type) - type_get_width(member_type);
  if (union_disp >= 0 && zero_count > 0) static_zero_pad(zero_count, where);
  return init_list;
}

ASTree *init_struct(const Type *struct_type, ptrdiff_t struct_disp,
                    ASTree *init_list, size_t *init_index, ListIter *where) {
  size_t init_count = MIN(type_member_count(struct_type),
                          astree_count(init_list) - *init_index);
  size_t bytes_initialized = 0;
  size_t member_index;

  for (member_index = 0; member_index < init_count; ++member_index) {
    Symbol *member_symbol = type_member_index(struct_type, member_index);
    Type *member_type = member_symbol->type;
    size_t member_padding = type_get_padding(member_type, bytes_initialized);
    ptrdiff_t member_disp = struct_disp + member_symbol->disp;
    if (member_disp >= 0 && member_padding > 0)
      static_zero_pad(member_padding, where);
    ASTree *errnode =
        init_agg_member(member_type, member_disp, init_list, init_index, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, init_list);
    }
    bytes_initialized += member_padding + type_get_width(member_type);
  }

  assert(bytes_initialized <= type_get_width(struct_type));
  if (bytes_initialized == type_get_width(struct_type)) {
    return init_list;
  } else if (struct_disp >= 0) {
    static_zero_pad(type_get_width(struct_type) - bytes_initialized, where);
  } else {
    int status = liter_advance(where, 1);
    if (status) abort();
    ListIter *temp = liter_prev(where, 1);
    if (temp == NULL) abort();
    bulk_mzero(RBP_VREG, struct_disp, bytes_initialized, struct_type, temp);
    free(temp);
    status = liter_advance(where, -1);
    if (status) abort();
  }

  return init_list;
}

static ASTree *init_agg_member(Type *member_type, ptrdiff_t disp,
                               ASTree *init_list, size_t *init_index,
                               ListIter *where) {
  ASTree *initializer = astree_get(init_list, *init_index);
  ASTree *errnode;
  if (initializer->tok_kind != TOK_INIT_LIST &&
      (initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    return astree_create_errnode(init_list, BCC_TERR_EXPECTED_CONST, 1,
                                 initializer);
  } else if (type_is_scalar(member_type)) {
    ++*init_index;
    errnode = init_scalar(member_type, disp, initializer, where);
  } else if (type_is_char_array(member_type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    ++*init_index;
    errnode = init_literal(member_type, disp, initializer, where);
  } else if (initializer->tok_kind == TOK_INIT_LIST) {
    ++*init_index;
    errnode = traverse_initializer(member_type, disp, initializer, where);
  } else if (type_is_array(member_type)) {
    errnode = init_array(member_type, disp, init_list, init_index, where);
  } else if (type_is_struct(member_type)) {
    errnode = init_struct(member_type, disp, init_list, init_index, where);
  } else if (type_is_union(member_type)) {
    errnode = init_union(member_type, disp, init_list, init_index, where);
  } else {
    return astree_create_errnode(init_list, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, member_type, initializer->type);
  }

  if (errnode->tok_kind == TOK_TYPE_ERROR &&
      (type_is_scalar(member_type) || initializer->tok_kind == TOK_INIT_LIST ||
       initializer->tok_kind == TOK_STRINGCON)) {
    /* make errnode parent of init list, not the initializer */
    assert(astree_get(errnode, 0) == initializer);
    (void)astree_remove(errnode, 0);
    return astree_adopt(errnode, 1, init_list);
  } else {
    return init_list;
  }
}

ASTree *traverse_initializer(Type *type, ptrdiff_t disp, ASTree *initializer,
                             ListIter *where) {
  if (type_is_scalar(type)) {
    return init_scalar(type, disp, initializer, where);
  } else if (type_is_char_array(type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    return init_literal(type, disp, initializer, where);
  } else if (type_is_array(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    ASTree *errnode = init_array(type, disp, initializer, &init_index, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      return errnode;
    } else if (init_index < astree_count(initializer)) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else if (type_is_struct(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    ASTree *errnode = init_struct(type, disp, initializer, &init_index, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      return errnode;
    } else if (init_index < astree_count(initializer)) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else if (type_is_union(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    ASTree *errnode = init_union(type, disp, initializer, &init_index, where);
    if (errnode->tok_kind == TOK_TYPE_ERROR) {
      return errnode;
    } else if (init_index < astree_count(initializer)) {
      return astree_create_errnode(initializer, BCC_TERR_EXCESS_INITIALIZERS, 1,
                                   initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, type, initializer->type);
  }
}
