#include "init.h"

#include "asmgen.h"
#include "assert.h"
#include "tchk_common.h"
#include "tchk_decl.h"

static ASTree *init_agg_member(const TypeSpec *member_type, ptrdiff_t disp,
                               ASTree *init_list, size_t *init_index,
                               ListIter *where);

static ASTree *set_init_list_iterators(ASTree *init_list, ListIter *where) {
  if (init_list->symbol == TOK_TYPE_ERROR) return init_list;
  if (init_list->symbol == TOK_INIT_LIST) {
    init_list->first_instr = liter_copy(astree_get(init_list, 0)->first_instr);
    if (init_list->first_instr == NULL) abort();
    init_list->last_instr = liter_copy(where);
    if (init_list->last_instr == NULL) abort();
  }
  return init_list;
}

ASTree *init_scalar(const TypeSpec *type, ptrdiff_t disp, ASTree *initializer,
                    ListIter *where) {
  if (initializer->symbol == TOK_TYPE_ERROR) return initializer;
  pointer_conversions(initializer);
  if (initializer->symbol == TOK_INIT_LIST) {
    ASTree *real_initializer = astree_get(initializer, 0);
    ASTree *errnode = init_scalar(type, disp, real_initializer, where);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, initializer);
    } else {
      return set_init_list_iterators(initializer, where);
    }
  } else if (types_assignable(type, initializer)) {
    if (disp >= 0)
      return translate_static_scalar_init(type, initializer, where);
    else
      return translate_auto_scalar_init(type, disp, initializer, where);
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, type, initializer->type);
  }
}

ASTree *init_literal(const TypeSpec *arr_type, ptrdiff_t arr_disp,
                     ASTree *literal, ListIter *where) {
  if (typespec_is_deduced_arr(arr_type) && typespec_get_width(arr_type) == 0) {
    AuxSpec *arr_aux = llist_front(&arr_type->auxspecs);
    /* subtract 2 for quotes, add 1 for nul terminator */
    arr_aux->data.memory_loc.length = strlen(literal->lexinfo) - 2 + 1;
  }

  if (arr_disp >= 0) {
    return translate_static_literal_init(arr_type, literal, where);
  } else {
    return translate_auto_literal_init(arr_type, arr_disp, literal, where);
  }
}

ASTree *init_array(const TypeSpec *arr_type, ptrdiff_t arr_disp,
                   ASTree *init_list, size_t *init_index, ListIter *where) {
  TypeSpec elem_type;
  int status = strip_aux_type(&elem_type, arr_type);
  if (status) abort();
  size_t elem_width = typespec_get_width(&elem_type),
         elem_count = typespec_member_count(arr_type),
         init_count = astree_count(init_list), elem_index;
  for (elem_index = 0;
       (elem_index < elem_count || typespec_is_deduced_arr(arr_type)) &&
       *init_index < init_count;
       ++elem_index) {
    ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
    ASTree *errnode =
        init_agg_member(&elem_type, elem_disp, init_list, init_index, where);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      typespec_destroy(&elem_type);
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, init_list);
    }
  }

  size_t zero_count =
      (elem_count - elem_index) * typespec_get_width(&elem_type);
  if (typespec_is_deduced_arr(arr_type)) {
    AuxSpec *arr_aux = typespec_get_aux(arr_type, 0);
    arr_aux->data.memory_loc.length = elem_index;
  } else if (zero_count > 0) {
    int status;
    if (arr_disp >= 0) {
      status = static_zero_pad(zero_count, where);
    } else {
      status = liter_advance(where, 1);
      if (status) abort();
      ListIter *temp = liter_prev(where, 1);
      if (temp == NULL) abort();
      status =
          bulk_mzero(RBP_VREG, arr_disp,
                     typespec_get_width(arr_type) - zero_count, arr_type, temp);
      free(temp);
      if (!status) status = liter_advance(where, -1);
    }

    if (status) abort();
  }
  typespec_destroy(&elem_type);
  return init_list;
}

ASTree *init_union(const TypeSpec *union_type, ptrdiff_t union_disp,
                   ASTree *init_list, size_t *init_index, ListIter *where) {
  SymbolValue *member_symval = typespec_member_index(union_type, 0);
  TypeSpec *member_type = &member_symval->type;
  ASTree *errnode =
      init_agg_member(member_type, union_disp, init_list, init_index, where);
  if (errnode->symbol == TOK_TYPE_ERROR) {
    (void)astree_remove(errnode, 0);
    return astree_adopt(errnode, 1, init_list);
  }
  size_t zero_count =
      typespec_get_width(union_type) - typespec_get_width(member_type);
  if (union_disp >= 0 && zero_count > 0) {
    int status = static_zero_pad(zero_count, where);
    if (status) abort(); /* library failure */
  }
  return init_list;
}

ASTree *init_struct(const TypeSpec *struct_type, ptrdiff_t struct_disp,
                    ASTree *init_list, size_t *init_index, ListIter *where) {
  size_t member_count = typespec_member_count(struct_type);
  size_t init_count = astree_count(init_list);
  size_t bytes_initialized = 0;
  size_t member_index = 0;
  for (; member_index < member_count && *init_index < init_count;
       ++member_index) {
    SymbolValue *member_symval =
        typespec_member_index(struct_type, member_index);
    TypeSpec *member_type = &member_symval->type;
    size_t member_padding =
        typespec_get_padding(member_type, bytes_initialized);
    ptrdiff_t member_disp = struct_disp + member_symval->disp;
    if (member_disp >= 0 && member_padding > 0) {
      int status = static_zero_pad(member_padding, where);
      if (status) abort(); /* library failure */
    }
    ASTree *errnode =
        init_agg_member(member_type, member_disp, init_list, init_index, where);
    if (errnode->symbol == TOK_TYPE_ERROR) {
      (void)astree_remove(errnode, 0);
      return astree_adopt(errnode, 1, init_list);
    }
    bytes_initialized += member_padding + typespec_get_width(member_type);
  }
  size_t struct_width = typespec_get_width(struct_type);
  if (bytes_initialized < struct_width) {
    int status;
    if (struct_disp >= 0) {
      status = static_zero_pad(struct_width - bytes_initialized, where);
    } else {
      status = liter_advance(where, 1);
      if (status) abort();
      ListIter *temp = liter_prev(where, 1);
      if (temp == NULL) abort();
      status = bulk_mzero(RBP_VREG, struct_disp, bytes_initialized, struct_type,
                          temp);
      free(temp);
      if (!status) status = liter_advance(where, -1);
    }

    if (status) abort(); /* library failure */
  }
  return init_list;
}

static ASTree *init_agg_member(const TypeSpec *member_type, ptrdiff_t disp,
                               ASTree *init_list, size_t *init_index,
                               ListIter *where) {
  ASTree *initializer = astree_get(init_list, *init_index);
  ASTree *errnode;
  if (typespec_is_scalar(member_type)) {
    ++*init_index;
    errnode = init_scalar(member_type, disp, initializer, where);
  } else if (typespec_is_chararray(member_type) &&
             initializer->symbol == TOK_STRINGCON) {
    ++*init_index;
    errnode = init_literal(member_type, disp, initializer, where);
  } else if (initializer->symbol == TOK_INIT_LIST) {
    ++*init_index;
    errnode = traverse_initializer(member_type, disp, initializer, where);
  } else if (typespec_is_array(member_type)) {
    errnode = init_array(member_type, disp, init_list, init_index, where);
  } else if (typespec_is_struct(member_type)) {
    errnode = init_struct(member_type, disp, init_list, init_index, where);
  } else if (typespec_is_union(member_type)) {
    errnode = init_union(member_type, disp, init_list, init_index, where);
  } else {
    errnode =
        astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                              initializer, member_type, initializer->type);
  }

  if (errnode->symbol == TOK_TYPE_ERROR &&
      initializer->symbol != TOK_INIT_LIST &&
      typespec_is_aggregate(member_type)) {
    assert(astree_get(errnode, 0) == initializer);
    (void)astree_remove(errnode, 0);
    return astree_adopt(errnode, 1, init_list);
  } else {
    /* no need to move the error node (if present) when the braces for the
     * initializer were elided; it should be the root of the tree already
     */
    return init_list;
  }
}

ASTree *traverse_initializer(const TypeSpec *type, ptrdiff_t disp,
                             ASTree *initializer, ListIter *where) {
  if (typespec_is_scalar(type)) {
    return init_scalar(type, disp, initializer, where);
  } else if (typespec_is_chararray(type) &&
             initializer->symbol == TOK_STRINGCON) {
    return init_literal(type, disp, initializer, where);
  } else if (typespec_is_array(type) && initializer->symbol == TOK_INIT_LIST) {
    size_t dummy_index = 0;
    return set_init_list_iterators(
        init_array(type, disp, initializer, &dummy_index, where), where);
  } else if (typespec_is_struct(type) && initializer->symbol == TOK_INIT_LIST) {
    size_t dummy_index = 0;
    return set_init_list_iterators(
        init_struct(type, disp, initializer, &dummy_index, where), where);
  } else if (typespec_is_union(type) && initializer->symbol == TOK_INIT_LIST) {
    size_t dummy_index = 0;
    return set_init_list_iterators(
        init_union(type, disp, initializer, &dummy_index, where), where);
  } else {
    return astree_create_errnode(initializer, BCC_TERR_INCOMPATIBLE_TYPES, 3,
                                 initializer, type, initializer->type);
  }
}
