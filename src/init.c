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
static void init_agg_member(Type *member_type, ptrdiff_t disp,
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

static void init_scalar(const Type *type, ptrdiff_t disp, ASTree *initializer,
                        ListIter *where) {
  assert(initializer->tok_kind != TOK_TYPE_ERROR);
  assert(!type_is_char_array(type));
  if (initializer->tok_kind == TOK_INIT_LIST) {
    if (astree_count(initializer) > 1) {
      (void)semerr_excess_init(initializer, type);
      /* we need to get back to the parser before it is aware of the semantic
       * error, so we need to set some iterators to make the code generator
       * happy. this function emits a single nop and sets iterators.
       */
      (void)translate_empty_expr(initializer);
      /* TODO(Robert): subverting abstractions is bad */
      if (disp < 0) where->node = initializer->last_instr->node;
    } else {
      init_scalar(type, disp, astree_get(initializer, 0), where);
      (void)set_init_list_iterators(initializer);
    }
  } else if ((type_is_pointer(type) &&
              initializer->tok_kind == TOK_STRINGCON) ||
             types_assignable(type, initializer->type,
                              astree_is_const_zero(initializer))) {
    if (disp >= 0) {
      if ((initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
        (void)semerr_expected_init(initializer);
        (void)translate_empty_expr(initializer);
      } else {
        (void)translate_static_scalar_init(type, initializer, where);
      }
    } else {
      (void)translate_auto_scalar_init(type, disp, initializer, where);
    }
  } else {
    (void)semerr_compat_init(initializer, type);
    (void)translate_empty_expr(initializer);
    if (disp < 0) where->node = initializer->last_instr->node;
  }
}

static void init_literal(Type *arr_type, ptrdiff_t arr_disp, ASTree *literal,
                         ListIter *where) {
  assert((literal->attributes & ATTR_MASK_CONST) == ATTR_CONST_INIT);
  /* subtract 2 for quotes */
  size_t literal_length = strlen(literal->lexinfo) - 2;
  if (!type_is_deduced_array(arr_type) &&
      type_get_width(arr_type) < literal_length) {
    (void)semerr_excess_init(literal, arr_type);
    (void)translate_empty_expr(literal);
    if (arr_disp < 0) where->node = literal->last_instr->node;
    return;
  } else if (type_is_deduced_array(arr_type) &&
             type_get_width(arr_type) <= literal_length) {
    /* add 1 for nul terminator */
    arr_type->array.length = literal_length + 1;
  }

  if (arr_disp >= 0) {
    (void)translate_static_literal_init(arr_type, literal, where);
  } else {
    (void)translate_auto_literal_init(arr_type, arr_disp, literal, where);
  }
}

static void init_array(Type *arr_type, ptrdiff_t arr_disp, ASTree *init_list,
                       size_t *init_index, ListIter *where) {
  Type *elem_type = type_strip_declarator(arr_type);
  size_t elem_width = type_get_width(elem_type);
  size_t elem_index;
  size_t init_count =
      MIN(type_member_count(arr_type), astree_count(init_list) - *init_index);

  for (elem_index = 0; elem_index < init_count; ++elem_index) {
    ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
    init_agg_member(elem_type, elem_disp, init_list, init_index, where);
  }

  if (type_member_count(arr_type) <= init_count) {
    return;
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
}

static void init_union(const Type *union_type, ptrdiff_t union_disp,
                       ASTree *init_list, size_t *init_index, ListIter *where) {
  Symbol *member_symbol = type_member_index(union_type, 0);
  Type *member_type = member_symbol->type;
  init_agg_member(member_type, union_disp, init_list, init_index, where);
  size_t zero_count = type_get_width(union_type) - type_get_width(member_type);
  if (union_disp >= 0 && zero_count > 0) static_zero_pad(zero_count, where);
}

static void init_struct(const Type *struct_type, ptrdiff_t struct_disp,
                        ASTree *init_list, size_t *init_index,
                        ListIter *where) {
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
    init_agg_member(member_type, member_disp, init_list, init_index, where);
    bytes_initialized += member_padding + type_get_width(member_type);
  }

  assert(bytes_initialized <= type_get_width(struct_type));
  if (bytes_initialized == type_get_width(struct_type)) {
    return;
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
}

static void init_agg_member(Type *member_type, ptrdiff_t disp,
                            ASTree *init_list, size_t *init_index,
                            ListIter *where) {
  ASTree *initializer = astree_get(init_list, *init_index);
  if (initializer->tok_kind != TOK_INIT_LIST &&
      (initializer->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT) {
    (void)semerr_expected_init(initializer);
    (void)translate_empty_expr(initializer);
    if (disp < 0) where->node = initializer->last_instr->node;
  } else if (type_is_scalar(member_type)) {
    ++*init_index;
    init_scalar(member_type, disp, initializer, where);
  } else if (type_is_char_array(member_type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    ++*init_index;
    init_literal(member_type, disp, initializer, where);
  } else if (initializer->tok_kind == TOK_INIT_LIST) {
    ++*init_index;
    traverse_initializer(member_type, disp, initializer, where);
  } else if (type_is_array(member_type)) {
    init_array(member_type, disp, init_list, init_index, where);
  } else if (type_is_struct(member_type)) {
    init_struct(member_type, disp, init_list, init_index, where);
  } else if (type_is_union(member_type)) {
    init_union(member_type, disp, init_list, init_index, where);
  } else {
    (void)semerr_compat_init(initializer, member_type);
    (void)translate_empty_expr(initializer);
    if (disp < 0) where->node = initializer->last_instr->node;
  }
}

ASTree *traverse_initializer(Type *type, ptrdiff_t disp, ASTree *initializer,
                             ListIter *where) {
  /* NOTE: the value of `where` is only used when initializing objects with
   * static storage class, so in the case of an error, we don't need to worry
   * about its exact value; there is no need for it to be identical to the last
   * instruction for the initializer
   */
  if (type_is_scalar(type)) {
    init_scalar(type, disp, initializer, where);
    return initializer;
  } else if (type_is_char_array(type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    init_literal(type, disp, initializer, where);
    return initializer;
  } else if (type_is_array(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    init_array(type, disp, initializer, &init_index, where);
    if (!semantic_error && init_index < astree_count(initializer)) {
      (void)semerr_excess_init(initializer, type);
      return translate_empty_expr(initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else if (type_is_struct(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    init_struct(type, disp, initializer, &init_index, where);
    if (!semantic_error && init_index < astree_count(initializer)) {
      (void)semerr_excess_init(initializer, type);
      return translate_empty_expr(initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else if (type_is_union(type) && initializer->tok_kind == TOK_INIT_LIST) {
    size_t init_index = 0;
    init_union(type, disp, initializer, &init_index, where);
    if (!semantic_error && init_index < astree_count(initializer)) {
      (void)semerr_excess_init(initializer, type);
      return translate_empty_expr(initializer);
    } else {
      return set_init_list_iterators(initializer);
    }
  } else {
    (void)semerr_compat_init(initializer, type);
    return translate_empty_expr(initializer);
  }
}
