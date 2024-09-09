#include "tchk_init.h"

#include "asmgen.h"
#include "assert.h"
#include "bcc_err.h"
#include "bcc_types.h"
#include "conversions.h"
#include "state.h"

#define MIN(x, y) ((y) < (x) ? (y) : (x))

/* TODO(Robert): perform type checking for all initializers here. this should
 * be as simple as adding a call to `types_assignable` in `init_scalar`
 */
/* TODO(Robert): do not allow string literals or initializer lists to exceed
 * capacity for fixed size arrays, either here or in tchk_decl.c
 */
static int init_agg_member(Type *member_type, ptrdiff_t disp, ASTree *init_list,
                           size_t *init_index);

static int init_scalar(const Type *type, ptrdiff_t disp, ASTree *initializer) {
  assert(!type_is_char_array(type));
  if (initializer->tok_kind == TOK_INIT_LIST) {
    if (astree_count(initializer) > 1) {
      (void)semerr_excess_init(initializer, type);
      return 1;
    } else {
      ASTree *real_initializer = astree_get(initializer, 0);
      int status = init_scalar(type, disp, real_initializer);
      (void)instr_append(initializer->instructions, 1,
                         real_initializer->instructions);
      return status;
    }
  } else if (!types_assignable(type, initializer->type,
                               astree_is_const_zero(initializer)) &&
             !type_is_pointer(type) && initializer->tok_kind != TOK_STRINGCON) {
    (void)semerr_compat_init(initializer, type);
    return 1;
  } else if (disp < 0) {
    translate_auto_scalar_init(type, disp, initializer);
    return 0;
  } else if (initializer->cexpr_kind < CEXPR_INIT) {
    (void)semerr_expected_init(initializer);
    return 1;
  } else {
    translate_static_scalar_init(type, initializer);
    return 0;
  }
}

static int init_literal(Type *arr_type, ptrdiff_t arr_disp, ASTree *literal) {
  assert(literal->cexpr_kind == CEXPR_INIT);
  assert(type_is_char_array(literal->type));
  /* size adjusted in tchk_expr */
  size_t literal_length = type_get_width(literal->type);

  if (type_is_deduced_array(arr_type)) {
    type_set_elem_count(arr_type, literal_length);
  } else if (literal_length - 1 > type_get_width(arr_type)) {
    (void)semerr_excess_init(literal, arr_type);
    return 1;
  }

  if (arr_disp >= 0) {
    translate_static_literal_init(arr_type, literal);
  } else {
    translate_auto_literal_init(arr_type, arr_disp, literal);
  }

  return 0;
}

static int init_array(Type *arr_type, ptrdiff_t arr_disp, ASTree *init_list,
                      size_t *init_index) {
  Type *elem_type = type_strip_declarator(arr_type);
  size_t elem_count = type_get_elem_count(arr_type);
  if (elem_count == 0) elem_count = SIZE_MAX;
  size_t elem_width = type_get_width(elem_type);
  size_t elem_index;
  size_t init_count = MIN(elem_count, astree_count(init_list) - *init_index);

  int member_err = 0;
  for (elem_index = 0; elem_index < init_count; ++elem_index) {
    ptrdiff_t elem_disp = arr_disp + (elem_index * elem_width);
    member_err |= init_agg_member(elem_type, elem_disp, init_list, init_index);
  }

  if (elem_count <= init_count) {
    return member_err;
  } else if (type_is_deduced_array(arr_type)) {
    type_set_elem_count(arr_type, init_count);
  } else if (arr_disp >= 0) {
    size_t zero_count = (elem_count - init_count) * type_get_width(elem_type);
    static_zero_pad(zero_count, init_list->instructions);
  } else {
    size_t zero_count = (elem_count - init_count) * type_get_width(elem_type);
    bulk_mzero(RBP_VREG, arr_disp, type_get_width(arr_type) - zero_count,
               arr_type, init_list->instructions);
  }

  return member_err;
}

static int init_union(const Type *union_type, ptrdiff_t union_disp,
                      ASTree *init_list, size_t *init_index) {
  Symbol *member_symbol = type_get_member_index(union_type, 0);
  Type *member_type = member_symbol->type;
  int member_err =
      init_agg_member(member_type, union_disp, init_list, init_index);
  size_t zero_count = type_get_width(union_type) - type_get_width(member_type);
  if (union_disp >= 0 && zero_count > 0)
    static_zero_pad(zero_count, init_list->instructions);
  return member_err;
}

static int init_struct(const Type *struct_type, ptrdiff_t struct_disp,
                       ASTree *init_list, size_t *init_index) {
  size_t init_count = MIN(type_get_member_count(struct_type),
                          astree_count(init_list) - *init_index);
  size_t bytes_initialized = 0;
  size_t member_index;
  int member_err = 0;

  for (member_index = 0; member_index < init_count; ++member_index) {
    Symbol *member_symbol = type_get_member_index(struct_type, member_index);
    Type *member_type = member_symbol->type;
    size_t member_padding = type_get_padding(member_type, bytes_initialized);
    ptrdiff_t member_disp = struct_disp + member_symbol->disp;
    if (member_disp >= 0 && member_padding > 0)
      static_zero_pad(member_padding, init_list->instructions);
    member_err |=
        init_agg_member(member_type, member_disp, init_list, init_index);
    bytes_initialized += member_padding + type_get_width(member_type);
  }

  assert(bytes_initialized <= type_get_width(struct_type));
  if (bytes_initialized == type_get_width(struct_type)) {
    return member_err;
  } else if (struct_disp >= 0) {
    size_t zero_count = type_get_width(struct_type) - bytes_initialized;
    static_zero_pad(zero_count, init_list->instructions);
  } else {
    bulk_mzero(RBP_VREG, struct_disp, bytes_initialized, struct_type,
               init_list->instructions);
  }

  return member_err;
}

static int traverse_initializer(Type *type, ptrdiff_t disp,
                                ASTree *initializer) {
  if (type_is_scalar(type)) {
    return init_scalar(type, disp, initializer);
  } else if (type_is_char_array(type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    return init_literal(type, disp, initializer);
  } else if (initializer->tok_kind != TOK_INIT_LIST) {
    (void)semerr_compat_init(initializer, type);
    return 1;
  }

  size_t init_index = 0;
  int status;
  if (type_is_array(type)) {
    status = init_array(type, disp, initializer, &init_index);
  } else if (type_is_struct(type)) {
    status = init_struct(type, disp, initializer, &init_index);
  } else if (type_is_union(type)) {
    status = init_union(type, disp, initializer, &init_index);
  } else {
    abort();
  }

  if (status || init_index >= astree_count(initializer)) {
    return status;
  } else {
    (void)semerr_excess_init(initializer, type);
    return 1;
  }
}

static int init_agg_member(Type *member_type, ptrdiff_t disp, ASTree *init_list,
                           size_t *init_index) {
  ASTree *initializer = astree_get(init_list, *init_index);
  if (initializer->tok_kind != TOK_INIT_LIST &&
      initializer->cexpr_kind < CEXPR_INIT) {
    (void)semerr_expected_init(initializer);
    (void)instr_append(init_list->instructions, 1, initializer->instructions);
    ++*init_index;
    return 1;
  } else if (type_is_scalar(member_type)) {
    ++*init_index;
    int status = init_scalar(member_type, disp, initializer);
    (void)instr_append(init_list->instructions, 1, initializer->instructions);
    return status;
  } else if (type_is_char_array(member_type) &&
             initializer->tok_kind == TOK_STRINGCON) {
    ++*init_index;
    int status = init_literal(member_type, disp, initializer);
    (void)instr_append(init_list->instructions, 1, initializer->instructions);
    return status;
  } else if (initializer->tok_kind == TOK_INIT_LIST) {
    ++*init_index;
    int status = traverse_initializer(member_type, disp, initializer);
    (void)instr_append(init_list->instructions, 1, initializer->instructions);
    return status;
  } else if (type_is_array(member_type)) {
    return init_array(member_type, disp, init_list, init_index);
  } else if (type_is_struct(member_type)) {
    return init_struct(member_type, disp, init_list, init_index);
  } else if (type_is_union(member_type)) {
    return init_union(member_type, disp, init_list, init_index);
  } else {
    (void)semerr_compat_init(initializer, member_type);
    ++*init_index;
    return 1;
  }
}

ASTree *define_symbol(ASTree *decl_list, ASTree *equal_sign,
                      ASTree *initializer) {
  ASTree *declarator = astree_disown(decl_list);
  assert(declarator->tok_kind == TOK_IDENT);
  equal_sign->type = declarator->type;

  if (initializer->tok_kind != TOK_INIT_LIST &&
      (!type_is_char_array(declarator->type) ||
       initializer->tok_kind != TOK_STRINGCON))
    initializer = tchk_ptr_conv(initializer, 1);

  Symbol *symbol;
  (void)state_get_symbol(state, declarator->lexinfo, &symbol);
  assert(symbol != NULL);
  assert(symbol->type = declarator->type);
  assert(symbol->linkage != LINK_MEMBER);
  if (initializer->cexpr_kind >= CEXPR_INIT ||
      initializer->tok_kind == TOK_INIT_LIST) {
    if (traverse_initializer(symbol->type, symbol->disp, initializer))
      return astree_adopt(decl_list, 1,
                          astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (symbol->storage == STORE_STAT) {
    (void)semerr_expected_init(initializer);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else if (!types_assignable(declarator->type, initializer->type,
                               astree_is_const_zero(initializer))) {
    (void)semerr_incompatible_types(equal_sign, declarator->type,
                                    initializer->type);
    return astree_adopt(decl_list, 1,
                        astree_adopt(equal_sign, 2, declarator, initializer));
  } else {
    initializer = tchk_scal_conv(tchk_rval_conv(initializer), declarator->type);
  }

  if (symbol->linkage == LINK_NONE || symbol->info == SYM_INHERITOR)
    return translate_local_init(decl_list, equal_sign, declarator, initializer);
  else
    return translate_global_init(decl_list, equal_sign, declarator,
                                 initializer);
}
