#include "tchk_common.h"

#include "stdlib.h"
#include "yyparse.h"

int params_equivalent(const AuxSpec *aux1, const AuxSpec *aux2) {
  LinkedList *params1 = aux1->data.fn.params;
  LinkedList *params2 = aux2->data.fn.params;
  if (llist_size(params1) != llist_size(params2)) return 0;
  size_t i;
  for (i = 0; i < llist_size(params1); ++i) {
    SymbolValue *symval1 = llist_get(params1, i);
    SymbolValue *symval2 = llist_get(params2, i);
    if (!types_equivalent(&symval1->type, &symval2->type, IGNORE_STORAGE_CLASS))
      return 0;
  }
  return 1;
}

int members_equivalent(const AuxSpec *aux1, const AuxSpec *aux2) {
  LinkedList *members1 = &aux1->data.tag.val->data.members.in_order;
  LinkedList *members2 = &aux2->data.tag.val->data.members.in_order;
  if (llist_size(members1) != llist_size(members2)) return 0;
  size_t i;
  for (i = 0; i < llist_size(members1); ++i) {
    SymbolValue *symval1 = llist_get(members1, i);
    SymbolValue *symval2 = llist_get(members2, i);
    if (!types_equivalent(&symval1->type, &symval2->type, IGNORE_STORAGE_CLASS))
      return 0;
  }
  return 1;
}

int aux_equivalent(const AuxSpec *aux1, const AuxSpec *aux2,
                   unsigned int flags) {
  switch (aux1->aux) {
    case AUX_FUNCTION:
      return params_equivalent(aux1, aux2) &&
             (aux1->data.fn.is_variadic == aux2->data.fn.is_variadic);
    case AUX_ARRAY:
      if ((flags & IGNORE_ARRAY_BOUNDS) && aux1->data.memory_loc.length == 0)
        return 1;
      else
        return aux1->data.memory_loc.length == aux2->data.memory_loc.length;
    case AUX_ENUM:
    case AUX_STRUCT:
    case AUX_UNION:
      /* can't use name since redefinition is possible */
      return aux1->data.tag.val == aux2->data.tag.val;
    case AUX_POINTER:
      if (flags & IGNORE_QUALIFIERS)
        return 1;
      else
        return !((aux1->data.memory_loc.qualifiers ^
                  aux2->data.memory_loc.qualifiers) &
                 (TYPESPEC_FLAG_CONST | TYPESPEC_FLAG_VOLATILE));
    default:
      return 0;
  }
}

/* TODO(Robert): comparing width, alignment, and base type isn't quite right,
 * but given the limits of the current representation of types, it is the best
 * that I can do.
 */
int types_equivalent(const TypeSpec *type1, const TypeSpec *type2,
                     unsigned int flags) {
  LinkedList *auxspecs1 = (LinkedList *)&type1->auxspecs;
  LinkedList *auxspecs2 = (LinkedList *)&type2->auxspecs;
  if (llist_size(auxspecs1) != llist_size(auxspecs2)) return 0;
  size_t i;
  for (i = 0; i < llist_size(auxspecs1); ++i) {
    AuxSpec *aux1 = llist_get(auxspecs1, i);
    AuxSpec *aux2 = llist_get(auxspecs2, i);

    if (aux1->aux != aux2->aux) return 0;
    if (!aux_equivalent(aux1, aux2, flags)) return 0;
  }
  if (type1->base != type2->base) return 0;
  if (type1->width != type2->width) return 0;
  if (type1->alignment != type2->alignment) return 0;
  unsigned int flags_diff = type1->flags ^ type2->flags;
  if (flags & IGNORE_QUALIFIERS) goto ignore_qualifiers;
  if (flags_diff & TYPESPEC_FLAG_VOLATILE) return 0;
  if (flags_diff & TYPESPEC_FLAG_CONST) return 0;
ignore_qualifiers:
  if (flags & IGNORE_STORAGE_CLASS) return 1;
  if (flags_diff & TYPESPEC_FLAGS_STORAGE_CLASS) return 0;
  return 1;
}

int is_const_zero(ASTree *tree) {
  return (tree->attributes & ATTR_EXPR_CONST) &&
         !(tree->attributes & ATTR_CONST_ADDR) &&
         (tree->constant.integral.value == 0);
}

int types_assignable(const TypeSpec *dest_type, ASTree *src) {
  if (typespec_is_arithmetic(dest_type) && typespec_is_arithmetic(src->type)) {
    return 1;
  } else if ((typespec_is_struct(dest_type) && typespec_is_struct(src->type)) ||
             (typespec_is_union(dest_type) && typespec_is_union(src->type))) {
    AuxSpec *dest_aux = llist_front(&dest_type->auxspecs);
    AuxSpec *src_aux = llist_front(&src->type->auxspecs);
    return dest_aux->data.tag.val == src_aux->data.tag.val;
  } else if (typespec_is_pointer(dest_type) && typespec_is_pointer(src->type)) {
    if (types_equivalent(
            dest_type, src->type,
            IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS | IGNORE_ARRAY_BOUNDS)) {
      return 1;
    } else if (typespec_is_voidptr(dest_type) ||
               typespec_is_voidptr(src->type)) {
      return 1;
    } else {
      return 0;
    }
  } else if (typespec_is_pointer(dest_type) && is_const_zero(src)) {
    return 1;
  } else {
    return 0;
  }
}

const TypeSpec *arithmetic_conversions(const TypeSpec *type1,
                                       const TypeSpec *type2) {
  size_t width1 = typespec_get_width(type1);
  size_t width2 = typespec_get_width(type2);
  const TypeSpec *ret;
  if (width1 < X64_SIZEOF_INT && width2 < X64_SIZEOF_INT) {
    ret = &SPEC_INT;
  } else if (width1 > width2) {
    ret = type1;
  } else if (width1 < width2) {
    ret = type2;
  } else if (typespec_is_unsigned(type1)) {
    ret = type1;
  } else if (typespec_is_unsigned(type2)) {
    ret = type2;
  } else {
    ret = type1;
  }
  return typespec_is_enum(ret) ? &SPEC_INT : ret;
}

/* TODO(Robert): make sure that nodes which have had their type altered free the
 * resulting type. it should work much the same as freeing types created by the
 * address operator.
 */
/*
 * Performs automatic conversions from function and array types to pointer
 * types. Replaces old type with appropriately converted type. Can safely be
 * called when `expr` is an error.
 */
void pointer_conversions(ASTree *expr) {
  const TypeSpec *old_type = expr->type;
  if (typespec_is_function(old_type) || typespec_is_array(old_type)) {
    TypeSpec *pointer_type = malloc(sizeof(*pointer_type));
    if (typespec_is_array(old_type)) {
      int status = strip_aux_type(pointer_type, old_type);
      if (status) {
        free(pointer_type);
        abort();
      }
    } else {
      int status = typespec_copy(pointer_type, old_type);
      if (status) {
        free(pointer_type);
        abort();
      }
    }
    int status = typespec_prepend_aux(pointer_type, (AuxSpec *)&AUXSPEC_PTR);
    if (status) {
      llist_destroy(&pointer_type->auxspecs);
      free(pointer_type);
      abort();
    }
    expr->type = pointer_type;
  }
}
