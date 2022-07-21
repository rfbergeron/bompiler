#include "tchk_common.h"

#include "stdlib.h"
#include "yyparse.h"

/*
 * TODO(Robert): recursively setting the block number no longer works because
 * of nested scoping; instead block numbers should be set during the validation
 * of expressions, at which point it is not possible to further nest scopes.
 */

/*
 * Performs automatic conversions from function and array types to pointer
 * types. Can be called safely without first checking for errors so that the
 * passed expression does not need to be checked twice.
 */
ASTree *perform_pointer_conv(ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR) return expr;
  const TypeSpec *spec = expr->type;
  if (!typespec_is_array(spec) && !typespec_is_function(spec)) {
    return expr;
  } else {
    TypeSpec *pointer_spec = malloc(sizeof(*pointer_spec));
    if (typespec_is_array(spec)) {
      int status = strip_aux_type(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
      }
    } else {
      int status = typespec_copy(pointer_spec, spec);
      if (status) {
        free(pointer_spec);
        return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
      }
    }
    AuxSpec *pointer_aux = calloc(1, sizeof(*pointer_aux));
    pointer_aux->aux = AUX_POINTER;
    int status = llist_push_front(&pointer_spec->auxspecs, pointer_aux);
    if (status) {
      llist_destroy(&pointer_spec->auxspecs);
      free(pointer_spec);
      return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
    }
    ASTree *cast = astree_init(TOK_CAST, expr->loc, "_cast");
    cast->type = pointer_spec;
    cast->attributes |= expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITH);
    return astree_adopt(cast, 1, expr);
  }
}

int types_compatible(const TypeSpec *type1, const TypeSpec *type2);

/*
 * Creates tree node for automatic conversions, if necessary and possible. Error
 * nodes can safely be passed to this function.
 */
ASTree *convert_type(ASTree *expr, const TypeSpec *type) {
  if (expr->symbol == TOK_TYPE_ERROR) return expr;
  if (type->base == TYPE_ERROR) return expr;
  int compatibility = types_compatible(type, expr->type);
  if (compatibility == TCHK_COMPATIBLE) {
    return expr;
  } else if (compatibility == TCHK_INCOMPATIBLE) {
    return astree_create_errnode(expr, BCC_TERR_INCOMPATIBLE_TYPES, 3, expr,
                                 expr->type, type);
  }
  TypeSpec *cast_spec = malloc(sizeof(*cast_spec));
  int status = typespec_copy(cast_spec, type);
  if (status) {
    free(cast_spec);
    return astree_create_errnode(expr, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  ASTree *cast = astree_init(TOK_CAST, expr->loc, "_cast");
  cast->type = cast_spec;
  cast->attributes |= expr->attributes & (ATTR_EXPR_CONST | ATTR_EXPR_ARITH);
  return expr;
}

int compare_params(LinkedList *dests, LinkedList *srcs) {
  size_t list_count = llist_size(dests) > llist_size(srcs) ? llist_size(dests)
                                                           : llist_size(srcs);
  size_t i;
  for (i = 0; i < list_count; ++i) {
    SymbolValue *dest = llist_get(dests, i);
    SymbolValue *src = llist_get(srcs, i);
    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else {
      int symbol_compat = types_compatible(&dest->type, &src->type);
      if (symbol_compat == TCHK_COMPATIBLE) continue;
      return TCHK_EXPLICIT_CAST;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_members(LinkedList *dests, LinkedList *srcs) {
  size_t max_len = dests->size > srcs->size ? dests->size : srcs->size;
  size_t i;
  for (i = 0; i < max_len; ++i) {
    SymbolValue *dest = llist_get(dests, i);
    SymbolValue *src = llist_get(srcs, i);

    if (dest == NULL || src == NULL) {
      return TCHK_EXPLICIT_CAST;
    } else {
      int ret = types_compatible(&dest->type, &src->type);
      if (ret != TCHK_COMPATIBLE) return ret;
    }
  }
  return TCHK_COMPATIBLE;
}

int compare_declspecs(const TypeSpec *dest, const TypeSpec *src) {
  /* TODO(Robert): check qualifiers */
  if (typespec_is_integer(dest) && typespec_is_integer(src)) {
    return TCHK_COMPATIBLE;
  } else if ((dest->base == TYPE_STRUCT && src->base == TYPE_STRUCT) ||
             (dest->base == TYPE_UNION && src->base == TYPE_UNION) ||
             (dest->base == TYPE_ENUM && src->base == TYPE_ENUM)) {
    return TCHK_COMPATIBLE;
  } else {
    return TCHK_INCOMPATIBLE;
  }
}

int compare_auxspecs(const LinkedList *dests, const LinkedList *srcs) {
  size_t aux_count = llist_size(dests) > llist_size(srcs) ? llist_size(dests)
                                                          : llist_size(srcs);
  size_t i;
  for (i = 0; i < aux_count; ++i) {
    AuxSpec *dest = llist_get(srcs, i);
    AuxSpec *src = llist_get(srcs, i);

    if (dest == NULL || src == NULL) {
      return TCHK_INCOMPATIBLE;
    } else if (dest->aux == AUX_POINTER && src->aux == AUX_POINTER) {
      continue;
    } else if ((dest->aux == AUX_FUNCTION && src->aux == AUX_FUNCTION)) {
      int ret = compare_params(dest->data.params, src->data.params);
      if (ret != TCHK_COMPATIBLE) return ret;
    } else if ((dest->aux == AUX_STRUCT && src->aux == AUX_STRUCT) ||
               (dest->aux == AUX_UNION && src->aux == AUX_UNION) ||
               (dest->aux == AUX_ENUM && src->aux == AUX_ENUM)) {
      int tags_different = strcmp(dest->data.tag.name, src->data.tag.name);
      if (tags_different) return TCHK_EXPLICIT_CAST;
    } else {
      return TCHK_EXPLICIT_CAST;
    }
  }

  return TCHK_COMPATIBLE;
}

/* This function determines compatibility in situations where there is a
 * distinct 'destination' and 'source' type. It answers the question "does X
 * need to be converted to Y, and if so, does that need to be done explicitly".
 *
 * It is used to determine conversions after promotions, casts, assignments, and
 * function calls/definitions/declarations.
 */
int types_compatible(const TypeSpec *type1, const TypeSpec *type2) {
  int action = TCHK_COMPATIBLE;

  const LinkedList *auxspecs1 = &type1->auxspecs;
  const LinkedList *auxspecs2 = &type2->auxspecs;
  /* special cases */
  if (typespec_is_pointer(type1) && typespec_is_pointer(type2) &&
      (typespec_is_voidptr(type1) || typespec_is_voidptr(type2))) {
    /* pointer to/from void pointer */
    return TCHK_COMPATIBLE;
  } else if ((typespec_is_pointer(type1) || typespec_is_enum(type1)) &&
             typespec_is_integer(type2) && llist_size(auxspecs2) == 0) {
    /* int to pointer or enum */
    return TCHK_IMPLICIT_CAST;
  } else if ((typespec_is_pointer(type2) || typespec_is_enum(type2)) &&
             typespec_is_integer(type1) && llist_size(auxspecs1) == 0) {
    /* pointer or enum to int */
    return TCHK_IMPLICIT_CAST;
  }

  int ret = compare_auxspecs(auxspecs1, auxspecs2);
  if (ret != TCHK_COMPATIBLE) return ret;

  return compare_declspecs(type1, type2);
}

/*
 * integer types, in order of priority, are as follows:
 * NOTE: long long won't be supported for the moment; words in parentheses are
 * optional and may be omitted; the signedness of char is implementation-defined
 * and here I have chosen that it is signed by default
 *
 * unsigned long (int)
 * (signed) long (int)
 * unsigned (int)
 * signed, int, singed int
 * unsigned short (int)
 * (signed) short (int)
 * unsigned char
 * (signed) char
 */
int determine_conversion(const TypeSpec *type1, const TypeSpec *type2,
                         const TypeSpec **out) {
  if (typespec_is_pointer(type1)) {
    *out = type1;
  } else if (typespec_is_pointer(type2)) {
    *out = type2;
  } else if (type1->base == TYPE_STRUCT || type1->base == TYPE_UNION) {
    *out = type1;
  } else if (type2->base == TYPE_STRUCT || type2->base == TYPE_UNION) {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  } else if (type1->width < X64_SIZEOF_INT && type2->width < X64_SIZEOF_INT) {
    /* promote to signed int if both operands could be represented as one */
    *out = &SPEC_INT;
  } else if (type1->width > type2->width) {
    /* promote to wider type; disregard signedness of type2 */
    *out = type1;
  } else if (type1->width < type2->width) {
    /* promote to wider type; disregard signedness of type1 */
    *out = type2;
  } else if (type1->base == TYPE_UNSIGNED) {
    /* same width, but type1 is unsigned so prefer it */
    *out = type1;
  } else if (type2->base == TYPE_UNSIGNED) {
    /* same width, but type2 is unsigned so prefer it */
    *out = type2;
  } else if (type1->base == TYPE_SIGNED && type2->base == TYPE_SIGNED) {
    /* both are signed integers, so just pick the left-hand type */
    *out = type1;
  } else {
    return BCC_TERR_INCOMPATIBLE_TYPES;
  }
  return BCC_TERR_SUCCESS;
}

int merge_block_controls(ASTree *block, ASTree *stmt) {
  ASTree *sub_block = NULL;
  switch (stmt->symbol) {
    case TOK_FOR:
    case TOK_WHILE:
    case TOK_SWITCH:
      if (astree_get(stmt, 1)->symbol == TOK_BLOCK)
        sub_block = astree_get(stmt, 1);
      break;
    case TOK_DO:
      if (astree_get(stmt, 0)->symbol == TOK_BLOCK)
        sub_block = astree_get(stmt, 0);
      break;
    case TOK_BLOCK:
      sub_block = stmt;
      break;
    default:
      break;
  }

  if (sub_block != NULL) {
    return symbol_table_merge_control(block->symbol_table,
                                      sub_block->symbol_table);
  } else {
    return 0;
  }
}
