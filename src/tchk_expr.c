#include "tchk_expr.h"

#include "ctype.h"
#include "evaluate.h"
#include "inttypes.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"
#include "yyparse.h"

ASTree *validate_intcon(ASTree *intcon) { return evaluate_intcon(intcon); }

ASTree *validate_charcon(ASTree *charcon) {
  charcon->type = &SPEC_CHAR;
  return evaluate_charcon(charcon);
}

ASTree *validate_stringcon(ASTree *stringcon) {
  TypeSpec *stringcon_type = malloc(sizeof(*stringcon->type));
  *stringcon_type = SPEC_CHAR;
  int status = typespec_init(stringcon_type);
  if (status)
    return astree_create_errnode(stringcon, BCC_TERR_LIBRARY_FAILURE, 0);

  AuxSpec *array_aux = calloc(1, sizeof(*array_aux));
  array_aux->aux = AUX_ARRAY;
  /* Normally, we would subtract 2 to omit the starting and ending doublequote,
   * but since strlen does not include the terminating null byte, we only
   * subtract one.
   */
  array_aux->data.memory_loc.length = strlen(stringcon->lexinfo) - 1;

  stringcon->type = stringcon_type;
  return stringcon;
}

ASTree *validate_ident(ASTree *ident) {
  DEBUGS('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strlen(id_str);
  SymbolValue *symval = NULL;
  int in_current_scope = state_get_symbol(state, id_str, id_str_len, &symval);
  if (symval) {
    DEBUGS('t', "Assigning %s a symbol", id_str);
    ident->type = &(symval->type);
    if (!typespec_is_array(ident->type) && !typespec_is_function(ident->type) &&
        !(ident->type->flags & TYPESPEC_FLAG_TYPEDEF) &&
        !(symval->flags & SYMFLAG_ENUM_CONST)) {
      ident->attributes |= ATTR_EXPR_LVAL;
    }
    return evaluate_ident(ident);
  } else {
    return astree_create_errnode(ident, BCC_TERR_SYM_NOT_FOUND, 1, ident);
  }
}

ASTree *finalize_call(ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR) {
    return call;
  }
  ASTree *function = astree_get(call, 0);
  TypeSpec *function_spec = (TypeSpec *)function->type;
  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function expression is also a child */
  if (astree_count(call) - 1 > llist_size(param_list)) {
    return astree_create_errnode(call, BCC_TERR_INSUFF_PARAMS, 1, call);
  }
  return call;
}

ASTree *validate_arg(ASTree *call, ASTree *arg) {
  if (call->symbol == TOK_TYPE_ERROR || arg->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, arg);
  }
  pointer_conversions(arg);
  /* functon subtree is the first child of the call node */
  ASTree *function = astree_get(call, 0);
  TypeSpec *function_spec = (TypeSpec *)function->type;
  /* second auxspec will be the function; first is pointer */
  AuxSpec *param_spec = llist_get(&function_spec->auxspecs, 1);
  LinkedList *param_list = param_spec->data.params;
  /* subtract one since function expression is also a child */
  size_t param_index = astree_count(call) - 1;
  if (param_index >= llist_size(param_list)) {
    return astree_create_errnode(astree_adopt(call, 1, arg),
                                 BCC_TERR_EXCESS_PARAMS, 1, call);
  }
  DEBUGS('t', "Validating argument %d", param_index);
  SymbolValue *symval = llist_get(param_list, param_index);
  DEBUGS('t', "Comparing types");
  if (types_assignable(&symval->type, arg)) {
    return astree_adopt(call, 1, arg);
  } else {
    return astree_create_errnode(astree_adopt(call, 1, arg),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, arg, arg->type,
                                 &symval->type);
  }
}

ASTree *validate_call(ASTree *expr, ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, expr);
  }
  pointer_conversions(expr);
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, expr);
  }
  TypeSpec *expr_spec = (TypeSpec *)expr->type;
  if (!typespec_is_fnptr(expr_spec)) {
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_EXPECTED_FN_PTR, 2, call, expr);
  }

  /* strip pointer */
  TypeSpec temp_spec = SPEC_EMPTY;
  int status = strip_aux_type(&temp_spec, expr_spec);
  if (status) {
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_LIBRARY_FAILURE, 0);
  }
  /* strip function */
  TypeSpec *return_spec = malloc(sizeof(*return_spec));
  status = strip_aux_type(return_spec, &temp_spec);
  if (status) {
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_LIBRARY_FAILURE, 0);
  }
  /* free temporaries created by stripping */
  typespec_destroy(&temp_spec);
  call->type = return_spec;
  return astree_adopt(call, 1, expr);
}

ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  if (condition->symbol == TOK_TYPE_ERROR ||
      true_expr->symbol == TOK_TYPE_ERROR ||
      false_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  }

  pointer_conversions(condition);
  pointer_conversions(true_expr);
  pointer_conversions(false_expr);
  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_EXPECTED_SCALAR, 2, qmark, condition);
  }

  if (typespec_is_arithmetic(true_expr->type) &&
      typespec_is_arithmetic(false_expr->type)) {
    qmark->type = arithmetic_conversions(true_expr->type, false_expr->type);
  } else if ((typespec_is_struct(true_expr->type) &&
              typespec_is_struct(false_expr->type)) ||
             (typespec_is_union(true_expr->type) &&
              typespec_is_union(false_expr->type)) ||
             (typespec_is_void(true_expr->type) &&
              typespec_is_void(false_expr->type))) {
    if (types_equivalent(true_expr->type, false_expr->type,
                         IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS)) {
      qmark->type = true_expr->type;
    } else {
      return astree_create_errnode(
          astree_adopt(qmark, 3, condition, true_expr, false_expr),
          BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
          false_expr->type);
    }
  } else if (typespec_is_pointer(true_expr->type) &&
             is_const_zero(false_expr)) {
    qmark->type = true_expr->type;
  } else if (is_const_zero(true_expr) &&
             typespec_is_pointer(false_expr->type)) {
    qmark->type = false_expr->type;
  } else if (typespec_is_pointer(true_expr->type) &&
             typespec_is_voidptr(false_expr->type)) {
    TypeSpec *common_type = malloc(sizeof(*common_type));
    /* remember to flip arguments so that the common type is a void ptr */
    int status =
        common_qualified_ptr(common_type, false_expr->type, true_expr->type);
    if (status)
      return astree_create_errnode(
          astree_adopt(qmark, 3, condition, true_expr, false_expr),
          BCC_TERR_FAILURE, 0);
    /* TODO(Robert): free resources used to create new type/auxspec */
    qmark->type = common_type;
  } else if (typespec_is_voidptr(true_expr->type) &&
             typespec_is_pointer(false_expr->type)) {
    TypeSpec *common_type = malloc(sizeof(*common_type));
    int status =
        common_qualified_ptr(common_type, true_expr->type, false_expr->type);
    if (status)
      return astree_create_errnode(
          astree_adopt(qmark, 3, condition, true_expr, false_expr),
          BCC_TERR_FAILURE, 0);
    /* TODO(Robert): free resources used to create new type/auxspec */
    qmark->type = common_type;
  } else if (typespec_is_pointer(true_expr->type) &&
             typespec_is_pointer(false_expr->type)) {
    if (types_equivalent(true_expr->type, false_expr->type,
                         IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS)) {
      TypeSpec *common_type = malloc(sizeof(*common_type));
      int status =
          common_qualified_ptr(common_type, true_expr->type, false_expr->type);
      if (status)
        return astree_create_errnode(
            astree_adopt(qmark, 3, condition, true_expr, false_expr),
            BCC_TERR_FAILURE, 0);
      /* TODO(Robert): free resources used to create new type/auxspec */
      qmark->type = common_type;
    } else {
      return astree_create_errnode(
          astree_adopt(qmark, 3, condition, true_expr, false_expr),
          BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
          false_expr->type);
    }
  }

  return evaluate_conditional(qmark, condition, true_expr, false_expr);
}

ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr) {
  if (left_expr->symbol == TOK_TYPE_ERROR ||
      right_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left_expr, right_expr);
  }
  pointer_conversions(left_expr);
  pointer_conversions(right_expr);
  comma->type = right_expr->type;
  return astree_adopt(comma, 2, left_expr, right_expr);
}

ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  pointer_conversions(expr);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  } else if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  }

  ASTree *type_name = astree_get(declaration, 1);
  if (!(typespec_is_scalar(type_name->type) ||
        typespec_is_void(type_name->type)) ||
      !typespec_is_scalar(expr->type)) {
    return astree_create_errnode(astree_adopt(cast, 2, declaration, expr),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, cast,
                                 type_name->type, expr->type);
  } else {
    cast->type = type_name->type;
    return evaluate_cast(astree_adopt(cast, 1, declaration), expr);
  }
}

/* logic:
 * casts to pointer disqualify expressions from being arithmetic constants
 */
ASTree *validate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  pointer_conversions(left);
  pointer_conversions(right);
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    operator->type = arithmetic_conversions(left->type, right->type);
    return evaluate_binop(operator, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_integer(right_type)) {
    operator->type = left_type;
    return evaluate_binop(operator, left, right);
  } else if (typespec_is_integer(left_type) &&
             typespec_is_pointer(right_type)) {
    if (operator->symbol == '-') {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left_type, right_type);
    } else {
      operator->type = right_type;
      return evaluate_binop(operator, left, right);
    }
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type) && operator->symbol == '-') {
    if (types_equivalent(left->type, right->type,
                         IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS)) {
      /* TODO(Robert): dedicated pointer difference type constant */
      operator->type = & SPEC_LONG;
      return evaluate_binop(operator, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left_type, right_type);
    }
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                 left_type, right_type);
  }
}

ASTree *validate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  pointer_conversions(left);
  pointer_conversions(right);
  if (typespec_is_scalar(left->type) && typespec_is_scalar(right->type)) {
    operator->type = & SPEC_INT;
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_SCALAR, 3, operator, left,
                                 right);
  }
}

ASTree *validate_relational(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  pointer_conversions(left);
  pointer_conversions(right);
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    return evaluate_binop(operator, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    if (types_equivalent(left->type, right->type,
                         IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS)) {
      return evaluate_binop(operator, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                 left->type, right->type);
  }
}

ASTree *validate_equality(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  pointer_conversions(left);
  pointer_conversions(right);
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    return evaluate_binop(operator, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    if (types_equivalent(left->type, right->type,
                         IGNORE_QUALIFIERS | IGNORE_STORAGE_CLASS)) {
      return evaluate_binop(operator, left, right);
    } else if (typespec_is_voidptr(left_type) ||
               typespec_is_voidptr(right_type)) {
      return evaluate_binop(operator, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
  } else if (typespec_is_pointer(right_type) && is_const_zero(left)) {
    return evaluate_binop(operator, left, right);
  } else if (typespec_is_pointer(left_type) && is_const_zero(right)) {
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                 left->type, right->type);
  }
}

ASTree *validate_multiply(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_arithmetic(left->type) &&
      typespec_is_arithmetic(right->type)) {
    operator->type = arithmetic_conversions(left->type, right->type);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 3, operator,
                                 left, right);
  }
}

ASTree *validate_shift(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    operator->type = arithmetic_conversions(left->type, &SPEC_INT);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *validate_bitwise(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    operator->type = arithmetic_conversions(left->type, right->type);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *validate_increment(ASTree *operator, ASTree * operand) {
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  if (typespec_is_pointer(operand->type)) {
    operator->type = operand->type;
    return astree_adopt(operator, 1, operand);
  } else if (typespec_is_arithmetic(operand->type)) {
    operator->type = arithmetic_conversions(operand->type, &SPEC_INT);
    return astree_adopt(operator, 1, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_SCALAR, 2, operator,
                                 operand);
  }
}

ASTree *validate_not(ASTree *operator, ASTree * operand) {
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  pointer_conversions(operand);
  operator->type = & SPEC_INT;
  if (typespec_is_scalar(operand->type)) {
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_SCALAR, 2, operand, operand);
  }
}

ASTree *validate_complement(ASTree *operator, ASTree * operand) {
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  pointer_conversions(operand);
  if (typespec_is_integer(operand->type)) {
    operator->type = arithmetic_conversions(operand->type, &SPEC_INT);
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_INTEGER, 2, operator,
                                 operand);
  }
}

ASTree *validate_negation(ASTree *operator, ASTree * operand) {
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  pointer_conversions(operand);
  if (typespec_is_arithmetic(operand->type)) {
    operator->type = arithmetic_conversions(operand->type, &SPEC_INT);
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 2, operator,
                                 operand);
  }
}

ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  pointer_conversions(operand);
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(indirection, operand);
  }

  if (typespec_is_pointer(operand->type)) {
    TypeSpec *indirection_spec = malloc(sizeof(*indirection_spec));
    int status = strip_aux_type(indirection_spec, operand->type);
    if (status) {
      return astree_create_errnode(astree_adopt(indirection, 1, operand),
                                   BCC_TERR_LIBRARY_FAILURE, 0);
    }
    indirection->type = indirection_spec;
    indirection->attributes |= ATTR_EXPR_LVAL;
    return astree_adopt(indirection, 1, operand);
  } else {
    return astree_create_errnode(astree_adopt(indirection, 1, operand),
                                 BCC_TERR_EXPECTED_POINTER, 2, indirection,
                                 operand);
  }
}

ASTree *validate_addrof(ASTree *addrof, ASTree *operand) {
  if (operand->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(addrof, operand);
  if (!(operand->attributes & ATTR_EXPR_LVAL)) {
    return astree_create_errnode(astree_adopt(addrof, 1, operand),
                                 BCC_TERR_EXPECTED_LVAL, 2, addrof, operand);
  }
  TypeSpec *addrof_spec = malloc(sizeof(*addrof_spec));
  int status = typespec_copy(addrof_spec, operand->type);
  if (status) {
    return astree_create_errnode(astree_adopt(addrof, 1, operand),
                                 BCC_TERR_LIBRARY_FAILURE, 0);
  }
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  llist_push_front(&addrof_spec->auxspecs, ptr_aux);
  addrof->type = addrof_spec;
  return evaluate_addrof(addrof, operand);
}

ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node) {
  const TypeSpec *spec = NULL;
  if (type_node->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(sizeof_, type_node);
  } else if (type_node->symbol == TOK_DECLARATION) {
    spec = astree_get(type_node, 1)->type;
  } else {
    spec = type_node->type;
  }

  if (typespec_is_incomplete(spec)) {
    return astree_create_errnode(astree_adopt(sizeof_, 1, type_node),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, sizeof_, spec);
  }
  /* TODO(Robert): compute actual size and also probably make sure that this
   * is actually the correct type name for the output of sizeof on this
   * platform
   */
  sizeof_->type = &SPEC_ULONG;
  return evaluate_unop(sizeof_, type_node);
}

ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  pointer_conversions(pointer);
  if (pointer->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  }

  pointer_conversions(index);
  if (index->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  }

  if (!typespec_is_pointer(pointer->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                                 BCC_TERR_EXPECTED_POINTER, 2, subscript,
                                 pointer);
  } else if (!typespec_is_integer(index->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                                 BCC_TERR_EXPECTED_INTEGER, 2, subscript,
                                 index);
  } else {
    TypeSpec *subscript_spec = malloc(sizeof(*subscript_spec));
    int status = strip_aux_type(subscript_spec, pointer->type);
    if (status)
      return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                                   BCC_TERR_LIBRARY_FAILURE, 0);
    subscript->type = subscript_spec;
    if (!typespec_is_array(subscript->type) &&
        !typespec_is_function(subscript->type)) {
      subscript->attributes |= ATTR_EXPR_LVAL;
    }
    return evaluate_subscript(subscript, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_, ASTree *member) {
  if (reference->symbol == TOK_ARROW) pointer_conversions(struct_);
  if (struct_->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(reference, 2, struct_, member);
  const TypeSpec *struct_type = struct_->type;
  if (reference->symbol == TOK_ARROW && !typespec_is_structptr(struct_type) &&
      !typespec_is_unionptr(struct_type)) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                 BCC_TERR_EXPECTED_TAG_PTR, 2, reference,
                                 struct_type);
  } else if (!typespec_is_struct(struct_type) &&
             !typespec_is_union(struct_type)) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                 BCC_TERR_EXPECTED_TAG, 2, reference,
                                 struct_type);
  }

  const char *member_name = member->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *struct_aux = reference->symbol == TOK_ARROW
                            ? llist_get(&struct_type->auxspecs, 1)
                            : llist_front(&struct_type->auxspecs);
  SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                 BCC_TERR_SYM_NOT_FOUND, 1, member);
  } else {
    reference->type = &symval->type;
    if (!typespec_is_array(reference->type) &&
        !typespec_is_function(reference->type)) {
      reference->attributes |= ATTR_EXPR_LVAL;
    }
    return evaluate_reference(reference, struct_, member);
  }
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  if (dest->symbol == TOK_TYPE_ERROR || src->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(assignment, 2, dest, src);
  }
  pointer_conversions(src);
  if (typespec_is_array(dest->type) || typespec_is_function(dest->type) ||
      typespec_is_incomplete(dest->type) || typespec_is_const(dest->type)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else if (!(dest->attributes & ATTR_EXPR_LVAL)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else if (types_assignable(dest->type, src)) {
    assignment->type = dest->type;
    return astree_adopt(assignment, 2, dest, src);
  } else {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, assignment,
                                 dest->type, src->type);
  }
}
