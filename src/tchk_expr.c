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
        !(ident->type->flags & TYPESPEC_FLAG_TYPEDEF)) {
      ident->attributes |= ATTR_EXPR_LVAL;
    }
    return ident;
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
  int compatibility = types_compatible(&symval->type, arg->type);
  if (compatibility == TCHK_INCOMPATIBLE ||
      compatibility == TCHK_EXPLICIT_CAST) {
    return astree_create_errnode(astree_adopt(call, 1, arg),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, arg, arg->type,
                                 &symval->type);
  } else {
    return astree_adopt(call, 1, arg);
  }
}

ASTree *validate_call(ASTree *expr, ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(call, expr);
  }
  expr = perform_pointer_conv(expr);
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
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_EXPECTED_SCALAR, 2, qmark, condition);
  }

  true_expr = perform_pointer_conv(true_expr);
  if (true_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  }

  false_expr = perform_pointer_conv(false_expr);
  if (false_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  }

  /* TODO(Robert): the rules for conversion on the output of the ternary
   * operator are different from usual conversions and compatibility rules, and
   * should have their own function
   */
  int status =
      determine_conversion(true_expr->type, false_expr->type, &qmark->type);
  if (status) {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
        false_expr->type);
  }

  return evaluate_conditional(
      astree_adopt(qmark, 3, condition, true_expr, false_expr));
}

ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr) {
  if (left_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left_expr, right_expr);
  }
  right_expr = perform_pointer_conv(right_expr);
  if (right_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left_expr, right_expr);
  }

  comma->type = right_expr->type;
  return astree_adopt(comma, 2, left_expr, right_expr);
}

ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  expr = perform_pointer_conv(expr);
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  } else if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  }

  ASTree *type_name = astree_get(declaration, 1);
  int compatibility = types_compatible(type_name->type, expr->type);
  if (compatibility == TCHK_INCOMPATIBLE) {
    return astree_create_errnode(astree_adopt(cast, 2, declaration, expr),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, cast,
                                 type_name->type, expr->type);
  } else {
    cast->type = type_name->type;
    return evaluate_cast(astree_adopt(cast, 2, declaration, expr));
  }
}

ASTree *typecheck_addop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    int status = determine_conversion(left_type, right_type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left_type, right_type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_integer(right_type)) {
    operator->type = left_type;
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_integer(left_type) &&
             typespec_is_pointer(right_type)) {
    operator->type = right_type;
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type) && operator->symbol == '-') {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE) {
      /* types should be the same; just pick the left one */
      operator->type = left_type;
      return astree_adopt(operator, 2, left, right);
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

ASTree *typecheck_logop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_scalar(left->type) && typespec_is_scalar(right->type)) {
    operator->type = & SPEC_INT;
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_SCALAR, 3, operator, left,
                                 right);
  }
}

int is_const_zero(ASTree *tree) {
  if (tree->symbol != TOK_INTCON) {
    return 0;
  } else {
    return strtol(tree->lexinfo, NULL, 0) == 0;
  }
}

ASTree *typecheck_relop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  const TypeSpec *left_type = left->type;
  const TypeSpec *right_type = right->type;
  operator->type = & SPEC_INT;

  if (typespec_is_arithmetic(left_type) && typespec_is_arithmetic(right_type)) {
    const TypeSpec *common_type;
    int status = determine_conversion(left_type, right_type, &common_type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
    left = convert_type(left, common_type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, common_type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else if (typespec_is_pointer(left_type) &&
             typespec_is_pointer(right_type)) {
    int compatibility = types_compatible(left_type, right_type);
    if (compatibility == TCHK_COMPATIBLE || typespec_is_voidptr(left_type) ||
        typespec_is_voidptr(right_type)) {
      return astree_adopt(operator, 2, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
  } else if (((typespec_is_pointer(left_type) && is_const_zero(right)) ||
              (is_const_zero(left) && typespec_is_pointer(right_type))) &&
             (operator->symbol == TOK_EQ || operator->symbol == TOK_NE)) {
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                 left->type, right->type);
  }
}

ASTree *typecheck_mulop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_arithmetic(left->type) &&
      typespec_is_arithmetic(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 3, operator,
                                 left, right);
  }
}

ASTree *typecheck_shfop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, &SPEC_INT, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, &SPEC_INT);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    /* promote right operand independently of left */
    const TypeSpec *dummy;
    status = determine_conversion(right->type, &SPEC_INT, &dummy);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   right->type, &SPEC_INT);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *typecheck_bitop(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (typespec_is_integer(left->type) && typespec_is_integer(right->type)) {
    int status = determine_conversion(left->type, right->type, &operator->type);
    if (status) {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
    left = convert_type(left, operator->type);
    if (left->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    right = convert_type(right, operator->type);
    if (right->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode_v(operator, 2, left, right);
    }
    return astree_adopt(operator, 2, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *validate_binop(ASTree *operator, ASTree * left_operand,
                       ASTree *right_operand) {
  DEBUGS('t', "Validating binary operator %c", operator->symbol);
  left_operand = perform_pointer_conv(left_operand);
  right_operand = perform_pointer_conv(right_operand);

  ASTree *result = NULL;
  switch (operator->symbol) {
    case TOK_SHL:
    case TOK_SHR:
      result = typecheck_shfop(operator, left_operand, right_operand);
      break;
    case '&':
    case '|':
    case '^':
      result = typecheck_bitop(operator, left_operand, right_operand);
      break;
    case '*':
    case '/':
    case '%':
      result = typecheck_mulop(operator, left_operand, right_operand);
      break;
    case '+':
    case '-':
      result = typecheck_addop(operator, left_operand, right_operand);
      break;
    case TOK_EQ:
    case TOK_NE:
    case TOK_GE:
    case TOK_LE:
    case '>':
    case '<':
      result = typecheck_relop(operator, left_operand, right_operand);
      break;
    case TOK_AND:
    case TOK_OR:
      result = typecheck_logop(operator, left_operand, right_operand);
      break;
    default:
      result = astree_create_errnode(
          astree_adopt(operator, 2, left_operand, right_operand),
          BCC_TERR_UNEXPECTED_TOKEN, 1, operator);
  }

  if (result->symbol == TOK_TYPE_ERROR) return result;
  return evaluate_binop(result);
}

int is_increment(const int symbol) {
  return symbol == TOK_INC || symbol == TOK_DEC || symbol == TOK_POST_INC ||
         symbol == TOK_POST_DEC;
}

ASTree *validate_unop(ASTree *operator, ASTree * operand) {
  DEBUGS('t', "Validating unary operator %c", operator->symbol);
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }
  const TypeSpec *operand_type = operand->type;

  if (is_increment(operator->symbol) && !typespec_is_scalar(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_SCALAR, 2, operator,
                                 operand);
  } else if ((operator->symbol == TOK_NEG || operator->symbol == TOK_POS) &&
             !typespec_is_arithmetic(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 2, operator,
                                 operand);
  } else if (operator->symbol == '~' && !typespec_is_integer(operand_type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_INTEGER, 2, operator,
                                 operand);
  } else if (operator->symbol == '!') {
    if (typespec_is_scalar(operand_type)) {
      operator->type = & SPEC_INT;
      return evaluate_unop(astree_adopt(operator, 1, operand));
    } else {
      return astree_create_errnode(astree_adopt(operator, 1, operand),
                                   BCC_TERR_EXPECTED_SCALAR, 2, operand,
                                   operand);
    }
  } else {
    if (!is_increment(operator->symbol)) {
      operand = perform_pointer_conv(operand);
      if (operand->symbol == TOK_TYPE_ERROR) {
        return astree_propogate_errnode(operator, operand);
      }
    }
    int status = determine_conversion(operand_type, &SPEC_INT, &operator->type);
    if (status)
      return astree_create_errnode(astree_adopt(operator, 1, operand),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   operand_type, &SPEC_INT);
    operand = convert_type(operand, operator->type);
    if (operand->symbol == TOK_TYPE_ERROR) {
      return astree_propogate_errnode(operator, operand);
    }
    return evaluate_unop(astree_adopt(operator, 1, operand));
  }
}

ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  operand = perform_pointer_conv(operand);
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
  /* TODO(Robert): check that operand is an lval */
  /* TODO(Robert): set constexpr attribute if operand is static/extern */
  if (operand->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(addrof, operand);
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
  return astree_adopt(addrof, 1, operand);
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
  return evaluate_unop(astree_adopt(sizeof_, 1, type_node));
}

ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  pointer = perform_pointer_conv(pointer);
  if (pointer->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  }

  index = perform_pointer_conv(index);
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
    return astree_adopt(subscript, 2, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_,
                           ASTree *member_name_node) {
  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_struct(struct_type) && !typespec_is_union(struct_type)) {
    return astree_create_errnode(
        astree_adopt(reference, 2, struct_, member_name_node),
        BCC_TERR_EXPECTED_TAG, 2, reference, struct_);
  }

  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  AuxSpec *struct_aux = llist_front(&struct_type->auxspecs);
  SymbolTable *member_table = struct_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return astree_create_errnode(
        astree_adopt(reference, 2, struct_, member_name_node),
        BCC_TERR_SYM_NOT_FOUND, 1, member_name_node);
  } else {
    reference->type = &symval->type;
    if (!typespec_is_array(reference->type) &&
        !typespec_is_function(reference->type)) {
      reference->attributes |= ATTR_EXPR_LVAL;
    }
    return astree_adopt(reference, 2, struct_, member_name_node);
  }
}

ASTree *validate_arrow(ASTree *arrow, ASTree *struct_,
                       ASTree *member_name_node) {
  struct_ = perform_pointer_conv(struct_);
  if (struct_->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(arrow, 2, struct_, member_name_node);
  }

  const TypeSpec *struct_type = struct_->type;
  if (!typespec_is_structptr(struct_type) &&
      !typespec_is_unionptr(struct_type)) {
    return astree_create_errnode(
        astree_adopt(arrow, 2, struct_, member_name_node),
        BCC_TERR_EXPECTED_TAG_PTR, 2, arrow, struct_type);
  }
  const char *member_name = member_name_node->lexinfo;
  const size_t member_name_len = strlen(member_name);
  /* first auxtype is pointer; second is struct/union */
  AuxSpec *strunion_aux = llist_get(&struct_type->auxspecs, 1);
  SymbolTable *member_table = strunion_aux->data.tag.val->data.members.by_name;
  SymbolValue *symval =
      symbol_table_get(member_table, (char *)member_name, member_name_len);

  if (symval == NULL) {
    return astree_create_errnode(
        astree_adopt(arrow, 2, struct_, member_name_node),
        BCC_TERR_SYM_NOT_FOUND, 1, member_name_node);
  } else {
    arrow->type = &symval->type;
    if (!typespec_is_array(arrow->type) && !typespec_is_function(arrow->type)) {
      arrow->attributes |= ATTR_EXPR_LVAL;
    }
    return astree_adopt(arrow, 2, struct_, member_name_node);
  }
}
