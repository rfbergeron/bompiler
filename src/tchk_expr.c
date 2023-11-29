#include "tchk_expr.h"

#include <assert.h>

#include "asmgen.h"
#include "ctype.h"
#include "evaluate.h"
#include "inttypes.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

ASTree *validate_empty_expr(ASTree *empty_expr) {
  assert(empty_expr != &EMPTY_EXPR && empty_expr->symbol == ';');
  empty_expr->type = (Type *)TYPE_VOID;
  return translate_empty_expr(empty_expr);
}

ASTree *validate_intcon(ASTree *intcon) { return evaluate_intcon(intcon); }

ASTree *validate_charcon(ASTree *charcon) {
  /* TODO(Robert): casting away const bad but also we need to assign a type here
   * which is not owned by any symbol
   */
  charcon->type = (Type *)TYPE_CHAR;
  return evaluate_charcon(charcon);
}

ASTree *validate_stringcon(ASTree *stringcon) {
  const char *stringcon_label;
  /* this function will emit the necessary directives for the literal */
  size_t static_id = asmgen_literal_label(stringcon->lexinfo, &stringcon_label);

  /* create symbol for string literal; it is more convenient that way */
  /* insert string literal into the symbol table if it's not already there */
  SymbolValue *symval = NULL;
  (void)state_get_symbol(state, stringcon_label, strlen(stringcon_label),
                         &symval);
  if (symval == NULL) {
    symval = symbol_value_init(&stringcon->loc, state_get_sequence(state));
    symval->disp = 0;
    symval->flags = SYMFLAG_LINK_INT | SYMFLAG_STORE_STAT | SYMFLAG_DEFINED;
    symval->static_id = static_id;

    /* subtract 2 for quotes, add one for terminating nul */
    assert(
        !type_init_array(&symval->type, strlen(stringcon->lexinfo) - 2 + 1, 0));

    Type *char_type;
    /* TODO(Robert): Type: not sure if storage class flag is really necessary */
    assert(!type_init_base(
        &char_type, SPEC_FLAG_CHAR | QUAL_FLAG_CONST | STOR_FLAG_STATIC));

    assert(!type_append(symval->type, char_type, 0));
    assert(!state_insert_symbol(state, stringcon_label, strlen(stringcon_label),
                                symval));
  }

  stringcon->type = symval->type;
  return evaluate_stringcon(stringcon);
}

ASTree *validate_ident(ASTree *ident) {
  PFDBG0('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strlen(id_str);
  SymbolValue *symval = NULL;
  (void)state_get_symbol(state, id_str, id_str_len, &symval);
  if (symval) {
    PFDBG1('t', "Assigning %s a symbol", id_str);
    ident->type = symval->type;
    if (!type_is_array(ident->type) && !type_is_function(ident->type) &&
        !type_is_typedef(ident->type) &&
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
  Type *function_type;
  /* first type node is pointer; second is function */
  int status = type_strip_declarator(&function_type, function->type);
  if (status) abort();
  assert(function_type->any.code == TYPE_CODE_FUNCTION);
  /* subtract one since function expression is also a child */
  if (astree_count(call) - 1 < function_type->function.parameters_size) {
    return astree_create_errnode(call, BCC_TERR_INSUFF_PARAMS, 1, call);
  } else if (astree_count(call) > 1) {
    /* make sure to emit constexpr load before all argument instructions */
    ASTree *first_arg = astree_get(call, 1);
    assert(first_arg != NULL && first_arg->first_instr != NULL);
    maybe_load_cexpr(function, first_arg->first_instr);
  } else { /* astree_count(call) == 0 */
    maybe_load_cexpr(function, NULL);
  }
  return translate_call(call);
}

ASTree *validate_arg(ASTree *call, ASTree *arg) {
  if (call->symbol == TOK_TYPE_ERROR || arg->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(call, arg);
  /* functon subtree is the first child of the call node */
  ASTree *function = astree_get(call, 0);
  Type *function_type;
  /* first type should be pointer; next should be function */
  int status = type_strip_declarator(&function_type, function->type);
  if (status) abort();
  assert(function_type->any.code == TYPE_CODE_FUNCTION);
  /* subtract one since function expression is also a child */
  size_t param_index = astree_count(call) - 1;
  if (param_index >= function_type->function.parameters_size) {
    if (type_is_variadic_function(function_type)) {
      PFDBG1('t', "Found variadic function parameter number %lu", param_index);
      maybe_load_cexpr(arg, NULL);
      return astree_adopt(call, 1, arg);
    } else {
      return astree_create_errnode(astree_adopt(call, 1, arg),
                                   BCC_TERR_EXCESS_PARAMS, 1, call);
    }
  }
  PFDBG1('t', "Validating argument %d", param_index);
  Type *param_type = type_param_index(function_type, param_index);
  PFDBG0('t', "Comparing types");
  if (types_assignable(param_type, arg->type, astree_is_const_zero(arg))) {
    maybe_load_cexpr(arg, NULL);
    return astree_adopt(call, 1, arg);
  } else {
    return astree_create_errnode(astree_adopt(call, 1, arg),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, arg, arg->type,
                                 param_type);
  }
}

ASTree *validate_call(ASTree *expr, ASTree *call) {
  if (call->symbol == TOK_TYPE_ERROR || expr->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(call, expr);
  else if (!type_is_function_pointer(expr->type))
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_EXPECTED_FN_PTR, 2, call, expr);

  /* strip pointer */
  Type *function_type, *return_type;
  int status = type_strip_declarator(&function_type, expr->type);
  if (status) abort();
  /* strip function */
  status = type_strip_declarator(&return_type, function_type);
  if (status) abort();
  call->type = return_type;

  return astree_adopt(call, 1, expr);
}

ASTree *validate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident) {
  if (expr->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(va_start_, 2, expr, ident);

  if (!types_assignable(TYPE_VA_LIST_POINTER, expr->type,
                        astree_is_const_zero(expr)))
    return astree_create_errnode(astree_adopt(va_start_, 2, expr, ident),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, va_start_,
                                 TYPE_VA_LIST_POINTER, expr->type);
  va_start_->type = (Type *)TYPE_VOID;
  return translate_va_start(va_start_, expr, ident);
}

ASTree *validate_va_end(ASTree *va_end_, ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(va_end_, expr);

  if (!types_assignable(TYPE_VA_LIST_POINTER, expr->type,
                        astree_is_const_zero(expr)))
    return astree_create_errnode(astree_adopt(va_end_, 1, expr),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, va_end_,
                                 TYPE_VA_LIST_POINTER, expr->type);
  va_end_->type = (Type *)TYPE_VOID;
  return translate_va_end(va_end_, expr);
}

ASTree *validate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name) {
  if (expr->symbol == TOK_TYPE_ERROR || type_name->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(va_arg_, 2, expr, type_name);

  Type *arg_type = astree_get(type_name, 1)->type;
  if (type_is_incomplete(arg_type))
    return astree_create_errnode(astree_adopt(va_arg_, 2, expr, type_name),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, va_arg_,
                                 arg_type);

  if (!types_assignable(TYPE_VA_LIST_POINTER, expr->type,
                        astree_is_const_zero(expr)))
    return astree_create_errnode(astree_adopt(va_arg_, 2, expr, type_name),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, va_arg_,
                                 TYPE_VA_LIST_POINTER, expr->type);
  va_arg_->type = arg_type;
  va_arg_->attributes |= ATTR_EXPR_LVAL;
  return translate_va_arg(va_arg_, expr, type_name);
}

ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  if (condition->symbol == TOK_TYPE_ERROR ||
      true_expr->symbol == TOK_TYPE_ERROR ||
      false_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  } else if (!type_is_scalar(condition->type)) {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_EXPECTED_SCALAR, 2, qmark, condition);
  }

  if (type_is_arithmetic(true_expr->type) &&
      type_is_arithmetic(false_expr->type)) {
    int status = type_arithmetic_conversions(&qmark->type, true_expr->type,
                                             false_expr->type);
    if (status) abort();
  } else if ((type_is_struct(true_expr->type) &&
              type_is_struct(false_expr->type)) ||
             (type_is_union(true_expr->type) &&
              type_is_union(false_expr->type)) ||
             (type_is_void(true_expr->type) &&
              type_is_void(false_expr->type))) {
    if (types_equivalent(true_expr->type, false_expr->type, 1, 1)) {
      qmark->type = true_expr->type;
    } else {
      return astree_create_errnode(
          astree_adopt(qmark, 3, condition, true_expr, false_expr),
          BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
          false_expr->type);
    }
  } else if (type_is_pointer(true_expr->type) &&
             astree_is_const_zero(false_expr)) {
    /* TODO(Robert): this will cause a double free when `astree_destroy` is
     * called because it assumes that a new pointer type is created, like it
     * is when its second and third operands are both pointers.
     */
    qmark->type = true_expr->type;
  } else if (astree_is_const_zero(true_expr) &&
             type_is_pointer(false_expr->type)) {
    qmark->type = false_expr->type;
  } else if ((type_is_pointer(true_expr->type) &&
              type_is_pointer(false_expr->type)) &&
             (type_is_void_pointer(true_expr->type) ||
              type_is_void_pointer(false_expr->type) ||
              types_equivalent(true_expr->type, false_expr->type, 1, 1))) {
    Type *common_type;
    int status = type_common_qualified_pointer(&common_type, true_expr->type,
                                               false_expr->type);
    if (status) abort();
    qmark->type = common_type;
  } else {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
        false_expr->type);
  }

  return evaluate_conditional(qmark, condition, true_expr, false_expr);
}

ASTree *validate_comma(ASTree *comma, ASTree *left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left, right);
  }

  comma->type = right->type;
  maybe_load_cexpr(right, NULL);
  return translate_comma(comma, left, right);
}

ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  if (declaration->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  } else if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  }

  ASTree *type_name = astree_get(declaration, 1);
  /* TODO(Robert): allow cast to void */
  if (!(type_is_scalar(type_name->type) || type_is_void(type_name->type)) ||
      !type_is_scalar(expr->type)) {
    return astree_create_errnode(astree_adopt(cast, 2, declaration, expr),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, cast,
                                 type_name->type, expr->type);
  } else {
    cast->type = type_name->type;
    return evaluate_cast(astree_adopt(cast, 1, declaration), expr);
  }
}

static int subtraction_type(Type **out, Type *left_type, Type *right_type) {
  if (type_is_arithmetic(left_type) && type_is_arithmetic(right_type)) {
    return type_arithmetic_conversions(out, left_type, right_type);
  } else if (type_is_pointer(left_type) && type_is_integral(right_type)) {
    return *out = left_type, 0;
  } else if (type_is_pointer(left_type) && type_is_pointer(right_type) &&
             types_equivalent(left_type, right_type, 1, 1)) {
    /* TODO(Robert): dedicated pointer difference type constant */
    /* TODO(Robert): Type: is return a casted const okay here? */
    return *out = (Type *)TYPE_LONG, 0;
  } else {
    return *out = NULL, -1;
  }
}

static int addition_type(Type **out, Type *left_type, Type *right_type) {
  if (type_is_arithmetic(left_type) && type_is_arithmetic(right_type)) {
    return type_arithmetic_conversions(out, left_type, right_type);
  } else if (type_is_pointer(left_type) && type_is_integral(right_type)) {
    return *out = left_type, 0;
  } else if (type_is_integral(left_type) && type_is_pointer(right_type)) {
    return *out = right_type, 0;
  } else {
    return *out = NULL, -1;
  }
}

ASTree *validate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }

  int status = operator->symbol == '-'
                   ? subtraction_type(&operator->type, left->type, right->type)
                   : addition_type(&operator->type, left->type, right->type);

  if (status || operator->type == NULL) {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                 left->type, right->type);
  } else {
    return evaluate_binop(operator, left, right);
  }
}

ASTree *validate_logical(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->symbol == TOK_TYPE_ERROR || right->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }

  if (type_is_scalar(left->type) && type_is_scalar(right->type)) {
    operator->type =(Type *) TYPE_INT;
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

  operator->type =(Type *) TYPE_INT;

  if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type)) {
    return evaluate_binop(operator, left, right);
  } else if (type_is_pointer(left->type) && type_is_pointer(right->type)) {
    if (types_equivalent(left->type, right->type, 1, 1)) {
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

  operator->type =(Type *) TYPE_INT;

  if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type)) {
    return evaluate_binop(operator, left, right);
  } else if (type_is_pointer(left->type) && type_is_pointer(right->type)) {
    if (types_equivalent(left->type, right->type, 1, 1)) {
      return evaluate_binop(operator, left, right);
    } else if (type_is_void_pointer(left->type) ||
               type_is_void_pointer(right->type)) {
      return evaluate_binop(operator, left, right);
    } else {
      return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, operator,
                                   left->type, right->type);
    }
  } else if (type_is_pointer(right->type) && astree_is_const_zero(left)) {
    return evaluate_binop(operator, left, right);
  } else if (type_is_pointer(left->type) && astree_is_const_zero(right)) {
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
  if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type)) {
    int status =
        type_arithmetic_conversions(&operator->type, left->type, right->type);
    if (status) abort();
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
  if (type_is_integral(left->type) && type_is_integral(right->type)) {
    int status = type_arithmetic_conversions(&operator->type, left->type,
                                             (Type *)TYPE_INT);
    if (status) abort();
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
  if (type_is_integral(left->type) && type_is_integral(right->type)) {
    int status =
        type_arithmetic_conversions(&operator->type, left->type, right->type);
    if (status) abort();
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
    /* TODO(Robert): separate error for arrays */
  } else if (!(operand->attributes & ATTR_EXPR_LVAL) ||
             type_is_array(operand->type)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_LVAL, 2, operator, operand);
  }

  if (type_is_pointer(operand->type)) {
    operator->type = operand->type;
    maybe_load_cexpr(operand, NULL);
    return operator->symbol == TOK_POST_INC || operator->symbol == TOK_POST_DEC
               ? translate_post_inc_dec(operator, operand)
               : translate_inc_dec(operator, operand);
  } else if (type_is_arithmetic(operand->type)) {
    int status = type_arithmetic_conversions(&operator->type, operand->type,
                                             (Type *)TYPE_INT);
    if (status) abort();
    maybe_load_cexpr(operand, NULL);
    return operator->symbol == TOK_POST_INC || operator->symbol == TOK_POST_DEC
               ? translate_post_inc_dec(operator, operand)
               : translate_inc_dec(operator, operand);
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

  operator->type =(Type *) TYPE_INT;
  if (type_is_scalar(operand->type)) {
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

  if (type_is_integral(operand->type)) {
    int status = type_arithmetic_conversions(&operator->type, operand->type,
                                             (Type *)TYPE_INT);
    if (status) abort();
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

  if (type_is_arithmetic(operand->type)) {
    int status = type_arithmetic_conversions(&operator->type, operand->type,
                                             (Type *)TYPE_INT);
    if (status) abort();
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 2, operator,
                                 operand);
  }
}

ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  if (operand->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(indirection, operand);
  }

  if (type_is_pointer(operand->type)) {
    int status = type_strip_declarator(&indirection->type, operand->type);
    if (status) abort();
    indirection->attributes |= ATTR_EXPR_LVAL;
    maybe_load_cexpr(operand, NULL);
    return translate_indirection(indirection, operand);
  } else {
    return astree_create_errnode(astree_adopt(indirection, 1, operand),
                                 BCC_TERR_EXPECTED_POINTER, 2, indirection,
                                 operand);
  }
}

ASTree *validate_addrof(ASTree *addrof, ASTree *operand) {
  if (operand->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(addrof, operand);
  if (!(operand->attributes & ATTR_EXPR_LVAL) && !type_is_array(operand->type))
    return astree_create_errnode(astree_adopt(addrof, 1, operand),
                                 BCC_TERR_EXPECTED_LVAL, 2, addrof, operand);

  int status = type_init_pointer(&addrof->type, QUAL_FLAG_NONE);
  if (status) abort();
  status = type_append(addrof->type, operand->type, 0);
  if (status) abort();
  return evaluate_addrof(addrof, operand);
}

ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node) {
  Type *type;
  if (type_node->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(sizeof_, type_node);
  } else if (type_node->symbol == TOK_DECLARATION) {
    type = astree_get(type_node, 1)->type;
  } else {
    type = type_node->type;
  }

  if (type_is_incomplete(type)) {
    return astree_create_errnode(astree_adopt(sizeof_, 1, type_node),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, sizeof_, type);
  }
  /* TODO(Robert): compute actual size and also probably make sure that this
   * is actually the correct type name for the output of sizeof on this
   * platform
   */
  sizeof_->type = (Type *)TYPE_UNSIGNED_LONG;
  return evaluate_unop(sizeof_, type_node);
}

ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  if (pointer->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  else if (index->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(subscript, 2, pointer, index);

  if (!type_is_pointer(pointer->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                                 BCC_TERR_EXPECTED_POINTER, 2, subscript,
                                 pointer);
  } else if (!type_is_integral(index->type)) {
    return astree_create_errnode(astree_adopt(subscript, 2, pointer, index),
                                 BCC_TERR_EXPECTED_INTEGER, 2, subscript,
                                 index);
  } else {
    int status = type_strip_declarator(&subscript->type, pointer->type);
    if (status) abort();
    if (!type_is_array(subscript->type) && !type_is_function(subscript->type)) {
      subscript->attributes |= ATTR_EXPR_LVAL;
    }
    return evaluate_subscript(subscript, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_, ASTree *member) {
  if (struct_->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(reference, 2, struct_, member);

  Type *tag_type;
  if (reference->symbol == TOK_ARROW) {
    if (!type_is_struct_pointer(struct_->type) &&
        !type_is_union_pointer(struct_->type))
      return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                   BCC_TERR_EXPECTED_TAG_PTR, 2, reference,
                                   struct_->type);
    int status = type_strip_declarator(&tag_type, struct_->type);
    if (status) abort();
  } else {
    if (!type_is_struct(struct_->type) && !type_is_union(struct_->type))
      return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                   BCC_TERR_EXPECTED_TAG, 2, reference,
                                   struct_->type);
    tag_type = struct_->type;
  }

  SymbolValue *member_symbol = type_member_name(tag_type, member->lexinfo);

  if (member_symbol == NULL) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                 BCC_TERR_SYM_NOT_FOUND, 1, member);
  } else {
    reference->type = member_symbol->type;
    /* TODO(Robert): while technically correct, shouldn't it be impossible for
     * the member to have function type?
     */
    if (!type_is_array(reference->type) && !type_is_function(reference->type)) {
      reference->attributes |= ATTR_EXPR_LVAL;
    }
    return evaluate_reference(reference, struct_, member);
  }
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  if (dest->symbol == TOK_TYPE_ERROR || src->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(assignment, 2, dest, src);

  /* TODO(Robert): this first `if` clause shouldn't be as comprehensive as it
   * is; arrays and functions should already not be marked as lvalues assuming
   * that the compiler is behaving correctly
   */
  if (type_is_array(dest->type) || type_is_function(dest->type) ||
      type_is_incomplete(dest->type) || type_is_const(dest->type)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else if (!(dest->attributes & ATTR_EXPR_LVAL)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else {
    switch (assignment->symbol) {
      int status;
      Type *result_type;
    incompatible:
      return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, assignment,
                                   dest->type, src->type);
      case TOK_ADDEQ:
        status = addition_type(&result_type, dest->type, src->type);
        if (status || result_type == NULL ||
            !types_assignable(dest->type, result_type,
                              astree_is_const_zero(src)))
          goto incompatible;
        break;
      case TOK_SUBEQ:
        status = subtraction_type(&result_type, dest->type, src->type);
        if (status || result_type == NULL ||
            !types_assignable(dest->type, result_type,
                              astree_is_const_zero(src)))
          goto incompatible;
        break;
      case TOK_MULEQ:
        /* fallthrough */
      case TOK_DIVEQ:
        /* fallthrough */
      case TOK_REMEQ:
        if (type_is_arithmetic(dest->type) && type_is_arithmetic(src->type))
          break;
        goto incompatible;
      case TOK_ANDEQ:
        /* fallthrough */
      case TOK_OREQ:
        /* fallthrough */
      case TOK_XOREQ:
        /* fallthrough */
      case TOK_SHREQ:
        /* fallthrough */
      case TOK_SHLEQ:
        if (type_is_integral(dest->type) && type_is_integral(src->type)) break;
        goto incompatible;
      case '=':
        if (types_assignable(dest->type, src->type, astree_is_const_zero(src)))
          break;
        goto incompatible;
      default:
        abort();
    }
    assignment->type = dest->type;
    maybe_load_cexpr(src, NULL);
    maybe_load_cexpr(dest, src->first_instr);
    return translate_assignment(assignment, dest, src);
  }
}
