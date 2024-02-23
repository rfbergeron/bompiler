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
  assert(empty_expr != &EMPTY_EXPR && empty_expr->tok_kind == ';');
  empty_expr->type = (Type *)TYPE_VOID;
  return translate_empty_expr(empty_expr);
}

ASTree *validate_intcon(ASTree *intcon) { return evaluate_intcon(intcon); }

ASTree *validate_charcon(ASTree *charcon) {
  charcon->type = (Type *)TYPE_CHAR;
  return evaluate_charcon(charcon);
}

ASTree *validate_stringcon(ASTree *stringcon) {
  const char *stringcon_label;
  /* this function will emit the necessary directives for the literal */
  size_t static_id = asmgen_literal_label(stringcon->lexinfo, &stringcon_label);

  /* create symbol for string literal; it is more convenient that way */
  /* insert string literal into the symbol table if it's not already there */
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, stringcon_label, strlen(stringcon_label),
                         &symbol);
  if (symbol == NULL) {
    symbol = symbol_init(&stringcon->loc, state_get_sequence(state));
    symbol->disp = 0;
    symbol->flags = SYMFLAG_LINK_INT | SYMFLAG_STORE_STAT | SYMFLAG_DEFINED;
    symbol->static_id = static_id;

    /* subtract 2 for quotes, add one for terminating nul */
    symbol->type = type_init_array(strlen(stringcon->lexinfo) - 2 + 1, 0);

    Type *char_type =
        type_init_base(SPEC_FLAG_CHAR | QUAL_FLAG_CONST | STOR_FLAG_STATIC);

    (void)type_append(symbol->type, char_type, 0);
    state_insert_symbol(state, stringcon_label, strlen(stringcon_label),
                        symbol);
  }

  stringcon->type = symbol->type;
  return evaluate_stringcon(stringcon);
}

ASTree *validate_ident(ASTree *ident) {
  PFDBG0('t', "Attempting to assign a type");
  const char *id_str = ident->lexinfo;
  size_t id_str_len = strlen(id_str);
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, id_str, id_str_len, &symbol);
  if (symbol) {
    PFDBG1('t', "Assigning %s a symbol", id_str);
    ident->type = symbol->type;
    if (symbol_is_lvalue(symbol)) ident->attributes |= ATTR_EXPR_LVAL;
    return evaluate_ident(ident);
  } else {
    return astree_create_errnode(ident, BCC_TERR_SYM_NOT_FOUND, 1, ident);
  }
}

ASTree *finalize_call(ASTree *call) {
  if (call->tok_kind == TOK_TYPE_ERROR) {
    return call;
  }
  ASTree *function = astree_get(call, 0);
  /* first type node is pointer; second is function */
  Type *function_type = type_strip_declarator(function->type);
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
  if (call->tok_kind == TOK_TYPE_ERROR || arg->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(call, arg);
  /* functon subtree is the first child of the call node */
  ASTree *function = astree_get(call, 0);
  /* first type should be pointer; next should be function */
  Type *function_type = type_strip_declarator(function->type);
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
  if (call->tok_kind == TOK_TYPE_ERROR || expr->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(call, expr);
  else if (!type_is_function_pointer(expr->type))
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_EXPECTED_FN_PTR, 2, call, expr);

  /* strip pointer */
  Type *function_type = type_strip_declarator(expr->type);
  /* strip function */
  Type *return_type = type_strip_declarator(function_type);
  call->type = return_type;

  if (!type_is_void(return_type) && type_is_incomplete(return_type))
    return astree_create_errnode(astree_adopt(call, 1, expr),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, call, call->type);

  return astree_adopt(call, 1, expr);
}

ASTree *validate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident) {
  if (expr->tok_kind == TOK_TYPE_ERROR)
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
  if (expr->tok_kind == TOK_TYPE_ERROR)
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
  if (expr->tok_kind == TOK_TYPE_ERROR || type_name->tok_kind == TOK_TYPE_ERROR)
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
  return translate_va_arg(va_arg_, expr, type_name);
}

ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr) {
  if (condition->tok_kind == TOK_TYPE_ERROR ||
      true_expr->tok_kind == TOK_TYPE_ERROR ||
      false_expr->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(qmark, 3, condition, true_expr,
                                      false_expr);
  } else if (!type_is_scalar(condition->type)) {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_EXPECTED_SCALAR, 2, qmark, condition);
  }

  /* NOTE: whenever the result type is a pointer, the pointer type information,
   * not the element type information, must be copied and not assigned, because
   * the result may be a common qualified pointer type and determining whether
   * or not this is the case at destruction time is harder than making
   * unnecessary copies
   */
  if (type_is_arithmetic(true_expr->type) &&
      type_is_arithmetic(false_expr->type)) {
    qmark->type =
        type_arithmetic_conversions(true_expr->type, false_expr->type);
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
    /* duplicate only the pointer type information */
    qmark->type = malloc(sizeof(*qmark->type));
    *(qmark->type) = *(true_expr->type);
  } else if (astree_is_const_zero(true_expr) &&
             type_is_pointer(false_expr->type)) {
    /* duplicate only the pointer type information */
    qmark->type = malloc(sizeof(*qmark->type));
    *(qmark->type) = *(false_expr->type);
  } else if ((type_is_pointer(true_expr->type) &&
              type_is_pointer(false_expr->type)) &&
             (type_is_void_pointer(true_expr->type) ||
              type_is_void_pointer(false_expr->type) ||
              types_equivalent(true_expr->type, false_expr->type, 1, 1))) {
    qmark->type =
        type_common_qualified_pointer(true_expr->type, false_expr->type);
  } else {
    return astree_create_errnode(
        astree_adopt(qmark, 3, condition, true_expr, false_expr),
        BCC_TERR_INCOMPATIBLE_TYPES, 3, qmark, true_expr->type,
        false_expr->type);
  }

  return evaluate_conditional(qmark, condition, true_expr, false_expr);
}

ASTree *validate_comma(ASTree *comma, ASTree *left, ASTree *right) {
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(comma, 2, left, right);
  }

  comma->type = right->type;
  return evaluate_comma(comma, left, right);
}

/* TODO(Robert): allow casting void to void */
ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr) {
  if (declaration->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  } else if (expr->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(cast, 2, declaration, expr);
  }

  ASTree *type_name = astree_get(declaration, 1);
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
    *out = type_arithmetic_conversions(left_type, right_type);
    return 0;
  } else if (type_is_pointer(left_type) && type_is_integral(right_type)) {
    return *out = left_type, 0;
  } else if (type_is_pointer(left_type) && type_is_pointer(right_type) &&
             types_equivalent(left_type, right_type, 1, 1)) {
    /* TODO(Robert): dedicated pointer difference type constant? */
    return *out = (Type *)TYPE_LONG, 0;
  } else {
    return *out = NULL, -1;
  }
}

static int addition_type(Type **out, Type *left_type, Type *right_type) {
  if (type_is_arithmetic(left_type) && type_is_arithmetic(right_type)) {
    *out = type_arithmetic_conversions(left_type, right_type);
    return 0;
  } else if (type_is_pointer(left_type) && type_is_integral(right_type)) {
    return *out = left_type, 0;
  } else if (type_is_integral(left_type) && type_is_pointer(right_type)) {
    return *out = right_type, 0;
  } else {
    return *out = NULL, -1;
  }
}

ASTree *validate_addition(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }

  int status = operator->tok_kind == '-'
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
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
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
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
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
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
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
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type)) {
    operator->type = type_arithmetic_conversions(left->type, right->type);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 3, operator,
                                 left, right);
  }
}

ASTree *validate_shift(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (type_is_integral(left->type) && type_is_integral(right->type)) {
    operator->type = type_arithmetic_conversions(left->type, (Type *)TYPE_INT);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *validate_bitwise(ASTree *operator, ASTree * left, ASTree *right) {
  if (left->tok_kind == TOK_TYPE_ERROR || right->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(operator, 2, left, right);
  }
  if (type_is_integral(left->type) && type_is_integral(right->type)) {
    operator->type = type_arithmetic_conversions(left->type, right->type);
    return evaluate_binop(operator, left, right);
  } else {
    return astree_create_errnode(astree_adopt(operator, 2, left, right),
                                 BCC_TERR_EXPECTED_INTEGER, 3, operator, left,
                                 right);
  }
}

ASTree *validate_increment(ASTree *operator, ASTree * operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  } else if (!(operand->attributes & ATTR_EXPR_LVAL)) {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_LVAL, 2, operator, operand);
  }

  if (type_is_pointer(operand->type)) {
    operator->type = operand->type;
    maybe_load_cexpr(operand, NULL);
    return operator->tok_kind == TOK_POST_INC || operator->tok_kind ==
               TOK_POST_DEC
               ? translate_post_inc_dec(operator, operand)
               : translate_inc_dec(operator, operand);
  } else if (type_is_arithmetic(operand->type)) {
    operator->type =
        type_arithmetic_conversions(operand->type, (Type *)TYPE_INT);
    maybe_load_cexpr(operand, NULL);
    return operator->tok_kind == TOK_POST_INC || operator->tok_kind ==
               TOK_POST_DEC
               ? translate_post_inc_dec(operator, operand)
               : translate_inc_dec(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_SCALAR, 2, operator,
                                 operand);
  }
}

static ASTree *validate_not(ASTree *operator, ASTree * operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR) {
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

static ASTree *validate_complement(ASTree *operator, ASTree * operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }

  if (type_is_integral(operand->type)) {
    operator->type =
        type_arithmetic_conversions(operand->type, (Type *)TYPE_INT);
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_INTEGER, 2, operator,
                                 operand);
  }
}

static ASTree *validate_negation(ASTree *operator, ASTree * operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(operator, operand);
  }

  if (type_is_arithmetic(operand->type)) {
    operator->type =
        type_arithmetic_conversions(operand->type, (Type *)TYPE_INT);
    return evaluate_unop(operator, operand);
  } else {
    return astree_create_errnode(astree_adopt(operator, 1, operand),
                                 BCC_TERR_EXPECTED_ARITHMETIC, 2, operator,
                                 operand);
  }
}

static ASTree *validate_indirection(ASTree *indirection, ASTree *operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(indirection, operand);
  }

  if (type_is_pointer(operand->type)) {
    indirection->type = type_strip_declarator(operand->type);
    if (type_is_object(indirection->type))
      indirection->attributes |= ATTR_EXPR_LVAL;
    maybe_load_cexpr(operand, NULL);
    return translate_indirection(indirection, operand);
  } else {
    return astree_create_errnode(astree_adopt(indirection, 1, operand),
                                 BCC_TERR_EXPECTED_POINTER, 2, indirection,
                                 operand);
  }
}

static ASTree *validate_addrof(ASTree *addrof, ASTree *operand) {
  if (operand->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode(addrof, operand);
  if (!(operand->attributes & ATTR_EXPR_LVAL))
    return astree_create_errnode(astree_adopt(addrof, 1, operand),
                                 BCC_TERR_EXPECTED_LVAL, 2, addrof, operand);

  addrof->type = type_init_pointer(QUAL_FLAG_NONE);
  (void)type_append(addrof->type, operand->type, 0);
  return evaluate_addrof(addrof, operand);
}

ASTree *validate_unary(ASTree *unary, ASTree *operand) {
  switch (unary->tok_kind) {
    case '!':
      return validate_not(unary, operand);
    case '~':
      return validate_complement(unary, operand);
    case TOK_POS:
      /* fallthrough */
    case TOK_NEG:
      return validate_negation(unary, operand);
    case TOK_INDIRECTION:
      return validate_indirection(unary, operand);
    case TOK_ADDROF:
      return validate_addrof(unary, operand);
    default:
      abort();
  }
}

ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_name) {
  Type *type;
  if (type_name->tok_kind == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(sizeof_, type_name);
  } else if (type_name->tok_kind == TOK_DECLARATION) {
    type = astree_get(type_name, 1)->type;
    if (type_is_function(type)) /* TODO(Robert): better error message */
      return astree_create_errnode(astree_adopt(sizeof_, 1, type_name),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, sizeof_,
                                   type, type);
  } else {
    type = type_name->type;
  }

  if (type_is_incomplete(type)) {
    return astree_create_errnode(astree_adopt(sizeof_, 1, type_name),
                                 BCC_TERR_INCOMPLETE_TYPE, 2, sizeof_, type);
  }

  sizeof_->type = (Type *)TYPE_UNSIGNED_LONG;
  return evaluate_unop(sizeof_, type_name);
}

ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index) {
  if (pointer->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(subscript, 2, pointer, index);
  else if (index->tok_kind == TOK_TYPE_ERROR)
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
    subscript->type = type_strip_declarator(pointer->type);
    if (type_is_object(subscript->type))
      subscript->attributes |= ATTR_EXPR_LVAL;
    return evaluate_subscript(subscript, pointer, index);
  }
}

ASTree *validate_reference(ASTree *reference, ASTree *struct_, ASTree *member) {
  if (struct_->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(reference, 2, struct_, member);

  Type *tag_type;
  if (reference->tok_kind == TOK_ARROW) {
    if (!type_is_struct_pointer(struct_->type) &&
        !type_is_union_pointer(struct_->type))
      return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                   BCC_TERR_EXPECTED_TAG_PTR, 2, reference,
                                   struct_->type);
    tag_type = type_strip_declarator(struct_->type);
  } else {
    if (!type_is_struct(struct_->type) && !type_is_union(struct_->type))
      return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                   BCC_TERR_EXPECTED_TAG, 2, reference,
                                   struct_->type);
    tag_type = struct_->type;
  }

  Symbol *member_symbol = type_member_name(tag_type, member->lexinfo);

  if (member_symbol == NULL) {
    return astree_create_errnode(astree_adopt(reference, 2, struct_, member),
                                 BCC_TERR_SYM_NOT_FOUND, 1, member);
  } else {
    reference->type = member_symbol->type;
    if (type_is_object(reference->type))
      reference->attributes |= ATTR_EXPR_LVAL;
    return evaluate_reference(reference, struct_, member);
  }
}

ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src) {
  if (dest->tok_kind == TOK_TYPE_ERROR || src->tok_kind == TOK_TYPE_ERROR)
    return astree_propogate_errnode_v(assignment, 2, dest, src);

  /* TODO(Robert): check if struct and union members/submembers are
   * const-qualified
   */
  if (type_is_incomplete(dest->type) || type_is_const(dest->type)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else if (!(dest->attributes & ATTR_EXPR_LVAL)) {
    return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                 BCC_TERR_EXPECTED_LVAL, 2, assignment, dest);
  } else {
    switch (assignment->tok_kind) {
    incompatible:
      return astree_create_errnode(astree_adopt(assignment, 2, dest, src),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, assignment,
                                   dest->type, src->type);
      case TOK_ADDEQ:
        /* fallthrough */
      case TOK_SUBEQ:
        if ((type_is_arithmetic(dest->type) && type_is_arithmetic(src->type)) ||
            (type_is_pointer(dest->type) && type_is_integral(src->type)))
          break;
        goto incompatible;
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
    /* `translate_assignment` does not perform adoption because it is reused
     * within `asmgen.c` for initialization. it also cannot return error nodes
     */
    return astree_adopt(translate_assignment(assignment, dest, src), 2, dest,
                        src);
  }
}
