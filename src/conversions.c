#include "conversions.h"

#include <assert.h>

#include "asmgen.h"
#include "astree.h"
#include "bcc_err.h"
#include "evaluate.h"

ASTree *tchk_scal_conv(ASTree *expr, Type *type) {
  assert(expr->type != NULL && type != NULL);
  assert(!(expr->attributes & ATTR_EXPR_LVAL));
  if (types_equivalent(type, expr->type, 1, 1)) {
    return expr;
  } else {
    assert(type_is_scalar(type) && type_is_scalar(expr->type));
    ASTree *scal_conv = astree_init(TOK_SCAL_CONV, expr->loc, "_scal_conv");
    scal_conv->type = type;
    return evaluate_scal_conv(scal_conv, expr);
  }
}

ASTree *tchk_disp_conv(ASTree *expr, const Type *pointer_type) {
  assert(expr->type != NULL && pointer_type != NULL);
  assert(!(expr->attributes & ATTR_EXPR_LVAL));
  assert(type_is_integral(expr->type));
  ASTree *disp_conv = astree_init(TOK_DISP_CONV, expr->loc, "_disp_conv");
  disp_conv->type = (Type *)TYPE_LONG;
  return evaluate_disp_conv(disp_conv, expr, pointer_type);
}

ASTree *tchk_diff_conv(ASTree *expr, const Type *pointer_type) {
  assert(expr->type == TYPE_LONG);
  assert(pointer_type != NULL && type_is_pointer(pointer_type));
  ASTree *diff_conv = astree_init(TOK_DIFF_CONV, expr->loc, "_diff_conv");
  diff_conv->type = (Type *)TYPE_LONG;
  return evaluate_diff_conv(diff_conv, expr, pointer_type);
}

ASTree *tchk_ptr_conv(ASTree *expr, int conv_arr) {
  if (expr->tok_kind != TOK_EMPTY && expr->type != NULL &&
      (type_is_function(expr->type) ||
       (conv_arr && type_is_array(expr->type)))) {
    assert(!(expr->attributes & ATTR_EXPR_LVAL));
    ASTree *ptr_conv = astree_init(TOK_PTR_CONV, expr->loc, "_ptr_conv");
    ptr_conv->type = type_pointer_conversions(expr->type);
    return evaluate_ptr_conv(ptr_conv, expr);
  } else {
    return expr;
  }
}

ASTree *tchk_rval_conv(ASTree *expr) {
  if (expr->tok_kind != TOK_EMPTY && expr->type != NULL &&
      (expr->attributes & ATTR_EXPR_LVAL)) {
    ASTree *rval_conv = astree_init(TOK_RVAL_CONV, expr->loc, "_rval_conv");
    rval_conv->type = expr->type;
    assert((expr->attributes & ATTR_MASK_CONST) < ATTR_CONST_INIT);
    expr = tchk_cexpr_conv(expr);
    /* no need to evaluate; result cannot be a constant expression */
    return translate_rval_conv(rval_conv, expr);
  } else {
    return expr;
  }
}

ASTree *tchk_cexpr_conv(ASTree *expr) {
  assert(expr->type != NULL);
  if ((expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_NONE) {
    ASTree *cexpr_conv = astree_init(TOK_CEXPR_CONV, expr->loc, "_cexpr_conv");
    cexpr_conv->type = expr->type;
    cexpr_conv->attributes |= expr->attributes & ATTR_EXPR_LVAL;
    return translate_cexpr_conv(cexpr_conv, expr);
  } else {
    assert(!instr_empty(expr->instructions));
    return expr;
  }
}
