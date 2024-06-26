#include "tchk_stmt.h"

#include "asmgen.h"
#include "assert.h"
#include "bcc_err.h"
#include "conversions.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  expr = TCHK_STD_CONV(expr, 1);
  Symbol *symbol = state_get_function(state);
  Type *return_type = type_strip_declarator(symbol->type);

  if (expr->tok_kind != TOK_EMPTY) {
    if (types_assignable(return_type, expr->type, astree_is_const_zero(expr))) {
      expr = tchk_cexpr_conv(tchk_scal_conv(expr, return_type));
      return translate_return(ret, expr);
    } else if (type_is_void(expr->type) && type_is_void(return_type)) {
      return translate_return(ret, expr);
    } else {
      (void)semerr_incompatible_types(ret, return_type, expr->type);
      return astree_adopt(ret, 1, expr);
    }
  } else if (type_is_void(return_type)) {
    return translate_return(ret, expr);
  } else {
    (void)semerr_expected_retval(ret, return_type);
    return astree_adopt(ret, 1, expr);
  }
}

ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body) {
  condition = TCHK_STD_CONV(condition, 1);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(ifelse, condition->type);
    return astree_adopt(ifelse, 3, condition, if_body, else_body);
  } else {
    condition = tchk_cexpr_conv(condition);
    return translate_ifelse(ifelse, condition, if_body, else_body);
  }
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  assert(state_get_selection_id(state) == switch_->jump_id);
  assert(state_get_break_id(state) == switch_->jump_id);
  int has_default = state_get_selection_default(state);
  /* fake case id for final fallthrough */
  switch_->case_id = state_get_case_id(state);
  state_pop_selection_id(state);
  state_pop_break_id(state);
  return translate_switch(switch_, expr, stmt, has_default);
}

ASTree *validate_switch_expr(ASTree *expr) {
  expr = TCHK_STD_CONV(expr, 1);
  if (!type_is_integral(expr->type)) {
    (void)semerr_expected_integral(expr, expr->type);
    return expr;
  } else {
    Type *promoted_type =
        type_arithmetic_conversions(expr->type, (Type *)TYPE_INT);
    state_set_control_type(state, promoted_type);
    return translate_switch_expr(
        tchk_cexpr_conv(tchk_scal_conv(expr, promoted_type)));
  }
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  assert(state_get_continue_id(state) == while_->jump_id);
  assert(state_get_break_id(state) == while_->jump_id);
  state_pop_continue_id(state);
  state_pop_break_id(state);
  condition = TCHK_STD_CONV(condition, 1);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(while_, condition->type);
    return astree_adopt(while_, 2, condition, stmt);
  } else {
    condition = tchk_cexpr_conv(condition);
    return translate_while(while_, condition, stmt);
  }
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  assert(state_get_continue_id(state) == do_->jump_id);
  assert(state_get_break_id(state) == do_->jump_id);
  state_pop_continue_id(state);
  state_pop_break_id(state);
  condition = TCHK_STD_CONV(condition, 1);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(do_, condition->type);
    return astree_adopt(do_, 2, stmt, condition);
  } else {
    condition = tchk_cexpr_conv(condition);
    return translate_do(do_, stmt, condition);
  }
}

ASTree *validate_for(ASTree *for_, ASTree *init_expr, ASTree *pre_iter_expr,
                     ASTree *reinit_expr, ASTree *body) {
  assert(state_get_continue_id(state) == for_->jump_id);
  assert(state_get_break_id(state) == for_->jump_id);
  state_pop_continue_id(state);
  state_pop_break_id(state);
  if (reinit_expr->tok_kind != TOK_EMPTY)
    reinit_expr = TCHK_STD_CONV(reinit_expr, 1);
  if (pre_iter_expr->tok_kind != TOK_EMPTY)
    pre_iter_expr = TCHK_STD_CONV(pre_iter_expr, 1);
  if (init_expr->tok_kind != TOK_EMPTY) init_expr = TCHK_STD_CONV(init_expr, 1);

  if (pre_iter_expr->tok_kind != TOK_EMPTY &&
      !type_is_scalar(pre_iter_expr->type)) {
    (void)semerr_expected_scalar(for_, pre_iter_expr->type);
    return astree_adopt(for_, 4, init_expr, pre_iter_expr, reinit_expr, body);
  } else {
    reinit_expr = tchk_cexpr_conv(reinit_expr);
    pre_iter_expr = tchk_cexpr_conv(pre_iter_expr);
    init_expr = tchk_cexpr_conv(init_expr);
    return translate_for(for_, init_expr, pre_iter_expr, reinit_expr, body);
  }
}

ASTree *validate_label(ASTree *label_node, ASTree *ident_node, ASTree *stmt) {
  const char *ident = ident_node->lexinfo;
  Label *existing_entry = state_get_label(state, ident);
  if (existing_entry == NULL) {
    Label *label = malloc(sizeof(*label));
    label->tree = ident_node;
    label->defined = 1;
    state_insert_label(state, ident, label);
    return translate_label(label_node, ident_node, stmt);
  } else if (existing_entry->defined) {
    (void)semerr_redefine_label(ident_node, existing_entry->tree);
    return astree_adopt(label_node, 2, ident_node, stmt);
  } else {
    existing_entry->tree = ident_node;
    existing_entry->defined = 1;
    return translate_label(label_node, ident_node, stmt);
  }
}

ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  if (case_->jump_id == SIZE_MAX) {
    (void)semerr_unexpected_stmt(case_);
    return astree_adopt(case_, 2, expr, stmt);
  }
  assert(case_->case_id != SIZE_MAX);

  Type *case_const_spec = expr->type;
  if (!type_is_integral(case_const_spec) ||
      (expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
    (void)semerr_expected_intconst(case_, expr);
    return astree_adopt(case_, 2, expr, stmt);
  }

  const Type *control_type = state_get_control_type(state);
  expr = tchk_cexpr_conv(tchk_scal_conv(expr, (Type *)control_type));
  return translate_case(case_, expr, stmt);
}

ASTree *validate_default(ASTree *default_, ASTree *stmt) {
  if (default_->jump_id == SIZE_MAX || state_get_selection_default(state)) {
    (void)semerr_unexpected_stmt(default_);
    return astree_adopt(default_, 1, stmt);
  } else {
    state_set_selection_default(state);
    return translate_default(default_, stmt);
  }
}

ASTree *validate_goto(ASTree *goto_, ASTree *ident) {
  const char *ident_str = ident->lexinfo;
  Label *existing_entry = state_get_label(state, ident_str);
  if (!existing_entry) {
    Label *label = malloc(sizeof(*label));
    label->tree = ident;
    label->defined = 0;
    state_insert_label(state, ident_str, label);
  }
  return translate_goto(goto_, ident);
}

ASTree *validate_continue(ASTree *continue_) {
  continue_->jump_id = state_get_continue_id(state);
  if (continue_->jump_id == SIZE_MAX) {
    (void)semerr_unexpected_stmt(continue_);
    return continue_;
  } else {
    return translate_continue(continue_);
  }
}

ASTree *validate_break(ASTree *break_) {
  break_->jump_id = state_get_break_id(state);
  if (break_->jump_id == SIZE_MAX) {
    (void)semerr_unexpected_stmt(break_);
    return break_;
  } else {
    return translate_break(break_);
  }
}

ASTree *validate_stmt_expr(ASTree *stmt_expr) {
  return translate_stmt_expr(stmt_expr);
}

ASTree *validate_block(ASTree *block) {
  block->scope = scope_init(SCOPE_BLOCK);
  state_enter_block(state, block);
  return block;
}

ASTree *validate_block_content(ASTree *block, ASTree *content) {
  return translate_block_content(block, content);
}

ASTree *finalize_block(ASTree *block) {
  state_leave_block(state);
  return translate_block(block);
}
