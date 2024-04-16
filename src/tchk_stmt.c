#include "tchk_stmt.h"

#include "asmgen.h"
#include "assert.h"
#include "bcc_err.h"
#include "conversions.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  expr = TCHK_STD_CONV(expr, 1, NULL);
  Symbol *symbol = state_get_function(state);
  Type *return_type = type_strip_declarator(symbol->type);

  if (expr->tok_kind != TOK_EMPTY) {
    if (types_assignable(return_type, expr->type, astree_is_const_zero(expr))) {
      expr = tchk_cexpr_conv(tchk_scal_conv(expr, return_type), NULL);
      return translate_return(ret, expr);
    } else if (type_is_void(expr->type) && type_is_void(return_type)) {
      return translate_return(ret, expr);
    } else {
      (void)semerr_incompatible_types(ret, return_type, expr->type);
      return translate_empty_expr(astree_adopt(ret, 1, expr));
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
  condition = TCHK_STD_CONV(condition, 1, if_body->first_instr);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(ifelse, condition->type);
    return astree_adopt(ifelse, 3, condition, if_body, else_body);
  } else {
    condition = tchk_cexpr_conv(condition, if_body->first_instr);
    return translate_ifelse(ifelse, condition, if_body, else_body);
  }
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  return translate_switch(switch_, expr, stmt);
}

ASTree *validate_switch_expr(ASTree *expr) {
  expr = TCHK_STD_CONV(expr, 1, NULL);
  if (!type_is_integral(expr->type)) {
    (void)semerr_expected_integral(expr, expr->type);
    return expr;
  } else {
    Type *promoted_type =
        type_arithmetic_conversions(expr->type, (Type *)TYPE_INT);
    state_set_control_type(state, promoted_type);
    expr = tchk_cexpr_conv(tchk_scal_conv(expr, promoted_type), NULL);
    return expr;
  }
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  /* TODO(Robert): safely process flow control statements before checking error
   * codes so that more things are cleaned up in the event of an error.
   */
  condition = TCHK_STD_CONV(condition, 1, stmt->first_instr);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(while_, condition->type);
    return astree_adopt(while_, 2, condition, stmt);
  } else {
    condition = tchk_cexpr_conv(condition, stmt->first_instr);
    return translate_while(while_, condition, stmt);
  }
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  condition = TCHK_STD_CONV(condition, 1, NULL);
  if (!type_is_scalar(condition->type)) {
    (void)semerr_expected_scalar(do_, condition->type);
    return astree_adopt(do_, 2, stmt, condition);
  } else {
    condition = tchk_cexpr_conv(condition, NULL);
    return translate_do(do_, stmt, condition);
  }
}

ASTree *validate_for(ASTree *for_, ASTree *init_expr, ASTree *pre_iter_expr,
                     ASTree *reinit_expr, ASTree *body) {
  ListIter *anchor_iter = body->first_instr;
  assert(anchor_iter != NULL);
  if (reinit_expr->tok_kind != TOK_EMPTY)
    reinit_expr = TCHK_STD_CONV(reinit_expr, 1, anchor_iter);
  if (reinit_expr->first_instr != NULL) anchor_iter = reinit_expr->first_instr;
  if (pre_iter_expr->tok_kind != TOK_EMPTY)
    pre_iter_expr = TCHK_STD_CONV(pre_iter_expr, 1, anchor_iter);
  if (pre_iter_expr->first_instr != NULL)
    anchor_iter = pre_iter_expr->first_instr;
  if (init_expr->tok_kind != TOK_EMPTY)
    init_expr = TCHK_STD_CONV(init_expr, 1, anchor_iter);

  if (pre_iter_expr->tok_kind != TOK_EMPTY &&
      !type_is_scalar(pre_iter_expr->type)) {
    (void)semerr_expected_scalar(for_, pre_iter_expr->type);
    return astree_adopt(for_, 4, init_expr, pre_iter_expr, reinit_expr, body);
  } else {
    reinit_expr = tchk_cexpr_conv(reinit_expr, body->first_instr);
    pre_iter_expr = tchk_cexpr_conv(pre_iter_expr, reinit_expr->first_instr);
    init_expr = tchk_cexpr_conv(init_expr, pre_iter_expr->first_instr);
    return translate_for(for_, init_expr, pre_iter_expr, reinit_expr, body);
  }
}

ASTree *validate_label(ASTree *label_node, ASTree *ident_node, ASTree *stmt) {
  const char *ident = ident_node->lexinfo;
  size_t ident_len = strlen(ident);
  Label *existing_entry = state_get_label(state, ident, ident_len);
  if (existing_entry == NULL) {
    Label *label = malloc(sizeof(*label));
    label->tree = ident_node;
    label->defined = 1;
    state_insert_label(state, ident, ident_len, label);
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
  expr = tchk_cexpr_conv(tchk_scal_conv(expr, (Type *)control_type),
                         stmt->first_instr);
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
  size_t ident_str_len = strlen(ident_str);
  Label *existing_entry = state_get_label(state, ident_str, ident_str_len);
  if (!existing_entry) {
    Label *label = malloc(sizeof(*label));
    label->tree = ident;
    label->defined = 0;
    state_insert_label(state, ident_str, ident_str_len, label);
  }
  return translate_goto(goto_, ident);
}

ASTree *validate_continue(ASTree *continue_) {
  if (continue_->jump_id == SIZE_MAX) {
    (void)semerr_unexpected_stmt(continue_);
    return continue_;
  } else {
    return translate_continue(continue_);
  }
}

ASTree *validate_break(ASTree *break_) {
  if (break_->jump_id == SIZE_MAX) {
    (void)semerr_unexpected_stmt(break_);
    return break_;
  } else {
    return translate_break(break_);
  }
}

ASTree *validate_block(ASTree *block) {
  block->symbol_table = symbol_table_init(TABLE_BLOCK);
  state_push_table(state, block->symbol_table);
  return block;
}

ASTree *validate_block_content(ASTree *block, ASTree *block_content) {
  if (block_content->tok_kind == TOK_DECLARATION)
    end_local_decls(block_content);
  return astree_adopt(block, 1, block_content);
}

ASTree *finalize_block(ASTree *block) {
  state_pop_table(state);
  return translate_block(block);
}
