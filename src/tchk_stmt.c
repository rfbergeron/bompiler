#include "tchk_stmt.h"

#include "asmgen.h"
#include "assert.h"
#include "bcc_err.h"
#include "state.h"
#include "stdlib.h"
#include "yyparse.h"

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  if (expr->symbol == TOK_TYPE_ERROR)
    return astree_propogate_errnode(ret, expr);
  SymbolValue *symval = state_get_function(state);
  Type *return_type = type_strip_declarator(symval->type);

  if (expr != &EMPTY_EXPR) {
    if (types_assignable(return_type, expr->type, astree_is_const_zero(expr))) {
      maybe_load_cexpr(expr, NULL);
      return translate_return(ret, expr);
    } else if (type_is_void(expr->type) && type_is_void(return_type)) {
      return translate_return(ret, expr);
    } else {
      return astree_create_errnode(astree_adopt(ret, 1, expr),
                                   BCC_TERR_INCOMPATIBLE_TYPES, 3, ret,
                                   &symval->type, expr->type);
    }
  } else if (type_is_void(return_type)) {
    maybe_load_cexpr(expr, NULL);
    return translate_return(ret, expr);
  } else {
    return astree_create_errnode(astree_adopt(ret, 1, expr),
                                 BCC_TERR_EXPECTED_RETVAL, 0);
  }
}

ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body) {
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (if_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (else_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  }

  if (!type_is_scalar(condition->type)) {
    return astree_create_errnode(
        astree_adopt(ifelse, 3, condition, if_body, else_body),
        BCC_TERR_EXPECTED_SCALAR, 2, ifelse, condition);
  }

  maybe_load_cexpr(condition, if_body->first_instr);
  return translate_ifelse(ifelse, condition, if_body, else_body);
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  }

  return translate_switch(switch_, expr, stmt);
}

ASTree *validate_switch_expr(ASTree *expr) {
  if (!type_is_integral(expr->type)) {
    return astree_create_errnode(expr, BCC_TERR_EXPECTED_INTEGER, 1, expr);
  }
  Type *promoted_type =
      type_arithmetic_conversions(expr->type, (Type *)TYPE_INT);
  int status = state_set_control_type(state, promoted_type);
  if (status) abort();
  maybe_load_cexpr(expr, NULL);
  return expr;
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  /* TODO(Robert): safely process flow control statements before checking error
   * codes so that more things are cleaned up in the event of an error.
   */
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  }

  if (!type_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(while_, 2, condition, stmt),
                                 BCC_TERR_EXPECTED_INTEGER, 2, while_,
                                 condition);
  }

  maybe_load_cexpr(condition, stmt->first_instr);
  return translate_while(while_, condition, stmt);
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  }

  if (!type_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(do_, 2, condition, stmt),
                                 BCC_TERR_EXPECTED_INTEGER, 2, do_, condition);
  }

  maybe_load_cexpr(condition, NULL);
  return translate_do(do_, stmt, condition);
}

ASTree *validate_for(ASTree *for_, ASTree *init_expr, ASTree *pre_iter_expr,
                     ASTree *reinit_expr, ASTree *body) {
  if (init_expr->symbol == TOK_TYPE_ERROR ||
      pre_iter_expr->symbol == TOK_TYPE_ERROR ||
      reinit_expr->symbol == TOK_TYPE_ERROR || body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(for_, 4, init_expr, pre_iter_expr,
                                      reinit_expr, body);
  }

  if (pre_iter_expr->symbol != ';') {
    if (!type_is_scalar(pre_iter_expr->type)) {
      return astree_create_errnode(
          astree_adopt(for_, 4, init_expr, pre_iter_expr, reinit_expr, body),
          BCC_TERR_EXPECTED_SCALCONST, 2, for_, pre_iter_expr);
    }
  }

  maybe_load_cexpr(reinit_expr, body->first_instr);
  maybe_load_cexpr(pre_iter_expr, reinit_expr->first_instr);
  maybe_load_cexpr(init_expr, pre_iter_expr->first_instr);
  return translate_for(for_, init_expr, pre_iter_expr, reinit_expr, body);
}

ASTree *validate_label(ASTree *label, ASTree *ident_node, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(astree_adopt(label, 1, ident_node), stmt);
  }

  const char *ident = ident_node->lexinfo;
  size_t ident_len = strlen(ident);
  LabelValue *existing_entry = state_get_label(state, ident, ident_len);
  if (existing_entry) {
    if (existing_entry->is_defined) {
      return astree_create_errnode(astree_adopt(label, 2, ident_node, stmt),
                                   BCC_TERR_REDEFINITION, 1, ident_node);
    } else {
      existing_entry->tree = ident_node;
      existing_entry->is_defined = 1;
      return translate_label(label, ident_node, stmt);
    }
  } else {
    LabelValue *labval = malloc(sizeof(*labval));
    labval->tree = ident_node;
    labval->is_defined = 1;
    int status = state_insert_label(state, ident, ident_len, labval);
    if (status) {
      abort();
    }
    return translate_label(label, ident_node, stmt);
  }
}

ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  }

  if (case_->jump_id == SIZE_MAX)
    return astree_create_errnode(case_, BCC_TERR_UNEXPECTED_TOKEN, 1, case_);
  assert(case_->case_id != SIZE_MAX);

  Type *case_const_spec = expr->type;
  if (!type_is_integral(case_const_spec) ||
      (expr->attributes & ATTR_MASK_CONST) != ATTR_CONST_INT) {
    return astree_create_errnode(astree_adopt(case_, 2, expr, stmt),
                                 BCC_TERR_EXPECTED_INTCONST, 2, case_, expr);
  }

  maybe_load_cexpr(expr, stmt->first_instr);
  return translate_case(case_, expr, stmt);
}

ASTree *validate_default(ASTree *default_, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(default_, stmt);
  }

  if (default_->jump_id == SIZE_MAX)
    return astree_create_errnode(astree_adopt(default_, 1, stmt),
                                 BCC_TERR_UNEXPECTED_TOKEN, 1, default_);
  else if (state_set_selection_default(state))
    return astree_create_errnode(astree_adopt(default_, 1, stmt),
                                 BCC_TERR_UNEXPECTED_TOKEN, 1, default_);

  return translate_default(default_, stmt);
}

ASTree *validate_goto(ASTree *goto_, ASTree *ident) {
  const char *ident_str = ident->lexinfo;
  size_t ident_str_len = strlen(ident_str);
  LabelValue *existing_entry = state_get_label(state, ident_str, ident_str_len);
  if (!existing_entry) {
    LabelValue *labval = malloc(sizeof(*labval));
    labval->tree = ident;
    labval->is_defined = 0;
    int status = state_insert_label(state, ident_str, ident_str_len, labval);
    if (status) {
      abort();
    }
  }
  return translate_goto(goto_, ident);
}

ASTree *validate_continue(ASTree *continue_) {
  if (continue_->jump_id == SIZE_MAX) {
    return astree_create_errnode(continue_, BCC_TERR_UNEXPECTED_TOKEN, 1,
                                 continue_);
  }

  return translate_continue(continue_);
}

ASTree *validate_break(ASTree *break_) {
  if (break_->jump_id == SIZE_MAX) {
    return astree_create_errnode(break_, BCC_TERR_UNEXPECTED_TOKEN, 1, break_);
  }

  return translate_break(break_);
}

ASTree *validate_block(ASTree *block) {
  block->symbol_table = symbol_table_init(BLOCK_TABLE);
  int status = state_push_table(state, block->symbol_table);
  if (status) {
    return astree_create_errnode(block, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return block;
}

ASTree *validate_block_content(ASTree *block, ASTree *block_content) {
  if (block->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(block, block_content);
  } else if (block_content->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(block, block_content);
  } else if (block_content->symbol == TOK_DECLARATION) {
    return translate_local_declarations(block, block_content);
  } else {
    return astree_adopt(block, 1, block_content);
  }
}

ASTree *finalize_block(ASTree *block) {
  int status = state_pop_table(state);
  if (status) {
    return astree_create_errnode(block, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return translate_block(block);
}
