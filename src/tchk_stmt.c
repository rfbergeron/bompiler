#include "tchk_stmt.h"
#include "yyparse.h"
#include "state.h"
#include "stdlib.h"
#include "tchk_common.h"
#include "assert.h"

ASTree *validate_return(ASTree *ret, ASTree *expr) {
  SymbolValue *symval = state_get_function(state);
  TypeSpec ret_spec = SPEC_EMPTY;
  int status = strip_aux_type(&ret_spec, &symval->type);
  if (status) {
    typespec_destroy(&ret_spec);
    return astree_create_errnode(astree_adopt(ret, 1, expr), BCC_TERR_FAILURE, 0);
  }
  if (expr != &EMPTY_EXPR) {
    expr = perform_pointer_conv(expr);
    if (expr->symbol == TOK_TYPE_ERROR) {
      typespec_destroy(&ret_spec);
      return astree_propogate_errnode(ret, expr);
    }
    expr = convert_type(expr, &ret_spec);
    if (expr->symbol == TOK_TYPE_ERROR) {
      typespec_destroy(&ret_spec);
      return astree_propogate_errnode(ret, expr);
    }
    typespec_destroy(&ret_spec);
    return astree_adopt(ret, 1, expr);
  } else {
    int compatibility = types_compatible(&ret_spec, &SPEC_VOID);
    if (compatibility != TCHK_COMPATIBLE) {
      typespec_destroy(&ret_spec);
      return astree_create_errnode(astree_adopt(ret, 1, expr), BCC_TERR_EXPECTED_RETVAL,
                         0);
    }
    typespec_destroy(&ret_spec);
    return astree_adopt(ret, 1, expr);
  }
}

ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (if_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  } else if (else_body->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(ifelse, 3, condition, if_body, else_body);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(ifelse, 3, condition, if_body, else_body),
                       BCC_TERR_EXPECTED_SCALAR, 2, ifelse, condition);
  }

  return astree_adopt(ifelse, 3, condition, if_body, else_body);
}

ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(switch_, 2, expr, stmt);
  }
  if (stmt->symbol == TOK_BLOCK) {
    TypeSpec *errspec = symbol_table_process_control(stmt->symbol_table,
            switch_->symbol);
    assert(errspec == NULL);
  } else if (stmt->symbol == CTRL_CASE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_DEFAULT) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_integer(expr->type)) {
    return astree_create_errnode(astree_adopt(switch_, 2, expr, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, switch_, expr);
  }

  return astree_adopt(switch_, 2, expr, stmt);
}

ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt) {
  /* TODO(Robert): safely process flow control statements before checking error
   * codes so that more things are cleaned up in the event of an error.
   */
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(while_, 2, condition, stmt);
  }

  if (stmt->symbol == TOK_BLOCK) {
    TypeSpec *errspec = symbol_table_process_control(stmt->symbol_table, while_->symbol);
    assert(errspec == NULL);
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(while_, 2, condition, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, while_, condition);
  }
  return astree_adopt(while_, 2, condition, stmt);
}

ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition) {
  condition = perform_pointer_conv(condition);
  if (condition->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(do_, 2, stmt, condition);
  }

  if (stmt->symbol == TOK_BLOCK) {
    TypeSpec *errspec = symbol_table_process_control(stmt->symbol_table, do_->symbol);
    assert(errspec == NULL);
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  if (!typespec_is_scalar(condition->type)) {
    return astree_create_errnode(astree_adopt(do_, 2, condition, stmt),
                       BCC_TERR_EXPECTED_INTEGER, 2, do_, condition);
  }
  return astree_adopt(do_, 2, condition, stmt);
}

ASTree *validate_for_exprs(ASTree *left_paren, ASTree *init_expr,
                           ASTree *pre_iter_expr, ASTree *reinit_expr) {
  if (init_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  } else if (pre_iter_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  } else if (reinit_expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(left_paren, 3, init_expr, pre_iter_expr,
                           reinit_expr);
  }

  if (pre_iter_expr != &EMPTY_EXPR) {
    if (!typespec_is_scalar(pre_iter_expr->type)) {
      return astree_create_errnode(
          astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr),
          BCC_TERR_EXPECTED_SCALCONST, 2, left_paren, pre_iter_expr);
    }
  }
  return astree_adopt(left_paren, 3, init_expr, pre_iter_expr, reinit_expr);
}

ASTree *validate_for(ASTree *for_, ASTree *left_paren, ASTree *stmt) {
  if (left_paren->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(for_, 2, left_paren, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(for_, 2, left_paren, stmt);
  }

  if (stmt->symbol == TOK_BLOCK) {
    TypeSpec *errspec = symbol_table_process_control(stmt->symbol_table, for_->symbol);
    assert(errspec == NULL);
  } else if (stmt->symbol == CTRL_CONTINUE) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  } else if (stmt->symbol == CTRL_BREAK) {
    SymbolTable *table = state_peek_table(state);
    ControlValue *ctrlval = symbol_table_remove_control(table, 0);
    free(ctrlval);
  }

  return astree_adopt(for_, 2, left_paren, stmt);
}

/* TODO(Robert): decide whether or not to create empty labels for goto
 * statements whose label has not been defined yet.
 */
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
      existing_entry->loc = &ident_node->loc;
      existing_entry->is_defined = 1;
      return astree_adopt(label, 2, ident_node, stmt);
    }
  } else {
    LabelValue *labval = malloc(sizeof(*labval));
    labval->loc = &ident_node->loc;
    labval->is_defined = 1;
    int status = state_insert_label(state, ident, ident_len, labval);
    if (status) {
      return astree_create_errnode(astree_adopt(label, 2, ident_node, stmt),
                         BCC_TERR_LIBRARY_FAILURE, 0);
    }
    return astree_adopt(label, 2, ident_node, stmt);
  }
}

ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt) {
  if (expr->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  } else if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode_v(case_, 2, expr, stmt);
  }

  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_CASE;
  ctrlval->tree = case_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(case_, 1, stmt), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }

  const TypeSpec *case_const_spec = expr->type;
  if (!typespec_is_integer(case_const_spec) ||
      !(expr->attributes | ATTR_EXPR_ARITHCONST)) {
    return astree_create_errnode(astree_adopt(case_, 2, expr, stmt),
                       BCC_TERR_EXPECTED_INTCONST, 2, case_, expr);
  }

  return astree_adopt(case_, 2, expr, stmt);
}

ASTree *validate_default(ASTree *default_, ASTree *stmt) {
  if (stmt->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(default_, stmt);
  }
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_DEFAULT;
  ctrlval->tree = default_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(default_, 1, stmt),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(default_, 1, stmt);
}

ASTree *validate_goto(ASTree *goto_, ASTree *ident) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_GOTO;
  ctrlval->tree = goto_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(astree_adopt(goto_, 1, ident), BCC_TERR_LIBRARY_FAILURE,
                       0);
  }
  return astree_adopt(goto_, 1, ident);
}

ASTree *validate_continue(ASTree *continue_) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_CONTINUE;
  ctrlval->tree = continue_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(continue_, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return continue_;
}

ASTree *validate_break(ASTree *break_) {
  ControlValue *ctrlval = malloc(sizeof(*ctrlval));
  ctrlval->type = CTRL_BREAK;
  ctrlval->tree = break_;
  int status = symbol_table_add_control(state_peek_table(state), ctrlval);
  if (status) {
    free(ctrlval);
    return astree_create_errnode(break_, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return break_;
}

ASTree *validate_block(ASTree *block) {
  block->symbol_table = symbol_table_init();
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
  }

  /* TODO(Robert): handle control flow statements even in the case of errors */
  int status = merge_block_controls(block, block_content);
  if (status) {
    return astree_create_errnode(astree_adopt(block, 1, block_content),
                       BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return astree_adopt(block, 1, block_content);
}

ASTree *finalize_block(ASTree *block) {
  int status = state_pop_table(state);
  if (status) {
    return astree_create_errnode(block, BCC_TERR_LIBRARY_FAILURE, 0);
  }
  return block;
}

