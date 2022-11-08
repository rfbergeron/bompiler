#ifndef __TCHK_STMT_H__
#define __TCHK_STMT_H__

#include "astree.h"

ASTree *validate_return(ASTree *ret, ASTree *expr);
ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body);
ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt);
ASTree *validate_switch_expr(ASTree *expr);
ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt);
ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition);
ASTree *validate_for(ASTree *for_, ASTree *init_expr, ASTree *pre_iter_expr,
                     ASTree *reinit_expr, ASTree *body);
ASTree *validate_label(ASTree *label, ASTree *ident_node, ASTree *stmt);
ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt);
ASTree *validate_default(ASTree *default_, ASTree *stmt);
ASTree *validate_goto(ASTree *goto_, ASTree *ident);
ASTree *validate_continue(ASTree *continue_);
ASTree *validate_break(ASTree *break_);
ASTree *validate_block(ASTree *block);
ASTree *validate_block_content(ASTree *block, ASTree *block_content);
ASTree *finalize_block(ASTree *block);
#endif
