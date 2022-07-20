#ifndef __TCHK_EXPR_H__
#define __TCHK_EXPR_H__

#include "astree.h"

ASTree *validate_intcon(ASTree *intcon);
ASTree *validate_charcon(ASTree *charcon);
ASTree *validate_stringcon(ASTree *stringcon);
ASTree *validate_ident(ASTree *ident);
ASTree *finalize_call(ASTree *call);
ASTree *validate_arg(ASTree *call, ASTree *arg);
ASTree *validate_call(ASTree *call, ASTree *function);
ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr);
ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr);
ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr);
ASTree *validate_binop(ASTree *operator, ASTree * left_operand,
                       ASTree *right_operand);
ASTree *validate_unop(ASTree *operator, ASTree * operand);
ASTree *validate_indirection(ASTree *indirection, ASTree *operand);
ASTree *validate_addrof(ASTree *addrof, ASTree *operand);
ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node);
ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index);
ASTree *validate_reference(ASTree *reference, ASTree *struct_,
                           ASTree *member_name_node);
ASTree *validate_arrow(ASTree *arrow, ASTree *struct_,
                       ASTree *member_name_node);
#endif
