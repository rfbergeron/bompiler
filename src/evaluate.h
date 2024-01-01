#ifndef __EVALUATE_H__
#define __EVALUATE_H__

#include "astree.h"
ASTree *evaluate_intcon(ASTree *intcon);
ASTree *evaluate_charcon(ASTree *charcon);
ASTree *evaluate_stringcon(ASTree *stringcon);
ASTree *evaluate_ident(ASTree *ident);
ASTree *evaluate_cast(ASTree *cast, ASTree *expr);
ASTree *evaluate_auto_conv(ASTree *auto_conv, ASTree *expr);
ASTree *evaluate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr);
ASTree *evaluate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index);
ASTree *evaluate_addrof(ASTree *addrof, ASTree *operand);
ASTree *evaluate_reference(ASTree *reference, ASTree *struct_, ASTree *member);
ASTree *evaluate_binop(ASTree *operator, ASTree * left, ASTree *right);
ASTree *evaluate_unop(ASTree *operator, ASTree * operand);
#endif
