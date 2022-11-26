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
ASTree *validate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident);
ASTree *validate_va_end(ASTree *va_end_, ASTree *expr);
ASTree *validate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name);
ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr);
ASTree *validate_comma(ASTree *comma, ASTree *left, ASTree *right);
ASTree *validate_cast(ASTree *cast, ASTree *declaration, ASTree *expr);
ASTree *validate_addition(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_logical(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_relational(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_equality(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_multiply(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_shift(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_bitwise(ASTree *operator, ASTree * left, ASTree *right);
ASTree *validate_increment(ASTree *operator, ASTree * operand);
ASTree *validate_not(ASTree *operator, ASTree * operand);
ASTree *validate_complement(ASTree *operator, ASTree * operand);
ASTree *validate_negation(ASTree *operator, ASTree * operand);
ASTree *validate_indirection(ASTree *indirection, ASTree *operand);
ASTree *validate_addrof(ASTree *addrof, ASTree *operand);
ASTree *validate_sizeof(ASTree *sizeof_, ASTree *type_node);
ASTree *validate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index);
ASTree *validate_reference(ASTree *reference, ASTree *struct_, ASTree *member);
ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src);
#endif
