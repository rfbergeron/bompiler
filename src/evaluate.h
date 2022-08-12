#ifndef __EVALUATE_H__
#define __EVALUATE_H__

#include "astree.h"
ASTree *evaluate_intcon(ASTree *intcon);
ASTree *evaluate_charcon(ASTree *charcon);
ASTree *evaluate_ident(ASTree *ident);
ASTree *evaluate_addition(ASTree *addition);
ASTree *evaluate_subtraction(ASTree *subtraction);
ASTree *evaluate_shiftl(ASTree *shiftl);
ASTree *evaluate_shiftr(ASTree *shiftr);
ASTree *evaluate_cast(ASTree *cast);
ASTree *evaluate_conditional(ASTree *conditional);
ASTree *evaluate_binop(ASTree *binop);
ASTree *evaluate_unop(ASTree *unop);
#endif
