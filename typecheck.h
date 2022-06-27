#ifndef __TYPECHECK_H__
#define __TYPECHECK_H__

#include "astree.h"

ASTree *validate_intcon(ASTree *intcon);
ASTree *validate_charcon(ASTree *charcon);
ASTree *validate_stringcon(ASTree *stringcon);
ASTree *validate_typespec(ASTree *spec_list, ASTree *type);
ASTree *validate_typespec_list(ASTree *spec_list);
ASTree *validate_ident(ASTree *ident);
ASTree *finalize_call(ASTree *call);
ASTree *validate_arg(ASTree *call, ASTree *arg);
ASTree *validate_call(ASTree *call, ASTree *function);
ASTree *validate_conditional(ASTree *qmark, ASTree *condition,
                             ASTree *true_expr, ASTree *false_expr);
ASTree *validate_comma(ASTree *comma, ASTree *left_expr, ASTree *right_expr);
ASTree *validate_assignment(ASTree *assignment, ASTree *dest, ASTree *src);
ASTree *finalize_declaration(ASTree *declaration);
ASTree *validate_array_size(ASTree *array, ASTree *expr);
ASTree *validate_param_list(ASTree *param_list);
ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator);
ASTree *finalize_param_list(ASTree *param_list);
ASTree *define_params(ASTree *declarator, ASTree *param_list);
ASTree *define_array(ASTree *declarator, ASTree *array);
ASTree *define_pointer(ASTree *declarator, ASTree *pointer);
ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl);
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
ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer);
ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body);
ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content);
ASTree *finalize_function(ASTree *function);
ASTree *validate_tag_def(ASTree *tag_type_node, ASTree *tag_name_node,
                         ASTree *left_brace);
ASTree *finalize_tag_def(ASTree *tag);
ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr);
ASTree *define_struct_member(ASTree *struct_, ASTree *member);
ASTree *validate_declarator(ASTree *declarator);
ASTree *declare_symbol(ASTree *declaration, ASTree *declarator);
ASTree *validate_return(ASTree *ret, ASTree *expr);
ASTree *validate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                        ASTree *else_body);
ASTree *validate_switch(ASTree *switch_, ASTree *expr, ASTree *stmt);
ASTree *validate_while(ASTree *while_, ASTree *condition, ASTree *stmt);
ASTree *validate_do(ASTree *do_, ASTree *stmt, ASTree *condition);
ASTree *validate_for_exprs(ASTree *left_paren, ASTree *init_expr,
                           ASTree *pre_iter_expr, ASTree *reinit_expr);
ASTree *validate_for(ASTree *for_, ASTree *left_paren, ASTree *stmt);
ASTree *validate_label(ASTree *label, ASTree *ident_node, ASTree *stmt);
ASTree *validate_case(ASTree *case_, ASTree *expr, ASTree *stmt);
ASTree *validate_default(ASTree *default_, ASTree *stmt);
ASTree *validate_block(ASTree *block);
ASTree *validate_block_content(ASTree *block, ASTree *block_content);
ASTree *finalize_block(ASTree *block);
ASTree *validate_topdecl(ASTree *root, ASTree *topdecl);
#endif
