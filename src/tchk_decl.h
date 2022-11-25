#ifndef __TCHK_DECL_H__
#define __TCHK_DECL_H__

#include "astree.h"

ASTree *validate_typespec(ASTree *spec_list, ASTree *type);
ASTree *validate_typespec_list(ASTree *spec_list);
ASTree *finalize_declaration(ASTree *declaration);
ASTree *validate_array_size(ASTree *array, ASTree *expr);
ASTree *validate_param_list(ASTree *param_list);
ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator);
ASTree *finalize_param_list(ASTree *param_list, ASTree *ellipsis);
ASTree *define_params(ASTree *declarator, ASTree *param_list);
ASTree *define_array(ASTree *declarator, ASTree *array);
ASTree *define_pointer(ASTree *declarator, ASTree *pointer);
ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl);
ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer);
ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body);
ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content);
ASTree *finalize_function(ASTree *function);
ASTree *validate_unique_tag(ASTree *tag_type_node, ASTree *left_brace);
ASTree *validate_tag_decl(ASTree *tag_type_node, ASTree *tag_name_node);
ASTree *validate_tag_def(ASTree *tag_type_node, ASTree *tag_name_node,
                         ASTree *left_brace);
ASTree *finalize_tag_def(ASTree *tag);
ASTree *define_enumerator(ASTree *enum_, ASTree *ident_node, ASTree *equal_sign,
                          ASTree *expr);
ASTree *define_struct_member(ASTree *struct_, ASTree *member);
ASTree *validate_declarator(ASTree *declarator);
ASTree *declare_symbol(ASTree *declaration, ASTree *declarator);
ASTree *validate_topdecl(ASTree *root, ASTree *topdecl);
#endif
