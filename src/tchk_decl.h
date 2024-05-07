#ifndef __TCHK_DECL_H__
#define __TCHK_DECL_H__

#include "astree.h"

ASTree *validate_decl_spec(ASTree *decl_specs, ASTree *decl_spec);
ASTree *finalize_decl_specs(ASTree *decl_specs);
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
ASTree *define_symbol(ASTree *decl_list, ASTree *equal_sign,
                      ASTree *initializer);
ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body);
ASTree *validate_fnbody_content(ASTree *function, ASTree *fnbody_content);
ASTree *finalize_function(ASTree *function);
ASTree *validate_unique_tag(ASTree *tag_spec, ASTree *left_brace);
ASTree *validate_tag_decl(ASTree *tag_spec, ASTree *ident);
ASTree *validate_tag_def(ASTree *tag_spec, ASTree *ident, ASTree *left_brace);
ASTree *finalize_tag_def(ASTree *tag_spec);
ASTree *define_enumerator(ASTree *enum_spec, ASTree *ident, ASTree *equal_sign,
                          ASTree *expr);
ASTree *declare_member(ASTree *struct_decl, ASTree *declarator);
ASTree *prepare_init(ASTree *declaration, ASTree *declarator);
ASTree *declare_symbol(ASTree *declaration, ASTree *declarator);
ASTree *validate_topdecl(ASTree *root, ASTree *topdecl);
#endif
