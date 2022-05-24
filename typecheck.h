#ifndef __TYPECHECK_H__
#define __TYPECHECK_H__

#include "astree.h"

int type_checker_make_table(ASTree *root);
ASTree *declare_symbol(ASTree *declaration, ASTree *declarator);
ASTree *define_symbol(ASTree *declaration, ASTree *declarator,
                      ASTree *equal_sign, ASTree *initializer);
ASTree *define_function(ASTree *declaration, ASTree *declarator, ASTree *body);
ASTree *define_pointer(ASTree *declarator, ASTree *pointer);
ASTree *define_dirdecl(ASTree *declarator, ASTree *dirdecl);
ASTree *validate_param_list(ASTree *param_list);
ASTree *validate_param(ASTree *param_list, ASTree *declaration,
                       ASTree *declarator);
ASTree *finalize_param_list(ASTree *param_list);
ASTree *validate_array(ASTree *array, ASTree *expr);
ASTree *validate_declarator(ASTree *declarator);
ASTree *validate_typespec(ASTree *spec_list, ASTree *spec);
ASTree *validate_typespec_list(ASTree *spec_list);
/* need a class of functions for cleaning up intermediate products stored in the
 * tree */
ASTree *finalize_declaration(ASTree *declaration);
/* need a class of functions for propogating errors in lists of e.g.
 * declarations */
ASTree *create_type_error(ASTree *child, int errcode);
ASTree *propogate_type_error(ASTree *parent, ASTree *errnode);

#endif
