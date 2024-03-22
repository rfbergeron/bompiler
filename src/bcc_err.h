#ifndef __BCC_ERR_H__
#define __BCC_ERR_H__

#include "astree.h"
#include "bcc_types.h"
#include "symtable.h"

int semerr_const_too_large(const ASTree *constant, const Type *type);
int semerr_excess_init(const ASTree *initializer, const Type *type);
int semerr_expected_init(const ASTree *initializer);
int semerr_compat_init(const ASTree *init, const Type *dest);
int semerr_incompatible_spec(const ASTree *decl_specs, const ASTree *decl_spec);
int semerr_symbol_not_found(const ASTree *identifier);
int semerr_expected_typedef_name(const ASTree *identifier,
                                 const Symbol *symbol);
int semerr_invalid_type(const ASTree *tree);
int semerr_incomplete_type(const ASTree *where, const Type *type);
int semerr_invalid_linkage(const ASTree *declarator, const Symbol *symbol);
int semerr_incompatible_linkage(const ASTree *declarator,
                                const Symbol *old_symbol,
                                const Symbol *new_symbol);
int semerr_redefine_symbol(const ASTree *declarator, const Symbol *symbol);
int semerr_define_extern(const ASTree *declarator);
int semerr_invalid_arr_size(const ASTree *array, const ASTree *expr);
int semerr_expected_ident(const ASTree *function, const ASTree *param);
int semerr_label_not_found(const ASTree *label);
int semerr_redefine_tag(const ASTree *tag_spec, const ASTree *tag_id,
                        const Tag *existing);
int semerr_enum_not_found(const ASTree *enum_spec, const ASTree *enum_id);
int semerr_expected_const(const ASTree *where, const ASTree *expr);
int semerr_incompatible_types(const ASTree *where, const Type *dest,
                              const Type *src);
int semerr_expected_retval(const ASTree *ret, const Type *type);
int semerr_expected_scalar(const ASTree *where, const Type *type);
int semerr_expected_integral(const ASTree *where, const Type *type);
int semerr_redefine_label(const ASTree *identifier, const ASTree *old_label);
int semerr_unexpected_stmt(const ASTree *stmt);
int semerr_expected_intconst(const ASTree *where, const ASTree *expr);
int semerr_insufficient_args(const ASTree *call);
int semerr_excess_args(const ASTree *call, const ASTree *arg);
int semerr_expected_fn_ptr(const ASTree *call, const Type *type);
int semerr_expected_arithmetic(const ASTree *where, const Type *type);
int semerr_expected_lvalue(const ASTree *where, const ASTree *offender);
int semerr_expected_pointer(const ASTree *where, const Type *type);
int semerr_sizeof_fn(const ASTree *sizeof_, const Type *type);
int semerr_sizeof_incomplete(const ASTree *sizeof_, const Type *type);
int semerr_expected_record_ptr(const ASTree *where, const Type *type);
int semerr_expected_record(const ASTree *where, const Type *type);
int semerr_not_assignable(const ASTree *where, const Type *type);
#endif
