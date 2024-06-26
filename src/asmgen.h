#ifndef __ASMGEN_H__
#define __ASMGEN_H__

#include "astree.h"
#include "instr.h"
#include "lyutils.h"

const char *mk_static_label(const char *name, size_t unique_id);
const char *asmgen_literal_label(const char *literal);
void bulk_mzero(size_t dest_memreg, ptrdiff_t dest_disp, size_t skip_bytes,
                const Type *type, Instruction *where);
void static_zero_pad(size_t count, Instruction *where);
ASTree *translate_cexpr_conv(ASTree *cexpr_conv, ASTree *expr);
ASTree *translate_scal_conv(ASTree *conv, ASTree *expr);
ASTree *translate_ptr_conv(ASTree *conv, ASTree *expr);
ASTree *translate_disp_conv(ASTree *conv, ASTree *expr,
                            const Type *pointer_type);
ASTree *translate_diff_conv(ASTree *conv, ASTree *expr,
                            const Type *pointer_type);
ASTree *translate_rval_conv(ASTree *conv, ASTree *expr);
ASTree *translate_empty_expr(ASTree *empty_expr);
ASTree *translate_ident(ASTree *ident);
ASTree *translate_logical_not(ASTree * not, ASTree *operand);
ASTree *translate_logical(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_comparison(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_indirection(ASTree *indirection, ASTree *operand);
ASTree *translate_addrof(ASTree *addrof, ASTree *operand);
ASTree *translate_subscript(ASTree *subscript, ASTree *pointer, ASTree *index);
ASTree *translate_reference(ASTree *reference, ASTree *struct_, ASTree *member);
ASTree *translate_post_inc_dec(ASTree *post_inc_dec, ASTree *operand);
ASTree *translate_inc_dec(ASTree *inc_dec, ASTree *operand);
ASTree *translate_unop(ASTree *operator, ASTree * operand);
ASTree *translate_addition(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_multiplication(ASTree *operator, ASTree * left,
                                 ASTree *right);
ASTree *translate_shift(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_binop(ASTree *operator, ASTree * left, ASTree *right);
ASTree *translate_conditional(ASTree *qmark, ASTree *condition,
                              ASTree *true_expr, ASTree *false_expr);
ASTree *translate_comma(ASTree *comma, ASTree *left, ASTree *right);
ASTree *translate_assignment(ASTree *assignment, ASTree *lvalue,
                             ASTree *rvalue);
ASTree *translate_call(ASTree *call);
ASTree *translate_va_start(ASTree *va_start_, ASTree *expr, ASTree *ident);
ASTree *translate_va_end(ASTree *va_end_, ASTree *expr);
ASTree *translate_va_arg(ASTree *va_arg_, ASTree *expr, ASTree *type_name);
ASTree *translate_ifelse(ASTree *ifelse, ASTree *condition, ASTree *if_body,
                         ASTree *else_body);
ASTree *translate_switch(ASTree *switch_, ASTree *condition, ASTree *body,
                         int has_default);
ASTree *translate_switch_expr(ASTree *expr);
ASTree *translate_while(ASTree *while_, ASTree *condition, ASTree *body);
ASTree *translate_for(ASTree *for_, ASTree *initializer, ASTree *condition,
                      ASTree *reinitializer, ASTree *body);
ASTree *translate_do(ASTree *do_, ASTree *body, ASTree *condition);
ASTree *translate_block(ASTree *block);
ASTree *translate_return(ASTree *ret, ASTree *expr);
ASTree *translate_continue(ASTree *continue_);
ASTree *translate_break(ASTree *break_);
ASTree *translate_goto(ASTree *goto_, ASTree *ident);
ASTree *translate_label(ASTree *label, ASTree *ident, ASTree *stmt);
ASTree *translate_case(ASTree *case_, ASTree *expr, ASTree *stmt);
ASTree *translate_default(ASTree *default_, ASTree *stmt);
ASTree *translate_local_init(ASTree *declaration, ASTree *assignment,
                             ASTree *declarator, ASTree *initializer);
ASTree *translate_local_decl(ASTree *declaration, ASTree *declarator);
ASTree *translate_block_content(ASTree *block, ASTree *content);
ASTree *translate_stmt_expr(ASTree *stmt_expr);
ASTree *translate_global_init(ASTree *declaration, ASTree *assignment,
                              ASTree *declarator, ASTree *initializer);
ASTree *translate_global_decl(ASTree *declaration, ASTree *declarator);
ASTree *end_translate_declaration(ASTree *declaration);
ASTree *begin_translate_fn(ASTree *declaration, ASTree *declarator,
                           ASTree *body);
ASTree *end_translate_fn(ASTree *declaration);
ASTree *translate_topdecl(ASTree *unit, ASTree *topdecl);
ASTree *translate_unit(ASTree *unit);
void translate_static_scalar_init(const Type *type, ASTree *initializer);
void translate_auto_scalar_init(const Type *type, ptrdiff_t disp,
                                ASTree *initializer);
void translate_static_literal_init(const Type *type, ASTree *literal);
void translate_auto_literal_init(const Type *type, ptrdiff_t disp,
                                 ASTree *literal);
int generator_print_il(FILE *out);
int generator_debug_il(FILE *out);
void asmgen_init_globals(void);
void asmgen_free_globals(void);

#endif
