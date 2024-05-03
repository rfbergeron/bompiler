#ifndef __STATE_H__
#define __STATE_H__
#include "scope.h"
#include "symtable.h"

typedef struct compiler_state CompilerState;
extern CompilerState *state;

CompilerState *state_init(void);
void state_destroy(CompilerState *state);
void state_enter_file(CompilerState *state, const struct astree *root);
void state_leave_file(CompilerState *state);
void state_enter_function(CompilerState *state, const struct astree *declarator,
                          const struct astree *body);
void state_leave_function(CompilerState *state);
void state_enter_prototype(CompilerState *state,
                           const struct astree *param_list);
void state_leave_prototype(CompilerState *state);
void state_enter_block(CompilerState *state, const struct astree *block);
void state_leave_block(CompilerState *state);
void state_enter_record(CompilerState *state, const Tag *record_tag);
void state_leave_record(CompilerState *state);
Scope *state_peek_scope(CompilerState *state);
Symbol *state_get_function(CompilerState *state);
const char *state_get_function_name(CompilerState *state);
int state_get_symbol(CompilerState *state, const char *ident, Symbol **out);
void state_insert_symbol(CompilerState *state, const char *ident,
                         Symbol *symbol);
int state_inheritance_valid(CompilerState *state, const char *ident,
                            Symbol *symbol);
int state_get_member(CompilerState *state, const char *ident, Symbol **out);
void state_insert_member(CompilerState *state, const char *ident,
                         Symbol *symbol);
int state_get_tag(CompilerState *state, const char *ident, Tag **out);
void state_insert_tag(CompilerState *state, const char *ident, Tag *tag);
Label *state_get_label(CompilerState *state, const char *ident);
void state_insert_label(CompilerState *state, const char *ident, Label *label);
size_t state_get_selection_id(CompilerState *state);
size_t state_get_case_id(CompilerState *state);
size_t state_get_control_reg(CompilerState *state);
int state_get_selection_default(CompilerState *state);
const Type *state_get_control_type(CompilerState *state);
size_t state_get_break_id(CompilerState *state);
size_t state_get_continue_id(CompilerState *state);
size_t state_next_jump_id(CompilerState *state);
void state_pop_selection_id(CompilerState *state);
void state_pop_break_id(CompilerState *state);
void state_pop_continue_id(CompilerState *state);
void state_push_selection_id(CompilerState *state, size_t id);
void state_inc_case_id(CompilerState *state);
void state_set_selection_default(CompilerState *state);
void state_set_control_type(CompilerState *state, const Type *type);
void state_push_break_id(CompilerState *state, size_t id);
void state_push_continue_id(CompilerState *state, size_t id);
void state_dec_jump_id_count(CompilerState *state);

#endif
