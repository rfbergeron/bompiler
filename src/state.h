#ifndef __STATE_H__
#define __STATE_H__
#include "badllist.h"
#include "simplestack.h"
#include "symtable.h"

DECLARE_STACK(size_t_stack, size_t)
typedef struct size_t_stack SizetStack;

typedef struct switch_info {
  size_t id;
  size_t case_id;
  size_t control_reg;
  const Type *control_type;
  int has_default;
} SwitchInfo;

typedef struct compiler_state {
  LinkedList table_stack;
  SizetStack break_stack;
  SizetStack continue_stack;
  LinkedList switch_stack;
  Symbol *enclosing_function;
  const char *enclosing_function_name;
  size_t jump_id_count;
} CompilerState;

extern CompilerState *state;

CompilerState *state_init(void);
void state_destroy(CompilerState *state);
void state_push_table(CompilerState *state, SymbolTable *table);
void state_pop_table(CompilerState *state);
SymbolTable *state_peek_table(CompilerState *state);
int state_get_symbol(CompilerState *state, const char *ident,
                     const size_t ident_len, Symbol **out);
void state_insert_symbol(CompilerState *state, const char *ident,
                         const size_t ident_len, Symbol *symbol);
int state_inheritance_valid(CompilerState *state, const char *ident,
                            const size_t ident_len, Symbol *symbol);
int state_get_member(CompilerState *state, const char *ident,
                     const size_t ident_len, Symbol **out);
void state_insert_member(CompilerState *state, const char *ident,
                         const size_t ident_len, Symbol *symbol);
size_t state_get_sequence(CompilerState *state);
int state_get_tag(CompilerState *state, const char *ident,
                  const size_t ident_len, Tag **out);
void state_insert_tag(CompilerState *state, const char *ident,
                      const size_t ident_len, Tag *tag);
Label *state_get_label(CompilerState *state, const char *ident,
                       const size_t ident_len);
void state_insert_label(CompilerState *state, const char *ident,
                        const size_t ident_len, Label *label);
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
void state_set_function(CompilerState *state, const char *function_name,
                        Symbol *function_symbol);
Symbol *state_get_function(CompilerState *state);
const char *state_get_function_name(CompilerState *state);
void state_unset_function(CompilerState *state);

#endif
