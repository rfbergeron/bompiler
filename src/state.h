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
  SymbolValue *enclosing_function;
  size_t jump_id_count;
} CompilerState;

extern CompilerState *state;

CompilerState *state_init(void);
int state_destroy(CompilerState *state);
int state_push_table(CompilerState *state, SymbolTable *table);
int state_pop_table(CompilerState *state);
SymbolTable *state_peek_table(CompilerState *state);
int state_get_symbol(CompilerState *state, const char *ident,
                     const size_t ident_len, SymbolValue **out);
int state_insert_symbol(CompilerState *state, const char *ident,
                        const size_t ident_len, SymbolValue *symval);
size_t state_get_sequence(CompilerState *state);
int state_get_tag(CompilerState *state, const char *ident,
                  const size_t ident_len, TagValue **out);
int state_insert_tag(CompilerState *state, const char *ident,
                     const size_t ident_len, TagValue *tagval);
LabelValue *state_get_label(CompilerState *state, const char *ident,
                            const size_t ident_len);
int state_insert_label(CompilerState *state, const char *ident,
                       const size_t ident_len, LabelValue *labval);
size_t state_get_selection_id(CompilerState *state);
size_t state_get_case_id(CompilerState *state);
size_t state_get_control_reg(CompilerState *state);
int state_get_selection_default(CompilerState *state);
const Type *state_get_control_type(CompilerState *state);
size_t state_get_break_id(CompilerState *state);
size_t state_get_continue_id(CompilerState *state);
size_t state_next_jump_id(CompilerState *state);
void state_pop_selection(CompilerState *state);
void state_pop_break_id(CompilerState *state);
void state_pop_continue_id(CompilerState *state);
void state_push_selection(CompilerState *state, size_t id);
void state_inc_case_id(CompilerState *state);
int state_set_selection_default(CompilerState *state);
int state_set_control_type(CompilerState *state, const Type *type);
void state_push_break_id(CompilerState *state, size_t id);
void state_push_continue_id(CompilerState *state, size_t id);
void state_dec_jump_id_count(CompilerState *state);
int state_set_function(CompilerState *state, SymbolValue *function_symval);
SymbolValue *state_get_function(CompilerState *state);
int state_unset_function(CompilerState *state);

#endif
