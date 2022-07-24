#ifndef __STATE_H__
#define __STATE_H__
#include "badllist.h"
#include "simplestack.h"
#include "symtable.h"
#define MAX_LABEL_LENGTH 32
#define MAX_OP_LENGTH 32

DECLARE_STACK(size_t_stack, size_t)
typedef struct size_t_stack SizetStack;

typedef struct switch_info {
  size_t id;
  size_t case_count;
} SwitchInfo;

typedef struct compiler_state {
  LinkedList table_stack;
  LinkedList jump_stack;
  LinkedList error_stack;
  SizetStack break_stack;
  SizetStack continue_stack;
  LinkedList switch_stack;
  SymbolValue *enclosing_function;
  size_t jump_id_count;
} CompilerState;

typedef enum jump_type { JUMP_ITERATION, JUMP_SWITCH } JumpType;

typedef struct jump_entry {
  union {
    struct {
      char cond_label[MAX_LABEL_LENGTH];
      char stmt_label[MAX_LABEL_LENGTH];
    } iteration;
    struct {
      LinkedList *case_labels;
      size_t next_case;
      char control_register[MAX_OP_LENGTH];
    } switch_;
  } data;
  JumpType type;
  char end_label[MAX_LABEL_LENGTH];
} JumpEntry;

extern CompilerState *state;

CompilerState *state_init(void);
int state_destroy(CompilerState *state);
int state_push_table(CompilerState *state, SymbolTable *table);
int state_pop_table(CompilerState *state);
SymbolTable *state_peek_table(CompilerState *state);
int state_push_jump(CompilerState *state, JumpEntry *jump_entry);
int state_pop_jump(CompilerState *state);
int state_push_type_error(CompilerState *state, TypeSpec *errtype);
int state_pop_type_error(CompilerState *state);
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
JumpEntry *state_get_iteration(CompilerState *state);
JumpEntry *state_get_switch(CompilerState *state);
JumpEntry *state_get_jump(CompilerState *state);
size_t state_get_selection_id(CompilerState *state);
size_t state_get_case_id(CompilerState *state);
size_t state_get_break_id(CompilerState *state);
size_t state_get_continue_id(CompilerState *state);
void state_pop_selection(CompilerState *state);
void state_pop_break_id(CompilerState *state);
void state_pop_continue_id(CompilerState *state);
void state_push_selection(CompilerState *state, size_t id);
void state_inc_case_id(CompilerState *state);
void state_push_break_id(CompilerState *state, size_t id);
void state_push_continue_id(CompilerState *state, size_t id);
void state_dec_jump_id_count(CompilerState *state);
int state_set_function(CompilerState *state, SymbolValue *function_symval);
SymbolValue *state_get_function(CompilerState *state);
int state_unset_function(CompilerState *state);

#endif
