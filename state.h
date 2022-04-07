#ifndef __STATE_H__
#define __STATE_H__
#include "symtable.h"
#include "badlib/badllist.h"
typedef struct compiler_state {
  LinkedList table_stack;
  LinkedList jump_stack;
  SymbolValue *enclosing_function;
} CompilerState;

CompilerState *state_init(void);
int state_destroy(CompilerState *state);
int state_push_table(CompilerState *state, SymbolTable *table);
int state_pop_table(CompilerState *state);
int state_push_jump(CompilerState *state, LabelValue *labval);
int state_pop_jump(CompilerState *state);
int state_get_symbol(CompilerState *state, const char *ident, const size_t ident_len, SymbolValue **out);
int state_insert_symbol(CompilerState *state, const char*ident, const size_t ident_len, SymbolValue *symval);
int state_get_tag(CompilerState *state, const char *ident, const size_t ident_len, TagValue **out);
int state_insert_tag(CompilerState *state, const char *ident, const size_t ident_len, TagValue *tagval);
LabelValue *state_get_label(CompilerState *state, const char *ident, const size_t ident_len);
int state_insert_label(CompilerState *state, const char *ident, const size_t ident_len, LabelValue *labval);
int state_set_function(CompilerState *state, SymbolValue *function_symval);
SymbolValue *state_get_function(CompilerState *state);
int state_unset_function(CompilerState *state);

#endif
