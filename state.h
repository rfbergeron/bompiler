#ifndef __STATE_H__
#define __STATE_H__
#include "badlib/badllist.h"
#include "symtable.h"
#define MAX_LABEL_LENGTH 32
typedef struct compiler_state {
  LinkedList table_stack;
  LinkedList jump_stack;
  SymbolValue *enclosing_function;
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
      char default_label[MAX_LABEL_LENGTH];
    } switch_;
  } data;
  JumpType type;
  char end_label[MAX_LABEL_LENGTH];
} JumpEntry;

CompilerState *state_init(void);
int state_destroy(CompilerState *state);
int state_push_table(CompilerState *state, SymbolTable *table);
int state_pop_table(CompilerState *state);
int state_push_jump(CompilerState *state, JumpEntry *jump_entry);
int state_pop_jump(CompilerState *state);
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
int state_set_function(CompilerState *state, SymbolValue *function_symval);
SymbolValue *state_get_function(CompilerState *state);
int state_unset_function(CompilerState *state);

#endif
