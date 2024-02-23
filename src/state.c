#include "state.h"

#include <assert.h>

#include "stdlib.h"

extern size_t next_vreg(void);
CompilerState *state;
PROC_STACK(size_t_stack, size_t)

CompilerState *state_init(void) {
  CompilerState *state = malloc(sizeof(*state));
  llist_init(&state->table_stack, NULL, NULL);
  llist_init(&state->switch_stack, NULL, NULL);
  size_t_stack_init(&state->break_stack, 4);
  size_t_stack_init(&state->continue_stack, 4);
  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  state->jump_id_count = 0;
  return state;
}

void state_destroy(CompilerState *state) {
  if (state == NULL) return;
  int status = llist_destroy(&state->table_stack);
  if (status) abort();
  status = llist_destroy(&state->switch_stack);
  if (status) abort();
  size_t_stack_destroy(&state->break_stack);
  size_t_stack_destroy(&state->continue_stack);
  free(state);
}

void state_push_table(CompilerState *state, SymbolTable *table) {
  assert(llist_front(&state->table_stack) != table);
  int status = llist_push_front(&state->table_stack, table);
  if (status) abort();
}

void state_pop_table(CompilerState *state) {
  assert(!llist_empty(&state->table_stack));
  (void)llist_pop_front(&state->table_stack);
}

SymbolTable *state_peek_table(CompilerState *state) {
  return llist_front(&state->table_stack);
}

int state_get_symbol(CompilerState *state, const char *ident,
                     const size_t ident_len, Symbol **out) {
  size_t i;
  for (i = 0; i < llist_size(&state->table_stack); ++i) {
    SymbolTable *current = llist_get(&state->table_stack, i);
    /* TODO(Robert): rewrite symbol table functions */
    Symbol *symbol = symbol_table_get(current, ident, ident_len);
    if (symbol != NULL) {
      *out = symbol;
      break;
    }
  }

  if (i >= llist_size(&state->table_stack)) {
    /* set out to NULL if symbol could not be found */
    *out = NULL;
  }

  /* return whether or not symbol is in current scope */
  return i == 0;
}

void state_insert_symbol(CompilerState *state, const char *ident,
                         const size_t ident_len, Symbol *symbol) {
  SymbolTable *top_scope = llist_front(&state->table_stack);
  /* TODO(Robert): rewrite symbol table functions */
  assert(symbol_table_get(top_scope, ident, ident_len) == NULL);
  symbol_table_insert(top_scope, ident, ident_len, symbol);
}

size_t state_get_sequence(CompilerState *state) {
  return map_size(llist_front(&state->table_stack));
}

int state_get_tag(CompilerState *state, const char *ident,
                  const size_t ident_len, Tag **out) {
  *out = NULL;
  size_t i;
  for (i = 0; i < llist_size(&state->table_stack); ++i) {
    SymbolTable *current = llist_get(&state->table_stack, i);
    if (current->tag_namespace != NULL) {
      *out = symbol_table_get_tag(current, ident, ident_len);
      if (*out != NULL) break;
    }
  }

  /* return whether or not tag is in current scope */
  return i == 0;
}

void state_insert_tag(CompilerState *state, const char *ident,
                      const size_t ident_len, Tag *tag) {
  SymbolTable *top_scope = NULL;
  size_t i;
  for (i = 0; i < llist_size(&state->table_stack); ++i) {
    top_scope = llist_get(&state->table_stack, i);
    if (top_scope->tag_namespace != NULL) break;
  }

  assert(top_scope->tag_namespace != NULL);
  assert(symbol_table_get_tag(top_scope, ident, ident_len) == NULL);
  symbol_table_insert_tag(top_scope, ident, ident_len, tag);
}

LabelValue *state_get_label(CompilerState *state, const char *ident,
                            const size_t ident_len) {
  SymbolTable *function_table =
      llist_get(&state->table_stack, llist_size(&state->table_stack) - 2);
  if (function_table == NULL || function_table->label_namespace == NULL) {
    fprintf(stderr, "ERROR: attempt to get label at top level.\n");
    return NULL;
  }
  return symbol_table_get_label(function_table, ident, ident_len);
}

void state_insert_label(CompilerState *state, const char *ident,
                        const size_t ident_len, LabelValue *labval) {
  SymbolTable *function_table =
      llist_get(&state->table_stack, llist_size(&state->table_stack) - 2);
  assert(function_table != NULL && function_table->label_namespace != NULL);
  symbol_table_insert_label(function_table, ident, ident_len, labval);
}

size_t state_get_selection_id(CompilerState *state) {
  if (llist_empty(&state->switch_stack)) {
    return (size_t)-1L;
  }

  return ((SwitchInfo *)llist_front(&state->switch_stack))->id;
}

size_t state_get_case_id(CompilerState *state) {
  if (llist_empty(&state->switch_stack)) {
    return (size_t)-1L;
  }

  return ((SwitchInfo *)llist_front(&state->switch_stack))->case_id;
}

size_t state_get_control_reg(CompilerState *state) {
  if (llist_empty(&state->switch_stack)) {
    return (size_t)-1L;
  }

  return ((SwitchInfo *)llist_front(&state->switch_stack))->control_reg;
}

int state_get_selection_default(CompilerState *state) {
  if (llist_empty(&state->switch_stack)) abort();
  return ((SwitchInfo *)llist_front(&state->switch_stack))->has_default;
}

const Type *state_get_control_type(CompilerState *state) {
  if (llist_empty(&state->switch_stack)) return NULL;
  return ((SwitchInfo *)llist_front(&state->switch_stack))->control_type;
}

size_t state_get_break_id(CompilerState *state) {
  if (size_t_stack_count(&state->break_stack) == 0) {
    return (size_t)-1L;
  }

  return size_t_stack_top(&state->break_stack);
}

size_t state_get_continue_id(CompilerState *state) {
  if (size_t_stack_count(&state->continue_stack) == 0) {
    return (size_t)-1L;
  }

  return size_t_stack_top(&state->continue_stack);
}

size_t state_next_jump_id(CompilerState *state) {
  return state->jump_id_count++;
}

void state_pop_selection_id(CompilerState *state) {
  if (llist_size(&state->switch_stack) == 0) {
    abort();
  }

  free(llist_pop_front(&state->switch_stack));
}

void state_pop_break_id(CompilerState *state) {
  if (size_t_stack_count(&state->break_stack) == 0) {
    abort();
  }

  (void)size_t_stack_pop(&state->break_stack);
}

void state_pop_continue_id(CompilerState *state) {
  if (size_t_stack_count(&state->continue_stack) == 0) {
    abort();
  }

  (void)size_t_stack_pop(&state->continue_stack);
}

void state_push_selection_id(CompilerState *state, size_t id) {
  SwitchInfo *info = malloc(sizeof(SwitchInfo));
  info->id = id;
  info->case_id = 0;
  info->has_default = 0;
  info->control_reg = next_vreg();
  info->control_type = NULL;
  llist_push_front(&state->switch_stack, info);
}

void state_inc_case_id(CompilerState *state) {
  SwitchInfo *info = llist_front(&state->switch_stack);
  if (info != NULL) ++info->case_id;
}

void state_set_selection_default(CompilerState *state) {
  SwitchInfo *info = llist_front(&state->switch_stack);
  /* do not call if there is no switch info */
  assert(info != NULL && info->has_default == 0);
  info->has_default = 1;
}

void state_set_control_type(CompilerState *state, const Type *type) {
  SwitchInfo *info = llist_front(&state->switch_stack);
  assert(info != NULL && info->control_type == NULL);
  info->control_type = type;
}

void state_push_break_id(CompilerState *state, size_t id) {
  size_t_stack_push(&state->break_stack, id);
}

void state_push_continue_id(CompilerState *state, size_t id) {
  size_t_stack_push(&state->continue_stack, id);
}

void state_dec_jump_id_count(CompilerState *state) { --state->jump_id_count; }

void state_set_function(CompilerState *state, const char *function_name,
                        Symbol *function_symbol) {
  assert(state->enclosing_function == NULL &&
         state->enclosing_function_name == NULL);
  state->enclosing_function = function_symbol;
  state->enclosing_function_name = function_name;
}

const char *state_get_function_name(CompilerState *state) {
  return state->enclosing_function_name;
}

Symbol *state_get_function(CompilerState *state) {
  return state->enclosing_function;
}

void state_unset_function(CompilerState *state) {
  assert(state->enclosing_function != NULL &&
         state->enclosing_function_name != NULL);
  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
}
