#include "state.h"

#include <assert.h>
#include <stdlib.h>

#include "astree.h"

extern size_t next_vreg(void);
CompilerState *state;

DECLARE_STACK(size_t_stack, size_t)
PROC_STACK(size_t_stack, size_t)
typedef struct size_t_stack SizetStack;

typedef struct switch_info {
  size_t id;
  size_t case_id;
  size_t control_reg;
  const Type *control_type;
  int has_default;
} SwitchInfo;

struct compiler_state {
  Scope **scopes;
  size_t scopes_len, scopes_cap;
  SwitchInfo **switches;
  size_t switches_len, switches_cap;
  SizetStack break_stack;
  SizetStack continue_stack;
  Symbol *enclosing_function;
  const char *enclosing_function_name;
  size_t jump_id_count;
};

CompilerState *state_init(void) {
  CompilerState *state = malloc(sizeof(*state));
  state->scopes_len = 0;
  state->scopes_cap = 4;
  state->scopes = malloc(state->scopes_cap * sizeof(*state->scopes));
  state->switches_len = 0;
  state->switches_cap = 1;
  state->switches = malloc(state->switches_cap * sizeof(*state->switches));
  size_t_stack_init(&state->break_stack, 4);
  size_t_stack_init(&state->continue_stack, 4);
  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  state->jump_id_count = 0;
  return state;
}

void state_destroy(CompilerState *state) {
  if (state == NULL) return;
  free(state->scopes);
  free(state->switches);
  size_t_stack_destroy(&state->break_stack);
  size_t_stack_destroy(&state->continue_stack);
  free(state);
}

void state_enter_file(CompilerState *state, const ASTree *root) {
  assert(state->scopes_len == 0);
  assert(root->tok_kind == TOK_ROOT);
  assert(root->scope != NULL);
  assert(scope_get_kind(root->scope) == SCOPE_FILE);

  state->scopes[state->scopes_len++] = root->scope;
}

void state_leave_file(CompilerState *state) {
  assert(state->scopes_len == 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FILE);

  --state->scopes_len;
}

void state_enter_function(CompilerState *state, const ASTree *declarator,
                          const ASTree *body) {
  assert(state->scopes_len == 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FILE);
  assert(declarator->tok_kind == TOK_IDENT);
  assert(body->scope != NULL);
  assert(scope_get_kind(body->scope) == SCOPE_FUNCTION);
  assert(state->enclosing_function == NULL);
  assert(state->enclosing_function_name == NULL);

  (void)state_get_symbol(state, declarator->lexinfo,
                         &state->enclosing_function);

  assert(state->enclosing_function != NULL);
  assert(type_is_function(state->enclosing_function->type));
  state->enclosing_function_name = declarator->lexinfo;
  state->scopes[state->scopes_len++] = body->scope;
}

void state_leave_function(CompilerState *state) {
  assert(state->scopes_len == 2);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);
  assert(state->enclosing_function != NULL);
  assert(state->enclosing_function_name != NULL);

  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  --state->scopes_len;
}

void state_enter_prototype(CompilerState *state, const ASTree *param_list) {
  assert(state->scopes_len != 0);
  assert(param_list->scope != NULL);
  assert(state_peek_scope(state) != param_list->scope);
  assert(param_list->tok_kind == TOK_PARAM_LIST);
  assert(scope_get_kind(param_list->scope) == SCOPE_FUNCTION);

  if (state->scopes_len == state->scopes_cap)
    state->scopes = realloc(state->scopes,
                            (state->scopes_cap *= 2) * sizeof(*state->scopes));
  state->scopes[state->scopes_len++] = param_list->scope;
}

void state_leave_prototype(CompilerState *state) {
  assert(state->scopes_len > 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);

  --state->scopes_len;
}

void state_enter_block(CompilerState *state, const ASTree *block) {
  assert(state->scopes_len >= 2);
  assert(block->scope != NULL);
  assert(scope_get_kind(block->scope) == SCOPE_BLOCK);
  assert(state_peek_scope(state) != block->scope);

  if (state->scopes_len == state->scopes_cap)
    state->scopes = realloc(state->scopes,
                            (state->scopes_cap *= 2) * sizeof(*state->scopes));
  state->scopes[state->scopes_len++] = block->scope;
}

void state_leave_block(CompilerState *state) {
  assert(state->scopes_len > 2);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_BLOCK);

  --state->scopes_len;
}

void state_enter_record(CompilerState *state, const Tag *tag) {
  assert(state->scopes_len != 0);
  assert(tag->record.kind != TAG_ENUM);
  assert(tag->record.members != NULL);
  assert(scope_get_kind(tag->record.members) == SCOPE_MEMBER);
  assert(state_peek_scope(state) != tag->record.members);

  if (state->scopes_len == state->scopes_cap)
    state->scopes = realloc(state->scopes,
                            (state->scopes_cap *= 2) * sizeof(*state->scopes));
  state->scopes[state->scopes_len++] = tag->record.members;
}

void state_leave_record(CompilerState *state) {
  assert(state->scopes_len > 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_MEMBER);

  --state->scopes_len;
}

Scope *state_peek_scope(CompilerState *state) {
  return state->scopes[state->scopes_len - 1];
}

int state_get_symbol(CompilerState *state, const char *ident, Symbol **out) {
  size_t i, skipped;
  for (i = 1, skipped = 1; i <= state->scopes_len; ++i) {
    Scope *current = state->scopes[state->scopes_len - i];
    if (scope_get_kind(current) == SCOPE_MEMBER) {
      ++skipped;
      continue;
    }
    *out = scope_get_symbol(current, ident);
    if (*out != NULL) break;
  }

  /* return whether or not tag is in current scope */
  return i == skipped;
}

void state_insert_symbol(CompilerState *state, const char *ident,
                         Symbol *symbol) {
  size_t i = 1;
  Scope *current;
  do current = state->scopes[state->scopes_len - (i++)];
  while (scope_get_kind(current) == SCOPE_MEMBER);
  scope_insert_symbol(current, ident, symbol);
}

/* as a quirk of the way symbols declared with the `extern` keyword at block
 * scope are handled, while changes to the type information in the form of
 * array completions and function prototypes will be reflected across all
 * symbols, changes to the storage class will not be. if a symbol which
 * previously had external storage class is later declared with static storage
 * class, this change will not be reflected in previously declared symbols.
 */
int state_inheritance_valid(CompilerState *state, const char *ident,
                            Symbol *symbol) {
  assert(state->scopes_len > 1);
  Symbol *benefactor;
  size_t i;
  for (i = 1; i <= state->scopes_len; ++i) {
    Scope *current = state->scopes[state->scopes_len - i];
    if (scope_get_kind(current) == SCOPE_MEMBER) continue;
    /* TODO(Robert): rewrite symbol table functions */
    benefactor = scope_get_symbol(current, ident);
    if (benefactor != NULL &&
        (types_equivalent(benefactor->type, symbol->type, 0, 1) ||
         type_complete_array(benefactor->type, symbol->type) ||
         type_prototype_function(benefactor->type, symbol->type))) {
      type_destroy(symbol->type);
      symbol->type = benefactor->type;
      symbol->linkage = benefactor->linkage;
      symbol->storage = benefactor->storage;
      assert(symbol->info == SYM_NONE);
      symbol->info = SYM_INHERITOR;
      return 1;
    }
  }

  if (benefactor == NULL) {
    /* create hidden benefactor for symbol to inherit from */
    benefactor = symbol_init(symbol->loc);
    benefactor->type = symbol->type;
    benefactor->linkage = LINK_EXT;
    benefactor->storage = STORE_EXT;
    benefactor->info = SYM_HIDDEN;
    scope_insert_symbol(state->scopes[0], ident, benefactor);

    symbol->linkage = LINK_EXT;
    symbol->storage = STORE_EXT;
    assert(symbol->info == SYM_NONE);
    symbol->info = SYM_INHERITOR;
    return 1;
  } else {
    return 0;
  }
}

int state_get_member(CompilerState *state, const char *ident, Symbol **out) {
  size_t i;
  for (i = 1; i <= state->scopes_len; ++i) {
    Scope *current = state->scopes[state->scopes_len - i];
    if (scope_get_kind(current) != SCOPE_MEMBER) {
      *out = NULL;
      return 0;
    }
    *out = scope_get_member(current, ident);
    if (*out != NULL) break;
  }

  return i == 1;
}

void state_insert_member(CompilerState *state, const char *ident,
                         Symbol *symbol) {
  scope_insert_member(state->scopes[state->scopes_len - 1], ident, symbol);
}

int state_get_tag(CompilerState *state, const char *ident, Tag **out) {
  size_t i, skipped;
  for (i = 1, skipped = 1; i <= state->scopes_len; ++i) {
    Scope *current = state->scopes[state->scopes_len - i];
    if (scope_get_kind(current) == SCOPE_MEMBER) {
      ++skipped;
      continue;
    }
    *out = scope_get_tag(current, ident);
    if (*out != NULL) break;
  }

  /* return whether or not tag is in current scope */
  return i == skipped;
}

void state_insert_tag(CompilerState *state, const char *ident, Tag *tag) {
  size_t i = 1;
  Scope *current;
  do current = state->scopes[state->scopes_len - (i++)];
  while (scope_get_kind(current) == SCOPE_MEMBER);
  scope_insert_tag(current, ident, tag);
}

Label *state_get_label(CompilerState *state, const char *ident) {
  return scope_get_label(state->scopes[1], ident);
}

void state_insert_label(CompilerState *state, const char *ident, Label *label) {
  scope_insert_label(state->scopes[1], ident, label);
}

size_t state_get_selection_id(CompilerState *state) {
  return state->switches_len == 0
             ? SIZE_MAX
             : state->switches[state->switches_len - 1]->id;
}

size_t state_get_case_id(CompilerState *state) {
  return state->switches_len == 0
             ? SIZE_MAX
             : state->switches[state->switches_len - 1]->case_id;
}

size_t state_get_control_reg(CompilerState *state) {
  return state->switches_len == 0
             ? SIZE_MAX
             : state->switches[state->switches_len - 1]->control_reg;
}

int state_get_selection_default(CompilerState *state) {
  assert(state->switches_len != 0);
  return state->switches[state->switches_len - 1]->has_default;
}

const Type *state_get_control_type(CompilerState *state) {
  return state->switches_len == 0
             ? NULL
             : state->switches[state->switches_len - 1]->control_type;
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
  assert(state->switches_len != 0);
  free(state->switches[--state->switches_len]);
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

  if (state->switches_len == state->switches_cap)
    state->switches = realloc(
        state->switches, (state->switches_cap *= 2) * sizeof(*state->switches));
  state->switches[state->switches_len++] = info;
}

void state_inc_case_id(CompilerState *state) {
  if (state->switches_len != 0)
    ++(state->switches[state->switches_len - 1])->case_id;
}

void state_set_selection_default(CompilerState *state) {
  /* do not call if there is no switch info */
  assert(state->switches_len != 0);
  assert(state->switches[state->switches_len - 1]->has_default == 0);
  state->switches[state->switches_len - 1]->has_default = 1;
}

void state_set_control_type(CompilerState *state, const Type *type) {
  assert(state->switches_len != 0);
  assert(state->switches[state->switches_len - 1]->control_type == NULL);
  state->switches[state->switches_len - 1]->control_type = type;
}

void state_push_break_id(CompilerState *state, size_t id) {
  size_t_stack_push(&state->break_stack, id);
}

void state_push_continue_id(CompilerState *state, size_t id) {
  size_t_stack_push(&state->continue_stack, id);
}

void state_dec_jump_id_count(CompilerState *state) { --state->jump_id_count; }

const char *state_get_function_name(CompilerState *state) {
  return state->enclosing_function_name;
}

Symbol *state_get_function(CompilerState *state) {
  return state->enclosing_function;
}
