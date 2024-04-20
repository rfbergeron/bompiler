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
  LinkedList scopes;
  SizetStack break_stack;
  SizetStack continue_stack;
  LinkedList switch_stack;
  Symbol *enclosing_function;
  const char *enclosing_function_name;
  size_t jump_id_count;
};

CompilerState *state_init(void) {
  CompilerState *state = malloc(sizeof(*state));
  llist_init(&state->scopes, NULL, NULL);
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
  int status = llist_destroy(&state->scopes);
  if (status) abort();
  status = llist_destroy(&state->switch_stack);
  if (status) abort();
  size_t_stack_destroy(&state->break_stack);
  size_t_stack_destroy(&state->continue_stack);
  free(state);
}

void state_enter_file(CompilerState *state, const ASTree *root) {
  assert(llist_empty(&state->scopes));
  assert(root->tok_kind == TOK_ROOT);
  assert(root->scope != NULL);
  assert(scope_get_kind(root->scope) == SCOPE_FILE);
  int status = llist_push_front(&state->scopes, root->scope);
  if (status) abort();
}

void state_leave_file(CompilerState *state) {
  assert(llist_size(&state->scopes) == 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FILE);
  (void)llist_pop_front(&state->scopes);
}

void state_enter_function(CompilerState *state, const ASTree *declarator,
                          const ASTree *body) {
  assert(llist_size(&state->scopes) == 1);
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
  int status = llist_push_front(&state->scopes, body->scope);
  if (status) abort();
}

void state_leave_function(CompilerState *state) {
  assert(!llist_empty(&state->scopes));
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);
  assert(state->enclosing_function != NULL);
  assert(state->enclosing_function_name != NULL);
  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  (void)llist_pop_front(&state->scopes);
}

void state_enter_prototype(CompilerState *state, const ASTree *param_list) {
  assert(!llist_empty(&state->scopes));
  assert(param_list->scope != NULL);
  assert(state_peek_scope(state) != param_list->scope);
  assert(param_list->tok_kind == TOK_PARAM_LIST);
  assert(scope_get_kind(param_list->scope) == SCOPE_FUNCTION);
  int status = llist_push_front(&state->scopes, param_list->scope);
  if (status) abort();
}

void state_leave_prototype(CompilerState *state) {
  assert(!llist_empty(&state->scopes));
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);
  (void)llist_pop_front(&state->scopes);
}

void state_enter_block(CompilerState *state, const ASTree *block) {
  assert(!llist_empty(&state->scopes));
  assert(block->scope != NULL);
  assert(scope_get_kind(block->scope) == SCOPE_BLOCK);
  assert(state_peek_scope(state) != block->scope);
  int status = llist_push_front(&state->scopes, block->scope);
  if (status) abort();
}

void state_leave_block(CompilerState *state) {
  assert(!llist_empty(&state->scopes));
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_BLOCK);
  (void)llist_pop_front(&state->scopes);
}

void state_enter_record(CompilerState *state, const Tag *tag) {
  assert(!llist_empty(&state->scopes));
  assert(tag->record.kind != TAG_ENUM);
  assert(tag->record.members != NULL);
  assert(scope_get_kind(tag->record.members) == SCOPE_MEMBER);
  assert(state_peek_scope(state) != tag->record.members);
  int status = llist_push_front(&state->scopes, tag->record.members);
  if (status) abort();
}

void state_leave_record(CompilerState *state) {
  assert(!llist_empty(&state->scopes));
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_MEMBER);
  (void)llist_pop_front(&state->scopes);
}

Scope *state_peek_scope(CompilerState *state) {
  return llist_front(&state->scopes);
}

int state_get_symbol(CompilerState *state, const char *ident, Symbol **out) {
  size_t i, skipped;
  for (i = 0, skipped = 0; i < llist_size(&state->scopes); ++i) {
    Scope *current = llist_get(&state->scopes, i);
    if (scope_get_kind(current) == SCOPE_MEMBER) {
      ++skipped;
      continue;
    }
    /* TODO(Robert): rewrite symbol table functions */
    *out = scope_get_symbol(current, ident);
    if (*out != NULL) break;
  }

  /* return whether or not tag is in current scope */
  return i == skipped;
}

void state_insert_symbol(CompilerState *state, const char *ident,
                         Symbol *symbol) {
  size_t i = 0;
  Scope *current;
  do current = llist_get(&state->scopes, i++);
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
  assert(llist_size(&state->scopes) > 1);
  Symbol *benefactor;
  size_t i;
  for (i = 0; i < llist_size(&state->scopes); ++i) {
    Scope *current = llist_get(&state->scopes, i);
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
    scope_insert_symbol(llist_back(&state->scopes), ident, benefactor);

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
  for (i = 0; i < llist_size(&state->scopes); ++i) {
    Scope *current = llist_get(&state->scopes, i);
    if (scope_get_kind(current) != SCOPE_MEMBER) {
      *out = NULL;
      return 0;
    }
    *out = scope_get_member(current, ident);
    if (*out != NULL) break;
  }

  return i == 0;
}

void state_insert_member(CompilerState *state, const char *ident,
                         Symbol *symbol) {
  Scope *top_scope = llist_front(&state->scopes);
  scope_insert_member(top_scope, ident, symbol);
}

int state_get_tag(CompilerState *state, const char *ident, Tag **out) {
  size_t i, skipped;
  for (i = 0, skipped = 0; i < llist_size(&state->scopes); ++i) {
    Scope *current = llist_get(&state->scopes, i);
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
  size_t i = 0;
  Scope *current;
  do current = llist_get(&state->scopes, i++);
  while (scope_get_kind(current) == SCOPE_MEMBER);
  scope_insert_tag(current, ident, tag);
}

Label *state_get_label(CompilerState *state, const char *ident) {
  Scope *function_scope =
      llist_get(&state->scopes, llist_size(&state->scopes) - 2);
  return scope_get_label(function_scope, ident);
}

void state_insert_label(CompilerState *state, const char *ident, Label *label) {
  Scope *function_table =
      llist_get(&state->scopes, llist_size(&state->scopes) - 2);
  scope_insert_label(function_table, ident, label);
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

const char *state_get_function_name(CompilerState *state) {
  return state->enclosing_function_name;
}

Symbol *state_get_function(CompilerState *state) {
  return state->enclosing_function;
}
