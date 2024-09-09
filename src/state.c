#include "state.h"

#include <assert.h>
#include <stdlib.h>

#include "arrlist.h"
#include "astree.h"

extern size_t next_vreg(void);
CompilerState *state;

typedef struct switch_info {
  size_t id;
  size_t case_id;
  size_t control_reg;
  const Type *control_type;
  int has_default;
} SwitchInfo;

struct compiler_state {
  ARR_DECL(Scope *, scopes);
  ARR_DECL(SwitchInfo *, switches);
  ARR_DECL(size_t, break_stack);
  ARR_DECL(size_t, continue_stack);
  ARR_DECL(Tag *, record_stack);
  Symbol *enclosing_function;
  const char *enclosing_function_name;
  size_t jump_id_count;
};

CompilerState *state_init(void) {
  CompilerState *state = malloc(sizeof(*state));
  ARR_INIT(state->scopes, 4);
  ARR_INIT(state->switches, 2);
  ARR_INIT(state->break_stack, 2);
  ARR_INIT(state->continue_stack, 2);
  ARR_INIT(state->record_stack, 2);
  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  state->jump_id_count = 0;
  return state;
}

void state_destroy(CompilerState *state) {
  if (state == NULL) return;
  ARR_DESTROY(state->scopes);
  /* free switch info in case of error */
  size_t i;
  for (i = 0; i < ARR_LEN(state->switches); ++i)
    free(ARR_GET(state->switches, i));
  ARR_DESTROY(state->switches);
  ARR_DESTROY(state->break_stack);
  ARR_DESTROY(state->continue_stack);
  ARR_DESTROY(state->record_stack);
  free(state);
}

void state_enter_file(CompilerState *state, const ASTree *root) {
  assert(ARR_EMPTY(state->scopes));
  assert(root->tok_kind == TOK_ROOT);
  assert(root->scope != NULL);
  assert(scope_get_kind(root->scope) == SCOPE_FILE);

  ARR_PUSH(state->scopes, root->scope);
}

void state_leave_file(CompilerState *state) {
  assert(ARR_LEN(state->scopes) == 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FILE);

  ARR_POP(state->scopes);
}

void state_enter_function(CompilerState *state, const ASTree *declarator,
                          const ASTree *body) {
  assert(ARR_LEN(state->scopes) == 1);
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
  ARR_PUSH(state->scopes, body->scope);
}

void state_leave_function(CompilerState *state) {
  assert(ARR_LEN(state->scopes) == 2);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);
  assert(state->enclosing_function != NULL);
  assert(state->enclosing_function_name != NULL);

  state->enclosing_function = NULL;
  state->enclosing_function_name = NULL;
  ARR_POP(state->scopes);
}

void state_enter_prototype(CompilerState *state, const ASTree *param_list) {
  assert(!ARR_EMPTY(state->scopes));
  assert(param_list->scope != NULL);
  assert(state_peek_scope(state) != param_list->scope);
  assert(param_list->tok_kind == TOK_PARAM_LIST);
  assert(scope_get_kind(param_list->scope) == SCOPE_FUNCTION);

  ARR_PUSH(state->scopes, param_list->scope);
}

void state_leave_prototype(CompilerState *state) {
  assert(ARR_LEN(state->scopes) > 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_FUNCTION);

  ARR_POP(state->scopes);
}

void state_enter_block(CompilerState *state, const ASTree *block) {
  assert(ARR_LEN(state->scopes) >= 2);
  assert(block->scope != NULL);
  assert(scope_get_kind(block->scope) == SCOPE_BLOCK);
  assert(state_peek_scope(state) != block->scope);

  ARR_PUSH(state->scopes, block->scope);
}

void state_leave_block(CompilerState *state) {
  assert(ARR_LEN(state->scopes) > 2);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_BLOCK);

  ARR_POP(state->scopes);
}

void state_enter_record(CompilerState *state, Tag *tag) {
  assert(!ARR_EMPTY(state->scopes));
  assert(tag->record.kind != TAG_ENUM);
  assert(tag->record.members != NULL);
  assert(scope_get_kind(tag->record.members) == SCOPE_MEMBER);
  assert(state_peek_scope(state) != tag->record.members);

  ARR_PUSH(state->scopes, tag->record.members);
  ARR_PUSH(state->record_stack, tag);
}

void state_leave_record(CompilerState *state) {
  assert(ARR_LEN(state->scopes) > 1);
  assert(scope_get_kind(state_peek_scope(state)) == SCOPE_MEMBER);

  ARR_POP(state->scopes);
  ARR_POP(state->record_stack);
}

Scope *state_peek_scope(CompilerState *state) {
  return ARR_PEEK(state->scopes);
}

int state_get_symbol(CompilerState *state, const char *ident, Symbol **out) {
  size_t i, skipped, scope_count = ARR_LEN(state->scopes);
  for (i = 1, skipped = 1; i <= scope_count; ++i) {
    Scope *current = ARR_GET(state->scopes, scope_count - i);
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
  size_t i = 1, scope_count = ARR_LEN(state->scopes);
  Scope *current;
  do current = ARR_GET(state->scopes, scope_count - (i++));
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
  Symbol *benefactor;
  size_t i, scope_count = ARR_LEN(state->scopes);
  assert(scope_count > 1);
  for (i = 1; i <= scope_count; ++i) {
    Scope *current = ARR_GET(state->scopes, scope_count - i);
    if (scope_get_kind(current) == SCOPE_MEMBER) continue;
    /* TODO(Robert): rewrite symbol table functions */
    benefactor = scope_get_symbol(current, ident);
    if (benefactor != NULL &&
        !type_compose(benefactor->type, symbol->type, 1)) {
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
    scope_insert_symbol(ARR_GET(state->scopes, 0), ident, benefactor);

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
  size_t i, scope_count = ARR_LEN(state->scopes);
  for (i = 1; i <= scope_count; ++i) {
    Scope *current = ARR_GET(state->scopes, scope_count - i);
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
                         Symbol *member) {
  scope_insert_member(ARR_PEEK(state->scopes), ident, member);
  size_t member_alignment = type_get_alignment(member->type);
  size_t member_width = type_get_width(member->type);

  Tag *tag = ARR_PEEK(state->record_stack);
  if (tag->record.alignment < member_alignment) {
    tag->record.alignment = member_alignment;
  }

  if (tag->record.kind == TAG_STRUCT) {
    size_t padding = member_alignment - (tag->record.width % member_alignment);
    if (padding != member_alignment) tag->record.width += padding;
    member->disp = tag->record.width;
    tag->record.width += member_width;
  } else if (tag->record.width < member_width) {
    tag->record.width = member_width;
  }
}

int state_get_tag(CompilerState *state, const char *ident, Tag **out) {
  size_t i, skipped, scope_count = ARR_LEN(state->scopes);
  for (i = 1, skipped = 1; i <= scope_count; ++i) {
    Scope *current = ARR_GET(state->scopes, scope_count - i);
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
  size_t i = 1, scope_count = ARR_LEN(state->scopes);
  Scope *current;
  do current = ARR_GET(state->scopes, scope_count - (i++));
  while (scope_get_kind(current) == SCOPE_MEMBER);
  scope_insert_tag(current, ident, tag);
}

Label *state_get_label(CompilerState *state, const char *ident) {
  return scope_get_label(ARR_GET(state->scopes, 1), ident);
}

void state_insert_label(CompilerState *state, const char *ident, Label *label) {
  scope_insert_label(ARR_GET(state->scopes, 1), ident, label);
}

size_t state_get_selection_id(CompilerState *state) {
  return ARR_EMPTY(state->switches) ? SIZE_MAX : ARR_PEEK(state->switches)->id;
}

size_t state_get_case_id(CompilerState *state) {
  return ARR_EMPTY(state->switches) ? SIZE_MAX
                                    : ARR_PEEK(state->switches)->case_id;
}

size_t state_get_control_reg(CompilerState *state) {
  return ARR_EMPTY(state->switches) ? SIZE_MAX
                                    : ARR_PEEK(state->switches)->control_reg;
}

int state_get_selection_default(CompilerState *state) {
  return ARR_PEEK(state->switches)->has_default;
}

const Type *state_get_control_type(CompilerState *state) {
  return ARR_EMPTY(state->switches) ? NULL
                                    : ARR_PEEK(state->switches)->control_type;
}

size_t state_get_break_id(CompilerState *state) {
  return ARR_PEEK(state->break_stack);
}

size_t state_get_continue_id(CompilerState *state) {
  return ARR_PEEK(state->continue_stack);
}

size_t state_next_jump_id(CompilerState *state) {
  return state->jump_id_count++;
}

void state_pop_selection_id(CompilerState *state) {
  free(ARR_PEEK(state->switches));
  ARR_POP(state->switches);
}

void state_pop_break_id(CompilerState *state) { ARR_POP(state->break_stack); }

void state_pop_continue_id(CompilerState *state) {
  ARR_POP(state->continue_stack);
}

void state_push_selection_id(CompilerState *state, size_t id) {
  SwitchInfo *info = malloc(sizeof(SwitchInfo));
  info->id = id;
  info->case_id = 0;
  info->has_default = 0;
  info->control_reg = next_vreg();
  info->control_type = NULL;

  ARR_PUSH(state->switches, info);
}

void state_inc_case_id(CompilerState *state) {
  if (!ARR_EMPTY(state->switches)) ++(ARR_PEEK(state->switches)->case_id);
}

void state_set_selection_default(CompilerState *state) {
  /* do not call if there is no switch info */
  assert(ARR_PEEK(state->switches)->has_default == 0);
  ARR_PEEK(state->switches)->has_default = 1;
}

void state_set_control_type(CompilerState *state, const Type *type) {
  assert(ARR_PEEK(state->switches)->control_type == NULL);
  ARR_PEEK(state->switches)->control_type = type;
}

void state_push_break_id(CompilerState *state, size_t id) {
  ARR_PUSH(state->break_stack, id);
}

void state_push_continue_id(CompilerState *state, size_t id) {
  ARR_PUSH(state->continue_stack, id);
}

void state_dec_jump_id_count(CompilerState *state) { --state->jump_id_count; }

const char *state_get_function_name(CompilerState *state) {
  return state->enclosing_function_name;
}

Symbol *state_get_function(CompilerState *state) {
  return state->enclosing_function;
}
