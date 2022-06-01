#include "state.h"

#include "stdlib.h"

CompilerState *state_init(void) {
  CompilerState *state = malloc(sizeof(*state));
  llist_init(&state->table_stack, NULL, NULL);
  llist_init(&state->jump_stack, NULL, NULL);
  state->enclosing_function = NULL;
  return state;
}

int state_destroy(CompilerState *state) {
  llist_destroy(&state->table_stack);
  llist_destroy(&state->jump_stack);
  free(state);
  return 0;
}

int state_push_table(CompilerState *state, SymbolTable *table) {
  if (llist_front(&state->table_stack) == table) {
    fprintf(stderr, "ERROR: attempted to enter same scope twice.\n");
    return -1;
  } else {
    return llist_push_front(&state->table_stack, table);
  }
}

int state_pop_table(CompilerState *state) {
  if (llist_empty(&state->table_stack)) {
    fprintf(stderr, "ERROR: attempted to leave scope at top level.\n");
    return -1;
  } else {
    llist_pop_front(&state->table_stack);
    return 0;
  }
}

int state_push_jump(CompilerState *state, JumpEntry *jump_entry) {
  if (llist_front(&state->jump_stack) == jump_entry) {
    fprintf(stderr, "ERROR: attempted to push same jump target twice.\n");
    return -1;
  } else {
    return llist_push_front(&state->jump_stack, jump_entry);
  }
}

int state_pop_jump(CompilerState *state) {
  if (llist_empty(&state->jump_stack)) {
    fprintf(stderr, "ERROR: attempted to pop jump target from empty stack.\n");
    return -1;
  } else {
    llist_pop_front(&state->jump_stack);
    return 0;
  }
}

int state_push_type_error(CompilerState *state, TypeSpec *errtype) {
  if (llist_front(&state->error_stack) == errtype) {
    fprintf(stderr, "ERROR: attempted to push same type error twice.\n");
    return -1;
  } else {
    return llist_push_front(&state->error_stack, errtype);
  }
}

int state_pop_type_error(CompilerState *state) {
  if (llist_empty(&state->error_stack)) {
    fprintf(stderr, "ERROR: attempted to pop type error from empty stack.\n");
    return -1;
  } else {
    llist_pop_front(&state->error_stack);
    return 0;
  }
}

int state_get_symbol(CompilerState *state, const char *ident,
                     const size_t ident_len, SymbolValue **out) {
  size_t i;
  for (i = 0; i < llist_size(&state->table_stack); ++i) {
    SymbolTable *current = llist_get(&state->table_stack, i);
    /* TODO(Robert): rewrite symbol table functions */
    SymbolValue *symval = symbol_table_get(current, ident, ident_len);
    if (symval != NULL) {
      *out = symval;
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

int state_insert_symbol(CompilerState *state, const char *ident,
                        const size_t ident_len, SymbolValue *symval) {
  SymbolTable *top_scope = llist_front(&state->table_stack);
  /* TODO(Robert): rewrite symbol table functions */
  SymbolValue *exists = symbol_table_get(top_scope, ident, ident_len);
  if (exists) {
    fprintf(stderr, "ERROR: redeclaration of symbol \"%s\".\n", ident);
    return -1;
  } else {
    return symbol_table_insert(top_scope, ident, ident_len, symval);
  }
}

size_t state_get_sequence(CompilerState *state) {
  return map_size(llist_front(&state->table_stack));
}

int state_get_tag(CompilerState *state, const char *ident,
                  const size_t ident_len, TagValue **out) {
  size_t i;
  for (i = 0; i < llist_size(&state->table_stack); ++i) {
    SymbolTable *current = llist_get(&state->table_stack, i);
    TagValue *tagval = symbol_table_get_tag(current, ident, ident_len);
    if (tagval != NULL) {
      *out = tagval;
      break;
    }
  }

  if (i >= llist_size(&state->table_stack)) {
    /* set out to NULL if tag could not be found */
    *out = NULL;
  }

  /* return whether or not tag is in current scope */
  return i == 0;
}

int state_insert_tag(CompilerState *state, const char *ident,
                     const size_t ident_len, TagValue *tagval) {
  SymbolTable *top_scope = llist_front(&state->table_stack);
  TagValue *exists = symbol_table_get_tag(top_scope, ident, ident_len);
  if (exists) {
    /* TODO(Robert): allow redefinition of tags in certain circumstances */
    fprintf(stderr, "ERROR: redeclaration of tag \"%s\".\n", ident);
    return -1;
  } else {
    return symbol_table_insert_tag(top_scope, ident, ident_len, tagval);
  }
}

LabelValue *state_get_label(CompilerState *state, const char *ident,
                            const size_t ident_len) {
  SymbolTable *function_table =
      llist_get(&state->table_stack, llist_size(&state->table_stack) - 1);
  if (function_table == NULL) {
    fprintf(stderr, "ERROR: attempt to get label at top level.\n");
    return NULL;
  }
  return symbol_table_get_label(function_table, ident, ident_len);
}

int state_insert_label(CompilerState *state, const char *ident,
                       const size_t ident_len, LabelValue *labval) {
  SymbolTable *function_table =
      llist_get(&state->table_stack, llist_size(&state->table_stack) - 1);
  if (function_table == NULL) {
    fprintf(stderr, "ERROR: attempt to insert label at top level.\n");
    return -1;
  }
  return symbol_table_insert_label(function_table, ident, ident_len, labval);
}

JumpEntry *state_get_iteration(CompilerState *state) {
  if (!llist_empty(&state->jump_stack)) {
    size_t i;
    for (i = 0; i < llist_size(&state->jump_stack); ++i) {
      JumpEntry *entry = llist_get(&state->jump_stack, i);
      if (entry->type == JUMP_ITERATION) {
        return entry;
      }
    }
  }
  fprintf(stderr, "ERROR: no valid iteration context.\n");
  return NULL;
}

JumpEntry *state_get_switch(CompilerState *state) {
  if (!llist_empty(&state->jump_stack)) {
    size_t i;
    for (i = 0; i < llist_size(&state->jump_stack); ++i) {
      JumpEntry *entry = llist_get(&state->jump_stack, i);
      if (entry->type == JUMP_SWITCH) {
        return entry;
      }
    }
  }
  fprintf(stderr, "ERROR: no valid switch context.\n");
  return NULL;
}

JumpEntry *state_get_jump(CompilerState *state) {
  if (llist_empty(&state->jump_stack)) {
    fprintf(stderr, "ERROR: no valid jump context.\n");
    return NULL;
  } else {
    return llist_front(&state->jump_stack);
  }
}

int state_set_function(CompilerState *state, SymbolValue *function_symval) {
  if (state->enclosing_function != NULL) {
    fprintf(stderr,
            "ERROR: attempt to set enclosing function while inside "
            "another function.\n");
    return -1;
  }
  state->enclosing_function = function_symval;
  return 0;
}

SymbolValue *state_get_function(CompilerState *state) {
  return state->enclosing_function;
}

int state_unset_function(CompilerState *state) {
  if (state->enclosing_function == NULL) {
    fprintf(stderr,
            "ERROR: attempt to unset enclosing function while at file "
            "scope.\n");
    return -1;
  } else {
    state->enclosing_function = NULL;
    return 0;
  }
}
