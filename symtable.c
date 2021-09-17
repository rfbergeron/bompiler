#include "symtable.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "err.h"
#include "simplestack.h"
#include "stdlib.h"
#include "string.h"

DECLARE_STACK(nrstack, size_t);
/* the 'stack' of tables */
static struct llist tables = BLIB_LLIST_EMPTY;
static struct nrstack block_nrs = {NULL, 0, 0};
static struct nrstack sequence_nrs = {NULL, 0, 0};
static SymbolValue *current_function = NULL;

/*
 * wrapper functions for use with badlib
 */
static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_IDENT_LEN);
  }
  return ret;
}

/*
 * SymbolValue functions
 */
SymbolValue *symbol_value_init(const Location *loc) {
  SymbolValue *ret = malloc(sizeof(*ret));
  ret->type = SPEC_EMPTY;
  ret->loc = *loc;
  ret->sequence = nrstack_postfix_inc(&sequence_nrs);
  typespec_init(&ret->type);
  return ret;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  typespec_destroy(&(symbol_value->type));
  free(symbol_value);

  DEBUGS('t', "done");
  return 0;
}

void symbol_value_print(const SymbolValue *symbol, FILE *out) {
  char type_buf[256];
  type_to_string(&(symbol->type), type_buf, 256);
  fprintf(out, "{ %lu, %lu, %lu } { %lu } %s", symbol->loc.filenr,
          symbol->loc.linenr, symbol->loc.offset, symbol->loc.blocknr,
          type_buf);
}

void symbol_table_init_globals() {
  /* stacks */
  nrstack_init(&block_nrs, 120);
  nrstack_init(&sequence_nrs, 120);
  /* linked lists, the syntax tree is responsible for freeing symbol tables so
   * the linked list just has to clean itself up
   */
  llist_init(&tables, NULL, NULL);
}

void symbol_table_free_globals() {
  DEBUGS('t', "DESTROYING SYMTABLES");
  llist_destroy(&tables);
  nrstack_destroy(&block_nrs);
  nrstack_destroy(&sequence_nrs);
  DEBUGS('t', "  SYMTABLES DESTROYED");
}

int insert_symbol(const char *ident, const size_t ident_len,
                  SymbolValue *symval) {
  return map_insert(llist_front(&tables), (void *)ident, ident_len, symval);
}

int locate_symbol(const char *ident, const size_t ident_len,
                  SymbolValue **out) {
  size_t i;
  for (i = 0; i < llist_size(&tables); ++i) {
    struct map *current_table = llist_get(&tables, i);
    *out = map_get(current_table, (char *)ident, ident_len);
    if (*out) break;
  }
  /* true if the top of the stack (current scope) contains the symbol */
  return !i;
}

/* scopes and blocks are kept separate; although a new scope is created with
 * each block, it is not the case that a new block is created with each scope
 */
int create_scope(Map *scope) {
  /* prevent scope from being created twice, specifically for compound
   * statements
   */
  if (llist_front(&tables) == scope) return 1;
  int status =
      map_init(scope, DEFAULT_MAP_SIZE, NULL,
               (void (*)(void *))symbol_value_destroy, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
  llist_push_front(&tables, scope);
  nrstack_push(&block_nrs, 0);
  nrstack_push(&sequence_nrs, 0);
  return 0;
}

int finalize_scope(Map *scope) {
  /* make sure that the caller is finalizing the scope they think they are */
  if (llist_front(&tables) != scope) return 1;
  llist_pop_front(&tables);
  nrstack_pop(&sequence_nrs);
  nrstack_pop(&block_nrs);
  nrstack_postfix_inc(&block_nrs);
  return 0;
}

int enter_scope(Map *scope) {
  /* prevent scope from being entered twice; this should not happen during a
   * correct execution, but we will try to prevent it here anyways
   */
  if (llist_front(&tables) == scope) return 1;
  llist_push_front(&tables, scope);
  return 0;
}

int leave_scope(Map *scope) {
  /* make sure that the caller is leaving the scope they think they are */
  if (llist_front(&tables) != scope) return 1;
  llist_pop_front(&tables);
  return 0;
}

int enter_body(Map *scope, SymbolValue *symbol) {
  current_function = symbol;
  return enter_scope(scope);
}

int leave_body(Map *scope, SymbolValue *symbol) {
  if (current_function != symbol) return -1;
  current_function = NULL;
  symbol->is_defined = 1;
  return finalize_scope(scope);
}

int get_ret_type(TypeSpec *out) {
  return strip_aux_type(out, &current_function->type);
}
