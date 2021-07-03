#include "symval.h"

#include "stdlib.h"
#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "simplestack.h"
#include "debug.h"
#include "err.h"
#include "string.h"

DECLARE_STACK(nrstack, size_t);
/* the 'stack' of tables */
static struct llist *tables;
static struct nrstack block_nrs = {NULL, 0, 0};
static struct nrstack sequence_nrs = {NULL, 0, 0};
static const size_t MAX_STRING_LENGTH = 31;

/*
 * wrapper functions for use with badlib
 */
static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_STRING_LENGTH);
  }
  return ret;
}

void symbol_table_destroy(Map *table) {
  /* destroy table, which frees symbol values */
  map_destroy(table);
  /* free the space allocated for the table itself */
  free(table);
}

/*
 * SymbolValue functions
 */
SymbolValue *symbol_value_init(const TypeSpec *type, const Location *loc) {
  SymbolValue *ret = malloc(sizeof(*ret));
  ret->type = *type;
  ret->loc = *loc;
  ret->sequence = nrstack_postfix_inc(&sequence_nrs);
  return ret;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  if (symbol_value->type.base == TYPE_STRUCT) {
    DEBUGS('t', "destroying struct member entries");
    map_destroy(symbol_value->type.data.members);
  } else if (symbol_value->type.base == TYPE_FUNCTION) {
    DEBUGS('t', "destroying function parameter entries");
    llist_destroy(symbol_value->type.data.params);
  }

  if (symbol_value->type.nested) {
    /* TODO(Robert): write function to recursively free symbol values */
    free(symbol_value->type.nested);
  }

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
  /* linked lists */
  tables = malloc(sizeof(*tables));
  llist_init(tables, (void (*)(void *)) symbol_table_destroy, NULL);
}

void symbol_table_free_globals() {
  DEBUGS('t', "DESTROYING SYMTABLES");
  llist_destroy(tables);
  nrstack_destroy(&block_nrs);
  nrstack_destroy(&sequence_nrs);
  DEBUGS('t', "  SYMTABLES DESTROYED");
}

int insert_symbol(const char *ident, const size_t ident_len, SymbolValue *symval) {
    return map_insert(llist_front(tables), (void *) ident, ident_len, symval);
}

int locate_symbol(const char *ident, const size_t ident_len, SymbolValue **out) {
  size_t i;
  for (i = 0; i < llist_size(tables); ++i) {
    struct map *current_table = llist_get(tables, i);
    *out = map_get(current_table, (char *)ident, ident_len);
    if (*out) break;
  }
  /* true if the top of the stack (current scope) contains the symbol */
  return !i;
}

/* scopes and blocks are kept separate; although a new scope is created with
 * each block, it is not the case that a new block is created with each scope
 */
/* create_scope and finalize_scope are for use by the type checker; enter_scope
 * and leave_scope are for use by the assembly generator
 */
void create_scope(Map **scope_location) {
  *scope_location = malloc(sizeof(Map));
  int status = map_init(*scope_location, DEFAULT_MAP_SIZE, NULL,
              (void (*)(void *)) symbol_value_destroy, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
  llist_push_front(tables, *scope_location);
  nrstack_push(&block_nrs, 0);
  nrstack_push(&sequence_nrs, 0);
}

void finalize_scope() {
  llist_pop_front(tables);
  nrstack_pop(&sequence_nrs);
  nrstack_pop(&block_nrs);
  nrstack_postfix_inc(&block_nrs);
}

void enter_scope(Map *scope) {
    llist_push_front(tables, scope);
}

void leave_scope() {
    llist_pop_front(tables);
}
