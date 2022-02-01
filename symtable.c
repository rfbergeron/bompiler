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
#define LINESIZE 1024
#define EMPTY_SYMTABLE = ((SymbolTable){ BLIB_MAP_EMPTY, NULL, NULL, NULL });

SymbolTable *current_table = NULL;

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
  ret->sequence = map_size(&current_table->map);
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

int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size) {
  if (!symbol || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to symbol_value_print\n");
    return -1;
  }
  char locstr[LINESIZE];
  location_to_string(&symbol->loc, locstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_string(&symbol->type, typestr, LINESIZE);

  return snprintf(buffer, size, "{%s} {%s}", locstr, typestr);
}

/*
 * SymbolTable functions
 */
SymbolTable *symbol_table_init(void) {
  SymbolTable *table = malloc(sizeof(*table));
  int status = map_init(&table->map, DEFAULT_MAP_SIZE, NULL,
               (void (*)(void *))symbol_value_destroy, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
  if (current_table != NULL) {
    if (current_table->nested_tables == NULL) {
      current_table->nested_tables = malloc(sizeof(LinkedList));
      int status = llist_init(current_table->nested_tables,
              (void(*)(void*))symbol_table_destroy, NULL);
      /* TODO(Robert): cleanup on failure */
      if (status) return NULL;
    }
    int status = llist_push_back(current_table->nested_tables, table);
      /* TODO(Robert): cleanup on failure */
    if (status) return NULL;
  }
  table->parent_table = current_table;
  current_table = table;
  return table;
}

int symbol_table_destroy(SymbolTable *table) {
  DEBUGS('t', "Freeing symbol table");
  int status = map_destroy(&table->map);
  if (status) return status;
  if (table->nested_tables != NULL) {
    int status = llist_destroy(table->nested_tables);
    if (status) return status;
    free(table->nested_tables);
  }
  free(table);
  return 0;
}

int symbol_table_insert(const char *ident, const size_t ident_len,
                  SymbolValue *symval) {
  return map_insert(&current_table->map, (void *)ident, ident_len, symval);
}

int symbol_table_get(const char *ident, const size_t ident_len,
                  SymbolValue **out) {
  SymbolTable *table = current_table;
  while (table != NULL) {
    *out = map_get(&table->map, (char *)ident, ident_len);
    if (*out) break;
    table = table->parent_table;
  }
  /* return whether or not the symbol is located in the current scope */
  return table == current_table;
}

int symbol_table_enter(SymbolTable *table) {
  /* prevent scope from being entered twice; this should not happen during a
   * correct execution, but we will try to prevent it here anyways
   */
  if (current_table == table) return 1;
  SymbolTable *old_table = current_table;
  current_table = table;
  /* also indicate an error if the new table is not reachable from the old table */
  return current_table->parent_table != old_table;
}

int symbol_table_leave(SymbolTable *table) {
  /* make sure that the caller is leaving the scope they think they are */
  if (current_table != table) return 1;
  /* do not allow the caller to leave file scope */
  else if (current_table->parent_table == NULL) return 1;
  current_table = current_table->parent_table;
  return 0;
}
