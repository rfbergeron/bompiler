#include "symval.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"
#include "err.h"
#include "lyutils.h"
#include "string.h"

struct llist *tables;
struct llist *string_constants;

/*
 * SymbolValue functions
 */
int symbol_value_init(SymbolValue *symbol, const TypeSpec *type,
                      const Location *loc, const size_t sequence) {
  symbol->type = *type;
  symbol->loc = *loc;
  symbol->sequence = sequence;
  return 0;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  if (symbol_value->type.base == TYPE_STRUCT) {
    DEBUGS('t', "destroying struct member entries");
    map_foreach_value(symbol_value->type.data.members,
                      (void (*)(void *))symbol_value_destroy);
    map_destroy(symbol_value->type.data.members);
  } else if (symbol_value->type.base == TYPE_FUNCTION) {
    DEBUGS('t', "destroying function parameter entries");
    /* this cast is undefined behavior but Glib uses similar casts and it should
     * work fine on most implementations
     */
    llist_foreach(symbol_value->type.data.params,
                  (void (*)(void *))symbol_value_destroy);
    llist_destroy(symbol_value->type.data.params);
  }

  if (symbol_value->type.nested) {
    /* TODO(Robert): write function to recursively free symbol values */
    free(symbol_value->type.nested);
  }

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

int locate_symbol(const char *ident, size_t ident_len, SymbolValue **out) {
  size_t i;
  for (i = 0; i < llist_size(tables); ++i) {
    struct map *current_table = llist_get(tables, i);
    *out = map_get(current_table, (char *)ident, ident_len);
    if (*out) break;
  }
  /* true if the top of the stack (current scope) contains the symbol */
  return !i;
}

