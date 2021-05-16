#include "symtable.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"
#include "err.h"
#include "lyutils.h"
#include "string.h"

/*
 * badlib wrapper functions
 */
static void symbol_value_destroy_wrapper(void *unused, void *symval,
                                         size_t unused2, size_t unused3) {
  symbol_value_destroy(symval);
}

/*
 * external functions
 */
int symbol_value_init(SymbolValue *symval, const TypeSpec *type,
                      const Location *loc, const size_t sequence) {
  symval->type = *type;
  symval->loc = *loc;
  symval->sequence = sequence;
  return 0;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  if (symbol_value->type.base == TYPE_STRUCT) {
    DEBUGS('t', "destroying struct member entries");
    map_foreach(symbol_value->type.data.members, symbol_value_destroy_wrapper);
    map_destroy(symbol_value->type.data.members);
  } else if (symbol_value->type.base == TYPE_FUNCTION) {
    DEBUGS('t', "destroying function parameter entries");
    /* this cast is undefined behavior but Glib uses similar casts and it should
     * work fine on most implementations
     */
    llist_foreach(symbol_value->type.data.params,
                  (void (*)(void *, size_t))symbol_value_destroy);
    llist_destroy(symbol_value->type.data.params);
  }

  if (symbol_value->type.nested) {
    /* TODO(Robert): write function to recursively free symbol values */
    free(symbol_value->type.nested);
  }

  DEBUGS('t', "done");
  return 0;
}

FILE *print_symbol_value(FILE *out, const SymbolValue *symval) {
  char type_buf[256];
  type_to_string(&(symval->type), type_buf, 256);
  fprintf(out, "{ %lu, %lu, %lu } { %lu } %s", symval->loc.filenr,
          symval->loc.linenr, symval->loc.offset, symval->loc.blocknr,
          type_buf);
  return out;
}
