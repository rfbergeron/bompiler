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
int symbol_value_init(SymbolValue *symval, ASTree *tree, size_t sequence_,
                      size_t blocknr_) {
  symval->loc = tree->loc;
  symval->sequence = sequence_;
  return 0;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  if (symbol_value->fields != NULL) {
    DEBUGS('t', "destroying fields");
    map_foreach(symbol_value->fields, symbol_value_destroy_wrapper);
    map_destroy(symbol_value->fields);
  }

  if (symbol_value->parameters != NULL) {
    DEBUGS('t', "destroying parameters");
    /* this cast is undefined behavior but Glib uses similar casts and it should
     * work fine on most implementations
     */
    llist_foreach(symbol_value->parameters,
                  (void (*)(void *, size_t))symbol_value_destroy);
    llist_destroy(symbol_value->parameters);
  }
  DEBUGS('t', "done");
  return 0;
}

FILE *print_symbol_value(FILE *out, const SymbolValue *symval) {
  char type_buf[256];
  type_to_string(symval->type, type_buf, 256);
  fprintf(out, "{ %u, %u, %u } { %u } %s", symval->loc.filenr,
          symval->loc.linenr, symval->loc.offset, symval->loc.blocknr,
          type_buf);
  return out;
}
