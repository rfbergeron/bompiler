#ifndef __SYMVAL_H__
#define __SYMVAL_H__

#include <stdint.h>
#include <string.h>

#include "astree.h"
#include "attributes.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "lyutils.h"

typedef struct symbol_value {
  /* This structure should be used for grouping together a function or object's
   * name, type, and declaration location
   */
  size_t sequence; /* used to order declarations in a given block */
  Location loc;    /* declaration location */
  int has_block;   /* whether this is a function definition or prototype */
  TypeSpec type;   /* type of symbol */
} SymbolValue;

int symbol_value_init(SymbolValue *symbol, const TypeSpec *type,
                      const Location *loc, const size_t sequence);
int symbol_value_destroy(SymbolValue *symbol_value);
#endif
