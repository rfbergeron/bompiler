#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include <stdint.h>
#include <string.h>

#include "astree.h"
#include "attributes.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "lyutils.h"

typedef struct SymbolValue SymbolValue;

struct SymbolValue {
  /* TODO(Robert): fields and parameters should be a part of the type, not the
   * symbol value.
   *
   * This structure should be used for grouping together a function or object's
   * name, type, and declaration location
   */
  struct typespec type; /* type of symbol */
  size_t sequence;
  Location loc;  /* declaration location */
  int has_block; /* whether this is a function definition or prototype */
};

int symbol_value_init(SymbolValue *symval, ASTree *tree, size_t sequence_,
                      size_t blocknr_);
int symbol_value_destroy(SymbolValue *symbol_value);
#endif
