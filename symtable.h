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
  int attributes[(size_t)NUM_ATTRIBUTES];
  size_t sequence;
  struct map *fields;
  Location loc;
  size_t blocknr;
  struct llist *parameters;
  const char *type_id;
  int has_block;
};

int symbol_value_init(SymbolValue *symval, ASTree *tree, size_t sequence_,
                      size_t blocknr_);
int symbol_value_destroy(SymbolValue *symbol_value);
#endif
