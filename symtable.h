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

SymbolValue *symbol_value_init(ASTree *tree, size_t sequence_, size_t blocknr_);
void symbol_value_free(SymbolValue *symbol_value_);
void symbol_value_print(SymbolValue *symbol_value_, FILE *out);
void type_checker_init_globals();
void type_checker_free_globals();
void type_checker_dump_symbols(FILE *out);

// may need functions for printing attributes and attribute sets

// where the magic happens
int check_types(ASTree *root);
int make_symbol_table(ASTree *root);
#endif
