#ifndef __SYMVAL_H__
#define __SYMVAL_H__

#include "attributes.h"
#include "lyutils.h"
#include "badlib/badllist.h"

#define DEFAULT_MAP_SIZE 100

/* This structure should be used for grouping together a function or object's
 * name, type, and declaration location
 */
typedef struct symbol_value {
  size_t sequence;          /* used to order declarations in a given block */
  Location loc;             /* declaration location */
  int has_block;            /* whether this is a function definition or prototype */
  TypeSpec type;            /* type of symbol */
  int stack_offset;         /* location in the current stack frame */
  char obj_loc[64];         /* location, represented as a string */
} SymbolValue;

/* the 'stack' of tables */
extern struct llist *tables;
/* store string constants for assembly generation */
extern struct llist *string_constants;

/* SymbolValue functions */
int symbol_value_init(SymbolValue *symbol, const TypeSpec *type,
                      const Location *loc, const size_t sequence);
int symbol_value_destroy(SymbolValue *symbol_value);
void symbol_value_print(const SymbolValue *symbol, FILE *out);

/* symbol utility functions */
int locate_symbol(const char *ident, size_t ident_len, SymbolValue **out);
#endif
