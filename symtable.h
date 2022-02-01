#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"

#define DEFAULT_MAP_SIZE 100

typedef struct symbol_value {
  size_t sequence;  /* used to order declarations in a given block */
  Location loc;     /* declaration location */
  int is_defined;   /* whether this function/struct/union has been
                       specified/defined */
  TypeSpec type;    /* type of symbol */
  int stack_offset; /* location in the current stack frame */
  char obj_loc[64]; /* location, represented as a string */
} SymbolValue;

typedef struct symbol_table {
  Map map;                           /* symbol values */
  struct symbol_table *parent_table; /* scope above this one */
  LinkedList *nested_tables;         /* scope(s) below this one */
  SymbolValue *enclosing_function;   /* function this belongs to, if applicable */
} SymbolTable;

extern SymbolTable *current_table;

/* SymbolValue functions */
SymbolValue *symbol_value_init(const Location *loc);
int symbol_value_destroy(SymbolValue *symbol_value);
int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size);

/* symbol table functions */
SymbolTable *symbol_table_init(void);
int symbol_table_destroy(SymbolTable *table);
int symbol_table_insert(const char *ident, const size_t ident_len,
                  SymbolValue *symval);
int symbol_table_get(const char *ident, const size_t ident_len, SymbolValue **out);
int symbol_table_enter(SymbolTable *table);
int symbol_table_leave(SymbolTable *table);
#endif
