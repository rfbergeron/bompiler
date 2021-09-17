#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"

#define DEFAULT_MAP_SIZE 100

/* This structure should be used for grouping together a function or object's
 * name, type, and declaration location
 */
typedef struct symbol_value {
  size_t sequence;  /* used to order declarations in a given block */
  Location loc;     /* declaration location */
  int is_defined;   /* whether this function/struct/union has been
                       specified/defined */
  TypeSpec type;    /* type of symbol */
  int stack_offset; /* location in the current stack frame */
  char obj_loc[64]; /* location, represented as a string */
  Map *label_namespace;
} SymbolValue;

/* SymbolValue functions */
SymbolValue *symbol_value_init(const Location *loc);
int symbol_value_destroy(SymbolValue *symbol_value);
void symbol_value_print(const SymbolValue *symbol, FILE *out);

/* symbol table functions */
void symbol_table_init_globals();
void symbol_table_free_globals();
int insert_symbol(const char *ident, const size_t ident_len,
                  SymbolValue *symval);
int locate_symbol(const char *ident, const size_t ident_len, SymbolValue **out);
int insert_tag();
int locate_tag();
int create_scope(Map *scope);
int finalize_scope(Map *scope);
int enter_scope(Map *scope);
int leave_scope(Map *scope);
int enter_body(Map *scope, SymbolValue *symbol);
int leave_body(Map *scope, SymbolValue *symbol);
int get_ret_type(TypeSpec *out);
#endif
