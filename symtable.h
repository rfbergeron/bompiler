#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"

#define DEFAULT_MAP_SIZE 100

typedef struct symbol_value SymbolValue;
typedef struct symbol_table {
  Map primary_namespace;
  Map *tag_namespace;
  Map *label_namespace;
} SymbolTable;

typedef struct symbol_value {
  size_t sequence;  /* used to order declarations in a given block */
  Location loc;     /* declaration location */
  TypeSpec type;    /* type of symbol */
  int flag;         /* context dependent flag */
  char obj_loc[64]; /* location, represented as a string */
} SymbolValue;

typedef enum tag_type { TAG_STRUCT = 0, TAG_UNION, TAG_ENUM } TagType;

typedef struct tag_value {
  size_t width;     /* struct/enum width */
  size_t alignment; /* struct/enum alignment */
  union {
    Map enumerators; /* mapping from names to string constants */
    struct {
      SymbolTable *by_name; /* struct members by name */
      LinkedList in_order;  /* struct members in declaration order */
    } members;
  } data;
  TagType tag;    /* indicates struct, union or enum tag */
  int is_defined; /* used to identify forward declarations */
} TagValue;

typedef struct label_value {
  Location *loc;
  int is_defined;
} LabelValue;

/* SymbolValue functions */
SymbolValue *symbol_value_init(const Location *loc, size_t sequence);
int symbol_value_destroy(SymbolValue *symbol_value);
int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size);

/* TagValue functions */
TagValue *tag_value_init(TagType tag);
int tag_value_destroy(TagValue *tagval);

/* symbol table functions */
SymbolTable *symbol_table_init();
int symbol_table_destroy(SymbolTable *table);
int symbol_table_insert(SymbolTable *table, const char *ident,
                        const size_t ident_len, SymbolValue *symval);
SymbolValue *symbol_table_get(SymbolTable *table, const char *ident,
                     const size_t ident_len);
int symbol_table_insert_tag(SymbolTable *table, const char *ident,
                            const size_t ident_len, TagValue *tagval);
TagValue *symbol_table_get_tag(SymbolTable *table, const char *ident,
                               const size_t ident_len);
int symbol_table_insert_label(SymbolTable *table, const char *ident,
                              const size_t ident_len, LabelValue *labval);
LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len);
#endif
