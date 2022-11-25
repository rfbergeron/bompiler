#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "attributes.h"
#include "badllist.h"
#include "badmap.h"

#define DEFAULT_MAP_SIZE 100

typedef enum table_type {
  TRANS_UNIT_TABLE,
  BLOCK_TABLE,
  MEMBER_TABLE,
  FUNCTION_TABLE
} TableType;

typedef struct symbol_table {
  Map *primary_namespace;
  Map *tag_namespace;
  Map *label_namespace;
  LinkedList *control_stack;
  TableType type;
} SymbolTable;

typedef enum symbol_flag {
  SYMFLAG_NONE = 0,
  SYMFLAG_DEFINED = 1 << 0,
  SYMFLAG_ENUM_CONST = 1 << 1,
  SYMFLAG_INCOMPLETE = 1 << 2,
  SYMFLAG_DECL_EXT = 1 << 3,
  SYMFLAG_LINK_NONE = 1 << 4,
  SYMFLAG_LINK_EXT = 1 << 5,
  SYMFLAG_LINK_INT = 1 << 6,
  SYMFLAG_STORE_AUTO = 1 << 7,
  SYMFLAG_STORE_EXT = 1 << 8,
  SYMFLAG_STORE_STAT = 1 << 9,
  SYMFLAG_INHERIT = 1 << 10,
  SYMFLAG_TYPEDEF = 1 << 11,
  SYMFLAG_TYPENAME = 1 << 12,
  SYMFLAG_OLD_FN = 1 << 13,
  SYMFLAGS_STORE = SYMFLAG_STORE_EXT | SYMFLAG_STORE_AUTO | SYMFLAG_STORE_STAT,
  SYMFLAGS_LINK = SYMFLAG_LINK_NONE | SYMFLAG_LINK_EXT | SYMFLAG_LINK_INT
} SymbolFlag;

typedef struct symbol_value {
  size_t sequence;    /* used to order declarations in a given block */
  Location loc;       /* declaration location */
  TypeSpec type;      /* type of symbol */
  unsigned int flags; /* flags, as enumerated above */
  ptrdiff_t disp;     /* displacement on stack/in struct */
  size_t static_id;   /* unique id for static local variables */
} SymbolValue;

typedef enum tag_type { TAG_STRUCT = 0, TAG_UNION, TAG_ENUM } TagType;

typedef struct tag_value {
  size_t width;     /* struct/enum width */
  size_t alignment; /* struct/enum alignment */
  union {
    struct {
      Map by_name; /* mapping from names to string constants */
      LinkedList struct_name_spaces; /* temporary struct member storage */
      int last_value;                /* value of last enum constant */
    } enumerators;
    struct {
      SymbolTable *by_name; /* struct members by name */
      LinkedList in_order;  /* struct members in declaration order */
    } members;
  } data;
  TagType tag;    /* indicates struct, union or enum tag */
  int is_defined; /* used to identify forward declarations */
} TagValue;

typedef enum control_type {
  CTRL_BREAK,
  CTRL_CONTINUE,
  CTRL_GOTO,
  CTRL_RETURN,
  CTRL_CASE,
  CTRL_DEFAULT
} ControlType;

typedef struct control_value {
  struct astree *tree;
  ControlType type;
} ControlValue;

typedef struct label_value {
  struct astree *tree;
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
SymbolTable *symbol_table_init(TableType type);
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
