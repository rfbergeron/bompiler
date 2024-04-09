#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "badllist.h"
#include "badmap.h"
#include "bcc_types.h"
#include "lyutils.h"

#define DEFAULT_MAP_SIZE 100

typedef enum table_kind {
  TABLE_TRANS_UNIT,
  TABLE_BLOCK,
  TABLE_MEMBER,
  TABLE_FUNCTION
} TableKind;

typedef struct symbol_table {
  Map *primary_namespace;
  Map *tag_namespace;
  Map *label_namespace;
  LinkedList *control_stack;
  TableKind kind;
} SymbolTable;

typedef enum linkage {
  LINK_NONE,
  LINK_EXT,
  LINK_INT,
  LINK_MEMBER,
  LINK_TYPEDEF,
  LINK_ENUM_CONST
} Linkage;

typedef enum storage_class {
  STORE_AUTO,
  STORE_EXT,
  STORE_STAT,
  STORE_MEMBER,
  STORE_TYPEDEF,
  STORE_ENUM_CONST
} StorageClass;

typedef enum symbol_info {
  SYM_NONE,
  SYM_DEFINED,
  SYM_INHERITOR,
  SYM_HIDDEN
} SymbolInfo;

typedef struct symbol {
  const Location *loc;
  Type *type;
  ListIter *directive_iter;
  StorageClass storage;
  Linkage linkage;
  ptrdiff_t disp;
  size_t static_id;
  SymbolInfo info;
} Symbol;

typedef enum tag_kind { TAG_STRUCT = 0, TAG_UNION, TAG_ENUM } TagKind;

/* TODO(Robert): make this a tagged union */
typedef struct tag {
  size_t width;     /* struct/enum width */
  size_t alignment; /* struct/enum alignment */
  union {
    struct {
      SymbolTable *by_name; /* struct members by name */
      LinkedList in_order;  /* struct members in declaration order */
    } members;
    struct {
      Map by_name; /* mapping from names to string constants */
      LinkedList struct_name_spaces; /* temporary struct member storage */
      int last_value;                /* value of last enum constant */
    } enumerators;
  } data;
  TagKind kind;   /* indicates struct, union or enum tag */
  int is_defined; /* used to identify forward declarations */
} Tag;

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

/* Symbol functions */
Symbol *symbol_init(const Location *loc);
void symbol_destroy(Symbol *symbol);
int symbol_print(const Symbol *symbol, char *buffer);
int symbol_is_lvalue(const Symbol *symbol);

/* Tag functions */
Tag *tag_init(TagKind kind);
void tag_destroy(Tag *tag);
int tag_print(const Tag *tag, char *buffer, size_t size);

/* symbol table functions */
SymbolTable *symbol_table_init(TableKind type);
void symbol_table_destroy(SymbolTable *table);
void symbol_table_insert(SymbolTable *table, const char *ident,
                         const size_t ident_len, Symbol *symbol);
Symbol *symbol_table_get(SymbolTable *table, const char *ident,
                         const size_t ident_len);
void symbol_table_insert_tag(SymbolTable *table, const char *ident,
                             const size_t ident_len, Tag *tag);
Tag *symbol_table_get_tag(SymbolTable *table, const char *ident,
                          const size_t ident_len);
void symbol_table_insert_label(SymbolTable *table, const char *ident,
                               const size_t ident_len, LabelValue *labval);
LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len);
#endif
