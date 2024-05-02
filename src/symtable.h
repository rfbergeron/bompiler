#ifndef __SYMTABLE_H__
#define __SYMTABLE_H__

#include "bcc_types.h"
#include "instr.h"
#include "lyutils.h"
#include "taboe_decl.h"

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
  Instruction *instructions;
  StorageClass storage;
  Linkage linkage;
  ptrdiff_t disp;
  size_t static_id;
  SymbolInfo info;
} Symbol;

typedef enum tag_kind { TAG_STRUCT = 0, TAG_UNION, TAG_ENUM } TagKind;

TABOE_TDEF(EnumTable)

typedef union tag {
  struct {
    TagKind kind;
    int defined;
    size_t width;
    size_t alignment;
    union scope *members;
  } record;
  struct {
    TagKind kind;
    int defined;
    size_t width;
    size_t alignment;
    EnumTable *constants;
  } enumeration;
} Tag;

typedef struct label {
  struct astree *tree;
  int defined;
} Label;

Symbol *symbol_init(const Location *loc);
void symbol_destroy(Symbol *symbol);
int symbol_print(const Symbol *symbol, char *buffer);
int symbol_is_lvalue(const Symbol *symbol);

Tag *tag_init(TagKind kind);
void tag_destroy(Tag *tag);
int tag_print(const Tag *tag, char *buffer, size_t size);
int tag_get_constant(const Tag *enum_tag, const char *enum_id);
int tag_last_constant(const Tag *enum_tag);
void tag_add_constant(Tag *enum_tag, const char *enum_id, int value);

#endif
