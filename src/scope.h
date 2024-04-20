#ifndef __SCOPE_H__
#define __SCOPE_H__

#include <stddef.h>

#include "taboe_decl.h"

typedef enum scope_kind {
  SCOPE_MEMBER,
  SCOPE_BLOCK,
  SCOPE_FUNCTION,
  SCOPE_FILE
} ScopeKind;

typedef union scope Scope;
struct symbol;
union tag;
struct label;

Scope *scope_init(ScopeKind type);
void scope_destroy(Scope *scope);
ScopeKind scope_get_kind(Scope *scope);

void scope_insert_symbol(Scope *scope, const char *ident,
                         struct symbol *symbol);
struct symbol *scope_get_symbol(Scope *scope, const char *ident);
void scope_symbol_at(Scope *scope, size_t index, const char **id_out,
                     struct symbol **sym_out);
size_t scope_symbol_count(Scope *scope);

void scope_insert_member(Scope *scope, const char *ident,
                         struct symbol *symbol);
struct symbol *scope_get_member(Scope *scope, const char *ident);
void scope_member_at(Scope *scope, size_t index, const char **id_out,
                     struct symbol **sym_out);
size_t scope_member_count(Scope *scope);

void scope_insert_tag(Scope *scope, const char *ident, union tag *tag);
union tag *scope_get_tag(Scope *scope, const char *ident);
void scope_tag_at(Scope *scope, size_t index, const char **id_out,
                  union tag **tag_out);
size_t scope_tag_count(Scope *scope);

void scope_insert_label(Scope *scope, const char *ident, struct label *label);
struct label *scope_get_label(Scope *scope, const char *ident);
void scope_label_at(Scope *scope, size_t index, const char **id_out,
                    struct label **lab_out);
size_t scope_label_count(Scope *scope);

#endif
