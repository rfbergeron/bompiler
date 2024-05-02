#include "scope.h"

#include <string.h>

#include "ctype.h"
#include "debug.h"
#include "murmur3.h"
#include "symtable.h"
#include "taboe_impl.h"

/* neighborhood size and starting map size. if we started the map at a smaller
 * size, virtual buckets would overlap, resulting in errors
 */
#define DEFAULT_SIZE (sizeof(unsigned long) * CHAR_BIT)

static unsigned long murmur_wrapper(const char *str) {
  /* entirely unnecessary but i think it's funny */
  static const unsigned int KNUTH_MAGIC = 0x9E3779B9U;
  size_t len = strlen(str);
  unsigned long out[2];
  MurmurHash3_x64_128(str, len, KNUTH_MAGIC, out);
  return out[0] ^ out[1];
}

TABOE_TDEF(SymbolTable)
TABOE_TYPE(SymbolTable, const char *, Symbol *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, SymbolTable, symbol_table, const char *, Symbol *,
           unsigned long, murmur_wrapper, strcmp)

TABOE_TDEF(MemberTable)
TABOE_TYPE(MemberTable, const char *, Symbol *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, MemberTable, member_table, const char *, Symbol *,
           unsigned long, murmur_wrapper, strcmp)

TABOE_TDEF(TagTable)
TABOE_TYPE(TagTable, const char *, Tag *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, TagTable, tag_table, const char *, Tag *,
           unsigned long, murmur_wrapper, strcmp)

TABOE_TDEF(LabelTable)
TABOE_TYPE(LabelTable, const char *, Label *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, LabelTable, label_table, const char *, Label *,
           unsigned long, murmur_wrapper, strcmp)

union scope {
  struct {
    ScopeKind kind;
    MemberTable *members;
  } member;
  struct {
    ScopeKind kind;
    SymbolTable *symbols;
    TagTable *tags;
  } block;
  struct {
    ScopeKind kind;
    SymbolTable *symbols;
    TagTable *tags;
    LabelTable *labels;
  } function;
  struct {
    ScopeKind kind;
    SymbolTable *symbols;
    TagTable *tags;
  } file;
};

Scope *scope_init(ScopeKind kind) {
  Scope *scope = malloc(sizeof(*scope));
  switch (kind) {
    case SCOPE_MEMBER:
      scope->member.kind = kind;
      scope->member.members = member_table_init(DEFAULT_SIZE);
      return scope;
    case SCOPE_BLOCK:
      scope->block.kind = kind;
      scope->block.tags = tag_table_init(DEFAULT_SIZE);
      scope->block.symbols = symbol_table_init(DEFAULT_SIZE);
      return scope;
    case SCOPE_FUNCTION:
      scope->function.kind = kind;
      scope->function.labels = label_table_init(DEFAULT_SIZE);
      scope->function.tags = tag_table_init(DEFAULT_SIZE);
      scope->function.symbols = symbol_table_init(DEFAULT_SIZE);
      return scope;
    case SCOPE_FILE:
      scope->file.kind = kind;
      scope->file.tags = tag_table_init(DEFAULT_SIZE);
      scope->file.symbols = symbol_table_init(DEFAULT_SIZE);
      return scope;
    default:
      abort();
  }
}

static void destroy_unique_name(const char *str) {
  if (isdigit(str[0])) free((char *)str);
}

void scope_destroy(Scope *scope) {
  PFDBG0('t', "Freeing scope");
  if (scope == NULL) return;
  size_t index, count;
  Tag *tag;
  Symbol *symbol;
  Label *label;
  const char *ident;
  switch (scope->member.kind) {
    case SCOPE_MEMBER:
      count = member_table_count(scope->member.members);
      for (index = 0; index < count; ++index) {
        (void)member_table_at(scope->member.members, index, &symbol);
        symbol_destroy(symbol);
        (void)member_table_key_at(scope->member.members, index, &ident);
        destroy_unique_name(ident);
      }
      member_table_destroy(scope->member.members);
      break;
    case SCOPE_BLOCK:
      count = tag_table_count(scope->block.tags);
      for (index = 0; index < count; ++index) {
        (void)tag_table_at(scope->block.tags, index, &tag);
        tag_destroy(tag);
        (void)tag_table_key_at(scope->block.tags, index, &ident);
        destroy_unique_name(ident);
      }
      tag_table_destroy(scope->block.tags);
      count = symbol_table_count(scope->block.symbols);
      for (index = 0; index < count; ++index) {
        (void)symbol_table_at(scope->block.symbols, index, &symbol);
        symbol_destroy(symbol);
        (void)symbol_table_key_at(scope->block.symbols, index, &ident);
        destroy_unique_name(ident);
      }
      symbol_table_destroy(scope->block.symbols);
      break;
    case SCOPE_FUNCTION:
      count = label_table_count(scope->function.labels);
      for (index = 0; index < count; ++index) {
        (void)label_table_at(scope->function.labels, index, &label);
        free(label);
      }
      label_table_destroy(scope->function.labels);
      count = tag_table_count(scope->function.tags);
      for (index = 0; index < count; ++index) {
        (void)tag_table_at(scope->function.tags, index, &tag);
        tag_destroy(tag);
        (void)tag_table_key_at(scope->function.tags, index, &ident);
        destroy_unique_name(ident);
      }
      tag_table_destroy(scope->function.tags);
      count = symbol_table_count(scope->function.symbols);
      for (index = 0; index < count; ++index) {
        (void)symbol_table_at(scope->function.symbols, index, &symbol);
        symbol_destroy(symbol);
        (void)symbol_table_key_at(scope->function.symbols, index, &ident);
        destroy_unique_name(ident);
      }
      symbol_table_destroy(scope->function.symbols);
      break;
    case SCOPE_FILE:
      count = tag_table_count(scope->file.tags);
      for (index = 0; index < count; ++index) {
        (void)tag_table_at(scope->file.tags, index, &tag);
        tag_destroy(tag);
        (void)tag_table_key_at(scope->file.tags, index, &ident);
        destroy_unique_name(ident);
      }
      tag_table_destroy(scope->file.tags);
      count = symbol_table_count(scope->file.symbols);
      for (index = 0; index < count; ++index) {
        (void)symbol_table_at(scope->file.symbols, index, &symbol);
        symbol_destroy(symbol);
        (void)symbol_table_key_at(scope->file.symbols, index, &ident);
        destroy_unique_name(ident);
      }
      symbol_table_destroy(scope->file.symbols);
      break;
    default:
      abort();
  }
  free(scope);
}

ScopeKind scope_get_kind(Scope *scope) { return scope->member.kind; }

void scope_insert_symbol(Scope *scope, const char *ident, Symbol *symbol) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      assert(symbol_table_get(scope->block.symbols, ident, NULL) == 0);
      symbol_table_put(scope->block.symbols, ident, symbol);
      return;
    case SCOPE_FUNCTION:
      assert(symbol_table_get(scope->function.symbols, ident, NULL) == 0);
      symbol_table_put(scope->function.symbols, ident, symbol);
      return;
    case SCOPE_FILE:
      assert(symbol_table_get(scope->file.symbols, ident, NULL) == 0);
      symbol_table_put(scope->file.symbols, ident, symbol);
      return;
  }
}

Symbol *scope_get_symbol(Scope *scope, const char *ident) {
  assert(scope->member.kind != SCOPE_MEMBER);
  Symbol *symbol;
  int exists;
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      exists = symbol_table_get(scope->block.symbols, ident, &symbol);
      return exists ? symbol : NULL;
    case SCOPE_FUNCTION:
      exists = symbol_table_get(scope->function.symbols, ident, &symbol);
      return exists ? symbol : NULL;
    case SCOPE_FILE:
      exists = symbol_table_get(scope->file.symbols, ident, &symbol);
      return exists ? symbol : NULL;
  }
}

void scope_symbol_at(Scope *scope, size_t index, const char **id_out,
                     struct symbol **sym_out) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      assert(index < symbol_table_count(scope->block.symbols));
      if (id_out != NULL)
        (void)symbol_table_key_at(scope->block.symbols, index, id_out);
      if (sym_out != NULL)
        (void)symbol_table_at(scope->block.symbols, index, sym_out);
      return;
    case SCOPE_FUNCTION:
      assert(index < symbol_table_count(scope->function.symbols));
      if (id_out != NULL)
        (void)symbol_table_key_at(scope->function.symbols, index, id_out);
      if (sym_out != NULL)
        (void)symbol_table_at(scope->function.symbols, index, sym_out);
      return;
    case SCOPE_FILE:
      assert(index < symbol_table_count(scope->file.symbols));
      if (id_out != NULL)
        (void)symbol_table_key_at(scope->file.symbols, index, id_out);
      if (sym_out != NULL)
        (void)symbol_table_at(scope->file.symbols, index, sym_out);
      return;
  }
}

size_t scope_symbol_count(Scope *scope) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      return symbol_table_count(scope->block.symbols);
    case SCOPE_FUNCTION:
      return symbol_table_count(scope->function.symbols);
    case SCOPE_FILE:
      return symbol_table_count(scope->file.symbols);
  }
}

void scope_insert_member(Scope *scope, const char *ident, Symbol *symbol) {
  assert(scope->member.kind == SCOPE_MEMBER);
  assert(member_table_get(scope->member.members, ident, NULL) == 0);
  member_table_put(scope->member.members, ident, symbol);
}

Symbol *scope_get_member(Scope *scope, const char *ident) {
  assert(scope->member.kind == SCOPE_MEMBER);
  Symbol *member;
  int exists = member_table_get(scope->member.members, ident, &member);
  return exists ? member : NULL;
}

void scope_member_at(Scope *scope, size_t index, const char **id_out,
                     Symbol **sym_out) {
  assert(scope->member.kind == SCOPE_MEMBER);
  assert(index < member_table_count(scope->member.members));
  if (id_out != NULL)
    (void)member_table_key_at(scope->member.members, index, id_out);
  if (sym_out != NULL)
    (void)member_table_at(scope->member.members, index, sym_out);
}

size_t scope_member_count(Scope *scope) {
  assert(scope->member.kind == SCOPE_MEMBER);
  return member_table_count(scope->member.members);
}

void scope_insert_tag(Scope *scope, const char *ident, Tag *tag) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      assert(tag_table_get(scope->block.tags, ident, NULL) == 0);
      tag_table_put(scope->block.tags, ident, tag);
      return;
    case SCOPE_FUNCTION:
      assert(tag_table_get(scope->function.tags, ident, NULL) == 0);
      tag_table_put(scope->function.tags, ident, tag);
      return;
    case SCOPE_FILE:
      assert(tag_table_get(scope->file.tags, ident, NULL) == 0);
      tag_table_put(scope->file.tags, ident, tag);
      return;
  }
}

Tag *scope_get_tag(Scope *scope, const char *ident) {
  assert(scope->member.kind != SCOPE_MEMBER);
  Tag *tag;
  int exists;
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      exists = tag_table_get(scope->block.tags, ident, &tag);
      return exists ? tag : NULL;
    case SCOPE_FUNCTION:
      exists = tag_table_get(scope->function.tags, ident, &tag);
      return exists ? tag : NULL;
    case SCOPE_FILE:
      exists = tag_table_get(scope->file.tags, ident, &tag);
      return exists ? tag : NULL;
  }
}

void scope_tag_at(Scope *scope, size_t index, const char **id_out,
                  Tag **tag_out) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      assert(index < tag_table_count(scope->block.tags));
      if (id_out != NULL)
        (void)tag_table_key_at(scope->block.tags, index, id_out);
      if (tag_out != NULL)
        (void)tag_table_at(scope->block.tags, index, tag_out);
      return;
    case SCOPE_FUNCTION:
      assert(index < tag_table_count(scope->function.tags));
      if (id_out != NULL)
        (void)tag_table_key_at(scope->function.tags, index, id_out);
      if (tag_out != NULL)
        (void)tag_table_at(scope->function.tags, index, tag_out);
      return;
    case SCOPE_FILE:
      assert(index < tag_table_count(scope->file.tags));
      if (id_out != NULL)
        (void)tag_table_key_at(scope->file.tags, index, id_out);
      if (tag_out != NULL) (void)tag_table_at(scope->file.tags, index, tag_out);
      return;
  }
}

size_t scope_tag_count(Scope *scope) {
  assert(scope->member.kind != SCOPE_MEMBER);
  switch (scope->member.kind) {
    default:
      /* fallthrough */
    case SCOPE_MEMBER:
      abort();
    case SCOPE_BLOCK:
      return tag_table_count(scope->block.tags);
    case SCOPE_FUNCTION:
      return tag_table_count(scope->function.tags);
    case SCOPE_FILE:
      return tag_table_count(scope->file.tags);
  }
}

void scope_insert_label(Scope *scope, const char *ident, Label *label) {
  assert(scope->member.kind == SCOPE_FUNCTION);
  assert(label_table_get(scope->function.labels, ident, NULL) == 0);
  label_table_put(scope->function.labels, ident, label);
  return;
}

Label *scope_get_label(Scope *scope, const char *ident) {
  assert(scope->member.kind == SCOPE_FUNCTION);
  Label *label;
  int exists = label_table_get(scope->function.labels, ident, &label);
  return exists ? label : NULL;
}

void scope_label_at(Scope *scope, size_t index, const char **id_out,
                    Label **lab_out) {
  assert(scope->member.kind == SCOPE_FUNCTION);
  assert(index < label_table_count(scope->function.labels));
  if (id_out != NULL)
    (void)label_table_key_at(scope->function.labels, index, id_out);
  if (lab_out != NULL)
    (void)label_table_at(scope->function.labels, index, lab_out);
}

size_t scope_label_count(Scope *scope) {
  assert(scope->member.kind == SCOPE_FUNCTION);
  return label_table_count(scope->function.labels);
}
