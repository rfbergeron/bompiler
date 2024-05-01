#include "symtable.h"

#include "assert.h"
#include "astree.h"
#include "badalist.h"
#include "badllist.h"
#include "badmap.h"
#include "ctype.h"
#include "debug.h"
#include "err.h"
#include "murmur3/murmur3.h"
#include "scope.h"
#include "simplestack.h"
#include "state.h"
#include "stdlib.h"
#include "string.h"
#include "taboe_impl.h"
#include "yyparse.h"

#define LINESIZE 1024
#define MAX_IDENT_LEN 31

const char TAG_TYPE_STRINGS[][8] = {
    "struct",
    "union",
    "enum",
};

/*
 * Symbol functions
 */
Symbol *symbol_init(const Location *loc) {
  Symbol *symbol = malloc(sizeof(*symbol));
  symbol->loc = loc;
  symbol->type = NULL;
  symbol->instructions = NULL;
  symbol->linkage = LINK_NONE;
  symbol->storage = STORE_AUTO;
  symbol->disp = 0;
  symbol->static_id = 0;
  symbol->info = SYM_NONE;
  return symbol;
}

void symbol_destroy(Symbol *symbol) {
  PFDBG0('t', "freeing symbol");
  if (symbol == NULL) return;

  /* type information owned by outer symbol */
  if (symbol->info != SYM_INHERITOR) type_destroy(symbol->type);
  free(symbol);

  PFDBG0('t', "done");
}

#ifndef UNIT_TESTING
int symbol_print(const Symbol *symbol, char *buffer) {
  static const char *LINKAGE_STRINGS[] = {"NONE",   "EXTERNAL", "INTERNAL",
                                          "MEMBER", "TYPEDEF",  "ENUM_CONST"};
  static const char *STORAGE_STRINGS[] = {"AUTO",   "EXTERNAL", "STATIC",
                                          "MEMBER", "TYPEDEF",  "ENUM_CONST"};
  static const char *INFO_STRINGS[] = {"UNDEFINED", "DEFINED", "INHERITOR",
                                       "HIDDEN"};
  static char locstr[LINESIZE], typestr[LINESIZE];
  if (!symbol || !buffer) {
    fprintf(stderr, "ERROR: invalid arguments to symbol_print\n");
    return -1;
  }

  location_to_string(symbol->loc, locstr);
  type_to_str(symbol->type, typestr);
  size_t sym_width = type_get_width(symbol->type);
  size_t sym_align = type_get_alignment(symbol->type);
  const char *link_str = LINKAGE_STRINGS[symbol->linkage];
  const char *stor_str = STORAGE_STRINGS[symbol->storage];

  if (symbol->storage == STORE_AUTO) {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s} "
                   "{INFO: %s} {DISPLACEMENT: %li}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str,
                   INFO_STRINGS[symbol->info], symbol->disp);
  } else if (symbol->linkage == LINK_NONE && symbol->storage == STORE_STAT) {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s} "
                   "{INFO: %s} {STATIC ID: %lu}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str,
                   INFO_STRINGS[symbol->info], symbol->static_id);
  } else {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s} "
                   "{INFO: %s}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str,
                   INFO_STRINGS[symbol->info]);
  }
}
#endif

int symbol_is_lvalue(const Symbol *symbol) {
  return type_is_object(symbol->type) && symbol->linkage < LINK_TYPEDEF &&
         symbol->storage < STORE_TYPEDEF;
}

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

TABOE_TYPE(EnumTable, const char *, int, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, EnumTable, enum_table, const char *, int,
           unsigned long, murmur_wrapper, strcmp)
/*
 * Tag functions
 */
Tag *tag_init(TagKind kind) {
  Tag *tag = malloc(sizeof(*tag));
  if (kind == TAG_STRUCT || kind == TAG_UNION) {
    tag->record.kind = kind;
    tag->record.defined = 0;
    tag->record.width = 0;
    tag->record.alignment = 0;
    tag->record.members = scope_init(SCOPE_MEMBER);
  } else {
    assert(kind == TAG_ENUM);
    tag->enumeration.kind = kind;
    tag->enumeration.defined = 0;
    tag->enumeration.width = X64_SIZEOF_INT;
    tag->enumeration.alignment = X64_ALIGNOF_INT;
    tag->enumeration.constants = enum_table_init(DEFAULT_SIZE);
  }
  return tag;
}

void tag_destroy(Tag *tag) {
  if (tag == NULL) {
    return;
  } else if (tag->record.kind == TAG_STRUCT || tag->record.kind == TAG_UNION) {
    scope_destroy(tag->record.members);
    free(tag);
  } else {
    assert(tag->enumeration.kind == TAG_ENUM);
    enum_table_destroy(tag->enumeration.constants);
    free(tag);
  }
}

#ifndef UNIT_TESTING
int tag_print(const Tag *tag, char *buffer, size_t size) {
  if (!tag || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to tag_print\n");
    return -1;
  }

  int buffer_offset =
      sprintf(buffer, "Type: %s, Width: %lu, Align: %lu, Members: {",
              TAG_TYPE_STRINGS[tag->record.kind], tag->record.width,
              tag->record.alignment);
  if (buffer_offset < 0) return -1;
  if (tag->record.kind == TAG_STRUCT || tag->record.kind == TAG_UNION) {
    size_t member_index, member_count = scope_member_count(tag->record.members);
    for (member_index = 0; member_index < member_count; ++member_index) {
      static char symbuf[LINESIZE];
      const char *ident;
      Symbol *member;
      scope_member_at(tag->record.members, member_index, &ident, &member);
      int status = symbol_print(member, symbuf);
      if (status) return status;
      int characters_printed =
          sprintf(buffer + buffer_offset, "%s: %s, ", ident, symbuf);
      buffer_offset += characters_printed;
    }
  }

  return sprintf(buffer + buffer_offset, "}");
}
#endif

int tag_get_constant(const Tag *enum_tag, const char *enum_id) {
  assert(enum_tag->enumeration.kind == TAG_ENUM);
  int constant;
  int exists =
      enum_table_get(enum_tag->enumeration.constants, enum_id, &constant);
  assert(exists);
  return constant;
}

int tag_last_constant(const Tag *enum_tag) {
  assert(enum_tag->enumeration.kind == TAG_ENUM);
  size_t count = enum_table_count(enum_tag->enumeration.constants);
  if (count == 0) {
    return -1;
  } else {
    int value;
    (void)enum_table_at(enum_tag->enumeration.constants, count - 1, &value);
    return value;
  }
}

void tag_add_constant(Tag *enum_tag, const char *enum_id, int value) {
  assert(enum_tag->enumeration.kind == TAG_ENUM);
  enum_table_put(enum_tag->enumeration.constants, enum_id, value);
}
