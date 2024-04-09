#include "symtable.h"

#include "assert.h"
#include "astree.h"
#include "badalist.h"
#include "badllist.h"
#include "badmap.h"
#include "ctype.h"
#include "debug.h"
#include "err.h"
#include "simplestack.h"
#include "state.h"
#include "stdlib.h"
#include "string.h"
#include "yyparse.h"
#define LINESIZE 1024
#define EMPTY_SYMTABLE = ((SymbolTable){BLIB_MAP_EMPTY, NULL, NULL, NULL});
#define MAX_IDENT_LEN 31

#ifdef UNIT_TESTING
extern void *_test_malloc(const size_t size, const char *file, const int line);
#define malloc(size) _test_malloc(size, __FILE__, __LINE__)
extern void *_test_calloc(const size_t nmemb, const size_t size,
                          const char *file, const int line);
#define calloc(nmemb, size) _test_calloc(nmemb, size, __FILE__, __LINE__)
extern void _test_free(void *ptr, const char *file, const int line);
#define free(ptr) _test_free(ptr, __FILE__, __LINE__)
extern void mock_assert(const int result, const char *const expression,
                        const char *const file, const int line);
#undef assert
#define assert(expression) \
  mock_assert((int)(expression), #expression, __FILE__, __LINE__);
#endif

const char TAG_TYPE_STRINGS[][8] = {
    "struct",
    "union",
    "enum",
};

/*
 * wrapper functions for use with badlib
 */
static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_IDENT_LEN);
  }
  return ret;
}

void destroy_unique_name(const char *str) {
  if (isdigit(str[0])) free((char *)str);
}

/*
 * Symbol functions
 */
Symbol *symbol_init(const Location *loc) {
  Symbol *symbol = malloc(sizeof(*symbol));
  symbol->loc = loc;
  symbol->type = NULL;
  symbol->directive_iter = NULL;
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
  free(symbol->directive_iter);
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

/*
 * Tag functions
 */
Tag *tag_init(TagKind kind) {
  Tag *tag = malloc(sizeof(*tag));
  tag->kind = kind;
  tag->width = 0;
  tag->alignment = 0;
  tag->is_defined = 0;
  if (kind == TAG_STRUCT || kind == TAG_UNION) {
    tag->data.members.by_name = symbol_table_init(TABLE_MEMBER);
    if (tag->data.members.by_name == NULL) {
      free(tag);
      return NULL;
    }
    int status = llist_init(&tag->data.members.in_order, NULL, NULL);
    if (status) {
      symbol_table_destroy(tag->data.members.by_name);
      free(tag);
      return NULL;
    }
  } else if (kind == TAG_ENUM) {
    /* this map stores (char*, int*) pairs, which are the names of enumeration
     * constants and their values, respectively. it is responsible for freeing
     * the resources allocated to store the constants, but not the strings
     */
    int status = map_init(&tag->data.enumerators.by_name, DEFAULT_MAP_SIZE,
                          NULL, free, strncmp_wrapper);
    if (status) return NULL;
    status = llist_init(&tag->data.enumerators.struct_name_spaces, NULL, NULL);
    if (status) return NULL;
    tag->data.enumerators.last_value = 0;
  } else {
    /* error: invalid tag type */
    fprintf(stderr, "ERROR: invalid tag type\n");
    return NULL;
  }
  return tag;
}

void tag_destroy(Tag *tag) {
  if (tag == NULL) {
    return;
  } else if (tag->kind == TAG_STRUCT || tag->kind == TAG_UNION) {
    int status = llist_destroy(&tag->data.members.in_order);
    if (status) {
      fprintf(stderr, "your data structures library sucks\n");
      abort();
    }
    symbol_table_destroy(tag->data.members.by_name);
  } else if (tag->kind == TAG_ENUM) {
    int status = map_destroy(&tag->data.enumerators.by_name);
    if (status) abort();
    status = llist_destroy(&tag->data.enumerators.struct_name_spaces);
    if (status) abort();
  } else {
    abort();
  }
  free(tag);
}

#ifndef UNIT_TESTING
int tag_print(const Tag *tag, char *buffer, size_t size) {
  if (!tag || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to tag_print\n");
    return -1;
  }

  int buffer_offset =
      sprintf(buffer, "Type: %s, Width: %lu, Align: %lu, Members: {",
              TAG_TYPE_STRINGS[tag->kind], tag->width, tag->alignment);
  if (buffer_offset < 0) return -1;
  if (tag->kind == TAG_STRUCT || tag->kind == TAG_UNION) {
    Map *symbols = tag->data.members.by_name->primary_namespace;
    ArrayList symnames;
    int status = alist_init(&symnames, map_size(symbols));
    if (status) abort();
    status = map_keys(symbols, &symnames);
    if (status) abort();
    size_t i;
    for (i = 0; i < alist_size(&symnames); ++i) {
      static char symbuf[LINESIZE];
      const char *symname = alist_get(&symnames, i);
      Symbol *symbol = map_get(symbols, (char *)symname, strlen(symname));
      int status = symbol_print(symbol, symbuf);
      if (status) return status;
      int characters_printed =
          sprintf(buffer + buffer_offset, "%s: %s, ", symname, symbuf);
      buffer_offset += characters_printed;
    }
    status = alist_destroy(&symnames, NULL);
    if (status) abort();
  }

  return sprintf(buffer + buffer_offset, "}");
}
#endif

/*
 * SymbolTable functions
 */
SymbolTable *symbol_table_init(TableKind kind) {
  SymbolTable *table = calloc(1, sizeof(*table));
  table->kind = kind;
  switch (kind) {
    int status;
    case TABLE_FUNCTION:
      table->label_namespace = malloc(sizeof(Map));
      status = map_init(table->label_namespace, DEFAULT_MAP_SIZE, NULL, free,
                        strncmp_wrapper);
      if (status) abort();
      /* fallthrough */
    case TABLE_TRANS_UNIT:
      /* fallthrough */
    case TABLE_BLOCK:
      table->tag_namespace = malloc(sizeof(Map));
      status = map_init(table->tag_namespace, DEFAULT_MAP_SIZE,
                        (void (*)(void *))destroy_unique_name,
                        (void (*)(void *))tag_destroy, strncmp_wrapper);
      if (status) abort();
      /* fallthrough */
    case TABLE_MEMBER:
      table->primary_namespace = malloc(sizeof(Map));
      status = map_init(table->primary_namespace, DEFAULT_MAP_SIZE,
                        (void (*)(void *))destroy_unique_name,
                        (void (*)(void *))symbol_destroy, strncmp_wrapper);
      if (status) abort();
      break;
  }
  return table;
}

void symbol_table_destroy(SymbolTable *table) {
  PFDBG0('t', "Freeing symbol table");
  if (table == NULL) return;
  switch (table->kind) {
    int status;
    case TABLE_FUNCTION:
      status = map_destroy(table->label_namespace);
      if (status) abort();
      free(table->label_namespace);
      /* fallthrough */
    case TABLE_TRANS_UNIT:
      /* fallthrough */
    case TABLE_BLOCK:
      status = map_destroy(table->tag_namespace);
      if (status) abort();
      free(table->tag_namespace);
      /* fallthrough */
    case TABLE_MEMBER:
      status = map_destroy(table->primary_namespace);
      if (status) abort();
      free(table->primary_namespace);
      break;
    default:
      abort();
  }
  free(table);
}

void symbol_table_insert(SymbolTable *table, const char *ident,
                         const size_t ident_len, Symbol *symbol) {
  map_insert(table->primary_namespace, (char *)ident, ident_len, symbol);
}

Symbol *symbol_table_get(SymbolTable *table, const char *ident,
                         const size_t ident_len) {
  return map_get(table->primary_namespace, (char *)ident, ident_len);
}

void symbol_table_insert_tag(SymbolTable *table, const char *ident,
                             const size_t ident_len, Tag *tag) {
  map_insert(table->tag_namespace, (char *)ident, ident_len, tag);
}

Tag *symbol_table_get_tag(SymbolTable *table, const char *ident,
                          const size_t ident_len) {
  return map_get(table->tag_namespace, (char *)ident, ident_len);
}

void symbol_table_insert_label(SymbolTable *table, const char *ident,
                               const size_t ident_len, LabelValue *labval) {
  map_insert(table->label_namespace, (char *)ident, ident_len, labval);
}

LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len) {
  return map_get(table->label_namespace, (char *)ident, ident_len);
}
