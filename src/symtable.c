#include "symtable.h"

#include "assert.h"
#include "astree.h"
#include "badalist.h"
#include "badllist.h"
#include "badmap.h"
#include "bcc_err.h"
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
 * SymbolValue functions
 */
SymbolValue *symbol_value_init(const Location *loc, const size_t sequence) {
  SymbolValue *ret = malloc(sizeof(*ret));
  ret->type = NULL;
  ret->loc = *loc;
  ret->sequence = sequence;
  ret->flags = SYMFLAG_NONE;
  ret->disp = 0;
  ret->static_id = 0;
  ret->next_use = NULL;
  return ret;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  if (symbol_value == NULL) return 0;

  int status = type_destroy(symbol_value->type);
  free(symbol_value);

  DEBUGS('t', "done");
  return status;
}

#ifndef UNIT_TESTING
int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size) {
  if (!symbol || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to symbol_value_print\n");
    return -1;
  }
  char locstr[LINESIZE];
  location_to_string(&symbol->loc, locstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_str(symbol->type, typestr, LINESIZE);
  size_t sym_width = type_get_width(symbol->type);
  size_t sym_align = type_get_alignment(symbol->type);
  const char *link_str;
  if (symbol->flags & SYMFLAG_TYPENAME)
    link_str = "TYPENAME";
  else if (symbol->flags & SYMFLAG_TYPEDEF)
    link_str = "TYPEDEF";
  else if (symbol->flags & SYMFLAG_LINK_EXT)
    link_str = "EXTERNAL";
  else if (symbol->flags & SYMFLAG_LINK_INT)
    link_str = "INTERNAL";
  else if (symbol->flags & SYMFLAG_LINK_NONE)
    link_str = "NONE";
  else
    link_str = "UNSPECIFIED";
  const char *stor_str;
  if (symbol->flags & SYMFLAG_TYPENAME)
    stor_str = "TYPENAME";
  else if (symbol->flags & SYMFLAG_TYPEDEF)
    stor_str = "TYPEDEF";
  else if (symbol->flags & SYMFLAG_STORE_EXT)
    stor_str = "EXTERNAL";
  else if (symbol->flags & SYMFLAG_STORE_STAT)
    stor_str = "STATIC";
  else if (symbol->flags & SYMFLAG_STORE_AUTO)
    stor_str = "AUTO";
  else
    stor_str = "UNSPECIFIED";

  /* TODO(Robert): check size without snprintf */
  if (symbol->disp < 0) {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s} "
                   "{DISPLACEMENT: %li}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str,
                   symbol->disp);
  } else if ((symbol->flags & (SYMFLAGS_LINK | SYMFLAGS_STORE)) ==
             (SYMFLAG_LINK_NONE | SYMFLAG_STORE_STAT)) {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s} "
                   "{STATIC ID: %lu}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str,
                   symbol->static_id);
  } else {
    return sprintf(buffer,
                   "{%s} {%s} {WIDTH: %lu, ALIGN: %lu} {LINK: %s, STORE: %s}",
                   locstr, typestr, sym_width, sym_align, link_str, stor_str);
  }
}
#endif

/*
 * TagValue functions
 */
TagValue *tag_value_init(TagType tag) {
  TagValue *tagval = malloc(sizeof(*tagval));
  tagval->tag = tag;
  tagval->width = 0;
  tagval->alignment = 0;
  tagval->is_defined = 0;
  if (tag == TAG_STRUCT || tag == TAG_UNION) {
    tagval->data.members.by_name = symbol_table_init(MEMBER_TABLE);
    if (tagval->data.members.by_name == NULL)
      /* TODO(Robert): cleanup data on error */
      return NULL;
    int status = llist_init(&tagval->data.members.in_order, NULL, NULL);
    /* TODO(Robert): cleanup data on error */
    if (status) return NULL;
  } else if (tag == TAG_ENUM) {
    /* this map stores (char*, int*) pairs, which are the names of enumeration
     * constants and their values, respectively. it is responsible for freeing
     * the resources allocated to store the constants, but not the strings
     */
    int status = map_init(&tagval->data.enumerators.by_name, DEFAULT_MAP_SIZE,
                          NULL, free, strncmp_wrapper);
    if (status) return NULL;
    status =
        llist_init(&tagval->data.enumerators.struct_name_spaces, NULL, NULL);
    if (status) return NULL;
    tagval->data.enumerators.last_value = 0;
  } else {
    /* error: invalid tag type */
    fprintf(stderr, "ERROR: invalid tag type\n");
    return NULL;
  }
  return tagval;
}

int tag_value_destroy(TagValue *tagval) {
  if (tagval == NULL) {
    return 0;
  } else if (tagval->tag == TAG_STRUCT || tagval->tag == TAG_UNION) {
    int status = llist_destroy(&tagval->data.members.in_order);
    if (status) {
      fprintf(stderr, "your data structures library sucks\n");
      abort();
    }
    status = symbol_table_destroy(tagval->data.members.by_name);
    if (status) {
      fprintf(stderr, "unable to destroy struct member table\n");
      abort();
    }
  } else if (tagval->tag == TAG_ENUM) {
    int status = map_destroy(&tagval->data.enumerators.by_name);
    if (status) {
      fprintf(stderr, "your data structures library sucks\n");
      abort();
    }
    status = llist_destroy(&tagval->data.enumerators.struct_name_spaces);
    if (status) {
      fprintf(stderr, "your data structures library sucks\n");
      abort();
    }
  } else {
    /* error: invalid tag type */
    fprintf(stderr, "ERROR: invalid tag type; unable to free tag value.\n");
    return -1;
  }
  free(tagval);
  return 0;
}

#ifndef UNIT_TESTING
int tag_value_print(const TagValue *tagval, char *buffer, size_t size) {
  if (!tagval || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to tag_value_print\n");
    return -1;
  }

  int buffer_offset =
      sprintf(buffer, "Type: %s, Width: %lu, Align: %lu, Members: {",
              TAG_TYPE_STRINGS[tagval->tag], tagval->width, tagval->alignment);
  /* TODO(Robert): check buffer_offset for snprintf errors */
  /* TODO(Robert): print tags defined within struct and union tags */
  if (tagval->tag == TAG_STRUCT || tagval->tag == TAG_UNION) {
    /* TODO(Robert): handle blib errors */
    Map *symbols = tagval->data.members.by_name->primary_namespace;
    ArrayList symnames;
    assert(!alist_init(&symnames, map_size(symbols)));
    assert(!map_keys(symbols, &symnames));
    size_t i;
    for (i = 0; i < alist_size(&symnames); ++i) {
      char symbuf[LINESIZE];
      const char *symname = alist_get(&symnames, i);
      SymbolValue *symval = map_get(symbols, (char *)symname, strlen(symname));
      int status = symbol_value_print(symval, symbuf, LINESIZE);
      if (status) return status;
      int characters_printed =
          sprintf(buffer + buffer_offset, "%s: %s, ", symname, symbuf);
      buffer_offset += characters_printed;
    }
    assert(!alist_destroy(&symnames, NULL));
  }

  return sprintf(buffer + buffer_offset, "}");
}
#endif

/*
 * SymbolTable functions
 */
SymbolTable *symbol_table_init(TableType type) {
  SymbolTable *table = calloc(1, sizeof(*table));
  table->type = type;
  switch (type) {
    case FUNCTION_TABLE:
      table->label_namespace = malloc(sizeof(Map));
      assert(!map_init(table->label_namespace, DEFAULT_MAP_SIZE, NULL, free,
                       strncmp_wrapper));
      /* fallthrough */
    case TRANS_UNIT_TABLE:
    case BLOCK_TABLE:
      table->tag_namespace = malloc(sizeof(Map));
      assert(!map_init(table->tag_namespace, DEFAULT_MAP_SIZE,
                       (void (*)(void *))destroy_unique_name,
                       (void (*)(void *))tag_value_destroy, strncmp_wrapper));
      /* fallthrough */
    case MEMBER_TABLE:
      table->primary_namespace = malloc(sizeof(Map));
      assert(!map_init(table->primary_namespace, DEFAULT_MAP_SIZE,
                       (void (*)(void *))destroy_unique_name,
                       (void (*)(void *))symbol_value_destroy,
                       strncmp_wrapper));
      break;
  }
  return table;
}

int symbol_table_destroy(SymbolTable *table) {
  DEBUGS('t', "Freeing symbol table");
  if (table == NULL) return 0;
  switch (table->type) {
    case FUNCTION_TABLE:
      assert(!map_destroy(table->label_namespace));
      free(table->label_namespace);
      /* fallthrough */
    case TRANS_UNIT_TABLE:
    case BLOCK_TABLE:
      assert(!map_destroy(table->tag_namespace));
      free(table->tag_namespace);
      /* fallthrough */
    case MEMBER_TABLE:
      assert(!map_destroy(table->primary_namespace));
      free(table->primary_namespace);
      break;
  }
  free(table);
  return 0;
}

int symbol_table_insert(SymbolTable *table, const char *ident,
                        const size_t ident_len, SymbolValue *symval) {
  return map_insert(table->primary_namespace, (char *)ident, ident_len, symval);
}

SymbolValue *symbol_table_get(SymbolTable *table, const char *ident,
                              const size_t ident_len) {
  return map_get(table->primary_namespace, (char *)ident, ident_len);
}

int symbol_table_insert_tag(SymbolTable *table, const char *ident,
                            const size_t ident_len, TagValue *tagval) {
  return map_insert(table->tag_namespace, (char *)ident, ident_len, tagval);
}

TagValue *symbol_table_get_tag(SymbolTable *table, const char *ident,
                               const size_t ident_len) {
  return map_get(table->tag_namespace, (char *)ident, ident_len);
}

int symbol_table_insert_label(SymbolTable *table, const char *ident,
                              const size_t ident_len, LabelValue *labval) {
  return map_insert(table->label_namespace, (char *)ident, ident_len, labval);
}

LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len) {
  return map_get(table->label_namespace, (char *)ident, ident_len);
}
