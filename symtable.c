#include "symtable.h"

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "debug.h"
#include "err.h"
#include "simplestack.h"
#include "stdlib.h"
#include "string.h"
#define LINESIZE 1024
#define EMPTY_SYMTABLE = ((SymbolTable){BLIB_MAP_EMPTY, NULL, NULL, NULL});

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

/*
 * SymbolValue functions
 */
SymbolValue *symbol_value_init(const Location *loc, const size_t sequence) {
  SymbolValue *ret = malloc(sizeof(*ret));
  ret->type = SPEC_EMPTY;
  ret->loc = *loc;
  ret->sequence = sequence;
  ret->flag = 0;
  ret->obj_loc[0] = 0;
  typespec_init(&ret->type);
  return ret;
}

int symbol_value_destroy(SymbolValue *symbol_value) {
  DEBUGS('t', "freeing symbol value");
  typespec_destroy(&(symbol_value->type));
  free(symbol_value);

  DEBUGS('t', "done");
  return 0;
}

int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size) {
  if (!symbol || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to symbol_value_print\n");
    return -1;
  }
  char locstr[LINESIZE];
  location_to_string(&symbol->loc, locstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_string(&symbol->type, typestr, LINESIZE);

  return snprintf(buffer, size, "{%s} {%s}", locstr, typestr);
}

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
    tagval->data.members.by_name = symbol_table_init();
    if (tagval->data.members.by_name == NULL)
      /* TODO(Robert): cleanup data on error */
      return NULL;
    int status = llist_init(&tagval->data.members.in_order, NULL, NULL);
    /* TODO(Robert): cleanup data on error */
    if (status) return NULL;
  } else if (tag == TAG_ENUM) {
    /* this map only stores possibly enumerator values; it is not responsible
     * for freeing them
     */
    int status = map_init(&tagval->data.enumerators, DEFAULT_MAP_SIZE, NULL,
                          NULL, strncmp_wrapper);
    if (status) return NULL;
  } else {
    /* error: invalid tag type */
    fprintf(stderr, "ERROR: invalid tag type\n");
    return NULL;
  }
  return tagval;
}

int tag_value_destroy(TagValue *tagval) {
  if (tagval->tag == TAG_STRUCT || tagval->tag == TAG_UNION) {
    int status = llist_destroy(&tagval->data.members.in_order);
    if (status) {
      fprintf(stderr, "your data structures library sucks\n");
      abort();
    }
    /* don't destroy the member table; it is a child of whatever scope the tag
     * was declared in, so it should be destroyed recursively
     */
  } else if (tagval->tag == TAG_ENUM) {
    int status = map_destroy(&tagval->data.enumerators);
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

int tag_value_print(const TagValue *tagval, char *buffer, size_t size) {
  if (!tagval || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to tag_value_print\n");
    return -1;
  }

  int buffer_offset =
      snprintf(buffer, size, "Type: %s, Width: %zu, Align: %zu, Members: {",
               TAG_TYPE_STRINGS[tagval->tag], tagval->width, tagval->alignment);
  /* TODO(Robert): check buffer_offset for snprintf errors */
  /* TODO(Robert): print tags defined within struct and union tags */
  if (tagval->tag == TAG_STRUCT || tagval->tag == TAG_UNION) {
    LinkedList symnames = BLIB_LLIST_EMPTY;
    /* TODO(Robert): handle blib errors */
    int status = llist_init(&symnames, NULL, NULL);
    Map *symbols = &tagval->data.members.by_name->primary_namespace;
    status = map_keys(symbols, &symnames);
    size_t i;
    for (i = 0; i < llist_size(&symnames); ++i) {
      char symbuf[LINESIZE];
      const char *symname = llist_get(&symnames, i);
      SymbolValue *symval = map_get(symbols, (char *)symname, strlen(symname));
      int status = symbol_value_print(symval, symbuf, LINESIZE);
      if (status) return status;
      int characters_printed =
          snprintf(buffer + buffer_offset, size - buffer_offset, "%s: %s, ",
                   symname, symbuf);
      buffer_offset += characters_printed;
    }
  }

  return snprintf(buffer + buffer_offset, size - buffer_offset, "}");
}

/*
 * SymbolTable functions
 */
SymbolTable *symbol_table_init() {
  SymbolTable *table = malloc(sizeof(*table));
  table->tag_namespace = table->label_namespace = NULL;
  int status =
      map_init(&table->primary_namespace, DEFAULT_MAP_SIZE, NULL,
               (void (*)(void *))symbol_value_destroy, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }

  return table;
}

int symbol_table_destroy(SymbolTable *table) {
  DEBUGS('t', "Freeing symbol table");
  int status = map_destroy(&table->primary_namespace);
  if (status) return status;
  if (table->tag_namespace != NULL) {
    int status = map_destroy(table->tag_namespace);
    if (status) return status;
    free(table->tag_namespace);
  }
  if (table->label_namespace != NULL) {
    int status = map_destroy(table->label_namespace);
    if (status) return status;
    free(table->label_namespace);
  }
  free(table);
  return 0;
}

int symbol_table_insert(SymbolTable *table, const char *ident,
                        const size_t ident_len, SymbolValue *symval) {
  return map_insert(&table->primary_namespace, (char *)ident, ident_len,
                    symval);
}

SymbolValue *symbol_table_get(SymbolTable *table, const char *ident,
                     const size_t ident_len) {
  return map_get(&table->primary_namespace, (char*)ident, ident_len);
}

int symbol_table_insert_tag(SymbolTable *table, const char *ident,
                            const size_t ident_len, TagValue *tagval) {
  if (table == NULL) {
    return -1;
  } else if (table->tag_namespace == NULL) {
    table->tag_namespace = malloc(sizeof(Map));
    int status = map_init(table->tag_namespace, DEFAULT_MAP_SIZE, NULL,
                          (void (*)(void *))tag_value_destroy, strncmp_wrapper);
  }
  return map_insert(table->tag_namespace, (char *)ident, ident_len, tagval);
}

TagValue *symbol_table_get_tag(SymbolTable *table, const char *ident,
                               const size_t ident_len) {
  return map_get(table->tag_namespace, (char*)ident, ident_len);
}

int symbol_table_insert_label(SymbolTable *table, const char *ident,
                              const size_t ident_len, LabelValue *labval) {
  if (table->label_namespace == NULL) {
    table->label_namespace = malloc(sizeof(Map));
    int status = map_init(table->label_namespace, DEFAULT_MAP_SIZE, NULL,
                          free, strncmp_wrapper);
    if (status) return status;
  }
  return map_insert(table->label_namespace, (char *)ident, ident_len, labval);
}

LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len) {
  return map_get(table->label_namespace, (char *)ident, ident_len);
}
