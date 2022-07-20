#include "symtable.h"

#include "assert.h"
#include "astree.h"
#include "attributes.h"
#include "badllist.h"
#include "badmap.h"
#include "bcc_err.h"
#include "debug.h"
#include "err.h"
#include "simplestack.h"
#include "state.h"
#include "stdlib.h"
#include "string.h"
#include "yyparse.h"
#define LINESIZE 1024
#define EMPTY_SYMTABLE = ((SymbolTable){BLIB_MAP_EMPTY, NULL, NULL, NULL});

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

/*
 * SymbolValue functions
 */
SymbolValue *symbol_value_init(const Location *loc, const size_t sequence) {
  SymbolValue *ret = malloc(sizeof(*ret));
  ret->type = SPEC_EMPTY;
  ret->loc = *loc;
  ret->sequence = sequence;
  ret->flags = SYMFLAG_NONE;
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

#ifndef UNIT_TESTING
int symbol_value_print(const SymbolValue *symbol, char *buffer, size_t size) {
  if (!symbol || !buffer || size < 1) {
    fprintf(stderr, "ERROR: invalid arguments to symbol_value_print\n");
    return -1;
  }
  char locstr[LINESIZE];
  location_to_string(&symbol->loc, locstr, LINESIZE);
  char typestr[LINESIZE];
  type_to_string(&symbol->type, typestr, LINESIZE);

  /* TODO(Robert): check size without snprintf */
  return sprintf(buffer, "{%s} {%s}", locstr, typestr);
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
    status = symbol_table_destroy(tagval->data.members.by_name);
    if (status) {
      fprintf(stderr, "unable to destroy struct member table\n");
      abort();
    }
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
          sprintf(buffer + buffer_offset, "%s: %s, ", symname, symbuf);
      buffer_offset += characters_printed;
    }
  }

  return sprintf(buffer + buffer_offset, "}");
}
#endif

/*
 * SymbolTable functions
 */
SymbolTable *symbol_table_init() {
  SymbolTable *table = malloc(sizeof(*table));
  table->tag_namespace = table->label_namespace = NULL;
  table->control_stack = NULL;
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
  if (table->control_stack != NULL) {
    int status = llist_destroy(table->control_stack);
    if (status) return status;
    free(table->control_stack);
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
  return map_get(&table->primary_namespace, (char *)ident, ident_len);
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
  return map_get(table->tag_namespace, (char *)ident, ident_len);
}

int symbol_table_insert_label(SymbolTable *table, const char *ident,
                              const size_t ident_len, LabelValue *labval) {
  if (table->label_namespace == NULL) {
    table->label_namespace = malloc(sizeof(Map));
    int status = map_init(table->label_namespace, DEFAULT_MAP_SIZE, NULL, free,
                          strncmp_wrapper);
    if (status) return status;
  }
  return map_insert(table->label_namespace, (char *)ident, ident_len, labval);
}

LabelValue *symbol_table_get_label(SymbolTable *table, const char *ident,
                                   const size_t ident_len) {
  return map_get(table->label_namespace, (char *)ident, ident_len);
}

int symbol_table_merge_control(SymbolTable *dest, SymbolTable *src) {
  if (src->control_stack == NULL) {
    return 0;
  } else if (dest->control_stack == NULL) {
    dest->control_stack = src->control_stack;
    src->control_stack = NULL;
    return 0;
  } else {
    while (!llist_empty(src->control_stack)) {
      ControlValue *ctrlval = llist_pop_back(src->control_stack);
      if (ctrlval == NULL) {
        return -1;
      }
      int status = llist_push_front(dest->control_stack, ctrlval);
      if (status) {
        return -1;
      }
    }
    int status = llist_destroy(src->control_stack);
    if (status) {
      return -1;
    }
    free(src->control_stack);
    src->control_stack = NULL;
  }
  return 0;
}

TypeSpec *symbol_table_process_control(SymbolTable *table, int parent_symbol) {
  assert(parent_symbol == TOK_DECLARATION || parent_symbol == TOK_SWITCH ||
         parent_symbol == TOK_FOR || parent_symbol == TOK_DO ||
         parent_symbol == TOK_WHILE);
  TypeSpec *ret = NULL;
  size_t i;
  for (i = 0; i < symbol_table_count_control(table); ++i) {
    ControlValue *ctrlval = symbol_table_get_control(table, i);
    /* TODO(Robert): create label information */
    if (parent_symbol == TOK_DECLARATION) {
      if (ctrlval->type == CTRL_GOTO) {
        const char *ident = ctrlval->tree->lexinfo;
        size_t ident_len = strlen(ident);
        LabelValue *labval = state_get_label(state, ident, ident_len);
        if (labval == NULL) {
          AuxSpec *erraux =
              create_erraux(BCC_TERR_SYM_NOT_FOUND, 1, ctrlval->tree);
          if (ret == NULL) {
            ret = calloc(1, sizeof(TypeSpec));
            int status = typespec_init(ret);
            if (status) abort();
            ret->base = TYPE_ERROR;
          }
          int status = llist_push_back(&ret->auxspecs, erraux);
          if (status) abort();
        }
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      } else {
        AuxSpec *erraux =
            create_erraux(BCC_TERR_UNEXPECTED_TOKEN, 1, ctrlval->tree);
        if (ret == NULL) {
          ret = calloc(1, sizeof(TypeSpec));
          int status = typespec_init(ret);
          if (status) abort();
          ret->base = TYPE_ERROR;
        }
        int status = llist_push_back(&ret->auxspecs, erraux);
        if (status) abort();
        symbol_table_remove_control(table, i--);
        free(ctrlval);
      }
    } else if (ctrlval->type == CTRL_CONTINUE &&
               (parent_symbol == TOK_FOR || parent_symbol == TOK_WHILE ||
                parent_symbol == TOK_DO)) {
      symbol_table_remove_control(table, i--);
      free(ctrlval);
    } else if ((ctrlval->type == CTRL_CASE || ctrlval->type == CTRL_DEFAULT) &&
               parent_symbol == TOK_SWITCH) {
      symbol_table_remove_control(table, i--);
      free(ctrlval);
    } else if (ctrlval->type == CTRL_BREAK &&
               parent_symbol != TOK_DECLARATION) {
      symbol_table_remove_control(table, i--);
      free(ctrlval);
    }
  }
  return ret;
}

int symbol_table_add_control(SymbolTable *table, ControlValue *ctrlval) {
  if (table->control_stack == NULL) {
    table->control_stack = malloc(sizeof(*table->control_stack));
    int status = llist_init(table->control_stack, free, NULL);
    if (status) {
      free(table->control_stack);
      return status;
    }
  }
  return llist_push_front(table->control_stack, ctrlval);
}

ControlValue *symbol_table_remove_control(SymbolTable *table, size_t i) {
  return llist_extract(table->control_stack, i);
}

ControlValue *symbol_table_get_control(SymbolTable *table, size_t i) {
  return llist_get(table->control_stack, i);
}

size_t symbol_table_count_control(SymbolTable *table) {
  return llist_size(table->control_stack);
}
