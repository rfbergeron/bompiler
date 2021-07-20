#include "attributes.h"

#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define INDEX_FROM_INT(sign, width) INDEX_##sign##_##width
#define SPECIFY_INT(sign, width) \
  {X64_SIZEOF_##width,           \
   X64_ALIGNOF_##width,          \
   NULL,                         \
   BLIB_MAP_EMPTY,               \
   TYPE_FLAG_NONE,               \
   TYPE_##sign,                  \
   STRING_INT_MAP[INDEX_FROM_INT(sign, width)]};

struct conversion_entry {
  enum base_type from;
  enum base_type to;
  enum conversion_type conversion;
};

enum string_int_index {
  INDEX_FROM_INT(UNSIGNED, LONG),
  INDEX_FROM_INT(SIGNED, LONG),
  INDEX_FROM_INT(UNSIGNED, INT),
  INDEX_FROM_INT(SIGNED, INT),
  INDEX_FROM_INT(UNSIGNED, SHORT),
  INDEX_FROM_INT(SIGNED, SHORT),
  INDEX_FROM_INT(UNSIGNED, CHAR),
  INDEX_FROM_INT(SIGNED, CHAR),
};

static const size_t NUM_ATTRIBUTES = 6;
static const size_t NUM_TYPES = 4;
const size_t MAX_IDENT_LEN = 31;

const char STRING_INT_MAP[][32] = {
    "unsigned long int",  "signed long int",  "unsigned int",  "signed int",
    "unsigned short int", "signed short int", "unsigned char", "signed char",
};

const char *STRING_ULONG = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, LONG)];
const char *STRING_LONG = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, LONG)];
const char *STRING_UINT = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, INT)];
const char *STRING_INT = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, INT)];
const char *STRING_USHRT = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, SHORT)];
const char *STRING_SHRT = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, SHORT)];
const char *STRING_UCHAR = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, CHAR)];
const char *STRING_CHAR = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, CHAR)];

const TypeSpec SPEC_PTR = {X64_SIZEOF_LONG, X64_ALIGNOF_LONG, NULL,
                           BLIB_MAP_EMPTY,  TYPE_FLAG_NONE,   TYPE_POINTER,
                           "_ptr_empty"};
const TypeSpec SPEC_EMPTY = {
    0, 0, NULL, BLIB_MAP_EMPTY, TYPE_FLAG_NONE, TYPE_NONE, "_empty"};
const TypeSpec SPEC_FUNCTION = {
    0, 0, NULL, BLIB_MAP_EMPTY, TYPE_FLAG_NONE, TYPE_FUNCTION, "_function"};
const TypeSpec SPEC_STRUCT = {
    0, 0, NULL, BLIB_MAP_EMPTY, TYPE_FLAG_NONE, TYPE_STRUCT, "_struct"};
const TypeSpec SPEC_ULONG = SPECIFY_INT(UNSIGNED, LONG);
const TypeSpec SPEC_LONG = SPECIFY_INT(SIGNED, LONG);
const TypeSpec SPEC_UINT = SPECIFY_INT(UNSIGNED, INT);
const TypeSpec SPEC_INT = SPECIFY_INT(SIGNED, INT);
const TypeSpec SPEC_USHRT = SPECIFY_INT(UNSIGNED, SHORT);
const TypeSpec SPEC_SHRT = SPECIFY_INT(SIGNED, SHORT);
const TypeSpec SPEC_UCHAR = SPECIFY_INT(UNSIGNED, CHAR);
const TypeSpec SPEC_CHAR = SPECIFY_INT(SIGNED, CHAR);

const Location LOC_EMPTY = {0, 0, 0, 0};

const char type_map[][16] = {"void", "int", "float", "struct"};

const char attr_map[][16] = {"reg", "lval", "rval", "addr", "const"};

/* Precedence for conversions:
 * 1. long double
 * 2. double
 * 3. float
 * 4. long unsigned int
 * 5. long int
 * 6. unsigned int
 * 7. int
 *
 * All other arithmetic types get promoted to int.
 *
 * The list includes valid conversions, including both those that do and do not
 * need additional operations to make them compatible.
 *
 * Compound types cannot be implicitly casted and do not need to be converted in
 * the same way that arithmetic types do. The only exception to this are
 * pointers, which may be implicitly converted to and from void pointers.
 *
 * When the assembly generator emits a promotion,
 * Assignment expressions need
 */

int attributes_to_string(const unsigned int attributes, char *buf,
                         size_t bufsize) {
  size_t i, buf_index = 0;
  for (i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes & (1 << i)) {
      if (buf_index + strlen(attr_map[i]) > bufsize) {
        fprintf(stderr, "WARN: buffer too small to print all attributes");
        return 1;
      } else if (i == 0) {
        buf_index += sprintf(buf, " %s", attr_map[i]);
      } else {
        buf_index += sprintf(buf, "%s", attr_map[i]);
      }
    }
  }
  buf[buf_index] = 0;
  return 0;
}

void location_to_string(const Location *loc, char *buffer, size_t size) {
  int bufsize = snprintf(buffer, size, "%lu, %lu, %lu, %lu", loc->filenr,
                         loc->linenr, loc->offset, loc->blocknr) +
                1;
}

int type_to_string(const TypeSpec *type, char *buf, size_t bufsize) {
  int ret = 0;
  int i = 0;
  /* TODO(Robert): use the mappings defined above to print instead of this
   * silly shit
   */
  switch (type->base) {
    case TYPE_VOID:
      ret = sprintf(buf, "void");
      break;
    case TYPE_FUNCTION:
      ret += type_to_string(type->nested, buf, bufsize);
      ret += sprintf((buf + ret), " ()(");
      if (type->data.params.size > 0) {
        for (i = 0; i < type->data.params.size; ++i) {
          SymbolValue *param_symval = llist_get((void *)&type->data.params, i);
          ret +=
              type_to_string(&(param_symval->type), (buf + ret), bufsize - ret);
          if (i + 1 < llist_size((void *)&type->data.params)) {
            ret += sprintf((buf + ret), ", ");
          }
        }
        ret += sprintf((buf + ret), ")");
      } else {
        ret += sprintf((buf + ret), "void)");
      }
      break;
    case TYPE_NONE:
      buf[0] = 0;
      break;
    case TYPE_SIGNED:
      switch (type->width) {
        case 8:
          ret = sprintf(buf, "signed long");
          break;
        case 4:
          ret = sprintf(buf, "signed int");
          break;
        case 2:
          ret = sprintf(buf, "signed short");
          break;
        case 1:
          ret = sprintf(buf, "signed char");
          break;
        default:
          fprintf(stderr, "ERROR: Unknown width of signed type: %lu\n",
                  type->width);
          break;
      }
      break;
    case TYPE_UNSIGNED:
      switch (type->width) {
        case 8:
          ret = sprintf(buf, "unsigned long");
          break;
        case 4:
          ret = sprintf(buf, "unsigned int");
          break;
        case 2:
          ret = sprintf(buf, "unsigned short");
          break;
        case 1:
          ret = sprintf(buf, "unsigned char");
          break;
        default:
          fprintf(stderr, "ERROR: Unknown width of unsigned type: %lu\n",
                  type->width);
          break;
      }
      break;
    default:
      break;
  }
  return ret;
}

int typespec_copy(TypeSpec *dst, const TypeSpec *src) {
  /* copy contents of source type
   * allocate new space to copy nested type into
   * recursively call typespec_copy on nested type
   */
  *dst = *src;
  if (src->nested != NULL) {
    dst->nested = malloc(sizeof(TypeSpec));
    return typespec_copy(dst->nested, src->nested);
  } else {
    dst->nested = NULL;
    return 0;
  }
}

int typespec_destroy(TypeSpec *type) {
  /* the badlib functions won't segfault when attempting to destroy a data
   * structure a second time because they do a paranoid free (free and set to
   * NULL) on the fields of the structure, so we can repeat the process as
   * many times as we like
   */
  if (type == NULL) return -1;
  switch (type->base) {
    case TYPE_FUNCTION:
      llist_destroy(&(type->data.params));
      goto free_nested_typesec;
    case TYPE_STRUCT:
    case TYPE_UNION:
      map_destroy(&(type->data.members));
      goto free_nested_typesec;
    case TYPE_POINTER:
    case TYPE_ARRAY:
    free_nested_typesec:
      typespec_destroy(type->nested);
      free(type->nested);
      break;
    default:
      /* do nothing */
      break;
  }
  return 0;
}
