#include "attributes.h"

#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define INDEX_FROM_INT(sign, width) INDEX_##sign##_##width
#define SPECIFY_INT(sign, width)                                 \
  {                                                              \
      X64_SIZEOF_##width, X64_ALIGNOF_##width, BLIB_LLIST_EMPTY, \
      TYPE_FLAG_NONE,     TYPE_##sign,                           \
  };

/*
struct conversion_entry {
  enum base_type from;
  enum base_type to;
  enum conversion_type conversion;
};
*/

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

const TypeSpec SPEC_EMPTY = {0, 0, BLIB_LLIST_EMPTY, TYPE_FLAG_NONE, TYPE_NONE};
const TypeSpec SPEC_VOID = {0, 0, BLIB_LLIST_EMPTY, TYPE_FLAG_NONE, TYPE_VOID};
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
  /* TODO(Robert): casting away const bad; redo badlib so that query functions
   * have const arguments
   */
  size_t i;
  for (i = 0; i < llist_size((LinkedList *)&type->auxspecs); ++i) {
    AuxSpec *auxspec = llist_get((LinkedList *)&type->auxspecs, i);
    size_t j;
    switch (auxspec->aux) {
      case AUX_ARRAY:
        if (auxspec->data.ptr_or_arr.length > 0) {
          ret += sprintf((buf + ret), " array of size %zu of",
                         auxspec->data.ptr_or_arr.length);
        } else {
          ret += sprintf((buf + ret), " array of");
        }
        break;
      case AUX_POINTER:
        ret += sprintf((buf + ret), " pointer to");
        break;
      case AUX_FUNCTION:
        ret += sprintf((buf + ret), " function with parameters (");
        LinkedList *params = &auxspec->data.params;
        for (j = 0; j < llist_size(params); ++j) {
          SymbolValue *param_symval = llist_get(params, j);
          ret +=
              type_to_string(&(param_symval->type), (buf + ret), bufsize - ret);
          if (j + 1 < llist_size(params)) {
            ret += sprintf((buf + ret), ", ");
          }
        }
        ret += sprintf((buf + ret), ") returning");
        break;
      case AUX_STRUCT:
        ret += sprintf((buf + ret), " struct with members {");
        LinkedList *members = &auxspec->data.composite.members;
        for (j = 0; j < llist_size(members); ++j) {
          SymbolValue *member = llist_get(members, j);
          ret += type_to_string(&(member->type), (buf + ret), bufsize - ret);
          if (j + 1 < llist_size(members)) {
            ret += sprintf((buf + ret), ", ");
          }
        }
        break;
      default:
        break;
    }
  }
  /* TODO(Robert): use the mappings defined above to print instead of this
   * silly shit
   */
  switch (type->base) {
    case TYPE_NONE:
      /* ret += sprintf(buf + ret, " none"); */
      buf[ret] = 0;
      break;
    case TYPE_VOID:
      ret += sprintf(buf + ret, " void");
      break;
    case TYPE_SIGNED:
      switch (type->width) {
        case 8:
          ret += sprintf(buf + ret, " signed long");
          break;
        case 4:
          ret += sprintf(buf + ret, " signed int");
          break;
        case 2:
          ret += sprintf(buf + ret, " signed short");
          break;
        case 1:
          ret += sprintf(buf + ret, " signed char");
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
          ret += sprintf(buf + ret, " unsigned long");
          break;
        case 4:
          ret += sprintf(buf + ret, " unsigned int");
          break;
        case 2:
          ret += sprintf(buf + ret, " unsigned short");
          break;
        case 1:
          ret += sprintf(buf + ret, " unsigned char");
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

int auxspec_destroy(AuxSpec *auxspec) {
  size_t i;
  switch (auxspec->aux) {
    case AUX_NONE:
      break;
    case AUX_POINTER:
      break;
    case AUX_ARRAY:
      break;
    case AUX_STRUCT:
    case AUX_UNION:
      llist_destroy(&auxspec->data.composite.members);
      break;
    case AUX_FUNCTION:
      llist_destroy(&auxspec->data.params);
      break;
    case AUX_TYPEDEF:
      break;
  }
  free(auxspec);
  return 0;
}

/* TODO(Robert): make sure this doesn't need to be more thorough */
int auxspec_copy(AuxSpec *dest, const AuxSpec *src) {
  *dest = *src;
  return 0;
}

int typespec_init(TypeSpec *spec) {
  llist_init(&spec->auxspecs, (void (*)(void *)) & auxspec_destroy, NULL);
  return 0;
}

int typespec_destroy(TypeSpec *spec) {
  /* the badlib functions won't segfault when attempting to destroy a data
   * structure a second time because they do a paranoid free (free and set to
   * NULL) on the fields of the structure, so we can repeat the process as
   * many times as we like
   */
  if (spec == NULL) return -1;
  llist_destroy(&spec->auxspecs);
  return 0;
}

int typespec_copy(TypeSpec *dest, const TypeSpec *src) {
  *dest = *src;
  memset(&dest->auxspecs, 0, sizeof(dest->auxspecs));
  llist_copy(&dest->auxspecs, (LinkedList *)&src->auxspecs);
  return 0;
}

int strip_aux_type(TypeSpec *dest, const TypeSpec *src) {
  int status = typespec_copy(dest, src);
  if (status) return status;
  void *stripped = llist_pop_front(&dest->auxspecs);
  if (stripped == NULL) {
    fprintf(stderr, "ERROR: unable to strip auxiliary type information\n");
    return -1;
  }
  return 0;
}

int typespec_is_arithmetic(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int typespec_is_integer(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED);
}

int typespec_is_aux(const TypeSpec *type, const AuxType aux) {
  AuxSpec *auxspec = llist_front((LinkedList *)&type->auxspecs);
  return auxspec != NULL && auxspec->aux == aux;
}

int typespec_is_pointer(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_POINTER);
}

int typespec_is_array(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_ARRAY);
}

int typespec_is_function(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_FUNCTION);
}

int typespec_is_int_or_ptr(const TypeSpec *type) {
  return (typespec_is_integer(type) || typespec_is_pointer(type));
}

int typespec_is_scalar(const TypeSpec *type) {
  return typespec_is_pointer(type) || typespec_is_arithmetic(type);
}

int typespec_is_comparable(const TypeSpec *type) {
  if (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED ||
      typespec_is_pointer(type)) {
    return 1;
  } else {
    return 0;
  }
}
