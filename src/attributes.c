#include "attributes.h"

#include "assert.h"
#include "badllist.h"
#include "badmap.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define INDEX_FROM_INT(sign, width) INDEX_##sign##_##width
#define SPECIFY_INT(sign, width)                                   \
  {                                                                \
    X64_SIZEOF_##width, X64_ALIGNOF_##width, BLIB_LLIST_EMPTY,     \
        TYPESPEC_FLAG_##sign | TYPESPEC_FLAG_##width, TYPE_##sign, \
  }

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
  INDEX_FROM_INT(SIGNED, CHAR)
};

const size_t MAX_IDENT_LEN = 31;

/* TODO(Robert): Implement "long long" integer type. For now, the type checker
 * should report an error if "long" is specified twice.
 */
const unsigned int INCOMPATIBLE_FLAGSETS[] = {
    TYPESPEC_FLAG_INT | TYPESPEC_FLAG_CHAR |
        TYPESPEC_FLAGS_NON_INTEGER, /* int */
    TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_INTEGER |
        TYPESPEC_FLAGS_NON_INTEGER, /* char */
    TYPESPEC_FLAG_SHORT | TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER, /* short */
    TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG | TYPESPEC_FLAG_SHORT |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER, /* long */
    TYPESPEC_FLAG_LONG | TYPESPEC_FLAG_LONG_LONG | TYPESPEC_FLAG_SHORT |
        TYPESPEC_FLAG_CHAR | TYPESPEC_FLAGS_NON_INTEGER,    /* long long */
    TYPESPEC_FLAGS_SIGNEDNESS | TYPESPEC_FLAGS_NON_INTEGER, /* signed */
    TYPESPEC_FLAGS_SIGNEDNESS | TYPESPEC_FLAGS_NON_INTEGER, /* unsigned */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* void */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* struct */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* union */
    TYPESPEC_FLAGS_INTEGER | TYPESPEC_FLAGS_NON_INTEGER |
        TYPESPEC_FLAGS_SIGNEDNESS, /* enum */
};

const char STRING_INT_MAP[][32] = {
    "unsigned long int",  "signed long int",  "unsigned int",  "signed int",
    "unsigned short int", "signed short int", "unsigned char", "signed char",
};

const char typespec_flag_string[][10] = {
    "int", "char", "short", "long", "long long", "signed", "unsigned", "void",
    "struct", "union", "enum",
    /* storage class */
    "register", "static", "extern", "auto", "typedef",
    /* qualifiers */
    "const", "volatile"};

const char *STRING_ULONG = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, LONG)];
const char *STRING_LONG = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, LONG)];
const char *STRING_UINT = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, INT)];
const char *STRING_INT = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, INT)];
const char *STRING_USHRT = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, SHORT)];
const char *STRING_SHRT = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, SHORT)];
const char *STRING_UCHAR = STRING_INT_MAP[INDEX_FROM_INT(UNSIGNED, CHAR)];
const char *STRING_SCHAR = STRING_INT_MAP[INDEX_FROM_INT(SIGNED, CHAR)];

const TypeSpec SPEC_EMPTY = {0, 0, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_NONE,
                             TYPE_NONE};
const TypeSpec SPEC_VOID = {0, 0, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_VOID,
                            TYPE_VOID};
const TypeSpec SPEC_CHAR = {1, 1, BLIB_LLIST_EMPTY, TYPESPEC_FLAG_CHAR,
                            TYPE_SIGNED};
const TypeSpec SPEC_ULONG = SPECIFY_INT(UNSIGNED, LONG);
const TypeSpec SPEC_LONG = SPECIFY_INT(SIGNED, LONG);
const TypeSpec SPEC_UINT = SPECIFY_INT(UNSIGNED, INT);
const TypeSpec SPEC_INT = SPECIFY_INT(SIGNED, INT);
const TypeSpec SPEC_USHRT = SPECIFY_INT(UNSIGNED, SHORT);
const TypeSpec SPEC_SHRT = SPECIFY_INT(SIGNED, SHORT);
const TypeSpec SPEC_UCHAR = SPECIFY_INT(UNSIGNED, CHAR);
const TypeSpec SPEC_SCHAR = SPECIFY_INT(SIGNED, CHAR);

const Location LOC_EMPTY = LOC_EMPTY_VALUE;

const char type_map[][16] = {"void", "int", "float", "struct"};

const char attr_map[][16] = {"LVAL", "DEFAULT", "CONST", "INITIALIZER",
                             "ADDRESS"};

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
                         size_t size) {
  if (attributes == ATTR_NONE) {
    buf[0] = 0;
    return 0;
  }

  size_t i, buf_index = 0;
  for (i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes & (1 << i)) {
      if (buf_index + strlen(attr_map[i]) > size) {
        fprintf(stderr, "WARN: buffer too small to print all attributes\n");
        return 1;
      } else {
        buf_index += sprintf(buf + buf_index, "%s ", attr_map[i]);
      }
    }
  }

  if (buf_index > 0) buf[buf_index - 1] = 0;
  return 0;
}

int location_to_string(const Location *loc, char *buf, size_t size) {
  /* TODO(Robert): check size without using snprintf */
  return sprintf(buf, "%lu, %lu, %lu, %lu", loc->filenr, loc->linenr,
                 loc->offset, loc->blocknr);
}

int flags_to_string(const unsigned int flags, char *buf, size_t size) {
  size_t i, offset = 0;
  for (i = 0; i < TYPESPEC_INDEX_COUNT; ++i) {
    enum typespec_flag flag_i = 1 << i;
    if (flags & flag_i) {
      if (offset > 0) {
        buf[offset++] = ' ';
        buf[offset] = 0;
      }
      strcpy(buf + offset, typespec_flag_string[i]);
      offset = strlen(buf);
    }
  }
  return offset;
}

int type_to_string(const TypeSpec *type, char *buf, size_t size) {
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
        if (auxspec->data.memory_loc.length > 0) {
          ret += sprintf((buf + ret), "array of size %lu of ",
                         auxspec->data.memory_loc.length);
        } else {
          ret += sprintf((buf + ret), "array of ");
        }
        break;
      case AUX_POINTER:
        ret += sprintf((buf + ret), "pointer to ");
        break;
      case AUX_FUNCTION:
        ret += sprintf((buf + ret), "function with parameters (");
        LinkedList *params = auxspec->data.params;
        for (j = 0; j < llist_size(params); ++j) {
          SymbolValue *param_symval = llist_get(params, j);
          ret += type_to_string(&(param_symval->type), (buf + ret), size - ret);
          if (j + 1 < llist_size(params)) {
            ret += sprintf((buf + ret), ", ");
          }
        }
        ret += sprintf((buf + ret), ") returning ");
        break;
      case AUX_STRUCT:
        ret += sprintf((buf + ret), "struct %s", auxspec->data.tag.name);
        break;
      case AUX_UNION:
        ret += sprintf((buf + ret), "union %s", auxspec->data.tag.name);
        break;
      case AUX_ENUM:
        ret += sprintf((buf + ret), "enum %s", auxspec->data.tag.name);
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
      ret += sprintf(buf + ret, "void");
      break;
    case TYPE_SIGNED:
      switch (type->width) {
        case 8:
          ret += sprintf(buf + ret, "signed long");
          break;
        case 4:
          ret += sprintf(buf + ret, "signed int");
          break;
        case 2:
          ret += sprintf(buf + ret, "signed short");
          break;
        case 1:
          ret += sprintf(buf + ret, "signed char");
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
          ret += sprintf(buf + ret, "unsigned long");
          break;
        case 4:
          ret += sprintf(buf + ret, "unsigned int");
          break;
        case 2:
          ret += sprintf(buf + ret, "unsigned short");
          break;
        case 1:
          ret += sprintf(buf + ret, "unsigned char");
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
  switch (auxspec->aux) {
    case AUX_NONE:
    case AUX_POINTER:
    case AUX_ARRAY:
    case AUX_ENUM:
    case AUX_STRUCT:
    case AUX_UNION:
      break;
    case AUX_ERROR:
      free(auxspec->data.err.info);
      break;
    case AUX_FUNCTION:
      llist_destroy(auxspec->data.params);
      free(auxspec->data.params);
      break;
  }
  free(auxspec);
  return 0;
}

int auxspec_copy(AuxSpec *dest, const AuxSpec *src) {
  int status;
  dest->aux = src->aux;
  switch (src->aux) {
    case AUX_POINTER:
    case AUX_ARRAY:
      dest->data.memory_loc = src->data.memory_loc;
      break;
    case AUX_UNION:
    case AUX_STRUCT:
    case AUX_ENUM:
      dest->data.tag = src->data.tag;
      break;
    case AUX_FUNCTION:
      dest->data.params = malloc(sizeof(*dest->data.params));
      status = llist_copy(dest->data.params, src->data.params);
      if (status) return status;
      break;
    case AUX_ERROR:
      dest->data.err.code = src->data.err.code;
      dest->data.err.info_count = src->data.err.info_count;
      dest->data.err.info = malloc(dest->data.err.info_count * sizeof(void *));
      memcpy(dest->data.err.info, src->data.err.info,
             dest->data.err.info_count * sizeof(void *));
      break;
    case AUX_NONE:
      break;
  }
  return 0;
}

int typespec_init(TypeSpec *spec) {
  return llist_init(&spec->auxspecs, (void (*)(void *)) & auxspec_destroy,
                    NULL);
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

SymbolValue *typespec_member_name(const TypeSpec *spec, const char *name) {
  assert(typespec_is_union(spec) || typespec_is_struct(spec) ||
         typespec_is_unionptr(spec) || typespec_is_structptr(spec));
  AuxSpec *struct_aux = typespec_is_pointer(spec)
                            ? llist_get(&spec->auxspecs, 1)
                            : llist_front(&spec->auxspecs);
  return symbol_table_get(struct_aux->data.tag.val->data.members.by_name, name,
                          strlen(name));
}

SymbolValue *typespec_member_index(const TypeSpec *spec, size_t index) {
  assert(typespec_is_union(spec) || typespec_is_struct(spec) ||
         typespec_is_unionptr(spec) || typespec_is_structptr(spec));
  AuxSpec *struct_aux = typespec_is_pointer(spec)
                            ? llist_get(&spec->auxspecs, 1)
                            : llist_front(&spec->auxspecs);
  return llist_get(&struct_aux->data.tag.val->data.members.in_order, index);
}

SymbolValue *typespec_param_index(const TypeSpec *spec, size_t index) {
  assert(typespec_is_function(spec) || typespec_is_fnptr(spec));
  AuxSpec *function_aux = typespec_is_pointer(spec)
                              ? llist_get(&spec->auxspecs, 1)
                              : llist_front(&spec->auxspecs);
  return llist_get(function_aux->data.params, index);
}

size_t typespec_elem_width(const TypeSpec *spec) {
  assert(typespec_is_array(spec) || typespec_is_pointer(spec));
  assert(!llist_empty(&spec->auxspecs));
  TypeSpec temp_spec;
  assert(!strip_aux_type(&temp_spec, spec));
  size_t elem_width = typespec_get_width(&temp_spec);
  assert(!typespec_destroy(&temp_spec));
  return elem_width;
}

size_t typespec_get_width(const TypeSpec *spec) {
  if (!llist_empty(&spec->auxspecs)) {
    AuxSpec *aux = llist_front(&spec->auxspecs);
    if (aux->aux == AUX_ARRAY) {
      return aux->data.memory_loc.length * typespec_elem_width(spec);
    } else if (aux->aux == AUX_POINTER || aux->aux == AUX_FUNCTION) {
      return X64_SIZEOF_LONG;
    } else if (aux->aux == AUX_ENUM) {
      return X64_SIZEOF_INT;
    }
  }
  return spec->width;
}

size_t typespec_get_alignment(const TypeSpec *spec) {
  if (!llist_empty(&spec->auxspecs)) {
    AuxSpec *aux = llist_front(&spec->auxspecs);
    if (aux->aux == AUX_POINTER || aux->aux == AUX_FUNCTION) {
      return X64_ALIGNOF_LONG;
    }
  }
  return spec->alignment;
}

size_t typespec_get_eightbytes(const TypeSpec *spec) {
  assert(!typespec_is_function(spec) && !typespec_is_array(spec));
  if (typespec_is_union(spec) || typespec_is_struct(spec)) {
    size_t ret = spec->width;
    size_t padding = 8 - (ret % 8);
    if (padding < 8) ret += padding;
    return ret / 8;
  } else {
    return 1;
  }
}

int typespec_append_auxspecs(TypeSpec *dest, const TypeSpec *src) {
  size_t i;
  for (i = 0; i < llist_size(&src->auxspecs); ++i) {
    AuxSpec *aux_copy = calloc(1, sizeof(*aux_copy));
    auxspec_copy(aux_copy, llist_get(&src->auxspecs, i));
    llist_push_back(&dest->auxspecs, aux_copy);
  }
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

int common_qualified_ptr(TypeSpec *dest, const TypeSpec *src1,
                         const TypeSpec *src2) {
  int status = strip_aux_type(dest, src1);
  if (status) return status;
  AuxSpec *aux1 = llist_front(&src1->auxspecs);
  AuxSpec *aux2 = llist_front(&src2->auxspecs);
  AuxSpec *ptr_aux = calloc(1, sizeof(*ptr_aux));
  ptr_aux->aux = AUX_POINTER;
  ptr_aux->data.memory_loc.qualifiers =
      aux1->data.memory_loc.qualifiers | aux2->data.memory_loc.qualifiers;
  llist_push_front(&dest->auxspecs, ptr_aux);
  return 0;
}

int typespec_is_incomplete(const TypeSpec *type) {
  int is_void = type->base == TYPE_VOID && llist_size(&type->auxspecs) == 0;
  int is_struct =
      (type->base == TYPE_STRUCT || type->base == TYPE_UNION) &&
      llist_size(&type->auxspecs) == 1 &&
      ((AuxSpec *)llist_front(&type->auxspecs))->data.tag.val->is_defined == 0;
  int is_array =
      llist_size(&type->auxspecs) >= 1 &&
      ((AuxSpec *)llist_front(&type->auxspecs))->aux == AUX_ARRAY &&
      ((AuxSpec *)llist_front(&type->auxspecs))->data.memory_loc.length == 0;
  return is_void || is_struct || is_array;
}

int typespec_is_integer(const TypeSpec *type) {
  return (type->base == TYPE_SIGNED || type->base == TYPE_UNSIGNED) &&
         llist_size(&type->auxspecs) == 0;
}

int typespec_is_arithmetic(const TypeSpec *type) {
  return typespec_is_integer(type) || typespec_is_enum(type);
}

int typespec_is_signed(const TypeSpec *type) {
  return (typespec_is_integer(type) && type->base == TYPE_SIGNED) ||
         typespec_is_enum(type) || typespec_is_pointer(type);
}

int typespec_is_unsigned(const TypeSpec *type) {
  return (typespec_is_integer(type) && type->base == TYPE_UNSIGNED);
}

int typespec_is_void(const TypeSpec *type) {
  return (type->base == TYPE_VOID) && llist_size(&type->auxspecs) == 0;
}

int typespec_is_aux(const TypeSpec *type, const AuxType aux,
                    const size_t index) {
  AuxSpec *auxspec = llist_get((LinkedList *)&type->auxspecs, index);
  return auxspec != NULL && auxspec->aux == aux;
}

int typespec_is_pointer(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_POINTER, 0);
}

int typespec_is_array(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_ARRAY, 0);
}

int typespec_is_function(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_FUNCTION, 0);
}

int typespec_is_scalar(const TypeSpec *type) {
  return typespec_is_pointer(type) || typespec_is_arithmetic(type);
}

int typespec_is_voidptr(const TypeSpec *type) {
  return typespec_is_pointer(type) && type->base == TYPE_VOID &&
         llist_size(&type->auxspecs) == 1;
}

int typespec_is_fnptr(const TypeSpec *type) {
  return typespec_is_pointer(type) && typespec_is_aux(type, AUX_FUNCTION, 1);
}

int typespec_is_struct(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_STRUCT, 0);
}

int typespec_is_structptr(const TypeSpec *type) {
  return typespec_is_pointer(type) && typespec_is_aux(type, AUX_STRUCT, 1);
}

int typespec_is_union(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_UNION, 0);
}

int typespec_is_unionptr(const TypeSpec *type) {
  return typespec_is_pointer(type) && typespec_is_aux(type, AUX_UNION, 1);
}

int typespec_is_aggregate(const TypeSpec *type) {
  return typespec_is_struct(type) || typespec_is_array(type) ||
         typespec_is_union(type);
}

int typespec_is_enum(const TypeSpec *type) {
  return typespec_is_aux(type, AUX_ENUM, 0);
}

int typespec_is_chararray(const TypeSpec *type) {
  return llist_size(&type->auxspecs) == 1 && typespec_is_array(type) &&
         (type->flags & TYPESPEC_FLAG_CHAR);
}

int typespec_is_const(const TypeSpec *type) {
  if (llist_empty(&type->auxspecs)) {
    return !!(type->flags & TYPESPEC_FLAG_CONST);
  } else {
    AuxSpec *auxspec = llist_front(&type->auxspecs);
    if (auxspec->aux == AUX_UNION || auxspec->aux == AUX_STRUCT) {
      if (type->flags & TYPESPEC_FLAG_CONST) return 1;
      LinkedList *members = &auxspec->data.tag.val->data.members.in_order;
      size_t i;
      for (i = 0; i < llist_size(members); ++i) {
        SymbolValue *member = llist_get(members, i);
        if (typespec_is_const(&member->type)) return 1;
      }
      return 0;
    } else if (auxspec->aux == AUX_POINTER) {
      return !!(auxspec->data.memory_loc.qualifiers & TYPESPEC_FLAG_CONST);
    } else {
      return 1;
    }
  }
}
