#include "bcc_types.h"

#include "assert.h"
#include "badllist.h"
#include "badmap.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

static Type TYPE_VOID_INTERNAL;
static Type TYPE_POINTER_INTERNAL;
static Type TYPE_CHAR_INTERNAL;
static Type TYPE_INT_INTERNAL;
static Type TYPE_LONG_INTERNAL;
static Type TYPE_UNSIGNED_INT_INTERNAL;
static Type TYPE_UNSIGNED_LONG_INTERNAL;
static Type TYPE_NONE_INTERNAL;

const Type *const TYPE_VOID = &TYPE_VOID_INTERNAL;
const Type *const TYPE_POINTER = &TYPE_POINTER_INTERNAL;
const Type *const TYPE_CHAR = &TYPE_CHAR_INTERNAL;
const Type *const TYPE_INT = &TYPE_INT_INTERNAL;
const Type *const TYPE_LONG = &TYPE_LONG_INTERNAL;
const Type *const TYPE_UNSIGNED_INT = &TYPE_UNSIGNED_INT_INTERNAL;
const Type *const TYPE_UNSIGNED_LONG = &TYPE_UNSIGNED_LONG_INTERNAL;
const Type *const TYPE_NONE = &TYPE_NONE_INTERNAL;

void type_init_globals(void) {
  /* we have to do this here since designated initializers don't exist in c90 */
  TYPE_VOID_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_VOID_INTERNAL.base.flags = SPEC_FLAG_VOID;
  TYPE_VOID_INTERNAL.base.symbol = NULL;
  TYPE_POINTER_INTERNAL.pointer.code = TYPE_CODE_POINTER;
  TYPE_POINTER_INTERNAL.pointer.qualifiers = QUAL_FLAG_NONE;
  TYPE_POINTER_INTERNAL.pointer.next = &TYPE_VOID_INTERNAL;
  TYPE_CHAR_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_CHAR_INTERNAL.base.flags = SPEC_FLAG_CHAR;
  TYPE_CHAR_INTERNAL.base.symbol = NULL;
  TYPE_INT_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_INT_INTERNAL.base.flags = SPEC_FLAGS_SINT;
  TYPE_INT_INTERNAL.base.symbol = NULL;
  TYPE_LONG_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_LONG_INTERNAL.base.flags = SPEC_FLAGS_SLONG;
  TYPE_LONG_INTERNAL.base.symbol = NULL;
  TYPE_UNSIGNED_INT_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_UNSIGNED_INT_INTERNAL.base.flags = SPEC_FLAGS_UINT;
  TYPE_UNSIGNED_INT_INTERNAL.base.symbol = NULL;
  TYPE_UNSIGNED_LONG_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_UNSIGNED_LONG_INTERNAL.base.flags = SPEC_FLAGS_ULONG;
  TYPE_UNSIGNED_LONG_INTERNAL.base.symbol = NULL;
  TYPE_NONE_INTERNAL.any.code = TYPE_CODE_NONE;
  TYPE_NONE_INTERNAL.any.flags = 0;
}

int type_init_none(Type **out, unsigned int flags) {
  /* may only be called with storage class and qualifier flags */
  assert(!(flags & SPEC_FLAG_MASK));
  if (out == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->any.code = TYPE_CODE_NONE;
  (*out)->any.flags = flags;
  return 0;
}

int type_init_pointer(Type **out, unsigned int qualifiers) {
  /* may only be called with qualifier flags */
  assert(!(qualifiers & ~QUAL_FLAG_MASK));
  if (out == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->pointer.code = TYPE_CODE_POINTER;
  (*out)->pointer.qualifiers = qualifiers;
  (*out)->pointer.next = NULL;
  return 0;
}

int type_init_array(Type **out, size_t length, int deduce_length) {
  if (out == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->array.code = TYPE_CODE_ARRAY;
  (*out)->array.length = length;
  (*out)->array.deduce_length = deduce_length;
  (*out)->array.next = NULL;
  return 0;
}

int type_init_function(Type **out, size_t parameters_size, Type **parameters,
                       int is_variadic, int is_old_style) {
  if (out == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->function.code = TYPE_CODE_FUNCTION;
  (*out)->function.parameters_size = parameters_size;
  (*out)->function.parameters = parameters;
  (*out)->function.is_variadic = is_variadic;
  (*out)->function.is_old_style = is_old_style;
  (*out)->function.next = NULL;
  return 0;
}

int type_init_tag(Type **out, unsigned int flags, const char *tag_name,
                  TagValue *tag_value) {
  /* flags may not include type specifiers */
  assert(!(flags & SPEC_FLAG_MASK));
  if (out == NULL || tag_name == NULL || tag_value == NULL) return -1;
  *out = malloc(sizeof(Type));
  switch (tag_value->tag) {
    case TAG_STRUCT:
      (*out)->tag.code = TYPE_CODE_STRUCT;
      break;
    case TAG_UNION:
      (*out)->tag.code = TYPE_CODE_UNION;
      break;
    case TAG_ENUM:
      (*out)->tag.code = TYPE_CODE_ENUM;
      break;
    default:
      abort();
  }
  (*out)->tag.flags = flags;
  (*out)->tag.name = tag_name;
  (*out)->tag.value = tag_value;
  (*out)->tag.symbol = NULL;
  return 0;
}

int type_init_base(Type **out, unsigned int flags) {
  /* flag must include a type specifier */
  assert(flags & SPEC_FLAG_MASK);
  if (out == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->base.code = TYPE_CODE_BASE;
  (*out)->base.flags = flags;
  (*out)->base.symbol = NULL;
  return 0;
}

int type_init_error(Type **out, CompileError *error) {
  if (out == NULL || error == NULL) return -1;
  *out = malloc(sizeof(Type));
  (*out)->error.code = TYPE_CODE_ERROR;
  (*out)->error.errors_cap = 4;
  (*out)->error.errors_size = 1;
  (*out)->error.errors = malloc(4 * sizeof(Type));
  if ((*out)->error.errors == NULL) abort();
  (*out)->error.errors[0] = error;
  return 0;
}

int type_destroy(Type *type) {
  while (type != NULL) {
    Type *current = type;
    size_t i;
    switch (type->any.code) {
      default:
        abort();
      case TYPE_CODE_NONE:
        free(current);
        return 0;
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        free(current);
        return 0;
      case TYPE_CODE_POINTER:
        type = type->pointer.next;
        free(current);
        break;
      case TYPE_CODE_FUNCTION:
        type = type->function.next;
        free(current->function.parameters);
        free(current);
        break;
      case TYPE_CODE_ARRAY:
        type = type->array.next;
        free(current);
        break;
      case TYPE_CODE_BASE:
        free(current);
        return 0;
      case TYPE_CODE_ERROR:
        for (i = 0; i < current->error.errors_size; ++i)
          compile_error_destroy(current->error.errors[i]);
        free(current->error.errors);
        free(current);
        return 0;
    }
  }
  /* a properly formed type should never reach this point, but if the type
   * was formed erroneously and we're performing cleanup it is possible that
   * this singly-linked list may be NULL, so it's not necessarily an error.
   */
  return 0;
}

static const char *qual_get_str(unsigned int qualifiers) {
  switch (qualifiers & QUAL_FLAG_MASK) {
    case QUAL_FLAG_CONST | QUAL_FLAG_VOLATILE:
      return "const volatile";
    case QUAL_FLAG_CONST:
      return "const";
    case QUAL_FLAG_VOLATILE:
      return "volatile";
    default:
      return "";
  }
}

/* TODO(Robert): this function should be able to print out types that haven't
 * been normalized yet, for diagnostic purposes
 */
static const char *base_get_str(unsigned int flags) {
  switch (flags & SPEC_FLAG_MASK) {
    case SPEC_FLAG_NONE:
      return "(none)";
    case SPEC_FLAG_VOID:
      return "void";
    case SPEC_FLAG_CHAR:
      return "char";
    case SPEC_FLAGS_SCHAR:
      return "signed char";
    case SPEC_FLAGS_UCHAR:
      return "unsigned char";
    case SPEC_FLAGS_SINT:
      return "signed int";
    case SPEC_FLAGS_UINT:
      return "unsigned int";
    case SPEC_FLAGS_SSHRT:
      return "signed short";
    case SPEC_FLAGS_USHRT:
      return "unsigned short";
    case SPEC_FLAGS_SLONG:
      return "signed long";
    case SPEC_FLAGS_ULONG:
      return "unsigned long";
    case SPEC_FLAG_FLOAT:
      return "float";
    case SPEC_FLAG_DOUBLE:
      return "double";
    case SPEC_FLAGS_LONG_DOUBLE:
      return "long double";
    default:
      return "(incomplete)";
  }
}

static int specifiers_to_str(const Type *type, char *buf) {
  assert(type->any.code == TYPE_CODE_NONE || type->any.code == TYPE_CODE_BASE ||
         type->any.code == TYPE_CODE_ENUM ||
         type->any.code == TYPE_CODE_UNION ||
         type->any.code == TYPE_CODE_STRUCT);
  /* this array assumes that the type codes for none, base, struct, union, and
   * enum are 0, 1, 2, 3 and 4, respectively. the type code is used as a key to
   * map to the format strings, with an offset of 5 if the type has qualifiers
   */
  static const char *FORMAT_STRINGS[] = {
      "%s",           /* unqualified incomplete type */
      "%s",           /* unqualified scalar/void */
      "struct %s",    /* unqualified struct */
      "union %s",     /* unqualified union */
      "enum %s",      /* unqualified enum */
      "%s %s",        /* qualified incomplete type */
      "%s %s",        /* qualified scalar/void */
      "%s struct %s", /* qualified struct */
      "%s union %s",  /* qualified union */
      "%s enum %s"    /* qualified enum */
  };
  static const size_t QUALIFIED_OFFSET = 5;
  const char *type_string =
      type->any.code == TYPE_CODE_BASE || type->any.code == TYPE_CODE_NONE
          ? base_get_str(type->base.flags)
          : type->tag.name;
  const char *qualifier_string = qual_get_str(
      type->any.code == TYPE_CODE_BASE ? type->base.flags : type->tag.flags);

  size_t format_index = type->any.code;
  if (type_is_qualified(type))
    return sprintf(buf, FORMAT_STRINGS[format_index + QUALIFIED_OFFSET],
                   qualifier_string, type_string);
  else
    return sprintf(buf, FORMAT_STRINGS[format_index], type_string);
}

int type_to_str(const Type *type, char *buf) {
  buf[0] = '\0';
  int ret = 0;
  const Type *current = type;
  while (current != NULL) {
    switch (current->any.code) {
      case TYPE_CODE_NONE:
        /* fallthrough */
      case TYPE_CODE_BASE:
        /* fallthrough */
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        ret += specifiers_to_str(current, buf + ret);
        current = NULL;
        break;
      case TYPE_CODE_FUNCTION:
        if (current->function.is_variadic)
          ret += sprintf(buf + ret, "variadic function with parameters (");
        else if (!current->function.is_old_style &&
                 current->function.parameters_size == 0)
          ret += sprintf(buf + ret, "function with parameters (void");
        else
          ret += sprintf(buf + ret, "function with parameters (");
        size_t i;
        for (i = 0; i < current->function.parameters_size; ++i) {
          if (i > 0) ret += sprintf(buf + ret, ", ");
          ret += type_to_str(current->function.parameters[i], buf + ret);
        }
        ret += sprintf(buf + ret, ") returning ");
        current = current->function.next;
        break;
      case TYPE_CODE_ARRAY:
        if (current->array.deduce_length)
          ret += sprintf(buf + ret, "array of deduced size %lu of ",
                         current->array.length);
        else
          ret += sprintf(buf + ret, "array of size %lu of ",
                         current->array.length);
        current = current->array.next;
        break;
      case TYPE_CODE_POINTER:
        if (type_is_qualified(current))
          ret += sprintf(buf + ret, "%s pointer to ",
                         qual_get_str(current->pointer.qualifiers));
        else
          ret += sprintf(buf + ret, "pointer to ");
        current = current->pointer.next;
        break;
      case TYPE_CODE_ERROR:
        ret += sprintf(buf + ret, "(error)");
        current = NULL;
        break;
      default:
        abort();
    }
  }
  return ret;
}

int type_is_void(const Type *type) {
  return type->any.code == TYPE_CODE_BASE &&
         ((type->base.flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_VOID);
}

int type_is_integer(const Type *type) {
  return type->any.code == TYPE_CODE_BASE &&
         ((type->base.flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_INTEGRAL ||
          (type->base.flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_CHAR);
}

/* pointers are not considered signed or unsigend */
int type_is_signed(const Type *type) {
  return type_is_integer(type) && !(type->base.flags & SPEC_FLAG_UNSIGNED);
}

int type_is_unsigned(const Type *type) {
  return type_is_integer(type) && !(type->base.flags & SPEC_FLAG_SIGNED);
}

int type_is_enum(const Type *type) { return type->any.code == TYPE_CODE_ENUM; }

int type_is_integral(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_arithmetic(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_pointer(const Type *type) {
  return type->any.code == TYPE_CODE_POINTER;
}

int type_is_scalar(const Type *type) {
  return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_array(const Type *type) {
  return type->any.code == TYPE_CODE_ARRAY;
}

int type_is_deduced_array(const Type *type) {
  return type->any.code == TYPE_CODE_ARRAY && type->array.deduce_length;
}

int type_is_function(const Type *type) {
  return type->any.code == TYPE_CODE_FUNCTION;
}

int type_is_variadic_function(const Type *type) {
  return type->any.code == TYPE_CODE_FUNCTION && type->function.is_variadic;
}

int type_is_old_style_function(const Type *type) {
  return type->any.code == TYPE_CODE_FUNCTION && type->function.is_old_style;
}

/* determines (in a roundabout way) whether or not a function has a prototype
 * yet. this exists so that the compiler can check to see if a function that
 * was at first declared without a prototype has since been given one.
 *
 * the logic is as follows:
 * - unprototyped functions initially have the `is_variadic` and `is_old_style`
 *   flags set, with zero parameters. this is the only way for a function to be
 *   variadic with no parameters
 * - when the function is given a prototype, either the `is_variadic` flag will
 *   be cleared, or the function will have parameters
 */
int type_is_prototyped_function(const Type *type) {
  return type->any.code == TYPE_CODE_FUNCTION &&
         (!type->function.is_old_style || !type->function.is_variadic ||
          type_param_count(type) > 0);
}

int type_is_void_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_void(type->pointer.next);
}

int type_is_function_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_function(type->pointer.next);
}

int type_is_struct(const Type *type) {
  return type->any.code == TYPE_CODE_STRUCT;
}

int type_is_struct_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_struct(type->pointer.next);
}

int type_is_union(const Type *type) {
  return type->any.code == TYPE_CODE_UNION;
}

int type_is_union_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_union(type->pointer.next);
}

int type_is_record(const Type *type) {
  return type_is_struct(type) || type_is_union(type);
}

int type_is_aggregate(const Type *type) {
  return type_is_struct(type) || type_is_union(type) || type_is_array(type);
}

int type_is_char_array(const Type *type) {
  return type_is_array(type) && type->array.next->any.code == TYPE_CODE_BASE &&
         (type->array.next->base.flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_CHAR;
}

int type_is_const(const Type *type) {
  switch (type->any.code) {
    case TYPE_CODE_NONE:
      return !!(type->any.flags & QUAL_FLAG_CONST);
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      return !!(type->tag.flags & QUAL_FLAG_CONST);
    case TYPE_CODE_POINTER:
      return !!(type->pointer.qualifiers & QUAL_FLAG_CONST);
    case TYPE_CODE_ARRAY:
      /* arrays are not const, they are non-assignable lvalues. arrays may not
       * have qualifiers, but their elements can.
       */
      return 0;
    case TYPE_CODE_BASE:
      return !!(type->base.flags & QUAL_FLAG_CONST);
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    default:
      abort();
  }
}

int type_is_volatile(const Type *type) {
  switch (type->any.code) {
    case TYPE_CODE_NONE:
      return !!(type->any.flags & QUAL_FLAG_VOLATILE);
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      return !!(type->tag.flags & QUAL_FLAG_VOLATILE);
    case TYPE_CODE_POINTER:
      return !!(type->pointer.qualifiers & QUAL_FLAG_VOLATILE);
    case TYPE_CODE_ARRAY:
      return 0;
    case TYPE_CODE_BASE:
      return !!(type->base.flags & QUAL_FLAG_VOLATILE);
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    default:
      abort();
  }
}

int type_is_qualified(const Type *type) {
  return type_is_const(type) || type_is_volatile(type);
}

int type_is_typedef(const Type *type) {
  while (type != NULL && (type->any.code == TYPE_CODE_POINTER ||
                          type->any.code == TYPE_CODE_ARRAY ||
                          type->any.code == TYPE_CODE_FUNCTION)) {
    switch (type->any.code) {
      case TYPE_CODE_POINTER:
        type = type->pointer.next;
        break;
      case TYPE_CODE_ARRAY:
        type = type->array.next;
        break;
      case TYPE_CODE_FUNCTION:
        type = type->function.next;
        break;
      default:
        abort();
    }
  }

  if (type == NULL) abort();
  switch (type->any.code) {
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      return !!(type->tag.flags & STOR_FLAG_TYPEDEF);
    case TYPE_CODE_BASE:
      return !!(type->base.flags & STOR_FLAG_TYPEDEF);
    case TYPE_CODE_NONE:
      return !!(type->base.flags & STOR_FLAG_TYPEDEF);
    default:
      abort();
  }
}

int type_is_error(const Type *type) {
  return type->any.code == TYPE_CODE_ERROR;
}

int type_is_declarator(const Type *type) {
  return type->any.code == TYPE_CODE_POINTER ||
         type->any.code == TYPE_CODE_ARRAY ||
         type->any.code == TYPE_CODE_FUNCTION;
}

int type_is_none(const Type *type) { return type->any.code == TYPE_CODE_NONE; }

int type_is_incomplete(const Type *type) {
  if (type_is_void(type) || type_is_none(type)) {
    return 1;
  } else if (type_is_deduced_array(type) && type->array.length == 0) {
    return 1;
  } else if ((type_is_union(type) || type_is_struct(type)) &&
             !type->tag.value->is_defined) {
    return 1;
  } else {
    return 0;
  }
}

SymbolValue *type_member_name(const Type *type, const char *name) {
  assert(type_is_union(type) || type_is_struct(type));
  return symbol_table_get(type->tag.value->data.members.by_name, name,
                          strlen(name));
}

SymbolValue *type_member_index(const Type *type, size_t index) {
  assert(type_is_union(type) || type_is_struct(type));
  return llist_get(&type->tag.value->data.members.in_order, index);
}

size_t type_member_count(const Type *type) {
  assert(type_is_aggregate(type));
  switch (type->any.code) {
    case TYPE_CODE_UNION:
      return 1;
    case TYPE_CODE_STRUCT:
      return llist_size(&type->tag.value->data.members.in_order);
    case TYPE_CODE_ARRAY:
      return type->array.length;
    default:
      abort();
  }
}

Type *type_param_index(const Type *type, size_t index) {
  assert(type_is_function(type) && index < type->function.parameters_size);
  return type->function.parameters[index];
}

size_t type_param_count(const Type *type) {
  return type->function.parameters_size;
}

static size_t type_base_alignment(const Type *type) {
  assert(type->any.code == TYPE_CODE_BASE);
  switch (type->base.flags & SPEC_FLAG_MASK) {
    case SPEC_FLAG_VOID:
      return 0;
    case SPEC_FLAG_CHAR:
      /* fallthrough */
    case SPEC_FLAGS_SCHAR:
      /* fallthrough */
    case SPEC_FLAGS_UCHAR:
      return X64_ALIGNOF_CHAR;
    case SPEC_FLAGS_SINT:
      /* fallthrough */
    case SPEC_FLAGS_UINT:
      return X64_ALIGNOF_INT;
    case SPEC_FLAGS_SSHRT:
      /* fallthrough */
    case SPEC_FLAGS_USHRT:
      return X64_ALIGNOF_SHORT;
    case SPEC_FLAGS_SLONG:
      /* fallthrough */
    case SPEC_FLAGS_ULONG:
      return X64_ALIGNOF_LONG;
    case SPEC_FLAG_FLOAT:
      return X64_ALIGNOF_FLOAT;
    case SPEC_FLAG_DOUBLE:
      return X64_ALIGNOF_DOUBLE;
    case SPEC_FLAGS_LONG_DOUBLE:
      return X64_ALIGNOF_LONG_DOUBLE;
    default:
      abort();
  }
}

size_t type_get_alignment(const Type *type) {
  switch (type->any.code) {
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_FUNCTION:
      return 0;
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      return type->tag.value->alignment;
    case TYPE_CODE_POINTER:
      return X64_ALIGNOF_LONG;
    case TYPE_CODE_ARRAY:
      return type_get_alignment(type->array.next);
    case TYPE_CODE_BASE:
      return type_base_alignment(type);
    case TYPE_CODE_ERROR:
      /* fallthrough */
    default:
      abort();
  }
}

static size_t type_base_width(const Type *type) {
  assert(type->any.code == TYPE_CODE_BASE);
  switch (type->base.flags & SPEC_FLAG_MASK) {
    case SPEC_FLAG_VOID:
      return 0;
    case SPEC_FLAG_CHAR:
      /* fallthrough */
    case SPEC_FLAGS_SCHAR:
      /* fallthrough */
    case SPEC_FLAGS_UCHAR:
      return X64_SIZEOF_CHAR;
    case SPEC_FLAGS_SINT:
      /* fallthrough */
    case SPEC_FLAGS_UINT:
      return X64_SIZEOF_INT;
    case SPEC_FLAGS_SSHRT:
      /* fallthrough */
    case SPEC_FLAGS_USHRT:
      return X64_SIZEOF_SHORT;
    case SPEC_FLAGS_SLONG:
      /* fallthrough */
    case SPEC_FLAGS_ULONG:
      return X64_SIZEOF_LONG;
    case SPEC_FLAG_FLOAT:
      return X64_SIZEOF_FLOAT;
    case SPEC_FLAG_DOUBLE:
      return X64_SIZEOF_DOUBLE;
    case SPEC_FLAGS_LONG_DOUBLE:
      return X64_SIZEOF_LONG_DOUBLE;
    default:
      abort();
  }
}

size_t type_get_width(const Type *type) {
  switch (type->any.code) {
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_FUNCTION:
      return 0;
    case TYPE_CODE_ENUM:
      return X64_SIZEOF_INT;
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_STRUCT:
      return type->tag.value->width;
    case TYPE_CODE_POINTER:
      return X64_SIZEOF_LONG;
    case TYPE_CODE_ARRAY:
      return type->array.length * type_elem_width(type);
    case TYPE_CODE_BASE:
      return type_base_width(type);
    case TYPE_CODE_ERROR:
      /* fallthrough */
    default:
      abort();
  }
}

size_t type_elem_width(const Type *type) {
  assert(type_is_array(type) || type_is_pointer(type));
  if (type->any.code == TYPE_CODE_ARRAY) {
    return type_get_width(type->array.next);
  } else if (type->any.code == TYPE_CODE_POINTER) {
    return type_get_width(type->pointer.next);
  } else {
    abort();
  }
}

size_t type_get_eightbytes(const Type *type) {
  size_t width = type_get_width(type);
  return (width / 8) + ((width % 8 == 0) ? 0 : 1);
}

size_t type_get_padding(const Type *type, size_t to_pad) {
  assert(!type_is_incomplete(type));
  size_t width =
      type_is_array(type) ? type_elem_width(type) : type_get_width(type);
  size_t remainder = to_pad % width;
  return remainder == 0 ? remainder : width - remainder;
}

unsigned int type_get_flags(const Type *type) {
  switch (type->any.code) {
    case TYPE_CODE_NONE:
      return type->any.flags;
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_UNION:
      return type->tag.flags;
    case TYPE_CODE_BASE:
      return type->base.flags;
    case TYPE_CODE_POINTER:
      return type->pointer.qualifiers;
    case TYPE_CODE_ARRAY:
      /* fallthrough */
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    default:
      return 0;
  }
}

SymbolValue *type_get_symbol(const Type *type) {
  Type *stripped;
  int status = type_strip_all_declarators(&stripped, type);
  if (type == NULL || status) return NULL;
  switch (type->any.code) {
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_UNION:
      return stripped->tag.symbol;
    case TYPE_CODE_BASE:
      return stripped->base.symbol;
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_ARRAY:
      /* fallthrough */
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    case TYPE_CODE_POINTER:
      /* fallthrough */
    default:
      return NULL;
  }
}

static int params_equivalent(const Type *type1, const Type *type2) {
  assert(type_is_function(type1) && type_is_function(type2));
  if (type1->function.parameters_size != type2->function.parameters_size)
    return 0;
  else if (type1->function.is_variadic != type2->function.is_variadic)
    return 0;
  size_t i;
  for (i = 0; i < type1->function.parameters_size; ++i) {
    if (!types_equivalent(type1->function.parameters[i],
                          type2->function.parameters[i], 0, 1))
      return 0;
  }
  return 1;
}

int types_equivalent(const Type *type1, const Type *type2,
                     int ignore_qualifiers, int ignore_storage_class) {
  if (type1->any.code != type2->any.code) return 0;
  switch (type1->any.code) {
    Type *stripped_type1, *stripped_type2;
    unsigned int flags_diff;
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_ERROR:
      abort();
    case TYPE_CODE_BASE:
      flags_diff = type1->base.flags ^ type2->base.flags;
      if (ignore_qualifiers) flags_diff &= ~QUAL_FLAG_MASK;
      if (ignore_storage_class) flags_diff &= ~STOR_FLAG_MASK;
      return !flags_diff;
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      flags_diff = type1->tag.flags ^ type2->tag.flags;
      if (ignore_qualifiers) flags_diff &= ~QUAL_FLAG_MASK;
      if (ignore_storage_class) flags_diff &= ~STOR_FLAG_MASK;
      return !flags_diff && type1->tag.name == type2->tag.name &&
             type1->tag.value == type2->tag.value;
    case TYPE_CODE_POINTER:
      type_strip_declarator(&stripped_type1, type1);
      type_strip_declarator(&stripped_type2, type2);
      if (!ignore_qualifiers &&
          type1->pointer.qualifiers != type2->pointer.qualifiers)
        return 0;
      else
        return types_equivalent(stripped_type1, stripped_type2,
                                ignore_qualifiers, ignore_storage_class);
    case TYPE_CODE_ARRAY:
      type_strip_declarator(&stripped_type1, type1);
      type_strip_declarator(&stripped_type2, type2);
      return type1->array.deduce_length == type2->array.deduce_length &&
             type1->array.length == type2->array.length &&
             types_equivalent(stripped_type1, stripped_type2, ignore_qualifiers,
                              ignore_storage_class);
    case TYPE_CODE_FUNCTION:
      type_strip_declarator(&stripped_type1, type1);
      type_strip_declarator(&stripped_type2, type2);
      return types_equivalent(stripped_type1, stripped_type2, ignore_qualifiers,
                              ignore_storage_class) &&
             params_equivalent(type1, type2);
    default:
      abort();
  }
}

/* TODO(Robert): consider checking to make sure that the destination is not
 * const-qualified here instead of elsewhere
 */
int types_assignable(const Type *dest, const Type *src, int is_const_zero) {
  if (type_is_arithmetic(dest) && type_is_arithmetic(src)) {
    return 1;
  } else if ((type_is_struct(dest) && type_is_struct(src)) ||
             (type_is_union(dest) && type_is_union(src))) {
    return types_equivalent(dest, src, 1, 1);
  } else if (type_is_pointer(dest) && type_is_pointer(src)) {
    if (types_equivalent(dest, src, 1, 1)) {
      return 1;
    } else if (type_is_void_pointer(dest) || type_is_void_pointer(src)) {
      return 1;
    } else {
      return 0;
    }
  } else {
    return is_const_zero && type_is_pointer(dest);
  }
}

int types_initializable(const Type *dest, const Type *src,
                        int src_is_const_zero, int src_is_init_list) {
  assert((src == NULL) ^ src_is_init_list);
  assert(!(src_is_const_zero && src_is_init_list));
  if (type_is_arithmetic(dest) && type_is_arithmetic(src)) {
    return 1;
  } else if ((type_is_struct(dest) && type_is_struct(src)) ||
             (type_is_union(dest) && type_is_union(src))) {
    return types_equivalent(dest, src, 1, 1);
  } else if (type_is_pointer(dest) && type_is_pointer(src)) {
    if (types_equivalent(dest, src, 1, 1)) {
      return 1;
    } else if (type_is_void_pointer(dest) || type_is_void_pointer(src)) {
      return 1;
    } else {
      return 0;
    }
  } else {
    return src_is_const_zero && type_is_pointer(dest);
  }
}

int type_strip_declarator(Type **dest, const Type *src) {
  switch (src->any.code) {
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_BASE:
      /* fallthrough */
    default:
      *dest = NULL;
      return -1;
    case TYPE_CODE_ARRAY:
      *dest = src->array.next;
      return 0;
    case TYPE_CODE_FUNCTION:
      *dest = src->function.next;
      return 0;
    case TYPE_CODE_POINTER:
      *dest = src->pointer.next;
      return 0;
  }
}

int type_strip_all_declarators(Type **out, const Type *type) {
  int status = type_strip_declarator(out, type);
  while (*out != NULL && type_is_declarator(*out) &&
         !(status = type_strip_declarator(out, *out)))
    ;
  return *out == NULL ? -1 : status;
}

int type_append(Type *dest, Type *src, int copy_src) {
  if (dest == NULL) return -1;
  Type *current = dest;
  while ((type_is_array(current) && current->array.next != NULL) ||
         (type_is_pointer(current) && current->pointer.next != NULL) ||
         (type_is_function(current) && current->function.next != NULL)) {
    switch (current->any.code) {
      case TYPE_CODE_ARRAY:
        current = current->array.next;
        break;
      case TYPE_CODE_POINTER:
        current = current->pointer.next;
        break;
      case TYPE_CODE_FUNCTION:
        current = current->function.next;
        break;
      default:
        abort();
    }
  }

  switch (current->any.code) {
    default:
      abort();
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_BASE:
      /* fallthrough */
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_UNION:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      return -1;
    case TYPE_CODE_POINTER:
      if (copy_src)
        assert(!type_copy(&current->pointer.next, src, 0));
      else
        current->pointer.next = src;
      return 0;
    case TYPE_CODE_ARRAY:
      if (copy_src)
        assert(!type_copy(&current->array.next, src, 0));
      else
        current->array.next = src;
      return 0;
    case TYPE_CODE_FUNCTION:
      if (copy_src)
        assert(!type_copy(&current->function.next, src, 0));
      else
        current->function.next = src;
      return 0;
  }
}

int type_copy(Type **out, const Type *type, int clear_typedef_flag) {
  if (out == NULL || type == NULL) return -1;
  Type anchor;
  anchor.pointer.code = TYPE_CODE_POINTER;
  anchor.pointer.next = NULL;
  anchor.pointer.qualifiers = 0;
  Type **out_current = &anchor.pointer.next;
  const Type *in_current = type;

  while (in_current != NULL) {
    *out_current = malloc(sizeof(Type));
    **out_current = *in_current;
    switch ((*out_current)->any.code) {
      case TYPE_CODE_NONE:
        /* fallthrough */
      default:
        abort();
      case TYPE_CODE_BASE:
        if (clear_typedef_flag)
          (*out_current)->base.flags &= ~STOR_FLAG_TYPEDEF;
        *out = anchor.pointer.next;
        return 0;
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        if (clear_typedef_flag) (*out_current)->tag.flags &= ~STOR_FLAG_TYPEDEF;
        *out = anchor.pointer.next;
        return 0;
      case TYPE_CODE_ARRAY:
        out_current = &((*out_current)->array.next);
        in_current = in_current->array.next;
        break;
      case TYPE_CODE_POINTER:
        out_current = &((*out_current)->pointer.next);
        in_current = in_current->pointer.next;
        break;
      case TYPE_CODE_FUNCTION:
        /* create new array for parameters */
        (*out_current)->function.parameters =
            malloc(in_current->function.parameters_size *
                   sizeof(*in_current->function.parameters));
        size_t i;
        for (i = 0; i < in_current->function.parameters_size; ++i)
          (*out_current)->function.parameters[i] =
              in_current->function.parameters[i];
        out_current = &((*out_current)->function.next);
        in_current = in_current->function.next;
        break;
    }
  }

  *out = anchor.pointer.next;
  /* indicates that an incomplete/malformed type was copied */
  return 1;
}

int type_common_qualified_pointer(Type **out, const Type *type1,
                                  const Type *type2) {
  Type *new_pointer = malloc(sizeof(Type));
  new_pointer->pointer.code = TYPE_CODE_POINTER;
  new_pointer->pointer.qualifiers =
      type1->pointer.qualifiers | type2->pointer.qualifiers;
  new_pointer->pointer.next = type_is_void_pointer(type1->pointer.next)
                                  ? type1->pointer.next
                                  : type2->pointer.next;
  *out = new_pointer;
  return 0;
}

int type_merge_errors(Type *dest, Type *src) {
  assert(dest->any.code == TYPE_CODE_ERROR && src->any.code == TYPE_CODE_ERROR);
  size_t new_cap = dest->error.errors_cap > src->error.errors_cap
                       ? dest->error.errors_cap
                       : src->error.errors_cap;
  if (new_cap < dest->error.errors_size + src->error.errors_size) new_cap *= 2;
  assert(new_cap >= dest->error.errors_size + src->error.errors_size);
  CompileError **new_errors =
      realloc(dest->error.errors, sizeof(CompileError *) * new_cap);
  if (new_errors == NULL) abort();
  dest->error.errors = new_errors;
  dest->error.errors_cap = new_cap;
  size_t i;
  for (i = 0; i < src->error.errors_size; ++i)
    dest->error.errors[dest->error.errors_size++] = src->error.errors[i];
  /* prevent src from freeing error info when it is destroyed */
  src->error.errors_size = 0;
  return 0;
}

/* TODO(Robert): create macro or data structure for self-resizing array */
int type_append_error(Type *type, CompileError *error) {
  assert(type->any.code == TYPE_CODE_ERROR);
  if (type->error.errors_size == type->error.errors_cap) {
    type->error.errors_cap *= 2;
    CompileError **new_errors = realloc(
        type->error.errors, type->error.errors_cap * sizeof(CompileError *));
    if (new_errors == NULL) abort();
    type->error.errors = new_errors;
  }

  type->error.errors[type->error.errors_size++] = error;
  return 0;
}

static int flags_valid(unsigned int old_flags, unsigned int new_flags,
                       int allow_stor_flags, int allow_spec_flags) {
  if (new_flags & QUAL_FLAG_MASK & old_flags) {
    return 0; /* duplicate qualifier */
  } else if ((new_flags & STOR_FLAG_MASK) && (old_flags & STOR_FLAG_MASK)) {
    return 0; /* multiple storage class specifiers */
  } else if (!allow_stor_flags && (new_flags & STOR_FLAG_MASK)) {
    return 0; /* flagset cannot accept storage class specifiers */
  } else if (!allow_spec_flags && (new_flags & SPEC_FLAG_MASK)) {
    return 0; /* flagset cannot accept type specifiers */
  } else {
    /* mask out qualifiers and storage class since they are valid */
    old_flags &= SPEC_FLAG_MASK, new_flags &= SPEC_FLAG_MASK;
    switch (new_flags) {
      case SPEC_FLAG_NONE: /* also SPEC_FLAG_INT */
        return 1;
      case SPEC_FLAG_FLOAT:
        /* fallthrough */
      case SPEC_FLAG_VOID:
        return old_flags == SPEC_FLAG_NONE;
      case SPEC_FLAG_CHAR:
        return (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_NONE;
      case SPEC_FLAG_INTEGRAL:
        return (old_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_NONE;
      case SPEC_FLAG_SHORT:
        return (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_NONE ||
               (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_INTEGRAL;
      case SPEC_FLAG_LONG: /* remember to support `long long` later on */
        return (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_NONE ||
               (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_INTEGRAL ||
               (old_flags & ~SPEC_FLAG_SIGN_MASK) == SPEC_FLAG_DOUBLE;
      case SPEC_FLAG_SIGNED:
        /* fallthrough */
      case SPEC_FLAG_UNSIGNED:
        return !(old_flags & SPEC_FLAG_SIGN_MASK) &&
               ((old_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_INTEGRAL ||
                (old_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_CHAR ||
                (old_flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_NONE);
      case SPEC_FLAG_DOUBLE:
        return old_flags == SPEC_FLAG_NONE || old_flags == SPEC_FLAG_LONG;
      default:
        abort();
    }
  }
}

int type_add_flags(Type *type, unsigned int flags) {
  assert(
      type->any.code == TYPE_CODE_POINTER || type->any.code == TYPE_CODE_BASE ||
      type->any.code == TYPE_CODE_STRUCT || type->any.code == TYPE_CODE_UNION ||
      type->any.code == TYPE_CODE_ENUM || type->any.code == TYPE_CODE_NONE);
  switch (type->any.code) {
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    case TYPE_CODE_ARRAY:
      /* fallthrough */
    case TYPE_CODE_ERROR:
      /* fallthrough */
    default:
      abort();
    case TYPE_CODE_NONE:
      assert((type->any.flags & SPEC_FLAG_MASK) == 0);
      if (!flags_valid(type->any.flags, flags, 1, 1)) {
        return 1;
      } else if ((flags & SPEC_FLAG_MASK) != 0) {
        unsigned int old_flags = type->any.flags;
        type->base.code = TYPE_CODE_BASE;
        type->base.flags = old_flags | flags;
        return 0;
      } else {
        return type->any.flags |= flags, 0;
      }
      /* fallthrough */
    case TYPE_CODE_BASE:
      if (!flags_valid(type->base.flags, flags, 1, 1))
        return 1;
      else
        return type->base.flags |= flags, 0;
    case TYPE_CODE_POINTER:
      if (!flags_valid(type->pointer.qualifiers, flags, 0, 0))
        return 1;
      else
        return type->pointer.qualifiers |= flags, 0;
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
      if (!flags_valid(type->tag.flags, flags, 1, 0))
        return 1;
      else
        return type->tag.flags |= flags, 0;
  }
}

int type_normalize(Type *type) {
  if (type_is_declarator(type)) {
    int status = type_strip_all_declarators(&type, type);
    if (status) return status;
  }

  if (type == NULL || type->any.code == TYPE_CODE_NONE)
    return -1;
  else if (type->any.code != TYPE_CODE_BASE)
    return 0;
  else if ((type->base.flags & SPEC_FLAG_LOW_MASK) != SPEC_FLAG_NONE &&
           (type->base.flags & SPEC_FLAG_LOW_MASK) != SPEC_FLAG_INTEGRAL)
    return 0;

  if ((type->base.flags & SPEC_FLAG_LOW_MASK) == SPEC_FLAG_NONE) {
    if ((type->base.flags & SPEC_FLAG_SIZE_MASK) == 0) {
      if (type->base.flags & SPEC_FLAG_SIGN_MASK) {
        type->base.flags |= SPEC_FLAG_INTEGRAL;
        return 0;
      } else {
        return -1;
      }
    } else if ((type->base.flags & SPEC_FLAG_SIZE_MASK) == SPEC_FLAG_SHORT) {
      type->base.flags |= (type->base.flags & SPEC_FLAG_UNSIGNED)
                              ? SPEC_FLAGS_USHRT
                              : SPEC_FLAGS_SSHRT;
      return 0;
    } else if ((type->base.flags & SPEC_FLAG_SIZE_MASK) == SPEC_FLAG_LONG) {
      type->base.flags |= (type->base.flags & SPEC_FLAG_UNSIGNED)
                              ? SPEC_FLAGS_ULONG
                              : SPEC_FLAGS_SLONG;
      return 0;
    } else { /* in the future, support `long long` */
      abort();
    }
  } else {
    if (!(type->base.flags & SPEC_FLAG_SIGN_MASK))
      type->base.flags |= SPEC_FLAG_SIGNED;
    return 0;
  }
}

/*
 * Performs automatic conversions from function and array types to pointer
 * types. Replaces old type with appropriately converted type. Can safely be
 * called when `expr` is an error.
 */
int type_pointer_conversions(Type **out, Type *type) {
  if (type_is_function(type) || type_is_array(type)) {
    /* do not override out until we know that we have succeeded so that the
     * type information is not lost when we fail
     */
    Type *pointer_type;
    int status = type_init_pointer(&pointer_type, 0);
    if (status) return -1;

    if (type_is_array(type)) {
      status = type_strip_declarator(&(pointer_type->pointer.next), type);
      if (status) abort();
    } else {
      pointer_type->pointer.next = type;
    }
    *out = pointer_type;
  } else {
    *out = type;
  }

  return 0;
}

int type_arithmetic_conversions(Type **out, Type *type1, Type *type2) {
  assert(out != NULL);
  assert(type_is_arithmetic(type1) && type_is_arithmetic(type2));
  unsigned int flags1 = type1->any.flags & SPEC_FLAG_MASK;
  unsigned int flags2 = type2->any.flags & SPEC_FLAG_MASK;

  if (flags1 == SPEC_FLAGS_LONG_DOUBLE || flags2 == SPEC_FLAGS_LONG_DOUBLE) {
    abort(); /* floating types not implemented */
  } else if (flags1 == SPEC_FLAG_DOUBLE || flags2 == SPEC_FLAG_DOUBLE) {
    abort(); /* floating types not implemented */
  } else if (flags1 == SPEC_FLAG_FLOAT || flags2 == SPEC_FLAG_FLOAT) {
    abort(); /* floating types not implemented */
  } else if (flags1 == SPEC_FLAGS_ULONG) {
    *out = type1;
  } else if (flags2 == SPEC_FLAGS_ULONG) {
    *out = type2;
  } else if (flags1 == SPEC_FLAGS_SLONG) {
    *out = type1;
  } else if (flags2 == SPEC_FLAGS_SLONG) {
    *out = type2;
  } else if (flags1 == SPEC_FLAGS_UINT) {
    *out = type1;
  } else if (flags2 == SPEC_FLAGS_UINT) {
    *out = type2;
  } else {
    /* no need to check for enum, char, etc. explicitly; all become int */
    *out = (Type *)TYPE_INT;
  }

  return 0;
}

int type_set_symbol(Type *type, SymbolValue *symbol) {
  int status = type_strip_all_declarators(&type, type);
  if (status || type == NULL) return -1;
  switch (type->any.code) {
    case TYPE_CODE_STRUCT:
      /* fallthrough */
    case TYPE_CODE_ENUM:
      /* fallthrough */
    case TYPE_CODE_UNION:
      type->tag.symbol = symbol;
      return 0;
    case TYPE_CODE_BASE:
      type->base.symbol = symbol;
      return 0;
    case TYPE_CODE_NONE:
      /* fallthrough */
    case TYPE_CODE_ARRAY:
      /* fallthrough */
    case TYPE_CODE_FUNCTION:
      /* fallthrough */
    case TYPE_CODE_POINTER:
      /* fallthrough */
    default:
      return -1;
  }
}
