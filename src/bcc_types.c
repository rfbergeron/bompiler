#include "bcc_types.h"

#include "assert.h"
#include "scope.h"
#include "stdint.h"
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
  TYPE_POINTER_INTERNAL.pointer.code = TYPE_CODE_POINTER;
  TYPE_POINTER_INTERNAL.pointer.qualifiers = QUAL_FLAG_NONE;
  TYPE_POINTER_INTERNAL.pointer.next = &TYPE_VOID_INTERNAL;
  TYPE_CHAR_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_CHAR_INTERNAL.base.flags = SPEC_FLAG_CHAR;
  TYPE_INT_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_INT_INTERNAL.base.flags = SPEC_FLAGS_SINT;
  TYPE_LONG_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_LONG_INTERNAL.base.flags = SPEC_FLAGS_SLONG;
  TYPE_UNSIGNED_INT_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_UNSIGNED_INT_INTERNAL.base.flags = SPEC_FLAGS_UINT;
  TYPE_UNSIGNED_LONG_INTERNAL.base.code = TYPE_CODE_BASE;
  TYPE_UNSIGNED_LONG_INTERNAL.base.flags = SPEC_FLAGS_ULONG;
  TYPE_NONE_INTERNAL.any.code = TYPE_CODE_NONE;
  TYPE_NONE_INTERNAL.any.flags = 0;
}

Type *type_init_none(unsigned int flags) {
  /* may only be called with storage class and qualifier flags */
  assert(!(flags & SPEC_FLAG_MASK));
  Type *type = malloc(sizeof(Type));
  type->any.code = TYPE_CODE_NONE;
  type->any.flags = flags;
  return type;
}

Type *type_init_pointer(unsigned int qualifiers) {
  /* may only be called with qualifier flags */
  assert(!(qualifiers & ~QUAL_FLAG_MASK));
  Type *type = malloc(sizeof(Type));
  type->pointer.code = TYPE_CODE_POINTER;
  type->pointer.qualifiers = qualifiers;
  type->pointer.next = NULL;
  return type;
}

Type *type_init_array(size_t length, int deduce_length) {
  Type *type = malloc(sizeof(Type));
  type->array.code = TYPE_CODE_ARRAY;
  type->array.length = length;
  type->array.deduce_length = deduce_length;
  type->array.next = NULL;
  return type;
}

Type *type_init_function(size_t parameters_size, Type **parameters,
                         int is_variadic, int is_old_style) {
  Type *type = malloc(sizeof(Type));
  type->function.code = TYPE_CODE_FUNCTION;
  type->function.parameters_size = parameters_size;
  type->function.parameters = parameters;
  type->function.is_variadic = is_variadic;
  type->function.is_old_style = is_old_style;
  type->function.next = NULL;
  return type;
}

Type *type_init_tag(unsigned int flags, const char *tag_name, Tag *tag) {
  /* flags may not include type specifiers */
  assert(!(flags & SPEC_FLAG_MASK));
  assert(tag_name != NULL && tag != NULL);
  Type *type = malloc(sizeof(Type));
  switch (tag->record.kind) {
    case TAG_STRUCT:
      type->tag.code = TYPE_CODE_STRUCT;
      break;
    case TAG_UNION:
      type->tag.code = TYPE_CODE_UNION;
      break;
    case TAG_ENUM:
      type->tag.code = TYPE_CODE_ENUM;
      break;
    default:
      abort();
  }
  type->tag.flags = flags;
  type->tag.name = tag_name;
  type->tag.value = tag;
  return type;
}

Type *type_init_base(unsigned int flags) {
  /* flag must include a type specifier */
  assert(flags & SPEC_FLAG_MASK);
  Type *type = malloc(sizeof(Type));
  type->base.code = TYPE_CODE_BASE;
  type->base.flags = flags;
  return type;
}

void type_destroy(Type *type) {
  while (type != NULL) {
    Type *current = type;
    switch (type->any.code) {
      default:
        abort();
      case TYPE_CODE_NONE:
        /* fallthrough */
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        /* fallthrough */
      case TYPE_CODE_BASE:
        free(current);
        return;
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
    }
  }
  /* a properly formed type should never reach this point, but if the type
   * was formed erroneously and we're performing cleanup it is possible that
   * this singly-linked list may be NULL, so it's not necessarily an error.
   */
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

int type_is_object(const Type *type) {
  return !type_is_function(type) && !type_is_array(type);
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
             !type->tag.value->record.defined) {
    return 1;
  } else {
    return 0;
  }
}

Symbol *type_member_name(const Type *type, const char *name) {
  assert(type_is_union(type) || type_is_struct(type));
  return scope_get_member(type->tag.value->record.members, name);
}

Symbol *type_member_index(const Type *type, size_t index) {
  assert(type_is_union(type) || type_is_struct(type));
  Symbol *member;
  scope_member_at(type->tag.value->record.members, index, NULL, &member);
  return member;
}

size_t type_member_count(const Type *type) {
  assert(type_is_aggregate(type));
  switch (type->any.code) {
    case TYPE_CODE_UNION:
      return 1;
    case TYPE_CODE_STRUCT:
      return scope_member_count(type->tag.value->record.members);
    case TYPE_CODE_ARRAY:
      if (type->array.deduce_length && type->array.length == 0)
        return SIZE_MAX;
      else
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
      return type->tag.value->enumeration.alignment;
    case TYPE_CODE_POINTER:
      return X64_ALIGNOF_LONG;
    case TYPE_CODE_ARRAY:
      return type_get_alignment(type->array.next);
    case TYPE_CODE_BASE:
      return type_base_alignment(type);
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
      return type->tag.value->record.width;
    case TYPE_CODE_POINTER:
      return X64_SIZEOF_LONG;
    case TYPE_CODE_ARRAY:
      return type->array.length * type_elem_width(type);
    case TYPE_CODE_BASE:
      return type_base_width(type);
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
  size_t alignment = type_get_alignment(type);
  return to_pad % alignment == 0 ? 0 : alignment - (to_pad % alignment);
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
      stripped_type1 = type_strip_declarator(type1);
      stripped_type2 = type_strip_declarator(type2);
      if (!ignore_qualifiers &&
          type1->pointer.qualifiers != type2->pointer.qualifiers)
        return 0;
      else
        return types_equivalent(stripped_type1, stripped_type2,
                                ignore_qualifiers, ignore_storage_class);
    case TYPE_CODE_ARRAY:
      stripped_type1 = type_strip_declarator(type1);
      stripped_type2 = type_strip_declarator(type2);
      return type1->array.deduce_length == type2->array.deduce_length &&
             type1->array.length == type2->array.length &&
             types_equivalent(stripped_type1, stripped_type2, ignore_qualifiers,
                              ignore_storage_class);
    case TYPE_CODE_FUNCTION:
      stripped_type1 = type_strip_declarator(type1);
      stripped_type2 = type_strip_declarator(type2);
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

Type *type_strip_declarator(const Type *type) {
  assert(type_is_declarator(type));
  switch (type->any.code) {
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
      return NULL;
    case TYPE_CODE_ARRAY:
      assert(type->array.next != NULL);
      return type->array.next;
    case TYPE_CODE_FUNCTION:
      assert(type->function.next != NULL);
      return type->function.next;
    case TYPE_CODE_POINTER:
      assert(type->pointer.next != NULL);
      return type->pointer.next;
  }
}

Type *type_get_decl_specs(Type *type) {
  assert(type != NULL);
  while (type_is_declarator(type)) type = type_strip_declarator(type);
  assert(type != NULL);
  return type;
}

Type *type_append(Type *dest, Type *src, int copy_src) {
  assert(dest != NULL);
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
      abort();
    case TYPE_CODE_POINTER:
      current->pointer.next = copy_src ? type_copy(src, 0) : src;
      return dest;
    case TYPE_CODE_ARRAY:
      current->array.next = copy_src ? type_copy(src, 0) : src;
      return dest;
    case TYPE_CODE_FUNCTION:
      current->function.next = copy_src ? type_copy(src, 0) : src;
      return dest;
  }
}

Type *type_copy(const Type *type, int clear_typedef_flag) {
  assert(type != NULL);
  Type anchor;
  anchor.pointer.code = TYPE_CODE_POINTER;
  anchor.pointer.next = NULL;
  anchor.pointer.qualifiers = 0;
  Type **type_tail = &anchor.pointer.next;
  const Type *in_current = type;

  while (in_current != NULL) {
    *type_tail = malloc(sizeof(Type));
    **type_tail = *in_current;
    switch ((*type_tail)->any.code) {
      case TYPE_CODE_NONE:
        /* fallthrough */
      default:
        abort();
      case TYPE_CODE_BASE:
        if (clear_typedef_flag) (*type_tail)->base.flags &= ~STOR_FLAG_TYPEDEF;
        return anchor.pointer.next;
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        if (clear_typedef_flag) (*type_tail)->tag.flags &= ~STOR_FLAG_TYPEDEF;
        return anchor.pointer.next;
      case TYPE_CODE_ARRAY:
        type_tail = &((*type_tail)->array.next);
        in_current = in_current->array.next;
        break;
      case TYPE_CODE_POINTER:
        type_tail = &((*type_tail)->pointer.next);
        in_current = in_current->pointer.next;
        break;
      case TYPE_CODE_FUNCTION:
        /* create new array for parameters */
        (*type_tail)->function.parameters =
            malloc(in_current->function.parameters_size *
                   sizeof(*in_current->function.parameters));
        size_t i;
        for (i = 0; i < in_current->function.parameters_size; ++i)
          (*type_tail)->function.parameters[i] =
              in_current->function.parameters[i];
        type_tail = &((*type_tail)->function.next);
        in_current = in_current->function.next;
        break;
    }
  }

  return anchor.pointer.next;
}

Type *type_common_qualified_pointer(const Type *type1, const Type *type2) {
  Type *new_pointer = malloc(sizeof(Type));
  new_pointer->pointer.code = TYPE_CODE_POINTER;
  new_pointer->pointer.qualifiers =
      type1->pointer.qualifiers | type2->pointer.qualifiers;
  new_pointer->pointer.next = type_is_void_pointer(type1->pointer.next)
                                  ? type1->pointer.next
                                  : type2->pointer.next;
  return new_pointer;
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
    default:
      abort();
    case TYPE_CODE_NONE:
      assert((type->any.flags & SPEC_FLAG_MASK) == 0);
      if (!flags_valid(type->any.flags, flags, 1, 1)) {
        return -1;
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
        return -1;
      else
        return type->base.flags |= flags, 0;
    case TYPE_CODE_POINTER:
      if (!flags_valid(type->pointer.qualifiers, flags, 0, 0))
        return -1;
      else
        return type->pointer.qualifiers |= flags, 0;
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
      if (!flags_valid(type->tag.flags, flags, 1, 0))
        return -1;
      else
        return type->tag.flags |= flags, 0;
  }
}

/* TODO(Robert): ensure that objects with automatic storage class have their
 * type defaulted to `int`
 */
int type_normalize(Type *type) {
  type = type_get_decl_specs(type);

  if (type->any.code == TYPE_CODE_NONE)
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
      return -1;
    }
  } else {
    if (!(type->base.flags & SPEC_FLAG_SIGN_MASK))
      type->base.flags |= SPEC_FLAG_SIGNED;
    return 0;
  }
}

int type_validate(const Type *type) {
  if (type_is_void(type) && !(type->base.flags & STOR_FLAG_TYPEDEF)) return 1;

  while (type != NULL) {
    switch (type->any.code) {
      case TYPE_CODE_BASE:
        /* fallthrough */
      case TYPE_CODE_STRUCT:
        /* fallthrough */
      case TYPE_CODE_UNION:
        /* fallthrough */
      case TYPE_CODE_ENUM:
        return 0;
      case TYPE_CODE_POINTER:
        type = type_strip_declarator(type);
        continue;
      case TYPE_CODE_ARRAY:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_incomplete(type) || type_is_function(type))
          return 1;
        continue;
      case TYPE_CODE_FUNCTION:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_array(type) || type_is_function(type))
          return 1;
        continue;
      case TYPE_CODE_NONE:
        return 1;
      default:
        abort();
    }
  }

  return 1;
}

/*
 * Performs automatic conversions from function and array types to pointer
 * types. Replaces old type with appropriately converted type.
 */
Type *type_pointer_conversions(Type *type) {
  if (type_is_function(type) || type_is_array(type)) {
    /* do not override out until we know that we have succeeded so that the
     * type information is not lost when we fail
     */
    Type *pointer_type = type_init_pointer(SPEC_FLAG_NONE);
    Type *append_type =
        type_is_array(type) ? type_strip_declarator(type) : type;
    return type_append(pointer_type, append_type, 0);
  } else {
    return type;
  }
}

Type *type_arithmetic_conversions(Type *type1, Type *type2) {
  assert(type1 != NULL && type2 != NULL);
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
    return type1;
  } else if (flags2 == SPEC_FLAGS_ULONG) {
    return type2;
  } else if (flags1 == SPEC_FLAGS_SLONG) {
    return type1;
  } else if (flags2 == SPEC_FLAGS_SLONG) {
    return type2;
  } else if (flags1 == SPEC_FLAGS_UINT) {
    return type1;
  } else if (flags2 == SPEC_FLAGS_UINT) {
    return type2;
  } else if (flags1 == SPEC_FLAGS_SINT) {
    return type1;
  } else if (flags2 == SPEC_FLAGS_SINT) {
    return type2;
  } else {
    /* no need to check for enum, char, etc. explicitly; all become int */
    return (Type *)TYPE_INT;
  }
}

int type_complete_array(Type *old_type, Type *new_type) {
  if (!(type_is_array(old_type) && type_is_array(new_type))) {
    return 0;
  } else if (!types_equivalent(type_strip_declarator(old_type),
                               type_strip_declarator(new_type), 0, 1)) {
    return 0;
  } else if (!old_type->array.deduce_length && !new_type->array.deduce_length) {
    /* neither array has deduced length and this function should only be called
     * when the types are not equivalent, so they must have differing lengths,
     * which indicates a semantic error
     */
    return 0;
  } else if (old_type->array.length != 0 && new_type->array.length != 0 &&
             old_type->array.length != new_type->array.length) {
    /* at least one type has deduced length, both the length of both arrays is
     * known and differs, which indicates a semantic error
     */
    return 0;
  } else {
    /* give old type a length if the new type has one */
    if (old_type->array.length < new_type->array.length) {
      old_type->array.length = new_type->array.length;
      old_type->array.deduce_length = new_type->array.deduce_length;
    }
    return 1;
  }
}

int type_prototype_function(Type *old_type, Type *new_type) {
  if (!(type_is_function(old_type) && type_is_function(new_type))) {
    return 0;
  } else if (!types_equivalent(type_strip_declarator(old_type),
                               type_strip_declarator(new_type), 0, 1)) {
    return 0;
  } else if (!type_is_prototyped_function(new_type)) {
    /* new function type has no prototype; nothing to do here */
    return 1;
  } else if (type_is_prototyped_function(old_type)) {
    /* both functions have a prototype; this function should only be called
     * when it has been determined that the types are not equivalent, so at
     * this point it is definitely a semantic error
     */
    return 0;
  } else {
    /* existing type may have already been used elsewhere so we have to replace
     * its contents in-place; all this means is moving some parameter info from
     * the new type to the old one
     */
    old_type->function.parameters = new_type->function.parameters;
    old_type->function.parameters_size = new_type->function.parameters_size;
    old_type->function.is_variadic = new_type->function.is_variadic;
    old_type->function.is_old_style = new_type->function.is_old_style;
    /* make old definition look like an old style declaration for the type
     * checker's sake
     */
    new_type->function.parameters = NULL;
    new_type->function.parameters_size = 0;
    new_type->function.is_old_style = 1;
    new_type->function.is_variadic = 1;
    return 1;
  }
}
