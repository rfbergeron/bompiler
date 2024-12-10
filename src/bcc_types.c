#include "bcc_types.h"

#include "assert.h"
#include "scope.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define GET_ARRAY_LEN(type) ((type->array.flags) >> 2)
#define GET_PARAM_COUNT(type) ((type->function.flags) >> 3)
#define SWAP(type, x, y) \
  do {                   \
    type temp = (x);     \
    (x) = (y);           \
    (y) = temp;          \
  } while (0)
#define BCC_ARRAY_MAX_LEN (0x4000000000000000 - 1ul)
#define BCC_FN_MAX_PARAMS (0x2000000000000000 - 1ul)

typedef enum type_flag {
  DECL_SPEC_NONE = 0,
  TYPE_KIND_MASK = 0x3, /* type kind: 2-bit integer */
  TYPE_KIND_SPECS = 0,
  TYPE_KIND_PTR,
  TYPE_KIND_ARR,
  TYPE_KIND_FN,
  TYPE_QUAL_MASK = 0x3c, /* type qualifiers: 4-bit flagset */
  TYPE_QUAL_CONST = 1 << 2,
  TYPE_QUAL_VOLATILE = 1 << 3,
  /* TYPE_QUAL_RESTRICT = 1 << 4, */
  /* TYPE_QUAL_ATOMIC = 1 << 5, */
  STORE_SPEC_MASK = 0x1c0, /* storage class: 3-bit integer */
  STORE_SPEC_REGISTER = 1 << 6,
  STORE_SPEC_AUTO = 2 << 6,
  STORE_SPEC_STATIC = 3 << 6,
  STORE_SPEC_EXTERN = 4 << 6,
  STORE_SPEC_TYPEDEF = 5 << 6,
  /* unused 6th value */
  /* unused 7th value */
  TYPE_SPEC_LO_MASK = 0xe00, /* "low" type info: 3-bit integer */
  TYPE_SPEC_VOID = 1 << 9,
  TYPE_SPEC_CHAR = 2 << 9,
  TYPE_SPEC_INT = 3 << 9,
  TYPE_SPEC_FLT = 4 << 9,
  TYPE_SPEC_DBL = 5 << 9,
  TYPE_SPEC_TAG = 6 << 9,
  /* TYPE_SPEC_BOOL = 7 << 9, */
  TYPE_SPEC_HI_MASK = 0xf000,   /* "high" type info: 4 bits */
  TYPE_SPEC_SIGN_MASK = 0x3000, /* signedness: 2-bit flagset */
  TYPE_SPEC_SIGNED = 1 << 12,
  TYPE_SPEC_UNSIGNED = 1 << 13,
  TYPE_SPEC_TAG_MASK = 0x3000, /* tag kind: 2-bit integer */
  /* TYPE_KIND_SPECS | TYPE_SPEC_TAG | 0 is invalid (for now) */
  TYPE_SPEC_STRUCT = 1 << 12,
  TYPE_SPEC_UNION = 2 << 12,
  TYPE_SPEC_ENUM = 3 << 12,
  TYPE_SPEC_SIZE_MASK = 0xc000, /* integer width: 2-bit integer */
  /* TYPE_SPEC_INT is also 0 << 14 */
  TYPE_SPEC_SHRT = 1 << 14,
  TYPE_SPEC_LONG = 1 << 15,
  /* TYPE_SPEC_LLONG = 3 << 14, */
  /* "canonical" flag representations of types */
  TYPE_SPECS_VOID = TYPE_KIND_SPECS | TYPE_SPEC_VOID,
  TYPE_SPECS_CHAR = TYPE_KIND_SPECS | TYPE_SPEC_CHAR,
  TYPE_SPECS_SCHAR = TYPE_KIND_SPECS | TYPE_SPEC_CHAR | TYPE_SPEC_SIGNED,
  TYPE_SPECS_UCHAR = TYPE_KIND_SPECS | TYPE_SPEC_CHAR | TYPE_SPEC_UNSIGNED,
  TYPE_SPECS_INT = TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_SIGNED,
  TYPE_SPECS_UINT = TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_UNSIGNED,
  TYPE_SPECS_SHRT =
      TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_SHRT | TYPE_SPEC_SIGNED,
  TYPE_SPECS_USHRT =
      TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_SHRT | TYPE_SPEC_UNSIGNED,
  TYPE_SPECS_LONG =
      TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_LONG | TYPE_SPEC_SIGNED,
  TYPE_SPECS_ULONG =
      TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_LONG | TYPE_SPEC_UNSIGNED,
  /* TYPE_SPECS_LLONG = TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_LLONG |
     TYPE_SPEC_SIGNED, */
  /* TYPE_SPECS_ULLONG = TYPE_KIND_SPECS | TYPE_SPEC_INT | TYPE_SPEC_LLONG |
     TYPE_SPEC_UNSIGNED, */
  TYPE_SPECS_FLT = TYPE_KIND_SPECS | TYPE_SPEC_FLT,
  TYPE_SPECS_DBL = TYPE_KIND_SPECS | TYPE_SPEC_DBL,
  TYPE_SPECS_LDBL = TYPE_KIND_SPECS | TYPE_SPEC_DBL | TYPE_SPEC_LONG,
  TYPE_SPECS_STRUCT = TYPE_KIND_SPECS | TYPE_SPEC_TAG | TYPE_SPEC_STRUCT,
  TYPE_SPECS_UNION = TYPE_KIND_SPECS | TYPE_SPEC_TAG | TYPE_SPEC_UNION,
  TYPE_SPECS_ENUM = TYPE_KIND_SPECS | TYPE_SPEC_TAG | TYPE_SPEC_ENUM,
  TYPE_SPEC_MASK = 0xffff ^ (TYPE_QUAL_MASK | STORE_SPEC_MASK),
  /* special flags for direct declarators */
  TYPE_FLAG_VARIADIC = 1 << 2
} TypeFlag;

union type {
  struct {
    unsigned long flags;
    union tag *tag;
  } decl_specs;
  struct {
    unsigned long flags;
    Type *next;
  } pointer;
  struct {
    unsigned long flags; /* length in upper bits */
    Type *next;
  } array;
  struct {
    unsigned long flags; /* param count in upper bits */
    Type **next;         /* return type is the element after last param type */
  } function;
};

static Tag TAG_VA_SPILL_REGION = {{TAG_STRUCT, 1,
                                   X64_SIZEOF_LONG *PARAM_REG_COUNT,
                                   X64_ALIGNOF_LONG, NULL, NULL}};

static Type TYPE_VOID_INTERNAL;
static Type TYPE_POINTER_INTERNAL;
static Type TYPE_CHAR_INTERNAL;
static Type TYPE_INT_INTERNAL;
static Type TYPE_LONG_INTERNAL;
static Type TYPE_UNSIGNED_INT_INTERNAL;
static Type TYPE_UNSIGNED_LONG_INTERNAL;
static Type TYPE_NONE_INTERNAL;
static const Type TYPE_VA_SPILL_REGION_INTERNAL = {
    {TYPE_SPECS_STRUCT, &TAG_VA_SPILL_REGION}};

const Type *const TYPE_VOID = &TYPE_VOID_INTERNAL;
const Type *const TYPE_POINTER = &TYPE_POINTER_INTERNAL;
const Type *const TYPE_CHAR = &TYPE_CHAR_INTERNAL;
const Type *const TYPE_INT = &TYPE_INT_INTERNAL;
const Type *const TYPE_LONG = &TYPE_LONG_INTERNAL;
const Type *const TYPE_UNSIGNED_INT = &TYPE_UNSIGNED_INT_INTERNAL;
const Type *const TYPE_UNSIGNED_LONG = &TYPE_UNSIGNED_LONG_INTERNAL;
const Type *const TYPE_NONE = &TYPE_NONE_INTERNAL;
const Type *const TYPE_VA_SPILL_REGION = &TYPE_VA_SPILL_REGION_INTERNAL;

void type_init_globals(void) {
  /* we have to do this here since designated initializers don't exist in c90 */
  TYPE_VOID_INTERNAL.decl_specs.flags = TYPE_SPECS_VOID;
  TYPE_POINTER_INTERNAL.pointer.flags = TYPE_KIND_PTR;
  TYPE_POINTER_INTERNAL.pointer.next = &TYPE_VOID_INTERNAL;
  TYPE_CHAR_INTERNAL.decl_specs.flags = TYPE_SPECS_CHAR;
  TYPE_INT_INTERNAL.decl_specs.flags = TYPE_SPECS_INT;
  TYPE_LONG_INTERNAL.decl_specs.flags = TYPE_SPECS_LONG;
  TYPE_UNSIGNED_INT_INTERNAL.decl_specs.flags = TYPE_SPECS_UINT;
  TYPE_UNSIGNED_LONG_INTERNAL.decl_specs.flags = TYPE_SPECS_ULONG;
  /* TYPE_NONE_INTERNAL already zero-filled; no need to initialize */
}

Type *type_init_basic(void) { return calloc(1, sizeof(Type)); }

Type *type_init_tag(Tag *tag) {
  static const TypeFlag TAG_TYPESPEC_MAP[] = {
      TYPE_SPECS_STRUCT, TYPE_SPECS_UNION, TYPE_SPECS_ENUM};
  Type *type = malloc(sizeof(Type));
  type->decl_specs.flags = TAG_TYPESPEC_MAP[tag_get_kind(tag)];
  type->decl_specs.tag = tag;
  return type;
}

Type *type_init_pointer(void) {
  /* may only be called with qualifier flags */
  Type *type = malloc(sizeof(Type));
  type->pointer.flags = TYPE_KIND_PTR;
  type->pointer.next = NULL;
  return type;
}

Type *type_init_array(size_t length) {
  assert(length <= BCC_ARRAY_MAX_LEN);
  Type *type = malloc(sizeof(Type));
  type->array.flags = TYPE_KIND_ARR | (length << 2);
  type->array.next = NULL;
  return type;
}

Type *type_init_function(size_t parameters_size, Type **parameters,
                         int variadic) {
  assert(parameters_size <= BCC_FN_MAX_PARAMS);
  assert(variadic == 0 || variadic == 1);
  Type *type = malloc(sizeof(Type));
  type->function.flags = TYPE_KIND_FN | variadic << 2 | parameters_size << 3;
  type->function.next = parameters;
  type->function.next[parameters_size] = NULL;
  return type;
}

void type_destroy(Type *type) {
  while (type != NULL) {
    Type *current = type;
    switch (type->decl_specs.flags & TYPE_KIND_MASK) {
      case TYPE_KIND_SPECS:
        free(current);
        return;
      case TYPE_KIND_PTR:
        type = type->pointer.next;
        free(current);
        break;
      case TYPE_KIND_FN:
        type = type->function.next[GET_PARAM_COUNT(type)];
        free(current->function.next);
        free(current);
        break;
      case TYPE_KIND_ARR:
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
  switch (qualifiers & TYPE_QUAL_MASK) {
    case TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE:
      return "const volatile";
    case TYPE_QUAL_CONST:
      return "const";
    case TYPE_QUAL_VOLATILE:
      return "volatile";
    default:
      return NULL;
  }
}

void type_dump_specs(unsigned long flags) {
  if ((flags & TYPE_KIND_MASK) != TYPE_KIND_SPECS) {
    (void)fprintf(stderr, "Cannot dump flags for direct declarators.\n");
    return;
  }

  (void)fprintf(stderr, "Type qualifiers: [");
  if (flags & TYPE_QUAL_CONST) (void)fprintf(stderr, " const ");
  if (flags & TYPE_QUAL_VOLATILE) (void)fprintf(stderr, " volatile ");

  static const char *STORAGE_DUMP_MAP[] = {"(none)",    "register", "auto",
                                           "static",    "extern",   "typedef",
                                           "(invalid)", "(invalid)"};
  (void)fprintf(stderr, "]\nStorage class specifier: %s\n",
                STORAGE_DUMP_MAP[(flags & STORE_SPEC_MASK) >> 6]);

  static const char *LO_DUMP_MAP[] = {"(none)", "void",   "char",  "int",
                                      "float",  "double", "(tag)", "bool"};
  (void)fprintf(stderr, "Low type specifier bits: %s\n",
                LO_DUMP_MAP[(flags & TYPE_SPEC_LO_MASK) >> 9]);

  if ((flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_TAG) {
    static const char *TAG_DUMP_MAP[] = {"(invalid)", "struct", "union",
                                         "enum"};
    (void)fprintf(stderr, "Tag type: %s\n",
                  TAG_DUMP_MAP[(flags & TYPE_SPEC_TAG_MASK) >> 12]);
  } else {
    (void)fprintf(stderr, "Sign flags: [");
    if (flags & TYPE_SPEC_SIGNED) (void)fprintf(stderr, " signed ");
    if (flags & TYPE_SPEC_UNSIGNED) (void)fprintf(stderr, " unsigned ");

    static const char *SIZE_DUMP_MAP[] = {"int", "short", "long", "(invalid)"};
    (void)fprintf(stderr, "]\nSize bits: %s\n",
                  SIZE_DUMP_MAP[(flags & TYPE_SPEC_SIZE_MASK) >> 14]);
  }
}

/* TODO(Robert): this function should be able to print out types that haven't
 * been normalized yet, for diagnostic purposes
 */
static const char *decl_specs_get_str(unsigned int flags) {
  switch (flags & TYPE_SPEC_MASK) {
    case DECL_SPEC_NONE:
      return "(none)";
    case TYPE_SPECS_VOID:
      return "void";
    case TYPE_SPECS_CHAR:
      return "char";
    case TYPE_SPECS_SCHAR:
      return "signed char";
    case TYPE_SPECS_UCHAR:
      return "unsigned char";
    case TYPE_SPECS_INT:
      return "signed int";
    case TYPE_SPECS_UINT:
      return "unsigned int";
    case TYPE_SPECS_SHRT:
      return "signed short";
    case TYPE_SPECS_USHRT:
      return "unsigned short";
    case TYPE_SPECS_LONG:
      return "signed long";
    case TYPE_SPECS_ULONG:
      return "unsigned long";
    case TYPE_SPECS_FLT:
      return "float";
    case TYPE_SPECS_DBL:
      return "double";
    case TYPE_SPECS_LDBL:
      return "long double";
    case TYPE_SPECS_STRUCT:
      return "struct";
    case TYPE_SPECS_UNION:
      return "union";
    case TYPE_SPECS_ENUM:
      return "enum";
    default:
      return "(incomplete)";
  }
}

static int specifiers_to_str(const Type *type, char *buf) {
  assert((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_SPECS);
  static const char *FMT_STRS[] = {"%s", "%s %s", "%s %s %s"};
  const char *type_str = decl_specs_get_str(type->decl_specs.flags);
  const char *qual_str = qual_get_str(type->decl_specs.flags);

  if ((type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_STRUCT ||
      (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_UNION ||
      (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_ENUM) {
    return qual_str == NULL ? sprintf(buf, FMT_STRS[1], type_str,
                                      type->decl_specs.tag->record.name)
                            : sprintf(buf, FMT_STRS[2], qual_str, type_str,
                                      type->decl_specs.tag->record.name);
  } else {
    return qual_str == NULL ? sprintf(buf, FMT_STRS[0], type_str)
                            : sprintf(buf, FMT_STRS[1], qual_str, type_str);
  }
}

int type_to_str(const Type *type, char *buf) {
  buf[0] = '\0';
  int ret = 0;
  const Type *current = type;
  while (current != NULL) {
    switch (current->decl_specs.flags & TYPE_KIND_MASK) {
      case TYPE_KIND_SPECS:
        ret += specifiers_to_str(current, buf + ret);
        current = NULL;
        break;
      case TYPE_KIND_FN:
        if (current->function.flags & TYPE_FLAG_VARIADIC)
          ret += sprintf(buf + ret, "variadic function with parameters (");
        else if (GET_PARAM_COUNT(current) == 0)
          ret += sprintf(buf + ret, "function with parameters (void");
        else
          ret += sprintf(buf + ret, "function with parameters (");
        size_t i, param_count = GET_PARAM_COUNT(current);
        for (i = 0; i < param_count; ++i) {
          if (i > 0) ret += sprintf(buf + ret, ", ");
          ret += type_to_str(current->function.next[i], buf + ret);
        }
        ret += sprintf(buf + ret, ") returning ");
        current = current->function.next[param_count];
        break;
      case TYPE_KIND_ARR:
        if (GET_ARRAY_LEN(current) == BCC_DEDUCE_ARR)
          ret += sprintf(buf + ret, "array of unknown size of ");
        else
          ret += sprintf(buf + ret, "array of size %lu of ",
                         GET_ARRAY_LEN(current));
        current = current->array.next;
        break;
      case TYPE_KIND_PTR:
        if (type_is_qualified(current))
          ret += sprintf(buf + ret, "%s pointer to ",
                         qual_get_str(current->pointer.flags));
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
  return (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_VOID;
}

int type_is_integer(const Type *type) {
  return (type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_SPECS &&
         ((type->decl_specs.flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_INT ||
          (type->decl_specs.flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_CHAR);
}

/* pointers are not considered signed or unsigend */
int type_is_signed(const Type *type) {
  return type_is_integer(type) &&
         !(type->decl_specs.flags & TYPE_SPEC_UNSIGNED);
}

int type_is_unsigned(const Type *type) { return !type_is_signed(type); }

int type_is_enum(const Type *type) {
  return (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_ENUM;
}

int type_is_integral(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_arithmetic(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_pointer(const Type *type) {
  return (type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_PTR;
}

int type_is_scalar(const Type *type) {
  return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_array(const Type *type) {
  return (type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_ARR;
}

int type_is_deduced_array(const Type *type) {
  return type_is_array(type) && GET_ARRAY_LEN(type) == BCC_DEDUCE_ARR;
}

int type_is_function(const Type *type) {
  return (type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_FN;
}

int type_is_variadic_function(const Type *type) {
  return type_is_function(type) && (type->function.flags & TYPE_FLAG_VARIADIC);
}

int type_is_void_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_void(type->pointer.next);
}

int type_is_function_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_function(type->pointer.next);
}

int type_is_struct(const Type *type) {
  return (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_STRUCT;
}

int type_is_struct_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_struct(type->pointer.next);
}

int type_is_union(const Type *type) {
  return (type->decl_specs.flags & TYPE_SPEC_MASK) == TYPE_SPECS_UNION;
}

int type_is_union_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_union(type->pointer.next);
}

int type_is_record(const Type *type) {
  return type_is_struct(type) || type_is_union(type);
}

int type_is_record_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_record(type->pointer.next);
}

int type_is_aggregate(const Type *type) {
  return type_is_struct(type) || type_is_union(type) || type_is_array(type);
}

int type_is_char_array(const Type *type) {
  return type_is_array(type) &&
         (type->array.next->decl_specs.flags & TYPE_KIND_MASK) ==
             TYPE_KIND_SPECS &&
         (type->array.next->decl_specs.flags & TYPE_SPEC_LO_MASK) ==
             TYPE_SPEC_CHAR;
}

int type_is_const(const Type *type) {
  return !type_is_array(type) && !type_is_function(type) &&
         (type->decl_specs.flags & TYPE_QUAL_CONST);
}

int type_is_volatile(const Type *type) {
  return !type_is_array(type) && !type_is_function(type) &&
         (type->decl_specs.flags & TYPE_QUAL_VOLATILE);
}

int type_is_qualified(const Type *type) {
  return type_is_const(type) || type_is_volatile(type);
}

int type_is_declarator(const Type *type) {
  return (type->decl_specs.flags & TYPE_KIND_MASK) != TYPE_KIND_SPECS;
}

int type_is_none(const Type *type) {
  return (type->decl_specs.flags & TYPE_SPEC_MASK) == DECL_SPEC_NONE;
}

int type_is_incomplete(const Type *type) {
  if (type_is_void(type) || type_is_none(type)) {
    return 1;
  } else if (type_is_deduced_array(type)) {
    return 1;
  } else if ((type_is_union(type) || type_is_struct(type)) &&
             !type->decl_specs.tag->record.defined) {
    return 1;
  } else {
    return 0;
  }
}

Symbol *type_get_member_name(const Type *type, const char *name) {
  assert(type_is_record(type));
  return scope_get_member(type->decl_specs.tag->record.members, name);
}

Symbol *type_get_member_index(const Type *type, size_t index) {
  assert(type_is_record(type));
  Symbol *member;
  scope_member_at(type->decl_specs.tag->record.members, index, NULL, &member);
  return member;
}

size_t type_get_member_count(const Type *type) {
  assert(type_is_record(type));
  return scope_member_count(type->decl_specs.tag->record.members);
}

size_t type_get_elem_count(const Type *type) {
  assert(type_is_array(type));
  return GET_ARRAY_LEN(type);
}

void type_set_elem_count(Type *type, size_t count) {
  assert(type_is_array(type) && GET_ARRAY_LEN(type) == BCC_DEDUCE_ARR);
  type->array.flags |= count << 2;
}

Type *type_get_param(const Type *type, size_t index) {
  assert(type_is_function(type) && index < GET_PARAM_COUNT(type));
  return type->function.next[index];
}

size_t type_get_param_count(const Type *type) {
  assert(type_is_function(type));
  return GET_PARAM_COUNT(type);
}

static size_t type_decl_specs_alignment(const Type *type) {
  assert((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_SPECS);
  switch (type->decl_specs.flags & TYPE_SPEC_MASK) {
    case TYPE_SPECS_VOID:
      return 0;
    case TYPE_SPECS_CHAR:
      /* fallthrough */
    case TYPE_SPECS_SCHAR:
      /* fallthrough */
    case TYPE_SPECS_UCHAR:
      return X64_ALIGNOF_CHAR;
    case TYPE_SPECS_INT:
      /* fallthrough */
    case TYPE_SPECS_UINT:
      return X64_ALIGNOF_INT;
    case TYPE_SPECS_SHRT:
      /* fallthrough */
    case TYPE_SPECS_USHRT:
      return X64_ALIGNOF_SHRT;
    case TYPE_SPECS_LONG:
      /* fallthrough */
    case TYPE_SPECS_ULONG:
      return X64_ALIGNOF_LONG;
    case TYPE_SPECS_FLT:
      return X64_ALIGNOF_FLT;
    case TYPE_SPECS_DBL:
      return X64_ALIGNOF_DBL;
    case TYPE_SPECS_LDBL:
      return X64_ALIGNOF_LDBL;
    case TYPE_SPECS_STRUCT:
      /* fallthrough */
    case TYPE_SPECS_UNION:
      /* fallthrough */
    case TYPE_SPECS_ENUM:
      return tag_get_alignment(type->decl_specs.tag);
    default:
      abort();
  }
}

size_t type_get_alignment(const Type *type) {
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_ALIGNOF_LONG;
    case TYPE_KIND_ARR:
      return type_get_alignment(type->array.next);
    case TYPE_KIND_SPECS:
      return type_decl_specs_alignment(type);
    default:
      abort();
  }
}

static size_t type_decl_specs_width(const Type *type) {
  assert((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_SPECS);
  switch (type->decl_specs.flags & TYPE_SPEC_MASK) {
    case TYPE_SPECS_VOID:
      return 0;
    case TYPE_SPECS_CHAR:
      /* fallthrough */
    case TYPE_SPECS_SCHAR:
      /* fallthrough */
    case TYPE_SPECS_UCHAR:
      return X64_SIZEOF_CHAR;
    case TYPE_SPECS_INT:
      /* fallthrough */
    case TYPE_SPECS_UINT:
      return X64_SIZEOF_INT;
    case TYPE_SPECS_SHRT:
      /* fallthrough */
    case TYPE_SPECS_USHRT:
      return X64_SIZEOF_SHRT;
    case TYPE_SPECS_LONG:
      /* fallthrough */
    case TYPE_SPECS_ULONG:
      return X64_SIZEOF_LONG;
    case TYPE_SPECS_FLT:
      return X64_SIZEOF_FLT;
    case TYPE_SPECS_DBL:
      return X64_SIZEOF_DBL;
    case TYPE_SPECS_LDBL:
      return X64_SIZEOF_LDBL;
    case TYPE_SPECS_STRUCT:
      /* fallthrough */
    case TYPE_SPECS_UNION:
      /* fallthrough */
    case TYPE_SPECS_ENUM:
      return tag_get_width(type->decl_specs.tag);
    default:
      abort();
  }
}

size_t type_get_width(const Type *type) {
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_SIZEOF_LONG;
    case TYPE_KIND_ARR:
      return GET_ARRAY_LEN(type) * type_get_elem_width(type);
    case TYPE_KIND_SPECS:
      return type_decl_specs_width(type);
    default:
      abort();
  }
}

int type_retrieve_storage_specifier(Type *type) {
  while (type != NULL && type_is_declarator(type))
    type = type_strip_declarator(type);
  assert(type != NULL);

  int ret;
  switch (type->decl_specs.flags & STORE_SPEC_MASK) {
    case DECL_SPEC_NONE:
      ret = TOK_NONE;
      break;
    case STORE_SPEC_AUTO:
      ret = TOK_AUTO;
      break;
    case STORE_SPEC_REGISTER:
      ret = TOK_REGISTER;
      break;
    case STORE_SPEC_STATIC:
      ret = TOK_STATIC;
      break;
    case STORE_SPEC_EXTERN:
      ret = TOK_EXTERN;
      break;
    case STORE_SPEC_TYPEDEF:
      ret = TOK_TYPEDEF;
      break;
    default: /* catch unused values in this range */
      abort();
  }

  /* type system no longer needs storage class; the symbol will store it */
  type->decl_specs.flags &= ~STORE_SPEC_MASK;
  return ret;
}

size_t type_get_elem_width(const Type *type) {
  assert(type_is_array(type) || type_is_pointer(type));
  if ((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_ARR) {
    return type_get_width(type->array.next);
  } else if ((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_PTR) {
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
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_SPECS:
      return type->decl_specs.flags;
    case TYPE_KIND_PTR:
      return type->pointer.flags;
    case TYPE_KIND_ARR:
      /* fallthrough */
    case TYPE_KIND_FN:
      /* fallthrough */
    default:
      return 0;
  }
}

Tag *type_get_tag(const Type *type) {
  assert(type_is_record(type) || type_is_enum(type));
  return type->decl_specs.tag;
}

static int params_compatible(const Type *type1, const Type *type2) {
  assert(type_is_function(type1) && type_is_function(type2));
  if ((GET_PARAM_COUNT(type1) == 0 &&
       (type1->function.flags & TYPE_FLAG_VARIADIC)) ||
      (GET_PARAM_COUNT(type2) == 0 &&
       (type2->function.flags & TYPE_FLAG_VARIADIC)))
    return 1;
  else if (GET_PARAM_COUNT(type1) != GET_PARAM_COUNT(type2))
    return 0;
  else if (((type1->function.flags ^ type2->function.flags) &
            TYPE_FLAG_VARIADIC))
    return 0;
  size_t i, param_count = GET_PARAM_COUNT(type1);
  for (i = 0; i < param_count; ++i)
    if (!types_compatible(type1->function.next[i], type2->function.next[i], 0))
      return 0;
  return 1;
}

int types_compatible(const Type *type1, const Type *type2, int qualified) {
  if ((type1->decl_specs.flags ^ type2->decl_specs.flags) & TYPE_KIND_MASK)
    return 0;

  switch (type1->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_SPECS:
      return !((type1->decl_specs.flags ^ type2->decl_specs.flags) &
               (qualified ? TYPE_SPEC_MASK | TYPE_QUAL_MASK
                          : TYPE_SPEC_MASK)) &&
             type1->decl_specs.tag == type2->decl_specs.tag;
    case TYPE_KIND_PTR:
      return !(qualified && ((type1->pointer.flags ^ type2->pointer.flags) &
                             TYPE_QUAL_MASK)) &&
             types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1);
    case TYPE_KIND_ARR:
      return (GET_ARRAY_LEN(type1) == 0 || GET_ARRAY_LEN(type2) == 0 ||
              GET_ARRAY_LEN(type1) == GET_ARRAY_LEN(type2)) &&
             types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1);
    case TYPE_KIND_FN:
      return types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1) &&
             params_compatible(type1, type2);
    default:
      abort();
  }
}

int types_assignable(const Type *dest, const Type *src, int is_const_zero) {
  if (type_is_arithmetic(dest) && type_is_arithmetic(src)) {
    return 1;
  } else if ((type_is_struct(dest) && type_is_struct(src)) ||
             (type_is_union(dest) && type_is_union(src)) ||
             (type_is_function(dest) && type_is_function(src))) {
    return types_compatible(dest, src, 0);
  } else if (type_is_pointer(dest) && type_is_pointer(src)) {
    if (types_assignable(type_strip_declarator(dest),
                         type_strip_declarator(src), 0)) {
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
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_SPECS:
      /* fallthrough */
    default:
      return NULL;
    case TYPE_KIND_ARR:
      assert(type->array.next != NULL);
      return type->array.next;
    case TYPE_KIND_FN:
      assert(type->function.next != NULL &&
             type->function.next[GET_PARAM_COUNT(type)]);
      return type->function.next[GET_PARAM_COUNT(type)];
    case TYPE_KIND_PTR:
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

Type *type_append(Type *dest, Type *src) {
  assert(dest != NULL);
  Type *current = dest;
  while ((type_is_array(current) && current->array.next != NULL) ||
         (type_is_pointer(current) && current->pointer.next != NULL) ||
         (type_is_function(current) && current->function.next != NULL &&
          current->function.next[GET_PARAM_COUNT(current)] != NULL)) {
    switch (current->decl_specs.flags & TYPE_KIND_MASK) {
      case TYPE_KIND_ARR:
        current = current->array.next;
        break;
      case TYPE_KIND_PTR:
        current = current->pointer.next;
        break;
      case TYPE_KIND_FN:
        current = current->function.next[GET_PARAM_COUNT(current)];
        break;
      default:
        abort();
    }
  }

  switch (current->decl_specs.flags & TYPE_KIND_MASK) {
    default:
      /* fallthrough */
    case TYPE_KIND_SPECS:
      abort();
    case TYPE_KIND_PTR:
      current->pointer.next = src;
      return dest;
    case TYPE_KIND_ARR:
      current->array.next = src;
      return dest;
    case TYPE_KIND_FN:
      current->function.next[GET_PARAM_COUNT(current)] = src;
      return dest;
  }
}

Type *type_detach(Type *type) {
  Type *ret;
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    default:
      /* fallthrough */
    case TYPE_KIND_SPECS:
      abort();
    case TYPE_KIND_FN:
      ret = type->function.next[GET_PARAM_COUNT(type)];
      type->function.next[GET_PARAM_COUNT(type)] = NULL;
      return ret;
    case TYPE_KIND_ARR:
      ret = type->array.next;
      type->array.next = NULL;
      return ret;
    case TYPE_KIND_PTR:
      ret = type->pointer.next;
      type->pointer.next = NULL;
      return ret;
  }
}

Type *type_copy(const Type *type) {
  assert(type != NULL);
  Type anchor;
  anchor.pointer.flags = TYPE_KIND_PTR;
  anchor.pointer.next = NULL;
  Type **type_tail = &anchor.pointer.next;
  const Type *in_current = type;

  while (in_current != NULL) {
    *type_tail = malloc(sizeof(Type));
    **type_tail = *in_current;
    switch (((*type_tail)->decl_specs.flags) & TYPE_KIND_MASK) {
      case TYPE_KIND_SPECS:
        return anchor.pointer.next;
      case TYPE_KIND_ARR:
        type_tail = &((*type_tail)->array.next);
        in_current = in_current->array.next;
        break;
      case TYPE_KIND_PTR:
        type_tail = &((*type_tail)->pointer.next);
        in_current = in_current->pointer.next;
        break;
      case TYPE_KIND_FN:
        /* create new array for parameters */
        (*type_tail)->function.next =
            malloc((GET_PARAM_COUNT(in_current) + 1) *
                   sizeof(*in_current->function.next));
        size_t i, param_count = GET_PARAM_COUNT(in_current);
        /* copy return type pointer as well */
        for (i = 0; i < param_count + 1; ++i)
          (*type_tail)->function.next[i] = in_current->function.next[i];
        type_tail = &((*type_tail)->function.next[param_count]);
        in_current = in_current->function.next[param_count];
        break;
    }
  }

  return anchor.pointer.next;
}

Type *type_common_qualified_pointer(const Type *type1, const Type *type2) {
  Type *new_pointer = malloc(sizeof(Type));
  new_pointer->pointer.flags = type1->pointer.flags | type2->pointer.flags;
  new_pointer->pointer.next = type_is_void_pointer(type1->pointer.next)
                                  ? type1->pointer.next
                                  : type2->pointer.next;
  return new_pointer;
}

static TypeFlag type_flag_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
    case TOK_VOID:
      return TYPE_SPEC_VOID;
    case TOK_CHAR:
      return TYPE_SPEC_CHAR;
    case TOK_INT:
      return TYPE_SPEC_INT;
    case TOK_SHORT:
      return TYPE_SPEC_SHRT;
    case TOK_LONG:
      return TYPE_SPEC_LONG;
    case TOK_SIGNED:
      return TYPE_SPEC_SIGNED;
    case TOK_UNSIGNED:
      return TYPE_SPEC_UNSIGNED;
    case TOK_FLOAT:
      return TYPE_SPEC_FLT;
    case TOK_DOUBLE:
      return TYPE_SPEC_DBL;
    case TOK_CONST:
      return TYPE_QUAL_CONST;
    case TOK_VOLATILE:
      return TYPE_QUAL_VOLATILE;
    case TOK_RESTRICT:
      return DECL_SPEC_NONE; /* C90: no restrict */
    case TOK_TYPEDEF:
      return STORE_SPEC_TYPEDEF;
    case TOK_AUTO:
      return STORE_SPEC_AUTO;
    case TOK_REGISTER:
      return STORE_SPEC_REGISTER;
    case TOK_STATIC:
      return STORE_SPEC_STATIC;
    case TOK_EXTERN:
      return STORE_SPEC_EXTERN;
    case TOK_UNION:
      /* fallthrough */
    case TOK_STRUCT:
      /* fallthrough */
    case TOK_ENUM:
      /* fallthrough */
    case TOK_TYPEDEF_NAME:
      /* fallthrough */
    default:
      abort();
  }
}

static int flag_valid(unsigned int old_flags, TypeFlag new_flag) {
  if (new_flag & TYPE_QUAL_MASK & old_flags) {
    return 0; /* duplicate qualifier */
  } else if (new_flag & STORE_SPEC_MASK & old_flags) {
    return 0; /* multiple storage class specifiers */
  } else {
    /* mask out qualifiers and storage class since they are valid */
    old_flags &= TYPE_SPEC_MASK, new_flag &= TYPE_SPEC_MASK;
    switch (new_flag) {
      case DECL_SPEC_NONE:
        return 1;
      case TYPE_SPEC_FLT:
        /* fallthrough */
      case TYPE_SPEC_VOID:
        return old_flags == DECL_SPEC_NONE;
      case TYPE_SPEC_CHAR:
        return (old_flags & ~TYPE_SPEC_SIGN_MASK) == DECL_SPEC_NONE;
      case TYPE_SPEC_INT:
        return (old_flags & TYPE_SPEC_LO_MASK) == DECL_SPEC_NONE;
      case TYPE_SPEC_SHRT:
        return (old_flags & ~TYPE_SPEC_SIGN_MASK) == DECL_SPEC_NONE ||
               (old_flags & ~TYPE_SPEC_SIGN_MASK) == TYPE_SPEC_INT;
      case TYPE_SPEC_LONG: /* remember to support `long long` later on */
        return (old_flags & ~TYPE_SPEC_SIGN_MASK) == DECL_SPEC_NONE ||
               (old_flags & ~TYPE_SPEC_SIGN_MASK) == TYPE_SPEC_INT ||
               (old_flags & ~TYPE_SPEC_SIGN_MASK) == TYPE_SPEC_DBL;
      case TYPE_SPEC_SIGNED:
        /* fallthrough */
      case TYPE_SPEC_UNSIGNED:
        return !(old_flags & TYPE_SPEC_SIGN_MASK) &&
               ((old_flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_INT ||
                (old_flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_CHAR ||
                (old_flags & TYPE_SPEC_LO_MASK) == DECL_SPEC_NONE);
      case TYPE_SPEC_DBL:
        return old_flags == DECL_SPEC_NONE || old_flags == TYPE_SPEC_LONG;
      case TYPE_SPECS_STRUCT:
        /* fallthrough */
      case TYPE_SPECS_UNION:
        /* fallthrough */
      case TYPE_SPECS_ENUM:
        /* fallthrough */
      default:
        abort(); /* tag type specifiers are added differently */
    }
  }
}

int type_add_decl_spec(Type *type, int tok_kind) {
  assert((type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_PTR ||
         (type->decl_specs.flags & TYPE_KIND_MASK) == TYPE_KIND_SPECS);
  TypeFlag new_flag = type_flag_from_tok_kind(tok_kind);
  switch (type->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_KIND_ARR:
      /* fallthrough */
    default:
      abort();
    case TYPE_KIND_SPECS:
      if (!flag_valid(type->decl_specs.flags, new_flag))
        return -1;
      else
        return type->decl_specs.flags |= new_flag, 0;
    case TYPE_KIND_PTR:
      if ((~TYPE_QUAL_MASK & new_flag))
        return -1; /* pointers can only have type qualifiers */
      else if (type->pointer.flags & new_flag)
        return -1; /* duplicate type qualifier */
      else
        return type->pointer.flags |= new_flag, 0;
  }
}

Type *type_merge(const Type *dest, const Type *src) {
  /* this function is currently called in two places, for tag specifiers and
   * typedef name specifiers. this does mean that it is possible for both the
   * source and destination type to have an arbitrary set of flags, but because
   * the source type must either be a typedef or a non-typedef'd tag, we can
   * make some assumptions and take some shortcuts.
   */
  if ((src->decl_specs.flags & TYPE_SPEC_MASK) != DECL_SPEC_NONE) {
    return NULL;
  } else if ((src->decl_specs.flags & TYPE_QUAL_MASK) != 0 &&
             type_is_declarator(dest) && !type_is_pointer(dest)) {
    /* attempt to qualify function or array type */
    return NULL;
  } else {
    Type *ret = type_copy(dest);
    Type *decl_specs = ret;
    while (decl_specs != NULL && type_is_declarator(decl_specs))
      decl_specs = type_strip_declarator(decl_specs);
    assert(decl_specs != NULL);
    ret->decl_specs.flags |= src->decl_specs.flags & TYPE_QUAL_MASK;
    decl_specs->decl_specs.flags |= src->decl_specs.flags & STORE_SPEC_MASK;
    return ret;
  }
}

int type_normalize(Type *type) {
  type = type_get_decl_specs(type);

  if (type->decl_specs.flags == DECL_SPEC_NONE) {
    return -1;
  } else if ((type->decl_specs.flags & TYPE_KIND_MASK) != TYPE_KIND_SPECS) {
    return 0;
  } else if ((type->decl_specs.flags & TYPE_SPEC_LO_MASK) == TYPE_SPEC_INT) {
    if (!(type->decl_specs.flags & TYPE_SPEC_SIGN_MASK))
      type->decl_specs.flags |= TYPE_SPEC_SIGNED;
    return 0;
  } else if ((type->decl_specs.flags & TYPE_SPEC_LO_MASK) != DECL_SPEC_NONE) {
    return 0;
  } else if (type->decl_specs.flags & TYPE_SPEC_SIGN_MASK) {
    /* only `signed` on `unsigned` specified; make it an `int` */
    type->decl_specs.flags |= TYPE_SPEC_INT;
    return 0;
  } else if ((type->decl_specs.flags & TYPE_SPEC_SIZE_MASK) ||
             (type->decl_specs.flags & STORE_SPEC_MASK) == STORE_SPEC_AUTO) {
    /* only size or `auto` specified; make it a `signed (size) int` */
    type->decl_specs.flags |= TYPE_SPEC_INT | TYPE_SPEC_SIGNED;
    return 0;
  } else {
    return -1;
  }
}

int type_validate(const Type *type) {
  /* allow declarations of type `void`, but only for typedefs */
  if (type_is_void(type) && !(type->decl_specs.flags & STORE_SPEC_TYPEDEF))
    return 1;

  while (type != NULL) {
    switch (type->decl_specs.flags & TYPE_KIND_MASK) {
      case TYPE_KIND_SPECS:
        return 0;
      case TYPE_KIND_PTR:
        type = type_strip_declarator(type);
        continue;
      case TYPE_KIND_ARR:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_incomplete(type) || type_is_function(type))
          return 1;
        continue;
      case TYPE_KIND_FN:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_array(type) || type_is_function(type))
          return 1;
        continue;
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
    Type *pointer_type = type_init_pointer();
    Type *append_type =
        type_is_array(type) ? type_strip_declarator(type) : type;
    return type_append(pointer_type, append_type);
  } else {
    return type;
  }
}

Type *type_arithmetic_conversions(Type *type1, Type *type2) {
  assert(type1 != NULL && type2 != NULL);
  assert(type_is_arithmetic(type1) && type_is_arithmetic(type2));
  unsigned int flags1 = type1->decl_specs.flags & TYPE_SPEC_MASK;
  unsigned int flags2 = type2->decl_specs.flags & TYPE_SPEC_MASK;

  /* TODO(Robert): support LLONG and ULLONG */
  if (flags1 == TYPE_SPECS_LDBL || flags2 == TYPE_SPECS_LDBL) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_SPECS_DBL || flags2 == TYPE_SPECS_DBL) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_SPECS_FLT || flags2 == TYPE_SPECS_FLT) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_SPECS_ULONG) {
    return type1;
  } else if (flags2 == TYPE_SPECS_ULONG) {
    return type2;
  } else if (flags1 == TYPE_SPECS_LONG) {
    return type1;
  } else if (flags2 == TYPE_SPECS_LONG) {
    return type2;
  } else if (flags1 == TYPE_SPECS_UINT) {
    return type1;
  } else if (flags2 == TYPE_SPECS_UINT) {
    return type2;
  } else if (flags1 == TYPE_SPECS_INT) {
    return type1;
  } else if (flags2 == TYPE_SPECS_INT) {
    return type2;
  } else {
    /* no need to check for enum, char, etc. explicitly; all become int */
    return (Type *)TYPE_INT;
  }
}

static int compose_arrays(Type *dest, Type *src) {
  if (!types_compatible(type_strip_declarator(dest), type_strip_declarator(src),
                        1)) {
    /* incompatible element types */
    return -1;
  } else if (GET_ARRAY_LEN(src) == BCC_DEDUCE_ARR) {
    /* source type incomplete; nothing to do */
    return 0;
  } else if (GET_ARRAY_LEN(dest) == BCC_DEDUCE_ARR) {
    /* complete destination type */
    dest->array.flags = src->array.flags;
    return 0;
  } else {
    /* both types complete; error if known lengths don't match */
    return dest->array.flags == src->array.flags ? 0 : -1;
  }
}

static int compose_functions(Type *dest, Type *src) {
  if (!types_compatible(type_strip_declarator(dest), type_strip_declarator(src),
                        1)) {
    /* incompatible return type */
    return -1;
  } else if ((dest->function.flags & TYPE_FLAG_VARIADIC) &&
             GET_PARAM_COUNT(dest) == 0) {
    /* dest unprototyped; use param info from new type; keep return type */
    dest->function.flags = src->function.flags;
    /* swap return types... */
    SWAP(Type *, dest->function.next[0],
         src->function.next[GET_PARAM_COUNT(src)]);
    /* ... and then the `next` field, preserving the return type memory; this
     * ensures that the resulting composite type uses the memory associated with
     * the original return type, which is important because it may have already
     * been used
     */
    SWAP(Type **, dest->function.next, src->function.next);
    /* set src flags to that of an unprototyped function (variadic, 0 params) */
    src->function.flags = TYPE_KIND_FN | TYPE_FLAG_VARIADIC;
    return 0;
  } else if ((src->function.flags & TYPE_FLAG_VARIADIC) &&
             GET_PARAM_COUNT(src) == 0) {
    /* nothing do do; move on */
    return 0;
  } else if (dest->function.flags ^ src->function.flags) {
    /* different param count or one function is variadic and the other is not */
    return -1;
  } else {
    size_t i, param_count = GET_PARAM_COUNT(dest);
    for (i = 0; i < param_count; ++i)
      if (type_compose(dest->function.next[i], src->function.next[i], 0))
        return -1;
    return 0;
  }
}

int type_compose(Type *dest, Type *src, int qualified) {
  if ((dest->decl_specs.flags ^ src->decl_specs.flags) & TYPE_KIND_MASK)
    return -1;
  switch (dest->decl_specs.flags & TYPE_KIND_MASK) {
    case TYPE_KIND_SPECS:
      /* fallthrough */
    case TYPE_KIND_PTR:
      return types_compatible(dest, src, qualified) ? 0 : -1;
    case TYPE_KIND_ARR:
      return compose_arrays(dest, src);
    case TYPE_KIND_FN:
      return compose_functions(dest, src);
    default:
      abort();
  }
}
