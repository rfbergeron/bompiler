#include "bcc_types.h"

#include "assert.h"
#include "scope.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define GET_ARRAY_LEN(type) (((type)->array.flags >> 1) + 1ul)
#define GET_PARAM_COUNT(type) ((type->function.flags) >> 4)
#define SWAP(type, x, y) \
  do {                   \
    type temp = (x);     \
    (x) = (y);           \
    (y) = temp;          \
  } while (0)
#define BCC_ARRAY_MAX_LEN 0x8000000000000000
#define BCC_FN_MAX_PARAMS 0x0fffffffffffffff

typedef enum type_flag {
  DECL_SPEC_NONE = 0,             /* 0x0000 no declaration specifiers */
  TYPE_FLAG_ARR = 1,              /* 0x0001 array */
  TYPE_MASK_KIND = 0x0007,        /*        type kind: 2-bit int, flag */
  TYPE_KIND_OTHER = 0,            /* 0x0000 */
  TYPE_KIND_ARTH = 1 << 1,        /* 0x0002 */
  TYPE_KIND_PTR = 2 << 1,         /* 0x0004 */
  TYPE_KIND_FN = 3 << 1,          /* 0x0006 */
  TYPE_MASK_EXTRA = 0x0008,       /* 0x0008 extra type kind flag */
  TYPE_FLAG_VARIADIC = 1 << 3,    /* 0x0008 variadic function */
  TYPE_FLAG_ARR_SPECIAL = 1 << 3, /* 0x0008 other array type */
  TYPE_MASK_OTHER = 0x01f0,       /*        other specifiers: 5 bits */
                                  /* 0x0000 no type specifiers */
  TYPE_OTHER_VOID = 1 << 4,       /* 0x0010 */
  TYPE_OTHER_STRUCT = 2 << 4,     /* 0x0020 */
  TYPE_OTHER_UNION = 3 << 4,      /* 0x0030 */
  TYPE_OTHER_ENUM = 4 << 4,       /* 0x0040 */
                                  /* 0x0050 - 0x01f0 reserved */
  TYPE_MASK_ARTH = 0x0018,        /*        arithmetic specifiers: 2 bit int */
  TYPE_ARTH_FLT = 0 << 3,         /* 0x0000 */
  TYPE_ARTH_DBL = 1 << 3,         /* 0x0008 */
  TYPE_ARTH_CHAR = 2 << 3,        /* 0x0010 */
  TYPE_ARTH_INT = 3 << 3,         /* 0x0018 */
  TYPE_MASK_SIGN = 0x0060,        /*        signedness specifiers: 2 bits */
  TYPE_SIGN_SIGNED = 1 << 5,      /* 0x0020 */
  TYPE_SIGN_UNSIGNED = 1 << 6,    /* 0x0040 */
  TYPE_MASK_SIZE = 0x0180,        /*        size specifiers: 2 bit int */
                                  /* 0x0000 no type specifiers */
  TYPE_SIZE_SHRT = 1 << 7,        /* 0x0080 */
  TYPE_SIZE_LONG = 2 << 7,        /* 0x0100 */
                                  /* 0x0180 reserved for long long */
  TYPE_MASK_QUAL = 0x1e00,        /*        type qualifiers: 4-bit flagset */
                                  /* 0x0000 no type qualifiers */
  TYPE_QUAL_CONST = 1 << 9,       /* 0x0200 */
  TYPE_QUAL_VOLATILE = 1 << 10,   /* 0x0400 */
                                  /* 0x0800 reserved for restrict */
                                  /* 0x1000 reserved for atomic */
  TYPE_MASK_STORE = 0xe000,       /*        storage class: 3-bit integer */
                                  /* 0x0000 no explicit storage class */
  TYPE_STORE_REGISTER = 1 << 13,  /* 0x2000 */
  TYPE_STORE_AUTO = 2 << 13,      /* 0x4000 */
  TYPE_STORE_STATIC = 3 << 13,    /* 0x6000 */
  TYPE_STORE_EXTERN = 4 << 13,    /* 0x8000 */
  TYPE_STORE_TYPEDEF = 5 << 13,   /* 0xa000 */
                                  /* 0xc000 invalid 7th value */
                                  /* 0xe000 invalid 8th value */
  /* valid representations of individual type specifiers */
  TYPE_SPEC_FLT = TYPE_KIND_ARTH | TYPE_ARTH_FLT,
  TYPE_SPEC_DBL = TYPE_KIND_ARTH | TYPE_ARTH_DBL,
  TYPE_SPEC_CHAR = TYPE_KIND_ARTH | TYPE_ARTH_CHAR,
  TYPE_SPEC_INT = TYPE_KIND_ARTH | TYPE_ARTH_INT,
  TYPE_SPEC_SIGNED = TYPE_KIND_ARTH | TYPE_SIGN_SIGNED,
  TYPE_SPEC_UNSIGNED = TYPE_KIND_ARTH | TYPE_SIGN_UNSIGNED,
  TYPE_SPEC_SHRT = TYPE_KIND_ARTH | TYPE_SIZE_SHRT,
  TYPE_SPEC_LONG = TYPE_KIND_ARTH | TYPE_SIZE_LONG,
  /* "canonical" bit representations of types */
  TYPE_MASK_REPR = TYPE_MASK_KIND | TYPE_MASK_OTHER | TYPE_MASK_EXTRA,
  TYPE_REPR_CHAR = TYPE_SPEC_CHAR,
  TYPE_REPR_SCHAR = TYPE_SPEC_CHAR | TYPE_SPEC_SIGNED,
  TYPE_REPR_UCHAR = TYPE_SPEC_CHAR | TYPE_SPEC_UNSIGNED,
  TYPE_REPR_INT = TYPE_SPEC_INT | TYPE_SPEC_SIGNED,
  TYPE_REPR_UINT = TYPE_SPEC_INT | TYPE_SPEC_UNSIGNED,
  TYPE_REPR_SHRT = TYPE_REPR_INT | TYPE_SPEC_SHRT,
  TYPE_REPR_USHRT = TYPE_REPR_UINT | TYPE_SPEC_SHRT,
  TYPE_REPR_LONG = TYPE_REPR_INT | TYPE_SPEC_LONG,
  TYPE_REPR_ULONG = TYPE_REPR_UINT | TYPE_SPEC_LONG,
  TYPE_REPR_FLT = TYPE_SPEC_FLT,
  TYPE_REPR_DBL = TYPE_SPEC_DBL,
  TYPE_REPR_LDBL = TYPE_SPEC_DBL | TYPE_SPEC_LONG,
  TYPE_REPR_VOID = TYPE_KIND_OTHER | TYPE_OTHER_VOID,
  TYPE_REPR_STRUCT = TYPE_KIND_OTHER | TYPE_OTHER_STRUCT,
  TYPE_REPR_UNION = TYPE_KIND_OTHER | TYPE_OTHER_UNION,
  TYPE_REPR_ENUM = TYPE_KIND_OTHER | TYPE_OTHER_ENUM,
  TYPE_REPR_INC_ARR = TYPE_KIND_OTHER | TYPE_FLAG_ARR_SPECIAL,
  /* types with representations that need special handling */
  TYPE_MASK_EXTRA_KIND = TYPE_MASK_KIND | TYPE_MASK_EXTRA,
  TYPE_EXTRA_ARR = TYPE_KIND_OTHER | TYPE_FLAG_ARR_SPECIAL,
  TYPE_EXTRA_VAR_FN = TYPE_KIND_FN | TYPE_FLAG_VARIADIC,
  TYPE_EXTRA_ARTH = TYPE_KIND_ARTH | TYPE_MASK_EXTRA
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
                                   X64_SIZEOF_LONG * PARAM_REG_COUNT,
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
    {TYPE_REPR_STRUCT, &TAG_VA_SPILL_REGION}};

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
  TYPE_VOID_INTERNAL.decl_specs.flags = TYPE_REPR_VOID;
  TYPE_POINTER_INTERNAL.pointer.flags = TYPE_KIND_PTR;
  TYPE_POINTER_INTERNAL.pointer.next = &TYPE_VOID_INTERNAL;
  TYPE_CHAR_INTERNAL.decl_specs.flags = TYPE_REPR_CHAR;
  TYPE_INT_INTERNAL.decl_specs.flags = TYPE_REPR_INT;
  TYPE_LONG_INTERNAL.decl_specs.flags = TYPE_REPR_LONG;
  TYPE_UNSIGNED_INT_INTERNAL.decl_specs.flags = TYPE_REPR_UINT;
  TYPE_UNSIGNED_LONG_INTERNAL.decl_specs.flags = TYPE_REPR_ULONG;
  /* TYPE_NONE_INTERNAL already zero-filled; no need to initialize */

  /* sanity checks */
  assert(!(TYPE_MASK_KIND & TYPE_MASK_EXTRA & TYPE_MASK_OTHER & TYPE_MASK_QUAL &
           TYPE_MASK_STORE & TYPE_FLAG_ARR));
  assert(!(TYPE_MASK_KIND & TYPE_MASK_ARTH & TYPE_MASK_SIZE & TYPE_MASK_SIGN &
           TYPE_MASK_QUAL & TYPE_MASK_STORE & TYPE_FLAG_ARR));

  assert((TYPE_MASK_SIGN | TYPE_SIGN_UNSIGNED) == TYPE_MASK_SIGN);
  assert((TYPE_MASK_SIGN | TYPE_SIGN_SIGNED) == TYPE_MASK_SIGN);
  assert((TYPE_MASK_SIZE | TYPE_SIZE_LONG) == TYPE_MASK_SIZE);
  assert((TYPE_MASK_SIZE | TYPE_SIZE_SHRT) == TYPE_MASK_SIZE);
  assert((TYPE_MASK_ARTH | TYPE_ARTH_FLT) == TYPE_MASK_ARTH);
  assert((TYPE_MASK_ARTH | TYPE_ARTH_DBL) == TYPE_MASK_ARTH);
  assert((TYPE_MASK_ARTH | TYPE_ARTH_CHAR) == TYPE_MASK_ARTH);
  assert((TYPE_MASK_ARTH | TYPE_ARTH_INT) == TYPE_MASK_ARTH);
  assert((TYPE_MASK_STORE | TYPE_STORE_TYPEDEF) == TYPE_MASK_STORE);
}

Type *type_init_basic(void) { return calloc(1, sizeof(Type)); }

Type *type_init_tag(Tag *tag) {
  static const TypeFlag TAG_TYPESPEC_MAP[] = {TYPE_REPR_STRUCT, TYPE_REPR_UNION,
                                              TYPE_REPR_ENUM};
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
  if (length == 0) /* incomplete array */
    type->array.flags = TYPE_KIND_OTHER | TYPE_FLAG_ARR_SPECIAL;
  else
    type->array.flags = TYPE_FLAG_ARR | ((length - 1) << 1);
  type->array.next = NULL;
  return type;
}

Type *type_init_function(size_t parameters_size, Type **parameters,
                         int variadic) {
  assert(parameters_size <= BCC_FN_MAX_PARAMS);
  Type *type = malloc(sizeof(Type));
  type->function.flags = TYPE_KIND_FN | parameters_size << 4;
  if (variadic) type->function.flags |= TYPE_FLAG_VARIADIC;
  type->function.next = parameters;
  type->function.next[parameters_size] = NULL;
  return type;
}

void type_destroy(Type *type) {
  while (type != NULL) {
    Type *current = type;
    switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
        /* fallthrough */
      case TYPE_EXTRA_ARTH:
        free(current);
        return;
      case TYPE_KIND_PTR:
        type = type->pointer.next;
        free(current);
        break;
      case TYPE_KIND_FN:
        /* fallthrough */
      case TYPE_EXTRA_VAR_FN:
        type = type->function.next[GET_PARAM_COUNT(type)];
        free(current->function.next);
        free(current);
        break;
      default:
        assert(type->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_EXTRA_ARR:
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

static const char *qual_get_str(unsigned long flags) {
  switch (flags & TYPE_MASK_QUAL) {
    case TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE:
      return "const volatile";
    case TYPE_QUAL_CONST:
      return "const";
    case TYPE_QUAL_VOLATILE:
      return "volatile";
    default:
      return "";
  }
}

static const char *sign_get_str(unsigned long flags) {
  static const char *SIGN_DUMP_MAP[] = {"", "signed", "unsigned",
                                        "signed unsigned (invalid)"};
  return SIGN_DUMP_MAP[(flags & TYPE_MASK_SIGN) >> 5];
}

static const char *arth_get_str(unsigned long flags) {
  static const char *ARTH_DUMP_MAP[] = {"float/(none)", "double", "char",
                                        "int"};
  return ARTH_DUMP_MAP[(flags & TYPE_MASK_ARTH) >> 3];
}

static const char *store_get_str(unsigned long flags) {
  static const char *STORAGE_DUMP_MAP[] = {"",          "register", "auto",
                                           "static",    "extern",   "typedef",
                                           "(invalid)", "(invalid)"};
  return STORAGE_DUMP_MAP[(flags & TYPE_MASK_STORE) >> 13];
}

static const char *size_get_str(unsigned long flags) {
  static const char *SIZE_DUMP_MAP[] = {"", "short", "long", "long long"};
  return SIZE_DUMP_MAP[(flags & TYPE_MASK_SIZE) >> 7];
}

static const char *other_get_str(unsigned long flags) {
  switch (flags & TYPE_MASK_OTHER) {
    case DECL_SPEC_NONE:
      return "(none)";
    case TYPE_OTHER_VOID:
      return "void";
    case TYPE_OTHER_STRUCT:
      return "struct";
    case TYPE_OTHER_UNION:
      return "union";
    case TYPE_OTHER_ENUM:
      return "enum";
    default:
      return "(invalid)";
  }
}

void type_dump_specs(unsigned long flags) {
  static const char *OTHER_DUMP_FMT =
      "Type kind is other.\n"
      "Type qualifiers: %s\n"
      "Storage class specifier: %s\n"
      "Type specifier(s): %s\n"
      "Raw bit value: %lx\n";
  static const char *ARTH_DUMP_FMT =
      "Type kind is arithmetic.\n"
      "Type qualifiers: %s\n"
      "Storage class specifier: %s\n"
      "Sign specifier(s): %s\n"
      "Size specifier(s): %s\n"
      "Type specifier(s): %s\n"
      "Raw bit value: %lx\n";
  static const char *PTR_DUMP_FMT =
      "Type qualifiers: %s\n"
      "Storage class specifier: %s\n"
      "Raw bit value: %lx\n";
  static const char *ARR_DUMP_FMT =
      "Type kind is array.\n"
      "Element count: %lu\n"
      "Raw bit value: %lx\n";
  static const char *FN_DUMP_FMT =
      "Type kind is function.\n"
      "Variadic: %s\n"
      "Parameter count: %s\n"
      "Raw bit value: %lx\n";
  switch (flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_OTHER:
      (void)fprintf(stderr, OTHER_DUMP_FMT, qual_get_str(flags),
                    store_get_str(flags), other_get_str(flags), flags);
      break;
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      (void)fprintf(stderr, ARTH_DUMP_FMT, qual_get_str(flags),
                    store_get_str(flags), sign_get_str(flags),
                    size_get_str(flags), arth_get_str(flags), flags);
      break;
    case TYPE_KIND_PTR:
      (void)fprintf(stderr, PTR_DUMP_FMT, qual_get_str(flags),
                    store_get_str(flags), flags);
      break;
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      (void)fprintf(stderr, FN_DUMP_FMT,
                    (flags & TYPE_FLAG_VARIADIC) ? "yes" : "no", flags >> 4,
                    flags);
      break;
    case TYPE_EXTRA_ARR:
      (void)fprintf(stderr,
                    "Type kind is incomplete array.\nRaw bit value: %lx\n",
                    flags);
      break;
    default:
      if (!(flags & TYPE_FLAG_ARR)) {
        (void)fprintf(stderr, "Type kind is invalid.\nRaw bit value: %lx\n",
                      flags);
      } else {
        (void)fprintf(stderr, ARR_DUMP_FMT, (flags >> 1) + 1, flags);
      }
      break;
  }
}

/* TODO(Robert): this function should be able to print out types that haven't
 * been normalized yet, for diagnostic purposes
 */
static const char *decl_specs_get_str(unsigned int flags) {
  switch (flags & TYPE_MASK_REPR) {
    case DECL_SPEC_NONE:
      return "(none)";
    case TYPE_REPR_VOID:
      return "void";
    case TYPE_REPR_CHAR:
      return "char";
    case TYPE_REPR_SCHAR:
      return "signed char";
    case TYPE_REPR_UCHAR:
      return "unsigned char";
    case TYPE_REPR_INT:
      return "signed int";
    case TYPE_REPR_UINT:
      return "unsigned int";
    case TYPE_REPR_SHRT:
      return "signed short";
    case TYPE_REPR_USHRT:
      return "unsigned short";
    case TYPE_REPR_LONG:
      return "signed long";
    case TYPE_REPR_ULONG:
      return "unsigned long";
    case TYPE_REPR_FLT:
      return "float";
    case TYPE_REPR_DBL:
      return "double";
    case TYPE_REPR_LDBL:
      return "long double";
    case TYPE_REPR_STRUCT:
      return "struct";
    case TYPE_REPR_UNION:
      return "union";
    case TYPE_REPR_ENUM:
      return "enum";
    default:
      return "(incomplete)";
  }
}

static int specifiers_to_str(const Type *type, char *buf) {
  assert(!type_is_declarator(type));
  static const char *FMT_STRS[] = {"%s", "%s %s", "%s %s %s"};
  const char *type_str = decl_specs_get_str(type->decl_specs.flags);
  const char *qual_str = qual_get_str(type->decl_specs.flags);

  if (type_is_record(type) || type_is_enum(type)) {
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
    switch (current->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
      case TYPE_KIND_ARTH:
        /* fallthrough */
      case TYPE_EXTRA_ARTH:
        /* fallthrough */
      case TYPE_KIND_OTHER:
        ret += specifiers_to_str(current, buf + ret);
        current = NULL;
        break;
      case TYPE_KIND_FN:
        /* fallthrough */
      case TYPE_EXTRA_VAR_FN:
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
      case TYPE_KIND_PTR:
        if (type_is_qualified(current))
          ret += sprintf(buf + ret, "%s pointer to ",
                         qual_get_str(current->pointer.flags));
        else
          ret += sprintf(buf + ret, "pointer to ");
        current = current->pointer.next;
        break;
      case TYPE_EXTRA_ARR:
        ret += sprintf(buf + ret, "incomplete array of ");
        current = current->array.next;
        break;
      default:
        assert(current->decl_specs.flags & TYPE_FLAG_ARR);
        ret +=
            sprintf(buf + ret, "array of size %lu of ", GET_ARRAY_LEN(current));
        current = current->array.next;
        break;
    }
  }
  return ret;
}

int type_is_void(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_VOID;
}

/* TODO(Robert): maybe we only need one integer predicate? `type_is_integral`
 * is used everywhere else outside this file. would just have to make sure sign
 * check only happens for arithmetic (integer) types
 */
int type_is_integer(const Type *type) {
  return ((type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_ARTH) &&
         ((type->decl_specs.flags & TYPE_MASK_ARTH) == TYPE_ARTH_INT ||
          (type->decl_specs.flags & TYPE_MASK_ARTH) == TYPE_ARTH_CHAR);
}

/* TODO(Robert): how to handle char without `signed` or `unsigned`? */
int type_is_signed(const Type *type) {
  /* pointers are not considered signed or unsigend */
  return type_is_integer(type) &&
         !(type->decl_specs.flags & TYPE_SIGN_UNSIGNED);
}

int type_is_unsigned(const Type *type) {
  /* pointers are not considered signed or unsigend */
  return type_is_integer(type) && (type->decl_specs.flags & TYPE_SIGN_UNSIGNED);
}

int type_is_enum(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_ENUM;
}

int type_is_integral(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_arithmetic(const Type *type) { return type_is_integral(type); }

int type_is_pointer(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_PTR;
}

int type_is_scalar(const Type *type) {
  return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_array(const Type *type) {
  return !!(type->decl_specs.flags & TYPE_FLAG_ARR) ||
         type_is_deduced_array(type);
}

int type_is_deduced_array(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_INC_ARR;
}

int type_is_function(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_FN;
}

int type_is_variadic_function(const Type *type) {
  return (type->decl_specs.flags & (TYPE_MASK_KIND | TYPE_MASK_EXTRA)) ==
         TYPE_EXTRA_VAR_FN;
}

int type_is_void_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_void(type->pointer.next);
}

int type_is_function_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_function(type->pointer.next);
}

int type_is_struct(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_STRUCT;
}

int type_is_struct_pointer(const Type *type) {
  return type_is_pointer(type) && type_is_struct(type->pointer.next);
}

int type_is_union(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_UNION;
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

/* TODO(Robert): should this return true for signed/unsigned char? */
int type_is_char_array(const Type *type) {
  return type_is_array(type) &&
         (type->array.next->decl_specs.flags & TYPE_MASK_KIND) ==
             TYPE_KIND_ARTH &&
         (type->array.next->decl_specs.flags & TYPE_MASK_ARTH) ==
             TYPE_ARTH_CHAR;
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
  return type_is_pointer(type) || type_is_function(type) || type_is_array(type);
}

int type_is_none(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == DECL_SPEC_NONE;
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
  assert(type_is_deduced_array(type));
  type->array.flags = TYPE_FLAG_ARR | ((count - 1) << 1);
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
  assert(!type_is_declarator(type));
  switch (type->decl_specs.flags & TYPE_MASK_REPR) {
    case TYPE_REPR_VOID:
      return 0;
    case TYPE_REPR_CHAR:
      /* fallthrough */
    case TYPE_REPR_SCHAR:
      /* fallthrough */
    case TYPE_REPR_UCHAR:
      return X64_ALIGNOF_CHAR;
    case TYPE_REPR_INT:
      /* fallthrough */
    case TYPE_REPR_UINT:
      return X64_ALIGNOF_INT;
    case TYPE_REPR_SHRT:
      /* fallthrough */
    case TYPE_REPR_USHRT:
      return X64_ALIGNOF_SHRT;
    case TYPE_REPR_LONG:
      /* fallthrough */
    case TYPE_REPR_ULONG:
      return X64_ALIGNOF_LONG;
    case TYPE_REPR_FLT:
      return X64_ALIGNOF_FLT;
    case TYPE_REPR_DBL:
      return X64_ALIGNOF_DBL;
    case TYPE_REPR_LDBL:
      return X64_ALIGNOF_LDBL;
    case TYPE_REPR_STRUCT:
      /* fallthrough */
    case TYPE_REPR_UNION:
      /* fallthrough */
    case TYPE_REPR_ENUM:
      return tag_get_alignment(type->decl_specs.tag);
    default:
      abort();
  }
}

size_t type_get_alignment(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_ALIGNOF_LONG;
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type_decl_specs_alignment(type);
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      return type_get_alignment(type->array.next);
      abort();
  }
}

static size_t type_decl_specs_width(const Type *type) {
  assert(!type_is_declarator(type));
  switch (type->decl_specs.flags & TYPE_MASK_REPR) {
    case TYPE_REPR_VOID:
      return 0;
    case TYPE_REPR_CHAR:
      /* fallthrough */
    case TYPE_REPR_SCHAR:
      /* fallthrough */
    case TYPE_REPR_UCHAR:
      return X64_SIZEOF_CHAR;
    case TYPE_REPR_INT:
      /* fallthrough */
    case TYPE_REPR_UINT:
      return X64_SIZEOF_INT;
    case TYPE_REPR_SHRT:
      /* fallthrough */
    case TYPE_REPR_USHRT:
      return X64_SIZEOF_SHRT;
    case TYPE_REPR_LONG:
      /* fallthrough */
    case TYPE_REPR_ULONG:
      return X64_SIZEOF_LONG;
    case TYPE_REPR_FLT:
      return X64_SIZEOF_FLT;
    case TYPE_REPR_DBL:
      return X64_SIZEOF_DBL;
    case TYPE_REPR_LDBL:
      return X64_SIZEOF_LDBL;
    case TYPE_REPR_STRUCT:
      /* fallthrough */
    case TYPE_REPR_UNION:
      /* fallthrough */
    case TYPE_REPR_ENUM:
      return tag_get_width(type->decl_specs.tag);
    default:
      abort();
  }
}

/* TODO(Robert): sizeof not allowed on incomplete arrays. what to do. */
size_t type_get_width(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_EXTRA_ARR:
      /* fallthrough */
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_SIZEOF_LONG;
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type_decl_specs_width(type);
    default: /* should be an array */
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      return GET_ARRAY_LEN(type) * type_get_elem_width(type);
      abort();
  }
}

int type_retrieve_storage_specifier(Type *type) {
  while (type != NULL && type_is_declarator(type))
    type = type_strip_declarator(type);
  assert(type != NULL);

  unsigned long storage = type->decl_specs.flags & TYPE_MASK_STORE;
  /* type system no longer needs storage class; the symbol will store it */
  type->decl_specs.flags &= ~TYPE_MASK_STORE;

  switch (storage) {
    case DECL_SPEC_NONE:
      return TOK_NONE;
    case TYPE_STORE_AUTO:
      return TOK_AUTO;
    case TYPE_STORE_REGISTER:
      return TOK_REGISTER;
    case TYPE_STORE_STATIC:
      return TOK_STATIC;
    case TYPE_STORE_EXTERN:
      return TOK_EXTERN;
    case TYPE_STORE_TYPEDEF:
      return TOK_TYPEDEF;
    default: /* catch unused values in this range */
      abort();
  }
}

size_t type_get_elem_width(const Type *type) {
  if (type_is_pointer(type)) {
    return type_get_width(type->pointer.next);
  } else {
    assert(type_is_array(type));
    return type_get_width(type->array.next);
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
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type->decl_specs.flags;
    case TYPE_KIND_PTR:
      return type->pointer.flags;
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      /* fallthrough */
    default: /* includes arrays */
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
  switch (type1->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_OTHER:
      /* fallthrough */
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      /* fallthrough */
      return !((type1->decl_specs.flags ^ type2->decl_specs.flags) &
               (qualified ? TYPE_MASK_REPR | TYPE_MASK_QUAL
                          : TYPE_MASK_REPR)) &&
             type1->decl_specs.tag == type2->decl_specs.tag;
    case TYPE_KIND_PTR:
      return !(qualified && ((type1->pointer.flags ^ type2->pointer.flags) &
                             TYPE_MASK_QUAL)) &&
             types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1);
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      return types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1) &&
             params_compatible(type1, type2);
    default:
      assert(type1->decl_specs.flags & TYPE_FLAG_ARR);
      if (!type_is_deduced_array(type2) ||
          GET_ARRAY_LEN(type1) != GET_ARRAY_LEN(type2))
        return 0;
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      return types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1);
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
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      assert(type->array.next != NULL);
      return type->array.next;
    case TYPE_KIND_FN:
      /* fallthough */
    case TYPE_EXTRA_VAR_FN:
      assert(type->function.next != NULL &&
             type->function.next[GET_PARAM_COUNT(type)] != NULL);
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
    switch (current->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
      default:
        assert(current->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_EXTRA_ARR:
        current = current->array.next;
        break;
      case TYPE_KIND_PTR:
        current = current->pointer.next;
        break;
      case TYPE_KIND_FN:
        /* fallthrough */
      case TYPE_EXTRA_VAR_FN:
        current = current->function.next[GET_PARAM_COUNT(current)];
        break;
    }
  }

  switch (current->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    default:
      assert(current->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      current->array.next = src;
      return dest;
    case TYPE_KIND_PTR:
      current->pointer.next = src;
      return dest;
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      current->function.next[GET_PARAM_COUNT(current)] = src;
      return dest;
  }
}

Type *type_detach(Type *type) {
  assert(type_is_declarator(type));
  Type *ret;
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      ret = type->array.next;
      type->array.next = NULL;
      return ret;
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      ret = type->function.next[GET_PARAM_COUNT(type)];
      type->function.next[GET_PARAM_COUNT(type)] = NULL;
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
    switch (((*type_tail)->decl_specs.flags) & TYPE_MASK_EXTRA_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
        /* fallthrough */
      case TYPE_EXTRA_ARTH:
        return anchor.pointer.next;
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
      default:
        assert(((*type_tail)->decl_specs.flags) & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_EXTRA_ARR:
        type_tail = &((*type_tail)->array.next);
        in_current = in_current->array.next;
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
      return TYPE_REPR_VOID;
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
      return TYPE_STORE_TYPEDEF;
    case TOK_AUTO:
      return TYPE_STORE_AUTO;
    case TOK_REGISTER:
      return TYPE_STORE_REGISTER;
    case TOK_STATIC:
      return TYPE_STORE_STATIC;
    case TOK_EXTERN:
      return TYPE_STORE_EXTERN;
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

static int flag_valid(unsigned long old_flags, unsigned long new_flag) {
  if (new_flag & TYPE_MASK_QUAL & old_flags) {
    return 0; /* duplicate qualifier */
  } else if (new_flag & TYPE_MASK_STORE & old_flags) {
    return 0; /* multiple storage class specifiers */
  }

  /* qualifiers and storage class specifiers good; discard them */
  old_flags &= TYPE_MASK_REPR, new_flag &= TYPE_MASK_REPR;
  /* get JUST these two bits of the type kind to make things simpler */
  unsigned long old_kind = old_flags & (TYPE_MASK_KIND ^ TYPE_FLAG_ARR);

  if (old_flags == DECL_SPEC_NONE || new_flag == DECL_SPEC_NONE) {
    /* no type specifiers; early return */
    return 1;
  } else if (old_kind != TYPE_KIND_ARTH || old_flags == TYPE_SPEC_FLT) {
    /* you can only combine arithmetic type specifiers, unless it's `float` */
    return 0;
  }

  unsigned long old_arth = old_flags & TYPE_MASK_ARTH;
  unsigned long old_size = old_flags & TYPE_MASK_SIZE;
  unsigned long old_sign = old_flags & TYPE_MASK_SIGN;
  switch (new_flag) {
    case TYPE_SPEC_FLT:
      /* fallthrough */
    case TYPE_REPR_VOID:
      /* no other type specifiers allowed */
      return 0;
    case TYPE_SPEC_CHAR:
      return old_flags == TYPE_SPEC_SIGNED || old_flags == TYPE_SPEC_UNSIGNED;
    case TYPE_SPEC_INT:
      return old_kind == TYPE_KIND_ARTH &&
             (old_flags & TYPE_MASK_ARTH) == DECL_SPEC_NONE;
    case TYPE_SPEC_SHRT:
      return old_size == DECL_SPEC_NONE && old_arth != TYPE_ARTH_DBL &&
             old_arth != TYPE_ARTH_CHAR;
    case TYPE_SPEC_LONG: /* TODO(Robert): support `long long` in future */
      return old_size == DECL_SPEC_NONE && old_arth != TYPE_ARTH_CHAR;
    case TYPE_SPEC_SIGNED:
      /* fallthrough */
    case TYPE_SPEC_UNSIGNED:
      return old_arth != TYPE_ARTH_DBL && old_sign == DECL_SPEC_NONE;
    case TYPE_SPEC_DBL:
      return old_flags == TYPE_SPEC_LONG;
    case TYPE_REPR_STRUCT:
      /* fallthrough */
    case TYPE_REPR_UNION:
      /* fallthrough */
    case TYPE_REPR_ENUM:
      /* fallthrough */
    default:
      abort(); /* tag type specifiers are added differently */
  }
}

int type_add_decl_spec(Type *type, int tok_kind) {
  assert(!type_is_declarator(type) || type_is_pointer(type));
  TypeFlag new_flag = type_flag_from_tok_kind(tok_kind);
  switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_OTHER:
      /* fallthrough */
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      if (!flag_valid(type->decl_specs.flags, new_flag))
        return -1;
      else
        return type->decl_specs.flags |= new_flag, 0;
    case TYPE_KIND_PTR:
      if ((~TYPE_MASK_QUAL & new_flag))
        return -1; /* pointers can only have type qualifiers */
      else if (type->pointer.flags & new_flag)
        return -1; /* duplicate type qualifier */
      else
        return type->pointer.flags |= new_flag, 0;
    default:
      abort();
  }
}

Type *type_merge(const Type *dest, const Type *src) {
  /* this function is currently called in two places, for tag specifiers and
   * typedef name specifiers. this does mean that it is possible for both the
   * source and destination type to have an arbitrary set of flags, but because
   * the source type must either be a typedef or a non-typedef'd tag, we can
   * make some assumptions and take some shortcuts.
   */
  if ((src->decl_specs.flags & TYPE_MASK_REPR) != DECL_SPEC_NONE) {
    return NULL;
  } else if ((src->decl_specs.flags & TYPE_MASK_QUAL) != 0 &&
             type_is_declarator(dest) && !type_is_pointer(dest)) {
    /* attempt to qualify function or array type */
    return NULL;
  } else {
    Type *ret = type_copy(dest);
    Type *decl_specs = ret;
    while (decl_specs != NULL && type_is_declarator(decl_specs))
      decl_specs = type_strip_declarator(decl_specs);
    assert(decl_specs != NULL);
    ret->decl_specs.flags |= src->decl_specs.flags & TYPE_MASK_QUAL;
    decl_specs->decl_specs.flags |= src->decl_specs.flags & TYPE_MASK_STORE;
    return ret;
  }
}

int type_normalize(Type *type) {
  type = type_get_decl_specs(type);

  if (type->decl_specs.flags == DECL_SPEC_NONE) {
    return -1;
  } else if ((type->decl_specs.flags & TYPE_MASK_KIND) != TYPE_KIND_ARTH) {
    /* not arithmetic; nothing to do */
    return 0;
  } else if ((type->decl_specs.flags & TYPE_MASK_ARTH) == TYPE_ARTH_INT) {
    /* specify signed integer if not explicit */
    if (!(type->decl_specs.flags & TYPE_MASK_SIGN))
      type->decl_specs.flags |= TYPE_SIGN_SIGNED;
    return 0;
  } else if ((type->decl_specs.flags & (TYPE_MASK_SIGN | TYPE_MASK_SIZE)) ==
             DECL_SPEC_NONE) {
    /* no signedness/integer size specifiers; nothing to do */
    return 0;
  } else if ((type->decl_specs.flags & TYPE_MASK_ARTH) != TYPE_ARTH_FLT) {
    /* char or double; nothing to do */
    return 0;
  } else if (type->decl_specs.flags & TYPE_MASK_SIGN) {
    /* only `signed` or `unsigned` specified; make it an `int` */
    type->decl_specs.flags |= TYPE_ARTH_INT;
    return 0;
  } else if ((type->decl_specs.flags & TYPE_MASK_SIZE) ||
             (type->decl_specs.flags & TYPE_MASK_STORE) == TYPE_STORE_AUTO) {
    /* only size or `auto` specified; make it a `signed (size) int` */
    type->decl_specs.flags |= TYPE_ARTH_INT | TYPE_SIGN_SIGNED;
    return 0;
  } else {
    return -1;
  }
}

int type_validate(const Type *type) {
  /* allow declarations of type `void`, but only for typedefs */
  if (type_is_void(type) && !(type->decl_specs.flags & TYPE_STORE_TYPEDEF))
    return 1;

  while (type != NULL) {
    switch (type->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
        /* fallthrough */
      case TYPE_EXTRA_ARTH:
        return 0;
      case TYPE_KIND_PTR:
        type = type_strip_declarator(type);
        continue;
      case TYPE_KIND_FN:
        /* fallthrough */
      case TYPE_EXTRA_VAR_FN:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_array(type) || type_is_function(type))
          return 1;
        continue;
      default:
        assert(type->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_EXTRA_ARR:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_incomplete(type) || type_is_function(type))
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
  unsigned int flags1 = type1->decl_specs.flags & TYPE_MASK_REPR;
  unsigned int flags2 = type2->decl_specs.flags & TYPE_MASK_REPR;

  /* TODO(Robert): support LLONG and ULLONG */
  if (flags1 == TYPE_REPR_LDBL || flags2 == TYPE_REPR_LDBL) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_REPR_DBL || flags2 == TYPE_REPR_DBL) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_REPR_FLT || flags2 == TYPE_REPR_FLT) {
    abort(); /* floating types not implemented */
  } else if (flags1 == TYPE_REPR_ULONG) {
    return type1;
  } else if (flags2 == TYPE_REPR_ULONG) {
    return type2;
  } else if (flags1 == TYPE_REPR_LONG) {
    return type1;
  } else if (flags2 == TYPE_REPR_LONG) {
    return type2;
  } else if (flags1 == TYPE_REPR_UINT) {
    return type1;
  } else if (flags2 == TYPE_REPR_UINT) {
    return type2;
  } else if (flags1 == TYPE_REPR_INT) {
    return type1;
  } else if (flags2 == TYPE_REPR_INT) {
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
  } else if ((src->decl_specs.flags & (TYPE_MASK_KIND | TYPE_MASK_EXTRA)) ==
             TYPE_EXTRA_ARR) {
    /* source type incomplete; nothing to do */
    return 0;
  } else if ((dest->decl_specs.flags & (TYPE_MASK_KIND | TYPE_MASK_EXTRA)) ==
             TYPE_EXTRA_ARR) {
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
  switch (dest->decl_specs.flags & TYPE_MASK_EXTRA_KIND) {
    case TYPE_KIND_OTHER:
      /* fallthrough */
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_EXTRA_ARTH:
      /* fallthrough */
    case TYPE_KIND_PTR:
      return types_compatible(dest, src, qualified) ? 0 : -1;
    case TYPE_KIND_FN:
      /* fallthrough */
    case TYPE_EXTRA_VAR_FN:
      return compose_functions(dest, src);
    default:
      assert(dest->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_EXTRA_ARR:
      return compose_arrays(dest, src);
  }
}
