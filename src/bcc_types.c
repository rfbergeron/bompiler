#include "bcc_types.h"

#include <limits.h>

#include "assert.h"
#include "scope.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "symtable.h"

#define GET_ARRAY_LEN(type) (((type)->array.flags >> 1) + 1ul)
#define GET_PARAM_COUNT(type) ((type->function.flags) >> 8)
#define SWAP(type, x, y) \
  do {                   \
    type temp = (x);     \
    (x) = (y);           \
    (y) = temp;          \
  } while (0)
#define BCC_ARRAY_MAX_LEN (1ul << (CHAR_BIT * sizeof(unsigned long) - 1))
#define BCC_FN_MAX_PARAMS (ULONG_MAX >> 8)

typedef enum arth_flag {
  ARTH_NONE = 0,           /* 0x00 no declaration specifiers */
  ARTH_MASK_ARTH = 0x0f,   /* mutually exclusive specifiers */
  ARTH_BIT_FIELD = 1 << 0, /* 0x01 bit field */
                           /* 0x00 */
  ARTH_INT = 1 << 1,       /* 0x02 */
  ARTH_CHAR = 2 << 1,      /* 0x04 */
  ARTH_FLOAT = 3 << 1,     /* 0x06 */
  ARTH_DOUBLE = 4 << 1,    /* 0x08 */
                           /* 0x0a reserved for _BitInt */
                           /* 0x0c reserved for _Bool/bool */
                           /* 0x0e reserved for __int128 */
  ARTH_MASK_SIGN = 0x30,   /*      integer sign flags */
  ARTH_SIGNED = 1 << 4,    /* 0x10 */
  ARTH_UNSIGNED = 1 << 5,  /* 0x20 */
  ARTH_MASK_SIZE = 0xc0,   /* 0xc0 */
  ARTH_SHRT = 1 << 6,      /* 0x40 */
  ARTH_LONG = 2 << 6       /* 0x80 */
} ArthFlag;

typedef enum type_flag {
  DECL_SPEC_NONE = 0,            /* 0x0000 no declaration specifiers */
  TYPE_MASK_KIND = 0x000f,       /*        type kind: 2 flags, 2-bit int */
  TYPE_FLAG_ARR = 1 << 0,        /* 0x0001 complete array */
  TYPE_KIND_VARR = 1 << 1,       /* 0x0002 other array type */
  TYPE_KIND_ARTH = 0 << 2,       /* 0x0000 */
  TYPE_KIND_OTHER = 1 << 2,      /* 0x0004 */
  TYPE_KIND_PTR = 2 << 2,        /* 0x0008 */
  TYPE_KIND_FN = 3 << 2,         /* 0x000c */
  TYPE_FN_OLD_STYLE = 1 << 4,    /* 0x0010 */
  TYPE_FN_VARIADIC = 1 << 5,     /* 0x0020 */
                                 /* 0x0040 reserved for inline */
                                 /* 0x0080 reserved for noreturn */
  TYPE_MASK_OTHER = 3 << 4,      /* 0x0030 other types */
  TYPE_OTHER_VOID = 0 << 4,      /* 0x0000 */
  TYPE_OTHER_STRUCT = 1 << 4,    /* 0x0010 */
  TYPE_OTHER_UNION = 2 << 4,     /* 0x0020 */
  TYPE_OTHER_ENUM = 3 << 4,      /* 0x0030 */
  TYPE_MASK_QUAL = 0x1e00,       /*        type qualifiers: 4-bit flagset */
                                 /* 0x0000 no type qualifiers */
  TYPE_QUAL_CONST = 1 << 9,      /* 0x0200 */
  TYPE_QUAL_VOLATILE = 1 << 10,  /* 0x0400 */
                                 /* 0x0800 reserved for restrict */
                                 /* 0x1000 reserved for atomic */
  TYPE_MASK_STORE = 0xe000,      /*        storage class: 3-bit integer */
                                 /* 0x0000 no explicit storage class */
  TYPE_STORE_REGISTER = 1 << 13, /* 0x2000 */
  TYPE_STORE_AUTO = 2 << 13,     /* 0x4000 */
  TYPE_STORE_STATIC = 3 << 13,   /* 0x6000 */
  TYPE_STORE_EXTERN = 4 << 13,   /* 0x8000 */
  TYPE_STORE_TYPEDEF = 5 << 13,  /* 0xa000 */
                                 /* 0xc000 invalid 7th value */
                                 /* 0xe000 invalid 8th value */
  TYPE_MASK_REPR = TYPE_MASK_KIND | TYPE_MASK_OTHER,
  TYPE_REPR_STRUCT = TYPE_KIND_OTHER | TYPE_OTHER_STRUCT,
  TYPE_REPR_UNION = TYPE_KIND_OTHER | TYPE_OTHER_UNION,
  TYPE_REPR_ENUM = TYPE_KIND_OTHER | TYPE_OTHER_ENUM,
  TYPE_REPR_VOID = TYPE_KIND_OTHER | TYPE_OTHER_VOID
} TypeFlag;

union type {
  struct {
    unsigned long flags;
    union {
      union tag *tag;
      unsigned long arth;
    } extra;
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
    {TYPE_KIND_OTHER | TYPE_OTHER_STRUCT, {&TAG_VA_SPILL_REGION}}};

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
  TYPE_CHAR_INTERNAL.decl_specs.flags = TYPE_KIND_ARTH;
  TYPE_CHAR_INTERNAL.decl_specs.extra.arth = ARTH_CHAR;
  TYPE_INT_INTERNAL.decl_specs.flags = TYPE_KIND_ARTH;
  TYPE_INT_INTERNAL.decl_specs.extra.arth = ARTH_INT;
  TYPE_LONG_INTERNAL.decl_specs.flags = TYPE_KIND_ARTH;
  TYPE_LONG_INTERNAL.decl_specs.extra.arth = ARTH_LONG;
  TYPE_UNSIGNED_INT_INTERNAL.decl_specs.flags = TYPE_KIND_ARTH;
  TYPE_UNSIGNED_INT_INTERNAL.decl_specs.extra.arth = ARTH_INT | ARTH_UNSIGNED;
  TYPE_UNSIGNED_LONG_INTERNAL.decl_specs.flags = TYPE_KIND_ARTH;
  TYPE_UNSIGNED_LONG_INTERNAL.decl_specs.extra.arth = ARTH_LONG | ARTH_UNSIGNED;
  /* TYPE_NONE_INTERNAL already zero-filled; no need to initialize */
}

Type *type_init_basic(void) { return calloc(1, sizeof(Type)); }

Type *type_init_tag(Tag *tag) {
  static const TypeFlag TAG_TYPESPEC_MAP[] = {TYPE_REPR_STRUCT, TYPE_REPR_UNION,
                                              TYPE_REPR_ENUM};
  Type *type = malloc(sizeof(Type));
  type->decl_specs.flags = TAG_TYPESPEC_MAP[tag_get_kind(tag)];
  type->decl_specs.extra.tag = tag;
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
    type->array.flags = TYPE_KIND_VARR;
  else
    type->array.flags = TYPE_FLAG_ARR | ((length - 1) << 1);
  type->array.next = NULL;
  return type;
}

Type *type_init_function(size_t parameters_size, Type **parameters,
                         int variadic) {
  assert(parameters_size <= BCC_FN_MAX_PARAMS);
  Type *type = malloc(sizeof(Type));
  type->function.flags = TYPE_KIND_FN | parameters_size << 8;
  if (variadic) type->function.flags |= TYPE_FN_VARIADIC;
  type->function.next = parameters;
  type->function.next[parameters_size] = NULL;
  return type;
}

void type_destroy(Type *type) {
  while (type != NULL) {
    Type *current = type;
    switch (type->decl_specs.flags & TYPE_MASK_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
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
      default:
        assert(type->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_KIND_VARR:
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

static const char *qual_get_str(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_QUAL) {
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

static const char *store_get_str(const Type *type) {
  static const char *STORAGE_DUMP_MAP[] = {NULL,        "register", "auto",
                                           "static",    "extern",   "typedef",
                                           "(invalid)", "(invalid)"};
  return STORAGE_DUMP_MAP[(type->decl_specs.flags & TYPE_MASK_STORE) >> 13];
}

static const char *arth_get_str(const Type *type) {
  switch (type->decl_specs.extra.arth) {
    default:
      return "(invalid)";
    case ARTH_NONE:
      return "(none)";
    case ARTH_INT:
      return "int";
    case ARTH_INT | ARTH_SIGNED:
      return "signed int";
    case ARTH_INT | ARTH_UNSIGNED:
      return "unsigned int";
    case ARTH_SHRT:
      return "short";
    case ARTH_SHRT | ARTH_SIGNED:
      return "signed short";
    case ARTH_SHRT | ARTH_UNSIGNED:
      return "unsigned short";
    case ARTH_INT | ARTH_SHRT:
      return "short int";
    case ARTH_INT | ARTH_SHRT | ARTH_SIGNED:
      return "signed short int";
    case ARTH_INT | ARTH_SHRT | ARTH_UNSIGNED:
      return "unsigned short int";
    case ARTH_LONG:
      return "long";
    case ARTH_LONG | ARTH_SIGNED:
      return "signed long";
    case ARTH_LONG | ARTH_UNSIGNED:
      return "unsigned long";
    case ARTH_INT | ARTH_LONG:
      return "long int";
    case ARTH_INT | ARTH_LONG | ARTH_SIGNED:
      return "signed long int";
    case ARTH_INT | ARTH_LONG | ARTH_UNSIGNED:
      return "unsigned long int";
    case ARTH_CHAR:
      return "char";
    case ARTH_CHAR | ARTH_SIGNED:
      return "signed char";
    case ARTH_CHAR | ARTH_UNSIGNED:
      return "unsigned char";
    case ARTH_FLOAT:
      return "float";
    case ARTH_DOUBLE:
      return "double";
    case ARTH_DOUBLE | ARTH_LONG:
      return "long double";
    case ARTH_BIT_FIELD:
      return "(bit field)";
    case ARTH_BIT_FIELD | ARTH_SIGNED:
      return "(signed bit field)";
    case ARTH_BIT_FIELD | ARTH_UNSIGNED:
      return "(unsigned bit field)";
  }
}

static const char *other_get_str(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_OTHER) {
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

void type_dump_specs(const Type *type) {
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
      "Type specifier(s): %s\n"
      "Raw bit value: %lx\n"
      "Raw arithmetic flags value: %lx\n";
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
      "Parameter count: %lu\n"
      "Raw bit value: %lx\n";
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_OTHER:
      (void)fprintf(stderr, OTHER_DUMP_FMT, qual_get_str(type),
                    store_get_str(type), other_get_str(type),
                    type->decl_specs.flags);
      break;
    case TYPE_KIND_ARTH:
      (void)fprintf(stderr, ARTH_DUMP_FMT, qual_get_str(type),
                    store_get_str(type), arth_get_str(type),
                    type->decl_specs.flags, type->decl_specs.extra.arth);
      break;
    case TYPE_KIND_PTR:
      (void)fprintf(stderr, PTR_DUMP_FMT, qual_get_str(type),
                    store_get_str(type), type->decl_specs.flags);
      break;
    case TYPE_KIND_FN:
      (void)fprintf(stderr, FN_DUMP_FMT,
                    (type->decl_specs.flags & TYPE_FN_VARIADIC) ? "yes" : "no",
                    GET_PARAM_COUNT(type), type->decl_specs.flags);
      break;
    case TYPE_KIND_VARR:
      (void)fprintf(stderr,
                    "Type kind is incomplete array.\nRaw bit value: %lx\n",
                    type->array.flags);
      break;
    default:
      if (!(type->array.flags & TYPE_FLAG_ARR)) {
        (void)fprintf(stderr, "Type kind is invalid.\nRaw bit value: %lx\n",
                      type->decl_specs.flags);
      } else {
        (void)fprintf(stderr, ARR_DUMP_FMT, GET_ARRAY_LEN(type),
                      type->array.flags);
      }
      break;
  }
}

static int specifiers_to_str(const Type *type, char *buf) {
  assert(!type_is_declarator(type));
  static const char *FMT_STRS[] = {"%s", "%s %s", "%s %s %s"};
  const char *type_str =
      type_is_arithmetic(type) ? arth_get_str(type) : other_get_str(type);
  const char *qual_str = qual_get_str(type);

  if (type_is_record(type) || type_is_enum(type)) {
    return qual_str == NULL ? sprintf(buf, FMT_STRS[1], type_str,
                                      type->decl_specs.extra.tag->record.name)
                            : sprintf(buf, FMT_STRS[2], qual_str, type_str,
                                      type->decl_specs.extra.tag->record.name);
  } else {
    return qual_str == NULL ? sprintf(buf, FMT_STRS[0], type_str)
                            : sprintf(buf, FMT_STRS[1], qual_str, type_str);
  }
}

int type_to_str(const Type *type, char *buf) {
  (void)strcpy(buf, "(null)");
  int ret = 0;
  const Type *current = type;
  while (current != NULL) {
    switch (current->decl_specs.flags & TYPE_MASK_KIND) {
      case TYPE_KIND_ARTH:
        /* fallthrough */
      case TYPE_KIND_OTHER:
        ret += specifiers_to_str(current, buf + ret);
        current = NULL;
        break;
      case TYPE_KIND_FN:
        if (current->function.flags & TYPE_FN_VARIADIC)
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
          ret += sprintf(buf + ret, "%s pointer to ", qual_get_str(current));
        else
          ret += sprintf(buf + ret, "pointer to ");
        current = current->pointer.next;
        break;
      case TYPE_KIND_VARR:
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
  if ((type->decl_specs.flags & TYPE_MASK_KIND) != TYPE_KIND_ARTH) return 0;

  switch (type->decl_specs.extra.arth & ARTH_MASK_ARTH) {
    case ARTH_FLOAT:
      /* fallthrough */
    case ARTH_DOUBLE:
      return 0;
    case ARTH_BIT_FIELD:
      /* fallthrough */
    case ARTH_INT:
      /* fallthrough */
    case ARTH_CHAR:
      /* fallthrough */
    default:
      /* anything without type specifiers is an `int` */
      return 1;
  }
}

int type_is_signed(const Type *type) {
  /* pointers are not considered signed or unsigend */
  if ((type->decl_specs.flags & TYPE_MASK_KIND) != TYPE_KIND_ARTH) return 0;

  switch (type->decl_specs.extra.arth & ARTH_MASK_ARTH) {
    case ARTH_FLOAT:
      /* fallthrough */
    case ARTH_DOUBLE:
      return 0;
    case ARTH_BIT_FIELD:
      return !(type->decl_specs.extra.arth & ARTH_SIGNED);
    case ARTH_INT:
      /* fallthrough */
    case ARTH_CHAR:
      /* fallthrough */
    default:
      return !(type->decl_specs.extra.arth & ARTH_UNSIGNED);
  }
}

int type_is_unsigned(const Type *type) {
  /* pointers are not considered signed or unsigend */
  if ((type->decl_specs.flags & TYPE_MASK_KIND) != TYPE_KIND_ARTH) return 0;

  switch (type->decl_specs.extra.arth & ARTH_MASK_ARTH) {
    case ARTH_FLOAT:
      /* fallthrough */
    case ARTH_DOUBLE:
      return 0;
    case ARTH_BIT_FIELD:
      return !!(type->decl_specs.extra.arth & ARTH_SIGNED);
    case ARTH_INT:
      /* fallthrough */
    case ARTH_CHAR:
      /* fallthrough */
    default:
      return !!(type->decl_specs.extra.arth & ARTH_UNSIGNED);
  }
}

int type_is_enum(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_REPR_ENUM;
}

int type_is_bit_field(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_REPR) == TYPE_KIND_ARTH &&
         (type->decl_specs.extra.arth & ARTH_BIT_FIELD);
}

int type_is_integral(const Type *type) {
  return type_is_integer(type) || type_is_enum(type);
}

int type_is_arithmetic(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_REPR) {
    case TYPE_KIND_ARTH:
      /* anything without type specifiers is an `int` */
      return type->decl_specs.flags || type->decl_specs.extra.arth;
    case TYPE_REPR_ENUM:
      return 1;
    default:
      return 0;
  }
}

int type_is_pointer(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_PTR;
}

int type_is_scalar(const Type *type) {
  return type_is_pointer(type) || type_is_arithmetic(type);
}

int type_is_array(const Type *type) {
  return (type->decl_specs.flags & TYPE_FLAG_ARR) ||
         type_is_deduced_array(type);
}

int type_is_deduced_array(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_VARR;
}

int type_is_function(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_FN;
}

int type_is_variadic_function(const Type *type) {
  return (type->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_FN &&
         (type->decl_specs.flags & (TYPE_FN_OLD_STYLE | TYPE_FN_VARIADIC));
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
         (type->array.next->decl_specs.extra.arth & ARTH_MASK_ARTH) ==
             ARTH_CHAR;
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
  return (type->decl_specs.flags & TYPE_MASK_REPR) == DECL_SPEC_NONE &&
         !type->decl_specs.extra.arth;
}

int type_is_incomplete(const Type *type) {
  if (type_is_void(type) || type_is_none(type)) {
    return 1;
  } else if (type_is_deduced_array(type)) {
    return 1;
  } else if ((type_is_union(type) || type_is_struct(type)) &&
             !type->decl_specs.extra.tag->record.defined) {
    return 1;
  } else {
    return 0;
  }
}

Symbol *type_get_member_name(const Type *type, const char *name) {
  assert(type_is_record(type));
  return scope_get_member(type->decl_specs.extra.tag->record.members, name);
}

Symbol *type_get_member_index(const Type *type, size_t index) {
  assert(type_is_record(type));
  Symbol *member;
  scope_member_at(type->decl_specs.extra.tag->record.members, index, NULL,
                  &member);
  return member;
}

size_t type_get_member_count(const Type *type) {
  assert(type_is_record(type));
  return scope_member_count(type->decl_specs.extra.tag->record.members);
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

static size_t arth_alignment(unsigned long flags) {
  switch (flags) {
    default:
      /* fallthrough */
    case ARTH_NONE: /* yep, it's an int */
      /* fallthrough */
    case ARTH_INT:
      /* fallthrough */
    case ARTH_INT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_UNSIGNED:
      return X64_ALIGNOF_INT;
    case ARTH_SHRT:
      /* fallthrough */
    case ARTH_SHRT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_SHRT | ARTH_UNSIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT | ARTH_UNSIGNED:
      return X64_ALIGNOF_SHRT;
    case ARTH_LONG:
      /* fallthrough */
    case ARTH_LONG | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_LONG | ARTH_UNSIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG | ARTH_UNSIGNED:
      return X64_ALIGNOF_LONG;
    case ARTH_CHAR:
      /* fallthrough */
    case ARTH_CHAR | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_CHAR | ARTH_UNSIGNED:
      return X64_ALIGNOF_CHAR;
    case ARTH_FLOAT:
      return X64_ALIGNOF_FLT;
    case ARTH_DOUBLE:
      return X64_ALIGNOF_DBL;
    case ARTH_DOUBLE | ARTH_LONG:
      return X64_ALIGNOF_LDBL;
    case ARTH_BIT_FIELD:
      /* fallthrough */
    case ARTH_BIT_FIELD | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_BIT_FIELD | ARTH_UNSIGNED:
      return X64_ALIGNOF_CHAR;
  }
}

static size_t type_decl_specs_alignment(const Type *type) {
  assert(!type_is_declarator(type));
  switch (type->decl_specs.flags & TYPE_MASK_REPR) {
    case TYPE_REPR_VOID:
      return 0;
    case TYPE_REPR_STRUCT:
      /* fallthrough */
    case TYPE_REPR_UNION:
      /* fallthrough */
    case TYPE_REPR_ENUM:
      return tag_get_alignment(type->decl_specs.extra.tag);
    case TYPE_KIND_ARTH:
      return arth_alignment(type->decl_specs.extra.arth);
    default:
      (void)fprintf(stderr, "Unable to get alignment for type specifiers.\n");
      abort();
  }
}

size_t type_get_alignment(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_ALIGNOF_LONG;
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type_decl_specs_alignment(type);
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_KIND_VARR:
      return type_get_alignment(type->array.next);
      abort();
  }
}

static size_t arth_width(unsigned long flags) {
  switch (flags) {
    default:
      /* fallthrough */
    case ARTH_NONE: /* yep, it's an int */
      /* fallthrough */
    case ARTH_INT:
      /* fallthrough */
    case ARTH_INT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_UNSIGNED:
      return X64_SIZEOF_INT;
    case ARTH_SHRT:
      /* fallthrough */
    case ARTH_SHRT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_SHRT | ARTH_UNSIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_SHRT | ARTH_UNSIGNED:
      return X64_SIZEOF_SHRT;
    case ARTH_LONG:
      /* fallthrough */
    case ARTH_LONG | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_LONG | ARTH_UNSIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_INT | ARTH_LONG | ARTH_UNSIGNED:
      return X64_SIZEOF_LONG;
    case ARTH_CHAR:
      /* fallthrough */
    case ARTH_CHAR | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_CHAR | ARTH_UNSIGNED:
      return X64_SIZEOF_CHAR;
    case ARTH_FLOAT:
      return X64_SIZEOF_FLT;
    case ARTH_DOUBLE:
      return X64_SIZEOF_DBL;
    case ARTH_DOUBLE | ARTH_LONG:
      return X64_SIZEOF_LDBL;
    case ARTH_BIT_FIELD:
      /* fallthrough */
    case ARTH_BIT_FIELD | ARTH_SIGNED:
      /* fallthrough */
    case ARTH_BIT_FIELD | ARTH_UNSIGNED:
      return X64_SIZEOF_CHAR;
  }
}

static size_t type_decl_specs_width(const Type *type) {
  assert(!type_is_declarator(type));
  switch (type->decl_specs.flags & TYPE_MASK_REPR) {
    case TYPE_REPR_VOID:
      return 0;
    case TYPE_REPR_STRUCT:
      /* fallthrough */
    case TYPE_REPR_UNION:
      /* fallthrough */
    case TYPE_REPR_ENUM:
      return tag_get_width(type->decl_specs.extra.tag);
    case TYPE_KIND_ARTH:
      return arth_width(type->decl_specs.extra.arth);
    default:
      (void)fprintf(stderr, "Unable to get size for type specifiers.\n");
      abort();
  }
}

/* TODO(Robert): sizeof not allowed on incomplete arrays. what to do. */
size_t type_get_width(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_VARR:
      /* fallthrough */
    case TYPE_KIND_FN:
      return 0;
    case TYPE_KIND_PTR:
      return X64_SIZEOF_LONG;
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type_decl_specs_width(type);
    default: /* should be an array */
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      return GET_ARRAY_LEN(type) * type_get_elem_width(type);
  }
}

static size_t bit_field_width(const Type *type) {
  assert(type_is_bit_field(type));
  return type->decl_specs.extra.arth >> 11;
}

size_t type_get_bit_width(const Type *type) {
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_PTR:
      return X64_SIZEOF_LONG * X64_CHAR_BIT;
    case TYPE_KIND_ARTH:
      if (type_is_bit_field(type)) return bit_field_width(type);
      /* fallthrough */
    case TYPE_KIND_OTHER:
      return type_decl_specs_width(type) * X64_CHAR_BIT;
    case TYPE_KIND_VARR:
      /* fallthrough */
    case TYPE_KIND_FN:
      /* fallthrough */
    default: /* should be an array */
      (void)fprintf(stderr, "Attempt to get bit width of declarator.\n");
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

Tag *type_get_tag(const Type *type) {
  assert(type_is_record(type) || type_is_enum(type));
  return type->decl_specs.extra.tag;
}

static int params_compatible(const Type *type1, const Type *type2) {
  assert(type_is_function(type1) && type_is_function(type2));
  if ((GET_PARAM_COUNT(type1) == 0 &&
       (type1->function.flags & TYPE_FN_VARIADIC)) ||
      (GET_PARAM_COUNT(type2) == 0 &&
       (type2->function.flags & TYPE_FN_VARIADIC)))
    return 1;
  else if (GET_PARAM_COUNT(type1) != GET_PARAM_COUNT(type2))
    return 0;
  else if (((type1->function.flags ^ type2->function.flags) & TYPE_FN_VARIADIC))
    return 0;
  size_t i, param_count = GET_PARAM_COUNT(type1);
  for (i = 0; i < param_count; ++i)
    if (!types_compatible(type1->function.next[i], type2->function.next[i], 0))
      return 0;
  return 1;
}

int types_compatible(const Type *type1, const Type *type2, int qualified) {
  switch (type1->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_OTHER:
      return !((type1->decl_specs.flags ^ type2->decl_specs.flags) &
               (qualified ? TYPE_MASK_REPR | TYPE_MASK_QUAL
                          : TYPE_MASK_REPR)) &&
             type1->decl_specs.extra.tag == type2->decl_specs.extra.tag;
    case TYPE_KIND_ARTH:
      if ((type1->decl_specs.flags ^ type2->decl_specs.flags) &
          (qualified ? TYPE_MASK_REPR | TYPE_MASK_QUAL : TYPE_MASK_REPR))
        return 0;
      /* TODO(Robert): check arithmetic compatibility; its complicated */
      return 1;
    case TYPE_KIND_PTR:
      return type_is_pointer(type2) &&
             (!qualified || !((type1->pointer.flags ^ type2->pointer.flags) &
                              TYPE_MASK_QUAL)) &&
             types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1);
    case TYPE_KIND_FN:
      return type_is_function(type2) &&
             types_compatible(type_strip_declarator(type1),
                              type_strip_declarator(type2), 1) &&
             params_compatible(type1, type2);
    default:
      assert(type1->decl_specs.flags & TYPE_FLAG_ARR);
      if (!type_is_deduced_array(type2) ||
          GET_ARRAY_LEN(type1) != GET_ARRAY_LEN(type2))
        return 0;
      /* fallthrough */
    case TYPE_KIND_VARR:
      return type_is_array(type2) &&
             types_compatible(type_strip_declarator(type1),
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
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_KIND_VARR:
      assert(type->array.next != NULL);
      return type->array.next;
    case TYPE_KIND_FN:
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
    switch (current->decl_specs.flags & TYPE_MASK_KIND) {
      default:
        assert(current->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_KIND_VARR:
        current = current->array.next;
        break;
      case TYPE_KIND_PTR:
        current = current->pointer.next;
        break;
      case TYPE_KIND_FN:
        current = current->function.next[GET_PARAM_COUNT(current)];
        break;
    }
  }

  switch (current->decl_specs.flags & TYPE_MASK_KIND) {
    default:
      assert(current->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_KIND_VARR:
      current->array.next = src;
      return dest;
    case TYPE_KIND_PTR:
      current->pointer.next = src;
      return dest;
    case TYPE_KIND_FN:
      current->function.next[GET_PARAM_COUNT(current)] = src;
      return dest;
  }
}

Type *type_detach(Type *type) {
  assert(type_is_declarator(type));
  Type *ret;
  switch (type->decl_specs.flags & TYPE_MASK_KIND) {
    default:
      assert(type->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_KIND_VARR:
      ret = type->array.next;
      type->array.next = NULL;
      return ret;
    case TYPE_KIND_FN:
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
    switch (((*type_tail)->decl_specs.flags) & TYPE_MASK_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
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
      case TYPE_KIND_VARR:
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

static ArthFlag arth_flag_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
    case TOK_INT:
      return ARTH_INT;
    case TOK_CHAR:
      return ARTH_CHAR;
    case TOK_FLOAT:
      return ARTH_FLOAT;
    case TOK_DOUBLE:
      return ARTH_DOUBLE;
    case TOK_SIGNED:
      return ARTH_SIGNED;
    case TOK_UNSIGNED:
      return ARTH_UNSIGNED;
    case TOK_SHORT:
      return ARTH_SHRT;
    case TOK_LONG:
      return ARTH_LONG;
    default:
      return ARTH_NONE;
  }
}

static TypeFlag type_flag_from_tok_kind(int tok_kind) {
  switch (tok_kind) {
    case TOK_VOID:
      return TYPE_REPR_VOID;
    case TOK_CHAR:
      /* fallthrough */
    case TOK_INT:
      /* fallthrough */
    case TOK_SHORT:
      /* fallthrough */
    case TOK_LONG:
      /* fallthrough */
    case TOK_SIGNED:
      /* fallthrough */
    case TOK_UNSIGNED:
      /* fallthrough */
    case TOK_FLOAT:
      /* fallthrough */
    case TOK_DOUBLE:
      return TYPE_KIND_ARTH;
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

static int arth_valid(unsigned long old_flags, unsigned long new_flag) {
  unsigned long base_type = old_flags & ARTH_MASK_ARTH;
  switch (new_flag) {
    case ARTH_NONE:
      return 1;
    case ARTH_INT:
      return !(old_flags & ARTH_MASK_ARTH);
    case ARTH_CHAR:
      return !(old_flags & (ARTH_MASK_ARTH | ARTH_MASK_SIZE));
    case ARTH_FLOAT:
      return !(old_flags & (ARTH_MASK_ARTH | ARTH_MASK_SIZE | ARTH_MASK_SIGN));
    case ARTH_DOUBLE:
      return !(old_flags & (ARTH_MASK_ARTH | ARTH_MASK_SIGN)) &&
             !(old_flags & (ARTH_SHRT));
    case ARTH_SIGNED:
      /* fallthrough */
    case ARTH_UNSIGNED:
      return base_type != ARTH_FLOAT && base_type != ARTH_DOUBLE &&
             !(old_flags & ARTH_MASK_SIGN);
    case ARTH_SHRT:
      return base_type != ARTH_FLOAT && base_type != ARTH_DOUBLE &&
             !(old_flags & ARTH_MASK_SIZE);
    case ARTH_LONG:
      return base_type != ARTH_FLOAT && !(old_flags & ARTH_MASK_SIZE);
    default:
      (void)fprintf(stderr, "Invalid argument to `arth_valid`.\n");
      abort();
  }
}

int type_add_decl_spec(Type *type, int tok_kind) {
  assert(!type_is_declarator(type) || type_is_pointer(type));
  TypeFlag new_flag = type_flag_from_tok_kind(tok_kind);
  ArthFlag arth_flag = arth_flag_from_tok_kind(tok_kind);

  if (new_flag & TYPE_MASK_QUAL) {
    if (type->decl_specs.flags & new_flag) {
      /* duplicate type qualifier */
      return -1;
    } else {
      type->decl_specs.flags |= new_flag;
      return 0;
    }
  } else if (type_is_pointer(type)) {
    /* pointers only accept type qualifiers */
    return -1;
  } else if (new_flag & TYPE_MASK_STORE) {
    if (type->decl_specs.flags & TYPE_MASK_STORE) {
      /* multiple storage class specifiers */
      return -1;
    } else {
      type->decl_specs.flags |= new_flag;
      return 0;
    }
  } else if (type_is_none(type)) {
    type->decl_specs.flags |= new_flag;
    if (arth_flag != ARTH_NONE) type->decl_specs.extra.arth |= arth_flag;
    return 0;
  } else if (type_is_arithmetic(type)) {
    if (arth_flag == ARTH_NONE) {
      /* mixed arithmetic and non-arithmetic specifiers */
      return -1;
    } else if (!arth_valid(type->decl_specs.extra.arth, arth_flag)) {
      /* duplicate/incompatible arithmetic flags */
      return -1;
    } else {
      type->decl_specs.extra.arth |= arth_flag;
      return 0;
    }
  } else {
    /* mixed arithmetic and non-arithmetic specifiers */
    return -1;
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

int type_validate(const Type *type) {
  /* allow declarations of type `void`, but only for typedefs */
  if (type_is_void(type) && !(type->decl_specs.flags & TYPE_STORE_TYPEDEF))
    return 1;

  while (type != NULL) {
    switch (type->decl_specs.flags & TYPE_MASK_KIND) {
      case TYPE_KIND_OTHER:
        /* fallthrough */
      case TYPE_KIND_ARTH:
        return 0;
      case TYPE_KIND_PTR:
        type = type_strip_declarator(type);
        continue;
      case TYPE_KIND_FN:
        type = type_strip_declarator(type);
        if (type == NULL || type_is_array(type) || type_is_function(type))
          return 1;
        continue;
      default:
        assert(type->decl_specs.flags & TYPE_FLAG_ARR);
        /* fallthrough */
      case TYPE_KIND_VARR:
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
  size_t bits1 = type_get_bit_width(type1), bits2 = type_get_bit_width(type2);

  if (bits1 < X64_SIZEOF_INT * X64_CHAR_BIT &&
      bits2 < X64_SIZEOF_INT * X64_CHAR_BIT) {
    return (Type *)TYPE_INT;
  } else if (bits1 > bits2) {
    return type1;
  } else if (bits1 < bits2) {
    return type2;
  } else if (type_is_unsigned(type1)) {
    return type_is_bit_field(type1) ? (Type *)TYPE_UNSIGNED_INT : type1;
  } else if (type_is_unsigned(type2)) {
    return type_is_bit_field(type2) ? (Type *)TYPE_UNSIGNED_INT : type2;
  } else if (!type_is_enum(type1) && !type_is_bit_field(type1)) {
    return type1;
  } else if (!type_is_enum(type2) && !type_is_bit_field(type2)) {
    return type2;
  } else {
    return (Type *)TYPE_INT;
  }
}

static int compose_arrays(Type *dest, Type *src) {
  if (!types_compatible(type_strip_declarator(dest), type_strip_declarator(src),
                        1)) {
    /* incompatible element types */
    return -1;
  } else if ((src->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_VARR) {
    /* source type incomplete; nothing to do */
    return 0;
  } else if ((dest->decl_specs.flags & TYPE_MASK_KIND) == TYPE_KIND_VARR) {
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
  } else if ((dest->function.flags & TYPE_FN_VARIADIC) &&
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
    src->function.flags = TYPE_KIND_FN | TYPE_FN_VARIADIC;
    return 0;
  } else if ((src->function.flags & TYPE_FN_VARIADIC) &&
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
  switch (dest->decl_specs.flags & TYPE_MASK_KIND) {
    case TYPE_KIND_OTHER:
      /* fallthrough */
    case TYPE_KIND_ARTH:
      /* fallthrough */
    case TYPE_KIND_PTR:
      return types_compatible(dest, src, qualified) ? 0 : -1;
    case TYPE_KIND_FN:
      return compose_functions(dest, src);
    default:
      assert(dest->decl_specs.flags & TYPE_FLAG_ARR);
      /* fallthrough */
    case TYPE_KIND_VARR:
      return compose_arrays(dest, src);
  }
}
