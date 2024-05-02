#ifndef BCC_TYPES_H
#define BCC_TYPES_H
#include "stdio.h"

#define X64_SIZEOF_LONG ((size_t)8UL)
#define X64_ALIGNOF_LONG ((size_t)8UL)
#define X64_SIZEOF_INT ((size_t)4UL)
#define X64_ALIGNOF_INT ((size_t)4UL)
#define X64_SIZEOF_SHORT ((size_t)2UL)
#define X64_ALIGNOF_SHORT ((size_t)2UL)
#define X64_SIZEOF_CHAR ((size_t)1UL)
#define X64_ALIGNOF_CHAR ((size_t)1UL)

#define X64_SIZEOF_FLOAT ((size_t)4UL)
#define X64_ALIGNOF_FLOAT ((size_t)4UL)
#define X64_SIZEOF_DOUBLE ((size_t)8UL)
#define X64_ALIGNOF_DOUBLE ((size_t)8UL)
#define X64_SIZEOF_LONG_DOUBLE ((size_t)16UL)
#define X64_ALIGNOF_LONG_DOUBLE ((size_t)16UL)

typedef enum type_code {
  TYPE_CODE_NONE,
  TYPE_CODE_BASE,
  TYPE_CODE_STRUCT,
  TYPE_CODE_UNION,
  TYPE_CODE_ENUM,
  TYPE_CODE_POINTER,
  TYPE_CODE_FUNCTION,
  TYPE_CODE_ARRAY
} TypeCode;

/* when bits 0-2 equal SPEC_FLAG_INTEGRAL, additional information about the
 * integer is stored in bits 5 and 6. bits 3 and 4 indicate the signed and
 * unsigned keywords, respectively.
 *
 * note that for lists specifying an integral type, it would be perfectly valid
 * for the type to be missing either the signedness or width, and for the
 * lower bits to be set to SPEC_FLAG_NONE. this would occur if there was only a
 * single specifier in the list which was `unsigned`, `signed`, `short`, `long`,
 * or `long long`.
 */
typedef enum type_spec_flag {
  SPEC_FLAG_NONE = 0,
  SPEC_FLAG_VOID = 1,
  SPEC_FLAG_CHAR = 2,
  SPEC_FLAG_INTEGRAL = 3,
  SPEC_FLAG_FLOAT = 4,
  SPEC_FLAG_DOUBLE = 5,
  /* SPEC_FLAG_BOOL = 6, */
  /* unused 8th value = 7 */
  SPEC_FLAG_SIGNED = 1 << 3,
  SPEC_FLAG_UNSIGNED = 1 << 4,
  SPEC_FLAG_SHORT = 1 << 5,
  SPEC_FLAG_INT = 0, /* SPEC_FLAG_INT == SPEC_FLAG_NONE == 0 */
  SPEC_FLAG_LONG = 1 << 6,
  /* SPEC_FLAG_LONG_LONG = 3 << 5, */
  SPEC_FLAGS_SCHAR = SPEC_FLAG_CHAR | SPEC_FLAG_SIGNED,
  SPEC_FLAGS_UCHAR = SPEC_FLAG_CHAR | SPEC_FLAG_UNSIGNED,
  SPEC_FLAGS_SINT = SPEC_FLAG_INTEGRAL | SPEC_FLAG_SIGNED | SPEC_FLAG_INT,
  SPEC_FLAGS_SSHRT = SPEC_FLAG_INTEGRAL | SPEC_FLAG_SIGNED | SPEC_FLAG_SHORT,
  SPEC_FLAGS_SLONG = SPEC_FLAG_INTEGRAL | SPEC_FLAG_SIGNED | SPEC_FLAG_LONG,
  /* SPEC_FLAGS_SLLONG = SPEC_FLAG_INTEGRAL | SPEC_FLAG_SIGNED |
     SPEC_FLAG_LONG_LONG, */
  SPEC_FLAGS_UINT = SPEC_FLAG_INTEGRAL | SPEC_FLAG_UNSIGNED | SPEC_FLAG_INT,
  SPEC_FLAGS_USHRT = SPEC_FLAG_INTEGRAL | SPEC_FLAG_UNSIGNED | SPEC_FLAG_SHORT,
  SPEC_FLAGS_ULONG = SPEC_FLAG_INTEGRAL | SPEC_FLAG_UNSIGNED | SPEC_FLAG_LONG,
  /* SPEC_FLAGS_ULLONG = SPEC_FLAG_INTEGRAL | SPEC_FLAG_UNSIGNED |
     SPEC_FLAG_LONG_LONG, */
  SPEC_FLAGS_LONG_DOUBLE = SPEC_FLAG_DOUBLE | SPEC_FLAG_LONG,
  SPEC_FLAG_MASK = 0x7fU,
  SPEC_FLAG_LOW_MASK = 0x7U,
  SPEC_FLAG_HIGH_MASK = 0x78U,
  SPEC_FLAG_SIGN_MASK = SPEC_FLAG_SIGNED | SPEC_FLAG_UNSIGNED,
  SPEC_FLAG_SIZE_MASK = SPEC_FLAG_HIGH_MASK ^ SPEC_FLAG_SIGN_MASK
} SpecFlag;

/* Qualifier and storage class flags will be specified such that they do not
   overlap with any of the base type enums, even if it is not valid for the
   given base type to have a given flag.

   Storage class flags are technically for symbols, not for types, but because
   of the compiler's structure the storage class information must be propogated
   through the types system in order for the symbol to receive the info.
 */

typedef enum type_qualifier_flag {
  QUAL_FLAG_NONE = 0,
  QUAL_FLAG_CONST = 1 << 7,
  QUAL_FLAG_VOLATILE = 1 << 8,
  /*
   * QUAL_FLAG_RESTRICT = 1 << 9,
   * QUAL_FLAG_ATOMIC = 1 << 10,
   */
  QUAL_FLAG_MASK = 0x780U
} QualifierFlag;

typedef enum type_storage_flag {
  STOR_FLAG_NONE = 0,
  STOR_FLAG_AUTO = 1 << 11,
  STOR_FLAG_REGISTER = 1 << 12,
  STOR_FLAG_STATIC = 1 << 13,
  STOR_FLAG_EXTERN = 1 << 14,
  STOR_FLAG_TYPEDEF = 1 << 15,
  STOR_FLAG_MASK = 0xf800U
} StorageFlag;

typedef union type Type;
union type {
  struct {
    TypeCode code;
    /* a generic flags field, used when no type specifiers have been parsed
     * yet to hold storage class specifiers and type qualifiers
     */
    unsigned int flags;
  } any;
  struct {
    TypeCode code;
    unsigned int qualifiers;
    Type *next;
  } pointer;
  struct {
    TypeCode code;
    int deduce_length;
    Type *next;
    size_t length;
  } array;
  struct {
    TypeCode code;
    short is_variadic;
    short is_old_style;
    Type *next;
    size_t parameters_size;
    Type **parameters;
  } function;
  struct {
    TypeCode code;
    unsigned int flags;
    const char *name;
    union tag *value;
  } tag;
  struct {
    TypeCode code;
    unsigned int flags;
  } base;
};

extern const Type *const TYPE_VOID;
extern const Type *const TYPE_POINTER;
extern const Type *const TYPE_CHAR;
extern const Type *const TYPE_INT;
extern const Type *const TYPE_LONG;
extern const Type *const TYPE_UNSIGNED_INT;
extern const Type *const TYPE_UNSIGNED_LONG;
/* this one is in `parser.c` */
extern const Type *TYPE_VA_LIST_POINTER;
/* TODO(Robert): Type: is this necessary anymore? */
extern const Type *const TYPE_NONE;

void type_init_globals(void);

Type *type_init_none(unsigned int flags);
Type *type_init_pointer(unsigned int qualifiers);
Type *type_init_array(size_t length, int deduce_length);
Type *type_init_function(size_t parameters_size, Type **parameters,
                         int is_variadic, int is_old_style);
Type *type_init_tag(unsigned int flags, const char *tag_name, union tag *tag);
Type *type_init_base(unsigned int flags);

void type_destroy(Type *type);

int type_to_str(const Type *type, char *buf);

int type_is_void(const Type *type);
int type_is_integer(const Type *type);
int type_is_signed(const Type *type);
int type_is_unsigned(const Type *type);
int type_is_enum(const Type *type);
int type_is_integral(const Type *type);
int type_is_arithmetic(const Type *type);
int type_is_pointer(const Type *type);
int type_is_scalar(const Type *type);
int type_is_array(const Type *type);
int type_is_deduced_array(const Type *type);
int type_is_function(const Type *type);
int type_is_object(const Type *type);
int type_is_variadic_function(const Type *type);
int type_is_old_style_function(const Type *type);
int type_is_prototyped_function(const Type *type);
int type_is_void_pointer(const Type *type);
int type_is_function_pointer(const Type *type);
int type_is_struct(const Type *type);
int type_is_struct_pointer(const Type *type);
int type_is_union(const Type *type);
int type_is_union_pointer(const Type *type);
int type_is_record(const Type *type);
int type_is_aggregate(const Type *type);
int type_is_char_array(const Type *type);
int type_is_const(const Type *type);
int type_is_volatile(const Type *type);
int type_is_qualified(const Type *type);
int type_is_declarator(const Type *type);
int type_is_none(const Type *type);
int type_is_incomplete(const Type *type);

struct symbol *type_member_name(const Type *type, const char *name);
struct symbol *type_member_index(const Type *type, size_t index);
size_t type_member_count(const Type *type);
Type *type_param_index(const Type *type, size_t index);
size_t type_param_count(const Type *type);

size_t type_get_alignment(const Type *type);
size_t type_get_width(const Type *type);
size_t type_elem_width(const Type *type);
size_t type_get_eightbytes(const Type *type);
size_t type_get_padding(const Type *type, size_t to_pad);
unsigned int type_get_flags(const Type *type);
int types_equivalent(const Type *type1, const Type *type2,
                     int ignore_qualifiers, int ignore_storage_class);
int types_assignable(const Type *dest, const Type *src, int is_const_zero);

Type *type_strip_declarator(const Type *type);
Type *type_get_decl_specs(Type *type);
Type *type_append(Type *dest, Type *src, int copy_src);
Type *type_copy(const Type *type, int clear_typedef_flag);
Type *type_common_qualified_pointer(const Type *type1, const Type *type2);
int type_add_flags(Type *type, unsigned int flags);
int type_normalize(Type *type);
int type_validate(const Type *type);
Type *type_pointer_conversions(Type *type);
Type *type_arithmetic_conversions(Type *type1, Type *type2);
int type_complete_array(Type *type1, Type *type2);
int type_prototype_function(Type *type1, Type *type2);

#endif
