#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__
#include "badlib/badllist.h"
#include "badlib/badmap.h"
#include "stdio.h"

#define X64_SIZEOF_LONG (size_t)8
#define X64_ALIGNOF_LONG (size_t)8
#define X64_SIZEOF_INT (size_t)4
#define X64_ALIGNOF_INT (size_t)4
#define X64_SIZEOF_SHORT (size_t)2
#define X64_ALIGNOF_SHORT (size_t)2
#define X64_SIZEOF_CHAR (size_t)1
#define X64_ALIGNOF_CHAR (size_t)1

/* attributes correspond to array indices in the order they are listed here */
/* the following operations resolve to an lvalue:
 * - most identifiers
 * - the result of the arrow operator
 * - the result of the dot operator
 * - the result of the indirection operator
 * - the result of the subscript operator
 *
 * the following resolve to an rvalue:
 * - plain array and function identifiers
 * - the result of the address-of operator
 * - the result of any expression not explicitly listed as an lval
 */
enum attribute {
  ATTR_NONE = 0,            /* no attributes set */
  ATTR_EXPR_VREG = 1 << 0,  /* requires a virtual register */
  ATTR_EXPR_LVAL = 1 << 1,  /* refers to an assignable location */
  ATTR_EXPR_CONST = 1 << 2, /* refers to a compile-time constant */
  ATTR_EXPR_BOOL = 1 << 3,  /* int guaranteed to be 0 or 1 */
  ATTR_EXPR_VADDR = 1 << 4, /* int is memory address */
};

typedef enum base_type {
  TYPE_NONE,
  TYPE_SIGNED,
  TYPE_UNSIGNED,
  TYPE_VOID,
  TYPE_STRUCT,
  TYPE_UNION,
} BaseType;

typedef enum aux_type {
  AUX_NONE,
  AUX_POINTER,
  AUX_ARRAY,
  AUX_STRUCT,
  AUX_UNION,
  AUX_FUNCTION,
  AUX_TYPEDEF,
} AuxType;

/*
enum conversion_type {
  CONV_COMPATIBLE,
  CONV_IMPLICIT_CAST,
  CONV_EXPLICIT_CAST,
  CONV_INCOMPATIBLE,
  CONV_PROMOTE_LEFT,
  CONV_PROMOTE_RIGHT,
  CONV_PROMOTE_BOTH,
  CONV_PROMOTE_WIDER
};
*/

enum typespec_index {
  TYPESPEC_INDEX_INT = 0,
  TYPESPEC_INDEX_CHAR,
  TYPESPEC_INDEX_SHORT,
  TYPESPEC_INDEX_LONG,
  TYPESPEC_INDEX_LONG_LONG,
  TYPESPEC_INDEX_SIGNED,
  TYPESPEC_INDEX_UNSIGNED,
  TYPESPEC_INDEX_VOID,
  TYPESPEC_INDEX_STRUCT,
  TYPESPEC_INDEX_UNION,
  TYPESPEC_INDEX_ENUM,
  /* storage class */
  TYPESPEC_INDEX_REGISTER,
  TYPESPEC_INDEX_STATIC,
  TYPESPEC_INDEX_EXTERN,
  TYPESPEC_INDEX_AUTO,
  /* qualifiers */
  TYPESPEC_INDEX_CONST,
  TYPESPEC_INDEX_VOLATILE,
  /* function only */
  TYPESPEC_INDEX_INLINE,
  /* number of type specifiers */
  TYPESPEC_INDEX_COUNT
};

enum typespec_flag {
  TYPESPEC_FLAG_NONE = 0,
  TYPESPEC_FLAG_INT = 1 << TYPESPEC_INDEX_INT,
  TYPESPEC_FLAG_CHAR = 1 << TYPESPEC_INDEX_CHAR,
  TYPESPEC_FLAG_SHORT = 1 << TYPESPEC_INDEX_SHORT,
  TYPESPEC_FLAG_LONG = 1 << TYPESPEC_INDEX_LONG,
  TYPESPEC_FLAG_LONG_LONG = 1 << TYPESPEC_INDEX_LONG_LONG,
  TYPESPEC_FLAG_SIGNED = 1 << TYPESPEC_INDEX_SIGNED,
  TYPESPEC_FLAG_UNSIGNED = 1 << TYPESPEC_INDEX_UNSIGNED,
  TYPESPEC_FLAG_VOID = 1 << TYPESPEC_INDEX_VOID,
  TYPESPEC_FLAG_STRUCT = 1 << TYPESPEC_INDEX_STRUCT,
  TYPESPEC_FLAG_UNION = 1 << TYPESPEC_INDEX_UNION,
  TYPESPEC_FLAG_ENUM = 1 << TYPESPEC_INDEX_ENUM,
  /* storage class */
  TYPESPEC_FLAG_REGISTER = 1 << TYPESPEC_INDEX_REGISTER,
  TYPESPEC_FLAG_STATIC = 1 << TYPESPEC_INDEX_STATIC,
  TYPESPEC_FLAG_EXTERN = 1 << TYPESPEC_INDEX_EXTERN,
  TYPESPEC_FLAG_AUTO = 1 << TYPESPEC_INDEX_AUTO,
  /* qualifiers */
  TYPESPEC_FLAG_CONST = 1 << TYPESPEC_INDEX_CONST,
  TYPESPEC_FLAG_VOLATILE = 1 << TYPESPEC_INDEX_VOLATILE,
  /* function only */
  TYPESPEC_FLAG_INLINE = 1 << TYPESPEC_INDEX_INLINE
};

/* Structs and unions require a member map, as well as the order of the members.
 * This would be best provided by an ordered_map data structure.
 */
typedef struct auxspec {
  union {
    const char *type_id;
    struct {
      unsigned int qualifiers;
      size_t length;
    } ptr_or_arr;
    struct {
      LinkedList members;
      Map *symbol_table;
    } composite;
    LinkedList params;
  } data;
  AuxType aux;
} AuxSpec;

/*
 * auxinfo will contain information about structs/unions, pointers, functions,
 * typedefs, and arrays.
 */
typedef struct typespec {
  size_t width;
  size_t alignment;
  LinkedList auxspecs;
  unsigned int flags;
  BaseType base;
} TypeSpec;

typedef struct location {
  size_t filenr;
  size_t linenr;
  size_t offset;
  size_t blocknr;
} Location;

/* TODO(Robert): maybe define this in some other common file? */
extern const size_t MAX_IDENT_LEN;
extern const TypeSpec SPEC_PTR;
extern const TypeSpec SPEC_FUNCTION;
extern const TypeSpec SPEC_STRUCT;
extern const TypeSpec SPEC_EMPTY;
extern const TypeSpec SPEC_VOID;
extern const TypeSpec SPEC_CHAR;
extern const TypeSpec SPEC_ULONG;
extern const TypeSpec SPEC_LONG;
extern const TypeSpec SPEC_UINT;
extern const TypeSpec SPEC_INT;
extern const TypeSpec SPEC_USHRT;
extern const TypeSpec SPEC_SHRT;
extern const TypeSpec SPEC_UCHAR;
extern const TypeSpec SPEC_SCHAR;

extern const Location LOC_EMPTY;

int attributes_to_string(const unsigned int attributes, char *buf, size_t size);
int location_to_string(const Location *loc, char *buf, size_t size);
int flags_to_string(const unsigned int flags, char *buf, size_t size);
int type_to_string(const TypeSpec *type, char *buf, size_t size);

int auxspec_destroy(AuxSpec *auxspec);
int auxspec_copy(AuxSpec *dest, const AuxSpec *src);

int typespec_init(TypeSpec *spec);
int typespec_destroy(TypeSpec *spec);
int typespec_copy(TypeSpec *dst, const TypeSpec *src);
int strip_aux_type(TypeSpec *dest, const TypeSpec *src);

int typespec_is_arithmetic(const TypeSpec *type);
int typespec_is_integer(const TypeSpec *type);
int typespec_is_pointer(const TypeSpec *type);
int typespec_is_array(const TypeSpec *type);
int typespec_is_function(const TypeSpec *type);
int typespec_is_scalar(const TypeSpec *type);
int typespec_is_voidptr(const TypeSpec *type);
int typespec_is_fnptr(const TypeSpec *type);
int typespec_is_struct(const TypeSpec *type);
int typespec_is_structptr(const TypeSpec *type);
int typespec_is_union(const TypeSpec *type);
int typespec_is_unionptr(const TypeSpec *type);
#endif
