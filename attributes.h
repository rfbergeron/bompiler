#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__
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
 * - identifiers
 * - the result of the arrow operator
 * - the result of the dot operator
 * - the result of the dereference operator
 * - the result of the array indexing operator
 */
enum attribute {
  ATTR_EXPR_VREG = 1 << 0, /* does this node require a virtual register */
  ATTR_EXPR_LVAL = 1 << 1, /* does this node refer to an assignable location */
  ATTR_EXPR_CONST =
      1 << 2, /* does this node refer to a compile-time constant */
};

enum base_type {
  TYPE_NONE,
  /* arithmetic types */
  TYPE_SIGNED,
  TYPE_UNSIGNED,
  TYPE_FLOAT,
  TYPE_DOUBLE,
  /* compound types */
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_FUNCTION,
  /* other types */
  TYPE_TYPEDEF,
  TYPE_VOID
};

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

enum type_flag {
  TYPE_FLAG_NONE = 0,
  /* storage class */
  TYPE_FLAG_REGISTER = 1 << 0,
  TYPE_FLAG_STATIC = 1 << 1,
  TYPE_FLAG_EXTERN = 1 << 2,
  TYPE_FLAG_AUTO = 1 << 2,
  /* qualifiers */
  TYPE_FLAG_CONST = 1 << 4,
  TYPE_FLAG_VOLATILE = 1 << 5,
  /* function only */
  TYPE_FLAG_INLINE = 1 << 6
};

typedef struct typespec {
  size_t width;
  /*
   * 1. only structures and unions need their alignment specified
   * 2. only structures and unions need a map
   * 3. only arrays need length
   * 4. only functions need  parameters
   * 5. only functions, pointers, and arrays need nested types
   */
  size_t alignment;
  struct typespec *nested; /* for functions, pointers and arrays */
  union {
    struct llist *params; /* for functions */
    struct map *members;  /* for structs and unions */
    size_t length;        /* for arrays */
  } data;
  unsigned int flags;
  enum base_type base;
  const char *identifier;
} TypeSpec;

typedef struct location {
  size_t filenr;
  size_t linenr;
  size_t offset;
  size_t blocknr;
} Location;

int attributes_to_string(const unsigned int attributes, char *buf,
                         size_t bufsize);
int type_to_string(const TypeSpec *type, char *buf, size_t bufsize);

extern const TypeSpec SPEC_PTR;
extern const TypeSpec SPEC_EMPTY;
extern const TypeSpec SPEC_FUNCTION;
extern const TypeSpec SPEC_INT;

extern const Location LOC_EMPTY;

#endif
