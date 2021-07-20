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
 * - identifiers
 * - the result of the arrow operator
 * - the result of the dot operator
 * - the result of the dereference operator
 * - the result of the array indexing operator
 */
enum attribute {
  ATTR_NONE = 0,            /* no attributes set */
  ATTR_EXPR_VREG = 1 << 0,  /* requires a virtual register */
  ATTR_EXPR_LVAL = 1 << 1,  /* refers to an assignable location */
  ATTR_EXPR_CONST = 1 << 2, /* refers to a compile-time constant */
  ATTR_EXPR_BOOL = 1 << 3,  /* int guaranteed to be 0 or 1 */
  ATTR_EXPR_VADDR = 1 << 4, /* int is memory address */
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
    struct map members;  /* for structs and unions */
    struct llist params; /* for functions */
    size_t length;       /* for arrays */
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

/* TODO(Robert): maybe define this in some other common file? */
extern const size_t MAX_IDENT_LEN;
extern const TypeSpec SPEC_PTR;
extern const TypeSpec SPEC_EMPTY;
extern const TypeSpec SPEC_FUNCTION;
extern const TypeSpec SPEC_STRUCT;
extern const TypeSpec SPEC_ULONG;
extern const TypeSpec SPEC_LONG;
extern const TypeSpec SPEC_UINT;
extern const TypeSpec SPEC_INT;
extern const TypeSpec SPEC_USHRT;
extern const TypeSpec SPEC_SHRT;
extern const TypeSpec SPEC_UCHAR;
extern const TypeSpec SPEC_CHAR;

extern const Location LOC_EMPTY;

int attributes_to_string(const unsigned int attributes, char *buf,
                         size_t bufsize);
void location_to_string(const Location *loc, char *buffer, size_t size);
int type_to_string(const TypeSpec *type, char *buf, size_t bufsize);
int typespec_copy(TypeSpec *dst, const TypeSpec *src);
int typespec_destroy(TypeSpec *type);

#endif
