#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__
#include "stdio.h"

/* attributes correspond to array indices in the order they are listed here */
enum attribute {
  ATTR_VREG = 1 << 0,
  ATTR_LVAL = 1 << 1,
  ATTR_RVAL = 1 << 2,
  ATTR_VADDR = 1 << 3,
  ATTR_CAST = 1 << 4,
  ATTR_CONST = 1 << 5,
};

enum base_type {
  /* arithmetic types */
  TYPE_INT,
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
  /* signedness */
  TYPE_FLAG_SIGNED = 1 << 0,
  TYPE_FLAG_UNSIGNED = 1 << 1,
  /* storage class */
  TYPE_FLAG_REGISTER = 1 << 2,
  TYPE_FLAG_STATIC = 1 << 3,
  TYPE_FLAG_EXTERN = 1 << 4,
  TYPE_FLAG_AUTO = 1 << 5,
  /* qualifiers */
  TYPE_FLAG_CONST = 1 << 6,
  TYPE_FLAG_VOLATILE = 1 << 7,
  /* function only */
  TYPE_FLAG_INLINE = 1 << 8
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
int type_to_string(struct typespec type, char *buf, size_t bufsize);

/* NOTE: it may be easier to give all structures and unions 8-byte alignment
 * requirements to make conversions require zero extra code generation
 */

extern const size_t SIZEOF_LONG, ALIGNOF_LONG;
extern const size_t SIZEOF_INT, ALIGNOF_INT;
extern const size_t SIZEOF_SHORT, ALIGNOF_SHORT;
extern const size_t SIZEOF_CHAR, ALIGNOF_CHAR;

extern const char STRING_ULONG[];
extern const char STRING_SLONG[];
extern const char STRING_UINT[];
extern const char STRING_SINT[];
extern const char STRING_USHORT[];
extern const char STRING_SSHORT[];
extern const char STRING_UCHAR[];
extern const char STRING_SCHAR[];

extern const TypeSpec TYPE_ULONG;
extern const TypeSpec TYPE_SLONG;
extern const TypeSpec TYPE_UINT;
extern const TypeSpec TYPE_SINT;
extern const TypeSpec TYPE_USHORT;
extern const TypeSpec TYPE_SSHORT;
extern const TypeSpec TYPE_UCHAR;
extern const TypeSpec TYPE_SCHAR;

extern const TypeSpec TYPE_PTR;
extern const TypeSpec TYPE_EMPTY;

extern const Location LOC_EMPTY;

#endif
