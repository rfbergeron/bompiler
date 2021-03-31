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

enum type_mod {
  /* storage class */
  TYPE_MOD_REGISTER = 1 << 5,
  TYPE_MOD_STATIC = 1 << 6,
  TYPE_MOD_EXTERN = 1 << 7,
  TYPE_MOD_AUTO = 1 << 8,
  TYPE_MOD_TYPEDEF = 1 << 9,
  /* qualifiers */
  TYPE_MOD_CONST = 1 << 10,
  TYPE_MOD_RESTRICT = 1 << 11,
  TYPE_MOD_VOLATILE = 1 << 12,
  /* function only */
  TYPE_MOD_INLINE = 1 << 13
};

struct typespec {
  enum base_type base;
  unsigned int modifiers;
  size_t width;
  /* it may be easier to give all structures and unions 8-byte alignment
   * requirements to make conversions require zero extra code generation
   */
  size_t alignment;
  size_t length; /* only used for arrays */
  /* Possible values of this pointer depending on base type:
   * pointers: nested typespec
   * arrays: nested typespec
   * function: llist of parameters
   * struct: symbol table of members
   * union: symbol table of members
   * typedef: nested typespec
   */
  void *data;
  const char *identifier;
};

int attributes_to_string(const unsigned int attributes, char *buf,
                         size_t bufsize);
int type_to_string(struct typespec type, char *buf, size_t bufsize);
#endif
