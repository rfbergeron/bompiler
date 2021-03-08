#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__
#include "stdio.h"

// attributes correspond to array indices in the order they are listed here
struct typespec {
  enum base_type base;
  unsigned int modifiers;
  size_t width;
  const char *name;
};

enum attribute {
    ATTR_REG = 1 << 0,
    ATTR_LVAL = 1 << 1,
    ATTR_RVAL = 1 << 2,
    ATTR_ADDR = 1 << 3,
    ATTR_CAST = 1 << 4,
    ATTR_CONST = 1 << 5
};

enum base_type {
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM
};

enum type_mod {
    /* width and signage */
    TYPE_MOD_SIGNED = 1 << 0,
    TYPE_MOD_UNSIGNED = 1 << 1,
    TYPE_MOD_SHORT = 1 << 2,
    TYPE_MOD_LONG = 1 << 3,
    TYPE_MOD_LONG_LONG = 3 << 3, /* set long and long long together */
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

void print_attributes(FILE *out, const int *attributes, const char *type_id);
#endif
