#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__
#include "stdio.h"

// attributes correspond to array indices in the order they are listed here
enum attr {
  ATTR_VOID = 1 << 0,
  ATTR_INT = 1 << 1,
  ATTR_CHAR = 1 << 3,
  ATTR_SHORT,
  ATTR_LONG,
  ATTR_DOUBLE,
  ATTR_FLOAT,
  ATTR_SIGNED,
  ATTR_UNSIGNED,
  ATTR_STRUCT = 1 << 4,
  ATTR_ARRAY = 1 << 5,
  ATTR_FUNCTION = 1 << 6,
  ATTR_VARIABLE = 1 << 7,
  ATTR_FIELD = 1 << 8,
  ATTR_TYPEID = 1 << 9,
  ATTR_PARAM = 1 << 10,
  ATTR_LOCAL = 1 << 11,
  ATTR_LVAL = 1 << 12,
  ATTR_CONST = 1 << 13,
  ATTR_VREG = 1 << 14,
  ATTR_VADDR = 1 << 15
};

struct typespec {
  unsigned int flags;
  size_t width;
  const char *name;
};

extern char attr_map[][32];

void print_attributes(FILE *out, const int *attributes, const char *type_id);
#endif
