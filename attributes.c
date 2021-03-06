#include "attributes.h"

#include "stdio.h"

char attr_map[][32] = {"void",   "int",    "null",      "string",
                       "struct", "array",  "function",  "variable",
                       "field",  "typeid", "parameter", "local",
                       "lval",   "const",  "vreg",      "vaddr"};

void print_attributes(FILE *out, const int *attributes, const char *type_id) {
  for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes[i]) {
      fprintf(out, " %s", attr_map[i]);
      if (i == ATTR_STRUCT) fprintf(out, "(%s)", type_id);
    }
  }
}
