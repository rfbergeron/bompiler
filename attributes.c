#include "attributes.h"

#include "stdio.h"

char attr_map[][32] = {"void",   "int",    "null",      "string",
                       "struct", "array",  "function",  "variable",
                       "field",  "typeid", "parameter", "local",
                       "lval",   "const",  "vreg",      "vaddr"};

/* we'll simplify matters internally by limiting base types to structs, unions,
 * ints (which are also considered chars), floats, doubles, void, and pointers
 * of each of these types.
 *
 * other types and type qualifiers will be implemented as flags on the typespec
 *
 * arrays are represented internally as pointers, since in the end they are just
 * stack-allocated pointers
 *
 * maybe don't even have an attributes bit field, just store the base type and
 * the width
 */
const char type_map[][16] = {"void", "int", "float", "struct", "union"};

/* qualifiers/specifiers that are flags:
 * - signed/unsigned
 * - wider/narrower types
 * - pointer level
 */
void print_attributes(FILE *out, const int *attributes, const char *type_id) {
  for (size_t i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes[i]) {
      fprintf(out, " %s", attr_map[i]);
      if (i == ATTR_STRUCT) fprintf(out, "(%s)", type_id);
    }
  }
}
