#include "attributes.h"

#include "stdio.h"
#include "string.h"

struct conversion_entry {
  enum base_type from;
  enum base_type to;
  enum conversion_type conversion;
};

static const size_t NUM_ATTRIBUTES = 6;
static const size_t NUM_TYPES = 4;

const char type_map[][16] = {"void", "int", "float", "struct"};

const char attr_map[][16] = {"reg", "lval", "rval", "addr", "const"};

/* Precedence for conversions:
 * 1. long double
 * 2. double
 * 3. float
 * 4. long unsigned int
 * 5. long int
 * 6. unsigned int
 * 7. int
 *
 * All other arithmetic types get promoted to int.
 *
 * The list includes valid conversions, including both those that do and do not
 * need additional operations to make them compatible.
 *
 * Compound types cannot be implicitly casted and do not need to be converted in
 * the same way that arithmetic types do. The only exception to this are
 * pointers, which may be implicitly converted to and from void pointers.
 *
 * When the assembly generator emits a promotion,
 * Assignment expressions need
 */
const struct conversion_entry conversion_list[] = {
    {TYPE_SIGNED, TYPE_UNSIGNED, CONV_PROMOTE_WIDER},
    {TYPE_FLOAT, TYPE_UNSIGNED, CONV_EXPLICIT_CAST},
    {TYPE_FLOAT, TYPE_SIGNED}};

int attributes_to_string(const unsigned int attributes, char *buf,
                         size_t bufsize) {
  size_t i, buf_index = 0;
  for (i = 0; i < NUM_ATTRIBUTES; ++i) {
    if (attributes & (1 << i)) {
      if (buf_index + strlen(attr_map[i]) > bufsize) {
        fprintf(stderr, "WARN: buffer too small to print all attributes");
        return 1;
      } else {
        buf_index += sprintf(buf, " %s", attr_map[i]);
      }
    }
  }
  buf[buf_index] = 0;
  return 0;
}

int type_to_string(struct typespec type, char *buf, size_t bufsize) {
  sprintf(buf, " %s", type_map[type.base]);
  return 0;
}
