#include "attributes.h"

#include "stdio.h"
#include "string.h"

static const size_t NUM_ATTRIBUTES = 6;
static const size_t NUM_TYPES = 4;

const char type_map[][16] = {"void", "int", "float", "struct"};

const char attr_map[][16] = {"reg", "lval", "rval", "addr", "const"};

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
  return 0;
}

int type_to_string(struct typespec type, char *buf, size_t bufsize) {
  sprintf(buf, " %s", type_map[type.base]);
  return 0;
}
