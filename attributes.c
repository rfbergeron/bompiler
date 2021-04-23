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
const size_t SIZEOF_LONG = 8, ALIGNOF_LONG = 8;
const size_t SIZEOF_INT = 4, ALIGNOF_INT = 4;
const size_t SIZEOF_SHORT = 2, ALIGNOF_SHORT = 2;
const size_t SIZEOF_CHAR = 1, ALIGNOF_CHAR = 1;

const char STRING_ULONG[] = "unsigned long int";
const char STRING_SLONG[] = "signed long int";
const char STRING_UINT[] = "unsigned int";
const char STRING_SINT[] = "signed int";
const char STRING_USHORT[] = "unsigned short int";
const char STRING_SSHORT[] = "signed short int";
const char STRING_UCHAR[] = "unsigned char";
const char STRING_SCHAR[] = "signed char";

const TypeSpec TYPE_ULONG = {SIZEOF_LONG, ALIGNOF_LONG,       NULL,
                             NULL,        TYPE_FLAG_UNSIGNED, TYPE_INT,
                             STRING_ULONG};
const TypeSpec TYPE_SLONG = {SIZEOF_LONG,      ALIGNOF_LONG, NULL,        NULL,
                             TYPE_FLAG_SIGNED, TYPE_INT,     STRING_SLONG};
const TypeSpec TYPE_UINT = {SIZEOF_INT,         ALIGNOF_INT, NULL,       NULL,
                            TYPE_FLAG_UNSIGNED, TYPE_INT,    STRING_UINT};
const TypeSpec TYPE_SINT = {SIZEOF_INT,       ALIGNOF_INT, NULL,       NULL,
                            TYPE_FLAG_SIGNED, TYPE_INT,    STRING_SINT};
const TypeSpec TYPE_USHORT = {SIZEOF_SHORT, ALIGNOF_SHORT,      NULL,
                              NULL,         TYPE_FLAG_UNSIGNED, TYPE_INT,
                              STRING_USHORT};
const TypeSpec TYPE_SSHORT = {SIZEOF_SHORT, ALIGNOF_SHORT,    NULL,
                              NULL,         TYPE_FLAG_SIGNED, TYPE_INT,
                              STRING_SSHORT};
const TypeSpec TYPE_UCHAR = {SIZEOF_CHAR, ALIGNOF_CHAR,       NULL,
                             NULL,        TYPE_FLAG_UNSIGNED, TYPE_INT,
                             STRING_UCHAR};
const TypeSpec TYPE_SCHAR = {SIZEOF_CHAR,      ALIGNOF_CHAR, NULL,        NULL,
                             TYPE_FLAG_SIGNED, TYPE_INT,     STRING_SCHAR};

const TypeSpec TYPE_PTR = {SIZEOF_LONG,    ALIGNOF_LONG, NULL,        NULL,
                           TYPE_FLAG_NONE, TYPE_POINTER, "_ptr_empty"};
const TypeSpec TYPE_EMPTY = {0,         0,       NULL, NULL, TYPE_FLAG_NONE,
                             TYPE_VOID, "_empty"};

const Location LOC_EMPTY = {0, 0, 0, 0};

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
