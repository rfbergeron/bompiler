#include "attributes.h"

#include "badlib/badllist.h"
#include "stdio.h"
#include "string.h"
#include "symtable.h"

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
const signed char BOMP_SCHAR_MAX = (1 << 7) - 1;
const signed char BOMP_SCHAR_MIN = ((1 << 7) - 1) * -1;

const char STRING_ULONG[] = "unsigned long int";
const char STRING_SLONG[] = "signed long int";
const char STRING_UINT[] = "unsigned int";
const char STRING_SINT[] = "signed int";
const char STRING_USHORT[] = "unsigned short int";
const char STRING_SSHORT[] = "signed short int";
const char STRING_UCHAR[] = "unsigned char";
const char STRING_SCHAR[] = "signed char";

const TypeSpec SPEC_PTR = {SIZEOF_LONG,    ALIGNOF_LONG, NULL,        NULL,
                           TYPE_FLAG_NONE, TYPE_POINTER, "_ptr_empty"};
const TypeSpec SPEC_EMPTY = {0,         0,       NULL, NULL, TYPE_FLAG_NONE,
                             TYPE_NONE, "_empty"};
const TypeSpec SPEC_FUNCTION = {
    0, 0, NULL, NULL, TYPE_FLAG_NONE, TYPE_FUNCTION, "_function"};
const TypeSpec SPEC_INT = {SIZEOF_INT,     ALIGNOF_INT, NULL,        NULL,
                           TYPE_FLAG_NONE, TYPE_SIGNED, "signed int"};

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

int type_to_string(const TypeSpec *type, char *buf, size_t bufsize) {
  int ret = 0;
  int i = 0;
  switch (type->base) {
    case TYPE_VOID:
      ret = sprintf(buf, "void");
      break;
    case TYPE_FUNCTION:
      ret += type_to_string(type->nested, buf, bufsize);
      ret += sprintf((buf + ret), " ()(");
      if (llist_size(type->data.params) > 0) {
        for (i = 0; i < llist_size(type->data.params); ++i) {
          SymbolValue *param_symval = llist_get(type->data.params, i);
          ret +=
              type_to_string(&(param_symval->type), (buf + ret), bufsize - ret);
          if (i + 1 < llist_size(type->data.params)) {
            ret += sprintf((buf + ret), ", ");
          }
        }
        ret += sprintf((buf + ret), ")");
      } else {
        ret += sprintf((buf + ret), "void)");
      }
      break;
    case TYPE_NONE:
      buf[0] = 0;
      break;
    case TYPE_SIGNED:
      switch (type->width) {
        case 8:
          ret = sprintf(buf, "signed long");
          break;
        case 4:
          ret = sprintf(buf, "signed int");
          break;
        case 2:
          ret = sprintf(buf, "signed short");
          break;
        case 1:
          ret = sprintf(buf, "signed char");
          break;
        default:
          fprintf(stderr, "ERROR: Unknown width of signed type: %lu\n",
                  type->width);
          break;
      }
      break;
    case TYPE_UNSIGNED:
      switch (type->width) {
        case 8:
          ret = sprintf(buf, "unsigned long");
          break;
        case 4:
          ret = sprintf(buf, "unsigned int");
          break;
        case 2:
          ret = sprintf(buf, "unsigned short");
          break;
        case 1:
          ret = sprintf(buf, "unsigned char");
          break;
        default:
          fprintf(stderr, "ERROR: Unknown width of unsigned type: %lu\n",
                  type->width);
          break;
      }
      break;
    default:
      break;
  }
  return ret;
}
