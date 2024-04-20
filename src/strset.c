#include "strset.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "murmur3/murmur3.h"
#include "taboe_impl.h"

/* neighborhood size and starting map size. if we started the map at a smaller
 * size, virtual buckets would overlap, resulting in errors
 */
#define DEFAULT_SIZE (sizeof(unsigned long) * CHAR_BIT)

static unsigned long murmur_wrapper(const char *str) {
  /* entirely unnecessary but i think it's funny */
  static const unsigned int KNUTH_MAGIC = 0x9E3779B9U;
  size_t len = strlen(str);
  unsigned long out[2];
  MurmurHash3_x64_128(str, len, KNUTH_MAGIC, out);
  return out[0] ^ out[1];
}

TABOE_TDEF(StringSet)
TABOE_TYPE(StringSet, const char *, char *, unsigned long)
TABOE_IMPL(DEFAULT_SIZE, StringSet, string_set, const char *, char *,
           unsigned long, murmur_wrapper, strcmp)

static StringSet *string_set;

void string_set_init_globals() {
  string_set = string_set_init(DEFAULT_SIZE);
  assert(string_set != NULL);
}

void string_set_free_globals() {
  size_t i, string_count = string_set_count(string_set);
  for (i = 0; i < string_count; ++i) {
    char *str;
    (void)string_set_at(string_set, i, &str);
    /* key and value are identical so we only need to free the value */
    free(str);
  }
  string_set_destroy(string_set);
}

const char *string_set_intern(const char *string) {
  assert(strlen(string) > 0);
  char *val;
  int exists = string_set_get(string_set, string, &val);
  if (exists) {
    assert(strcmp(string, val) == 0);
    return val;
  } else {
    size_t len = strlen(string);
    val = malloc((len + 1) * sizeof(char));
    memcpy(val, string, len + 1);
    PFDBG3(
        's',
        "First apearance of string %s, length %lu; duplicated and stored in %p",
        val, len, val);
    string_set_put(string_set, val, val);
    return val;
  }
}

int string_set_print(FILE *out) {
  PFDBG0('s', "Printing string set");
  size_t string_index, string_count = string_set_count(string_set);
  int total = 0;
  for (string_index = 0; string_index < string_count; ++string_index) {
    const char *str;
    (void)string_set_key_at(string_set, string_index, &str);
    int printed = fprintf(out, "%p->\"%s\"\n", (void *)str, str);
    if (printed < 0)
      return printed;
    else
      total += printed;
  }
  return total;
}
