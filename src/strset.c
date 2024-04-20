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

static StringSet *input_text;
static StringSet *generated_text;

void string_set_init_globals() {
  input_text = string_set_init(DEFAULT_SIZE);
  assert(input_text != NULL);
  generated_text = string_set_init(DEFAULT_SIZE);
  assert(generated_text != NULL);
}

void string_set_free_globals() {
  size_t i, string_count = string_set_count(input_text);
  for (i = 0; i < string_count; ++i) {
    char *str;
    (void)string_set_at(input_text, i, &str);
    /* key and value are identical so we only need to free the value */
    free(str);
  }
  string_set_destroy(input_text);

  string_count = string_set_count(generated_text);
  for (i = 0; i < string_count; ++i) {
    char *str;
    (void)string_set_at(generated_text, i, &str);
    free(str);
  }
  string_set_destroy(generated_text);
}

static const char *deduplicate_text(StringSet *strings, const char *string) {
  assert(strlen(string) > 0);
  char *val;
  int exists = string_set_get(strings, string, &val);
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
    string_set_put(strings, val, val);
    return val;
  }
}

const char *string_set_intern(const char *string) {
  return deduplicate_text(input_text, string);
}

const char *gen_string_intern(const char *string) {
  return deduplicate_text(generated_text, string);
}

int string_set_print(FILE *out) {
  PFDBG0('s', "Printing string set");
  size_t string_index, string_count = string_set_count(input_text);
  int total = 0;
  for (string_index = 0; string_index < string_count; ++string_index) {
    const char *str;
    (void)string_set_key_at(input_text, string_index, &str);
    int printed = fprintf(out, "%p->\"%s\"\n", (void *)str, str);
    if (printed < 0)
      return printed;
    else
      total += printed;
  }
  return total;
}
