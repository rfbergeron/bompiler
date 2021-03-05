#include "strset.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "attributes.h"
#include "badlib/badmap.h"
#include "debug.h"

static const size_t MAX_STRING_LENGTH = 31;
static const size_t starting_size = 100;
static const char *entry_format = "string_set[%4d,%4d]: %p->\"%s\"\n";
Map string_set = {0};
extern FILE *strfile;

static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_STRING_LENGTH);
  }
  DEBUGS('w', "Comparing strings '%s' and '%s': %i", s1, s2, ret);
  return ret;
}

void dump_string(void *string, void *unused, size_t i, size_t j) {
  int result = fprintf(strfile, entry_format, i, j, string, (char *)string);
}

/* the key and value will point to the same thing so only one of the destructors
 * should be set.
 */
void string_set_init_globals() {
  map_init(&string_set, starting_size, free, NULL, strncmp_wrapper);
}

void string_set_free_globals() { map_destroy(&string_set); }

const char *string_set_intern(const char *string) {
  DEBUGS('s', "Evaluating status of string '%s'", string);
  const size_t len = strnlen(string, MAX_STRING_LENGTH);
  if (len <= 0) abort();
  const char *ret = map_get(&string_set, (char *)string, len);

  if (!ret) {
    ret = strndup(string, len);
    DEBUGS('s',
           "First apearance of string '%s', length %i; duplicated and stored "
           "in %p",
           ret, len, ret);
    map_insert(&string_set, (char *)ret, len, (char *)ret);
  } else {
    const size_t len2 = strnlen(ret, MAX_STRING_LENGTH);
    if (len2 != len) {
      fprintf(stderr, "fuck you\n");
      abort();
    }
    DEBUGS('s', "String '%s', length %i already present; address: %p", ret, len,
           ret);
  }

  return ret;
}

void string_set_dump(FILE *out) {
  DEBUGS('s', "Dumping string set");
  map_foreach(&string_set, dump_string);
}
