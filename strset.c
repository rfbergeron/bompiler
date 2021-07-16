#include "strset.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "attributes.h"
#include "badlib/badmap.h"
#include "debug.h"

static const size_t starting_size = 100;
static const char *entry_format = "string_set[%4d,%4d]: %p->\"%s\"\n";
static Map string_set = {0};
static FILE *strfile;

static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_IDENT_LEN);
  }
  return ret;
}

void dump_string(void *string) {
  size_t map_location[] = {-1, -1};
  map_find(&string_set, string, strnlen(string, MAX_IDENT_LEN), map_location);
  int result = fprintf(strfile, entry_format, map_location[0], map_location[1],
                       string, (char *)string);
}

/* the key and value will point to the same thing so only one of the destructors
 * should be set.
 */
void string_set_init_globals() {
  int status =
      map_init(&string_set, starting_size, free, NULL, strncmp_wrapper);
  if (status) {
    fprintf(stderr, "fuck you\n");
    abort();
  }
}

void string_set_free_globals() { map_destroy(&string_set); }

const char *string_set_intern(const char *string) {
  const size_t len = strlen(string);
  if (len <= 0) {
    fprintf(stderr, "This string (%s) is too fucking short.\n", string);
    abort();
  }
  char *ret = map_get(&string_set, (char *)string, len);
  if (!ret) {
    ret = malloc((len + 1) * sizeof(char));
    memcpy(ret, string, len + 1);
    DEBUGS(
        's',
        "First apearance of string %s, length %lu; duplicated and stored in %p",
        ret, len, ret);
    map_insert(&string_set, ret, len, ret);
  } else {
    const size_t len2 = strnlen(ret, MAX_IDENT_LEN);
    if (len2 != len) {
      fprintf(stderr, "fuck you\n");
      abort();
    }
  }

  return ret;
}

void string_set_dump(FILE *out) {
  DEBUGS('s', "Dumping string set");
  strfile = out;
  map_foreach_key(&string_set, dump_string);
}
