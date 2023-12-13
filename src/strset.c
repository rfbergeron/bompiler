#include "strset.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "badalist.h"
#include "badmap.h"
#include "debug.h"

#define MAX_TOKEN_LEN 4095

static const size_t starting_size = 100;
static Map string_set = {0};

static int strncmp_wrapper(void *s1, void *s2) {
  int ret = 0;
  if (!s1 || !s2) {
    ret = s1 == s2;
  } else {
    ret = !strncmp(s1, s2, MAX_TOKEN_LEN);
  }
  return ret;
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
    PFDBG3(
        's',
        "First apearance of string %s, length %lu; duplicated and stored in %p",
        ret, len, ret);
    map_insert(&string_set, ret, len, ret);
  } else {
    if (strcmp(string, ret) != 0) {
      fprintf(stderr, "fuck you\n");
      abort();
    }
  }

  return ret;
}

int string_set_print(FILE *out) {
  PFDBG0('s', "Printing string set");
  ArrayList key_list;
  assert(!alist_init(&key_list, map_size(&string_set)));
  assert(!map_keys(&string_set, &key_list));

  size_t i;
  for (i = 0; i < alist_size(&key_list); ++i) {
    char *key = alist_get(&key_list, i);
    size_t map_location[] = {-1, -1};
    map_find(&string_set, key, strlen(key), map_location);
    fprintf(out, "string_set[%4lu,%4lu]: %p->\"%s\"\n", map_location[0],
            map_location[1], (void *)key, key);
  }
  assert(!alist_destroy(&key_list, NULL));
  return 0;
}
