#include "strset.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "attributes.h"
#include "badlib/badmap.h"

static const size_t MAX_STRING_LENGTH = 31;
static const size_t starting_size = 100;
static const char *entry_format = "string_set[%4d,%4d]: %22p->\"%s\"\n";
static Map string_set = {0};
extern FILE* strfile;

static int strncmp_wrapper (void *s1, void *s2) {
    return strncmp(s1, s2, MAX_STRING_LENGTH);
}

void dump_string (void *string, void *unused, size_t i, size_t j) {
    fprintf (strfile, entry_format, i, j, string, (char*)string);
}

/* the key and value will point to the same thing so only one of the destructors
 * should be set.
 */
void string_set_init_globals () { map_init(&string_set, starting_size, free, NULL, strncmp_wrapper); }

void string_set_free_globals () {
    map_destroy(&string_set);
}

const char *string_set_intern (const char *string) {
    const size_t len = strnlen(string, MAX_STRING_LENGTH);
    const char *ret = map_get(&string_set, (char*)string, len);

    if (!ret) {
        ret = strdup(string);
        map_insert(&string_set, (char*)string, len, (char*)string);
    }

    return ret;
}

void string_set_dump (FILE *out) {
    DEBUGS ('s', "Dumping string set");
    map_foreach(&string_set, dump_string);
}
