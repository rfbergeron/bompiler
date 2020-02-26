#include "strset.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"
#include "klib/khash.h"

static const size_t starting_set_size = 100;
static const char *entry_format = "string_set[%4d]: %22p->\"%s\"\n";
KHASH_SET_INIT_STR (str);
khash_t (str) * string_set;

void string_set_init_globals () { string_set = kh_init (str); }

void string_set_free_globals () {
    for (khint_t k = 0; k < kh_end (string_set); ++k) {
        if (kh_exist (string_set, k)) free ((char *) kh_key (string_set, k));
    }
    kh_destroy (str, string_set);
}

const char *string_set_intern (const char *string) {
    int absent;
    khint_t key = kh_put (str, string_set, string, &absent);
    if (absent) kh_key (string_set, key) = strdup (string);
    return (const char *) kh_key (string_set, key);
}

void string_set_dump (FILE *out) {
    DEBUGS ('s', "Dumping string set");
    for (khint_t k = 0; k < kh_end (string_set); ++k) {
        if (kh_exist (string_set, k)) {
            const char *token = (const char *) kh_key (string_set, k);
            fprintf (out, entry_format, k, (void *) token, token);
        }
    }
}
