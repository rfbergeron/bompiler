#include "string_set.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"
#include "map.h"

static const size_t starting_set_size = 100;
static const char *entry_format = "string_set[%4d]: %22p->\"%s\"\n";
static struct map *set;

void string_set_init_globals () { set = map_init (starting_set_size); }

const char **string_set_get_existing (const char *string) {
    for (size_t i = 0; i < set->size; ++i) {
        const char **current_ptr = *(set->values + i);
        if (current_ptr == NULL) continue;
        if (strcmp (string, *current_ptr) == 0) {
            DEBUGS ('s', "Strings equal: %s %s", string, *current_ptr);
            return current_ptr;
        }
    }
    return NULL;
}

void string_set_free_globals () {
    for (size_t i = 0; i < set->size; ++i) {
        char **string_ptr = (char **) *(set->values + i);

        if (string_ptr != NULL) {
            if (*(string_ptr) != NULL) free (*(string_ptr));
            free (string_ptr);
        }
    }
    map_free (set);
}

const char **string_set_intern (const char *string) {
    const char **existing_entry = string_set_get_existing (string);
    if (existing_entry != NULL) return existing_entry;

    const char **string_ptr = (const char **) malloc (sizeof (char *));
    *(string_ptr) = strdup (string);
    map_put (set, string_ptr, string_ptr);
    return string_ptr;
}

void string_set_dump (FILE *out) {
    DEBUGS ('s', "Dumping string set");
    for (size_t i = 0; i < set->size; ++i) {
        char **token = (char **) *(set->values + i);

        if (token == NULL) continue;
        fprintf (out, entry_format, i, (void *) token, *(token));
    }
}
