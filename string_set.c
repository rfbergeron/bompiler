#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "string_set.h"
#include "map.h"
#include "auxlib.h"

static const size_t starting_set_size = 100;
static const char *entry_format = "string_set[%4d]: %22p->\"%s\"\n";
static struct map *set;

void string_set_init_globals () {
    set = map_init (starting_set_size);
}

void string_set_free_globals () {
    for (size_t i = 0; i < set->size; ++i) {
        char ** string_ptr = (char **) *(set->values + i);
        if (string_ptr != NULL) {
            if (*(string_ptr) != NULL)
                free (*(string_ptr));
            free (string_ptr);
            DEBUGS ('s', "Successfully freed pointer");
        }
    }
    map_free (set);
}

const char **string_set_intern (const char *string) {
    const char **string_ptr = (const char **) malloc (sizeof (char *));
    *(string_ptr) = strdup (string);

    DEBUGS ('s', "Adding string to set: %s", string);
    map_put (set, string_ptr, string_ptr);
    return string_ptr;
}

void string_set_dump (FILE * out) {
    DEBUGS ('s', "Dumping string set");
    for (size_t i = 0; i < set->size; ++i) {
        char **token = (char **) *(set->values + i);
        if (token == NULL)
            continue;
        fprintf (out, entry_format, i, (void *) token, *(token));
    }
}
