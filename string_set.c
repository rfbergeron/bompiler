#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "string_set.h"
#include "map.h"

static const size_t starting_set_size = 100;
static const char *entry_format = "string_set[%4d]: %22p->\"%s\"\n";
static struct map *set;

void string_set_init () {
    set = map_init (starting_set_size);
}

const char **string_set_intern (const char *string) {
    const char **string_ptr = (const char **) malloc (sizeof (char *));

    // likely wont work but we shall see if the value returned by yytext
    // falls out of scope or is dynamically allocated.
    *(string_ptr) = string;
    map_put (set, string_ptr, string_ptr);
    return string_ptr;
}

void string_set_dump (FILE * out) {
    for (size_t i = 0; i < set->size; ++i) {
        char **token = (char **) *(set->values + i);

        printf (entry_format, i, (void *) token, *(token));
    }
}
