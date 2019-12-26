#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include "map.h"

int main (int argc, char **argv) {
    struct map *test_map = map_init (30);
    const char *test_key = "yeet";
    const char *test_val = "yomp";

    printf ("Putting key-value pair (\"%s\", \"%s\") into map.\n", test_key,
            test_val);
    map_put (test_map, test_key, test_val);
    printf ("Retrieving value for key \"%s\": \"%s\"\n", test_key,
            map_get (test_map, (void *) test_key));

    map_free (test_map);
    return EXIT_SUCCESS;
}
