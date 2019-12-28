#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "map.h"

int map_unit_test ()
{
    size_t test_size = 30;
    struct map *test_map = map_init (test_size);
    char *test_key = (char *) calloc (sizeof (char), 5);
    char *test_val = (char *) calloc (sizeof (char), 5);

    strcpy (test_key, "yeet");
    strcpy (test_val, "yomp");

    // try a single value
    printf ("Putting key-value pair (\"%s\",\"%s\") into map.\n", test_key,
            test_val);
    map_put (test_map, test_key, test_val);
    printf ("Retrieving value for key \"%s\": \"%s\"\n\n", test_key,
            map_get (test_map, (void *) test_key));

    // fill up the map
    size_t kv_max_len = strlen ("test_key_") + 13;

    for (size_t i = 1; i < test_size; ++i) {
        char *fill_key = (char *) calloc (sizeof (char), kv_max_len);
        char *fill_val = (char *) calloc (sizeof (char), kv_max_len);

        strcpy (fill_key, "test_key_");
        strcpy (fill_val, "test_val_");
        sprintf (fill_key + strlen (fill_key), "%d", i);
        sprintf (fill_val + strlen (fill_val), "%d", i);
        printf ("Putting key-value pair (\"%s\",\"%s\") into map.\n", fill_key,
                fill_val);
        map_put (test_map, fill_key, fill_val);
    }

    // print out contents
    printf ("\n%-5s   %-16s %-16s\n", "Index", "Key", "Value");
    for (size_t i = 0; i < test_map->size; ++i) {
        char *stored_key = (char *) *(test_map->keys + i);
        char *stored_val = (char *) *(test_map->values + i);

        if (stored_key == NULL) {
            stored_key = "NULL";
        } else if (strlen (stored_key) == 0) {
            stored_key = "EMPTY";
        }

        if (stored_val == NULL) {
            stored_val = "NULL";
        } else if (strlen (stored_val) == 0) {
            stored_val = "EMPTY";
        }

        printf ("%-5d: (%-16s,%-16s)\n", i, stored_key, stored_val);
    }

    // put in just one more
    size_t ov_max_len = strlen ("overflow_key");
    char *overflow_key = (char *) calloc (sizeof (char), ov_max_len);
    char *overflow_val = (char *) calloc (sizeof (char), ov_max_len);

    strcpy (overflow_key, "overflow_key");
    strcpy (overflow_val, "overflow_val");
    printf ("\nOverflowing the map, which should cause it to expand\n");
    map_put (test_map, overflow_key, overflow_val);

    // print out the contents again
    printf ("%-5s   %-16s %-16s\n", "Index", "Key", "Value");
    for (size_t i = 0; i < test_map->size; ++i) {
        char *stored_key = (char *) *(test_map->keys + i);
        char *stored_val = (char *) *(test_map->values + i);

        if (stored_key == NULL) {
            stored_key = "NULL";
        } else if (strlen (stored_key) == 0) {
            stored_key = "EMPTY";
        }

        if (stored_val == NULL) {
            stored_val = "NULL";
        } else if (strlen (stored_val) == 0) {
            stored_val = "EMPTY";
        }

        printf ("%-5d: (%-16s,%-16s)\n", i, stored_key, stored_val);
    }

    // cleanup
    for (size_t i = 0; i < test_size; ++i) {
        char *stored_key = (char *) *(test_map->keys + i);
        char *stored_val = (char *) *(test_map->values + i);

        if (stored_key != NULL) {
            //printf ("Freeing key: %s\n", stored_key);
            free (stored_key);
        }
        if (stored_val != NULL) {
            //printf ("Freeing val: %s\n", stored_val);
            free (stored_val);
        }
    }

    map_free (test_map);
    return EXIT_SUCCESS;
}

int main (int argc, char **argv)
{
    map_unit_test ();
    return EXIT_SUCCESS;
}
