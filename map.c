#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include "map.h"

// Smallest prime less than (2^32)-1. Alternatively we could swap the last digit
// to a 9 but I'm not sure exactly what the difference is.
const uint32_t KNUTH_CONST = 2654435761;
const uint32_t UINT32_T_MAX = 0xFFFFFFFF;
const uint32_t UINT32_T_MSB = 0x80000000;

uint32_t base2_leading_zeroes_32bit (uint32_t arg) {
    for (size_t i = 0; i < 32; ++i) {
        if (arg & (UINT32_T_MSB >> i) != 0) {
            return i;
        }
    }
    return 32;
}

uint32_t knuth_hash (uintptr_t to_hash, size_t max_value) {
    assert (max_value <= UINT32_T_MAX && max_value > 0);
    uint32_t shift_amount = base2_leading_zeroes_32bit ((uint32_t) max_value);

    // Want to preserve high order bits since those are the "best", but we also
    // need the value to fit inside the max value.
    return ((to_hash * KNUTH_CONST) >> shift_amount) % max_value;
}

void map_put (struct map *map_, void *key, void *value) {
    assert (key != NULL);
    uint32_t map_index = knuth_hash ((uintptr_t) key, map_->size);
    uintptr_t stored_key = *(map_->keys + map_index);

    if (key == (void *) stored_key) {
        // Already present
        *(map_->values + map_index) = (uintptr_t) value;
        ++(map_->count);
    } else {
        size_t open_index = 0;
        size_t current_index = 0;

        // check if key has been open addressed and look for an empty slot
        while (current_index < map_->size &&
               (map_->size < map_->max_open_addr_index || open_index == 0)) {
            if (*(map_->keys + current_index) == (uintptr_t) key) {
                *(map_->values + current_index) = (uintptr_t) value;
                return;
            } else if ((void *) *(map_->keys + current_index) == NULL
                       && open_index == 0) {
                open_index = current_index;
            }
            ++current_index;
        }

        // emplace the value somewhere
        if ((void *) stored_key == NULL) {
            *(map_->keys + map_index) = (uintptr_t) key;
            *(map_->values + map_index) = (uintptr_t) value;
        } else if (open_index != 0) {
            *(map_->keys + open_index) = (uintptr_t) key;
            *(map_->values + open_index) = (uintptr_t) value;
            if (open_index > map_->max_open_addr_index) {
                map_->max_open_addr_index = open_index;
            }
        } else {
            map_expand (map_);
            map_put (map_, key, value);
        }
    }
}

void *map_get (struct map *map_, void *key) {
    assert (key != NULL);
    uint32_t map_index = knuth_hash ((uintptr_t) key, map_->size);
    uintptr_t stored_key = *(map_->keys + map_index);

    if (stored_key == (uintptr_t) key) {
        return (void *) *(map_->values + map_index);
    } else {
        for (size_t i = 0; i < map_->max_open_addr_index; ++i) {
            stored_key = *(map_->keys + i);
            if (stored_key == (uintptr_t) key) {
                return (void *) *(map_->values + map_index);
            }
        }
        return NULL;
    }
}

void map_free (struct map *map_) {
    if (map_->keys != NULL) {
        free (map_->keys);
        map_->keys = NULL;
    }

    if (map_->values != NULL) {
        free (map_->values);
        map_->values = NULL;
    }
}

void map_expand (struct map *map_) {
    size_t old_size = map_->size;
    uintptr_t *old_keys = map_->keys;
    uintptr_t *old_values = map_->values;

    map_->size *= 2;
    map_->keys = calloc (sizeof (uintptr_t), map_->size);
    map_->values = calloc (sizeof (uintptr_t), map_->size);
    map_->count = 0;
    map_->max_open_addr_index = 0;

    for (size_t i = 0; i < old_size; ++i) {
        uintptr_t stored_key = *(old_keys + i);

        if ((void *) stored_key != NULL) {
            map_put (map_, (void *) stored_key, (void *) *(old_values + i));
        }
    }
}
