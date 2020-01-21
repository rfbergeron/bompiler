#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include "map.h"

// largest prime less than (2^32)-1. Alternatively we could swap the last digit
// to a 9 but I'm not sure exactly what the difference is.
static const uint32_t KNUTH_CONST = 2654435761;
static const uint32_t UINT32_T_MAX = 0xFFFFFFFF;
static const uint32_t UINT32_T_MSB = 0x80000000;

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
    void *stored_key = *(map_->keys + map_index);

    if (key == stored_key) {
        // Already present
        *(map_->values + map_index) = value;
        ++(map_->count);
    } else {
        size_t open_index = 0;
        size_t current_index = 0;

        // check if key has been open addressed and look for an empty slot
        while (current_index < map_->size &&
               (map_->size < map_->max_open_addr_index || open_index == 0)) {
            if (*(map_->keys + current_index) == key) {
                *(map_->values + current_index) = value;
                return;
            } else if (*(map_->keys + current_index) == NULL
                       && open_index == 0) {
                open_index = current_index;
            }
            ++current_index;
        }

        // emplace the value somewhere
        if (stored_key == NULL) {
            *(map_->keys + map_index) = key;
            *(map_->values + map_index) = value;
        } else if (open_index != 0) {
            *(map_->keys + open_index) = key;
            *(map_->values + open_index) = value;
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
    void *stored_key = *(map_->keys + map_index);

    if (stored_key == key) {
        return *(map_->values + map_index);
    } else {
        for (size_t i = 0; i < map_->max_open_addr_index; ++i) {
            stored_key = *(map_->keys + i);
            if (stored_key == key) {
                return *(map_->values + map_index);
            }
        }
        return NULL;
    }
}

void map_expand (struct map *map_) {
    size_t old_size = map_->size;
    void **old_keys = map_->keys;
    void **old_values = map_->values;

    map_->size *= 2;
    map_->keys = calloc (sizeof (void *), map_->size);
    map_->values = calloc (sizeof (void *), map_->size);
    map_->count = 0;
    map_->max_open_addr_index = 0;

    for (size_t i = 0; i < old_size; ++i) {
        void *stored_key = *(old_keys + i);

        if (stored_key != NULL) {
            map_put (map_, stored_key, *(old_values + i));
        }
    }
    free (old_keys);
    free (old_values);
}

struct map *map_init (size_t size_) {
    struct map *map_ = (struct map *) calloc (sizeof (struct map), size_);

    map_->size = size_;
    map_->count = 0;
    map_->max_open_addr_index = 0;
    map_->keys = calloc (sizeof (void *), size_);
    map_->values = calloc (sizeof (void *), size_);
    return map_;
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
    free (map_);
}
