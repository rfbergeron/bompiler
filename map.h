#ifndef __MAP_H__
#define __MAP_H__

#include <stddef.h>

struct map {
    size_t size;
    size_t count;
    size_t max_open_addr_index;
    void **keys;
    void **values;
};

void map_put (struct map *map_, void *key, void *value);
void *map_get (struct map *map_, void *key);
void map_expand (struct map *map_);
struct map *map_init (size_t size_);
void map_free (struct map *map_);
#endif
