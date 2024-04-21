#ifndef TABOE_IMPL_H
#define TABOE_IMPL_H

#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#ifndef TABOE_FN
#define TABOE_FN(pfx, fn) pfx##_##fn
#endif

#ifndef TABOE_NAME
#define TABOE_NAME(tab_t) struct TABOE_##tab_t##_INTERNAL
#endif

#ifndef TABOE_TYPE
#define TABOE_TYPE(tab_t, key_t, val_t, bit_t) \
  TABOE_NAME(tab_t) {                          \
    key_t *keys;                               \
    val_t *values;                             \
    size_t *indices;                           \
    bit_t *neighbors;                          \
    size_t size, count;                        \
  };
#endif

#ifndef TABOE_TDEF
#define TABOE_TDEF(tab_t) typedef TABOE_NAME(tab_t) tab_t;
#endif

#define TABOE_IMPL(NEIGHBORHOOD_SIZE, tab_t, fn_pfx, key_t, val_t, bit_t,      \
                   hash_fn, key_comp)                                          \
  static size_t TABOE_FN(fn_pfx, hash)(key_t key, size_t size) {               \
    int power = 0;                                                             \
    while (1UL << power < size) ++power;                                       \
    bit_t result = hash_fn(key);                                               \
    return (result ^ (result >> (64 - power))) & (size - 1);                   \
  }                                                                            \
                                                                               \
  static bit_t TABOE_FN(fn_pfx, check)(const TABOE_NAME(tab_t) * table,        \
                                       size_t home_index) {                    \
    bit_t is_occupied = 0;                                                     \
    size_t offset;                                                             \
    for (offset = 1; offset <= (NEIGHBORHOOD_SIZE); ++offset) {                \
      size_t current_index = (home_index - offset) % table->size;              \
      bit_t adj_neighborhood =                                                 \
          table->neighbors[current_index] >> (offset - 1);                     \
      assert((adj_neighborhood & is_occupied) == 0);                           \
      is_occupied |= adj_neighborhood;                                         \
    }                                                                          \
    return is_occupied;                                                        \
  }                                                                            \
                                                                               \
  static bit_t TABOE_FN(fn_pfx, mask)(const TABOE_NAME(tab_t) * table,         \
                                      size_t home_index,                       \
                                      size_t neighbor_index) {                 \
    assert((neighbor_index - home_index) % table->size < (NEIGHBORHOOD_SIZE)); \
    return 1UL << ((neighbor_index - home_index) % table->size);               \
  }                                                                            \
                                                                               \
  static size_t TABOE_FN(fn_pfx, target)(const TABOE_NAME(tab_t) * table,      \
                                         key_t key, size_t start_index) {      \
    size_t target_index = SIZE_MAX;                                            \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, start_index);         \
    size_t i;                                                                  \
    for (i = 0; i < table->size; ++i) {                                        \
      size_t current_index = (start_index + i) % table->size;                  \
      assert(((occupied_bits >> 1) & table->neighbors[current_index]) == 0);   \
      occupied_bits = (occupied_bits >> 1) | table->neighbors[current_index];  \
      if (i < (NEIGHBORHOOD_SIZE)) {                                           \
        size_t sequence_index = table->indices[current_index];                 \
        if ((occupied_bits & 1) == 1 &&                                        \
            key_comp(table->keys[sequence_index], key) == 0) {                 \
          return current_index;                                                \
        } else if ((occupied_bits & 1) == 0 && target_index == SIZE_MAX) {     \
          target_index = current_index;                                        \
        } else {                                                               \
          assert(target_index != SIZE_MAX ||                                   \
                 key_comp(table->keys[sequence_index], key) != 0);             \
        }                                                                      \
      } else if (target_index != SIZE_MAX) {                                   \
        return target_index;                                                   \
      } else if ((occupied_bits & 1) == 0) {                                   \
        return current_index;                                                  \
      }                                                                        \
    }                                                                          \
                                                                               \
    return target_index;                                                       \
  }                                                                            \
                                                                               \
  static void TABOE_FN(fn_pfx, put_index)(                                     \
      TABOE_NAME(tab_t) * table, size_t target_index, size_t proper_index,     \
      size_t sequence_index);                                                  \
                                                                               \
  static void TABOE_FN(fn_pfx, resize)(TABOE_NAME(tab_t) * table) {            \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, 0);                   \
    size_t old_size = table->size;                                             \
    size_t *old_indices = table->indices;                                      \
    bit_t *old_neighbors = table->neighbors;                                   \
                                                                               \
    table->size <<= 1;                                                         \
    table->keys = realloc(table->keys, table->size * sizeof(key_t));           \
    table->values = realloc(table->values, table->size * sizeof(val_t));       \
    table->indices = malloc(table->size * sizeof(size_t));                     \
    table->neighbors = calloc(table->size, sizeof(bit_t));                     \
                                                                               \
    size_t i;                                                                  \
    for (i = 0; i < old_size; ++i) {                                           \
      assert(((occupied_bits >> 1) & old_neighbors[i]) == 0);                  \
      occupied_bits = (occupied_bits >> 1) | old_neighbors[i];                 \
      if (occupied_bits & 1) {                                                 \
        key_t key = table->keys[old_indices[i]];                               \
        size_t proper_index = TABOE_FN(fn_pfx, hash)(key, table->size);        \
        size_t target_index =                                                  \
            TABOE_FN(fn_pfx, target)(table, key, proper_index);                \
                                                                               \
        while (target_index == SIZE_MAX) {                                     \
          TABOE_FN(fn_pfx, resize)(table);                                     \
          proper_index = TABOE_FN(fn_pfx, hash)(key, table->size);             \
          target_index = TABOE_FN(fn_pfx, target)(table, key, proper_index);   \
        }                                                                      \
        TABOE_FN(fn_pfx, put_index)                                            \
        (table, target_index, proper_index, old_indices[i]);                   \
      }                                                                        \
    }                                                                          \
                                                                               \
    free(old_indices);                                                         \
    free(old_neighbors);                                                       \
  }                                                                            \
                                                                               \
  static size_t TABOE_FN(fn_pfx, helper)(                                      \
      TABOE_NAME(tab_t) * table, size_t empty_index, size_t start_offset) {    \
    assert(start_offset > 0);                                                  \
    size_t neighbor_index = (empty_index - start_offset) % table->size;        \
    size_t offset;                                                             \
    for (offset = start_offset; offset < (NEIGHBORHOOD_SIZE); ++offset) {      \
      size_t home_index = (empty_index - offset) % table->size;                \
      size_t sequence_index = table->indices[neighbor_index];                  \
      if (table->neighbors[home_index] &                                       \
          TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index)) {         \
        assert(table->neighbors[home_index] &                                  \
               TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index));     \
        assert(TABOE_FN(fn_pfx, hash)(table->keys[sequence_index],             \
                                      table->size) == home_index);             \
        if (neighbor_index != home_index)                                      \
          assert(!(table->neighbors[neighbor_index] & 1));                     \
        return home_index;                                                     \
      } else {                                                                 \
        assert(!(table->neighbors[home_index] &                                \
                 TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index)));  \
        assert(TABOE_FN(fn_pfx, hash)(table->keys[sequence_index],             \
                                      table->size) != home_index);             \
      }                                                                        \
    }                                                                          \
    return SIZE_MAX;                                                           \
  }                                                                            \
                                                                               \
  static size_t TABOE_FN(fn_pfx, relocate)(TABOE_NAME(tab_t) * table,          \
                                           size_t empty_index) {               \
    size_t neighbor_index = SIZE_MAX, home_index = SIZE_MAX;                   \
    size_t offset;                                                             \
    for (offset = (NEIGHBORHOOD_SIZE)-1; offset > 0; --offset) {               \
      home_index = TABOE_FN(fn_pfx, helper)(table, empty_index, offset);       \
      if (home_index != SIZE_MAX) {                                            \
        neighbor_index = (empty_index - offset) % table->size;                 \
        break;                                                                 \
      }                                                                        \
    }                                                                          \
                                                                               \
    assert((home_index == SIZE_MAX && neighbor_index == SIZE_MAX) ||           \
           (home_index != SIZE_MAX && neighbor_index != SIZE_MAX));            \
    if (home_index == SIZE_MAX) return home_index;                             \
                                                                               \
    size_t sequence_index = table->indices[neighbor_index];                    \
    assert(table->neighbors[home_index] &                                      \
           TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index));         \
    assert(TABOE_FN(fn_pfx, hash)(table->keys[sequence_index], table->size) == \
           home_index);                                                        \
    assert(!(table->neighbors[home_index] &                                    \
             TABOE_FN(fn_pfx, mask)(table, home_index, empty_index)));         \
                                                                               \
    table->indices[empty_index] = table->indices[neighbor_index];              \
    table->neighbors[home_index] ^=                                            \
        TABOE_FN(fn_pfx, mask)(table, home_index, empty_index) |               \
        TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index);             \
                                                                               \
    assert(table->neighbors[home_index] &                                      \
           TABOE_FN(fn_pfx, mask)(table, home_index, empty_index));            \
    assert(!(table->neighbors[home_index] &                                    \
             TABOE_FN(fn_pfx, mask)(table, home_index, neighbor_index)));      \
    return neighbor_index;                                                     \
  }                                                                            \
                                                                               \
  static void TABOE_FN(fn_pfx, put_index)(                                     \
      TABOE_NAME(tab_t) * table, size_t target_index, size_t proper_index,     \
      size_t sequence_index) {                                                 \
    assert(target_index != SIZE_MAX);                                          \
    assert((target_index - proper_index) % table->size >=                      \
               (NEIGHBORHOOD_SIZE) ||                                          \
           !(table->neighbors[proper_index] &                                  \
             TABOE_FN(fn_pfx, mask)(table, proper_index, target_index)));      \
                                                                               \
    while ((target_index - proper_index) % table->size >=                      \
           (NEIGHBORHOOD_SIZE)) {                                              \
      target_index = TABOE_FN(fn_pfx, relocate)(table, target_index);          \
      if (target_index == SIZE_MAX) {                                          \
        TABOE_FN(fn_pfx, resize)(table);                                       \
        proper_index =                                                         \
            TABOE_FN(fn_pfx, hash)(table->keys[sequence_index], table->size);  \
        target_index = TABOE_FN(fn_pfx, target)(                               \
            table, table->keys[sequence_index], proper_index);                 \
        TABOE_FN(fn_pfx, put_index)                                            \
        (table, target_index, proper_index, sequence_index);                   \
        return;                                                                \
      }                                                                        \
    }                                                                          \
                                                                               \
    table->indices[target_index] = sequence_index;                             \
    table->neighbors[proper_index] |=                                          \
        TABOE_FN(fn_pfx, mask)(table, proper_index, target_index);             \
  }                                                                            \
                                                                               \
  TABOE_NAME(tab_t) * TABOE_FN(fn_pfx, init)(size_t size) {                    \
    if (size == 0) return NULL;                                                \
    TABOE_NAME(tab_t) *table = malloc(sizeof(*table));                         \
    table->count = 0;                                                          \
    table->size = 1;                                                           \
    while (table->size < size) table->size <<= 1;                              \
    table->keys = malloc(table->size * sizeof(*table->keys));                  \
    table->values = malloc(table->size * sizeof(*table->values));              \
    table->indices = malloc(table->size * sizeof(*table->indices));            \
    table->neighbors = calloc(table->size, sizeof(*table->neighbors));         \
    return table;                                                              \
  }                                                                            \
                                                                               \
  void TABOE_FN(fn_pfx, put)(TABOE_NAME(tab_t) * table, key_t key,             \
                             val_t value) {                                    \
    if (table == NULL) abort();                                                \
    size_t proper_index = TABOE_FN(fn_pfx, hash)(key, table->size);            \
    size_t target_index = TABOE_FN(fn_pfx, target)(table, key, proper_index);  \
                                                                               \
    while (target_index == SIZE_MAX) {                                         \
      TABOE_FN(fn_pfx, resize)(table);                                         \
      proper_index = TABOE_FN(fn_pfx, hash)(key, table->size);                 \
      target_index = TABOE_FN(fn_pfx, target)(table, key, proper_index);       \
    }                                                                          \
                                                                               \
    if ((target_index - proper_index) % table->size < (NEIGHBORHOOD_SIZE) &&   \
        (table->neighbors[proper_index] &                                      \
         TABOE_FN(fn_pfx, mask)(table, proper_index, target_index))) {         \
      size_t sequence_index = table->indices[target_index];                    \
      assert(key_comp(table->keys[sequence_index], key) == 0);                 \
      table->values[sequence_index] = value;                                   \
    } else {                                                                   \
      assert(table->count < table->size);                                      \
      table->keys[table->count] = key;                                         \
      table->values[table->count] = value;                                     \
      TABOE_FN(fn_pfx, put_index)                                              \
      (table, target_index, proper_index, table->count++);                     \
    }                                                                          \
  }                                                                            \
                                                                               \
  int TABOE_FN(fn_pfx, get)(const TABOE_NAME(tab_t) * table, key_t key,        \
                            val_t * out) {                                     \
    if (table == NULL) return 0;                                               \
    size_t start_index = TABOE_FN(fn_pfx, hash)(key, table->size);             \
    size_t i;                                                                  \
    for (i = 0; i < (NEIGHBORHOOD_SIZE); ++i) {                                \
      size_t current_index = (start_index + i) % table->size;                  \
      size_t sequence_index = table->indices[current_index];                   \
      if ((table->neighbors[start_index] & (1UL << i)) &&                      \
          key_comp(table->keys[sequence_index], key) == 0) {                   \
        if (out != NULL) *out = table->values[sequence_index];                 \
        return 1;                                                              \
      }                                                                        \
    }                                                                          \
    return 0;                                                                  \
  }                                                                            \
                                                                               \
  int TABOE_FN(fn_pfx, get_key)(const TABOE_NAME(tab_t) * table, key_t key,    \
                                key_t * out) {                                 \
    if (table == NULL) return 0;                                               \
    size_t start_index = TABOE_FN(fn_pfx, hash)(key, table->size);             \
    size_t i;                                                                  \
    for (i = 0; i < (NEIGHBORHOOD_SIZE); ++i) {                                \
      size_t current_index = (start_index + i) % table->size;                  \
      size_t sequence_index = table->indices[current_index];                   \
      if ((table->neighbors[start_index] & (1UL << i)) &&                      \
          key_comp(table->keys[sequence_index], key) == 0) {                   \
        if (out != NULL) *out = table->keys[sequence_index];                   \
        return 1;                                                              \
      }                                                                        \
    }                                                                          \
    return 0;                                                                  \
  }                                                                            \
                                                                               \
  int TABOE_FN(fn_pfx, at)(const TABOE_NAME(tab_t) * table, size_t index,      \
                           val_t * out) {                                      \
    if (table == NULL || table->count <= index) return 0;                      \
    if (out != NULL) *out = table->values[index];                              \
    return 1;                                                                  \
  }                                                                            \
                                                                               \
  int TABOE_FN(fn_pfx, key_at)(const TABOE_NAME(tab_t) * table, size_t index,  \
                               key_t * out) {                                  \
    if (table == NULL || table->count <= index) return 0;                      \
    if (out != NULL) *out = table->keys[index];                                \
    return 1;                                                                  \
  }                                                                            \
                                                                               \
  size_t TABOE_FN(fn_pfx, size)(const TABOE_NAME(tab_t) * table) {             \
    return table == NULL ? 0 : table->size;                                    \
  }                                                                            \
                                                                               \
  size_t TABOE_FN(fn_pfx, count)(const TABOE_NAME(tab_t) * table) {            \
    return table == NULL ? 0 : table->count;                                   \
  }                                                                            \
                                                                               \
  size_t TABOE_FN(fn_pfx, hard_count)(const TABOE_NAME(tab_t) * table) {       \
    if (table == NULL) return 0;                                               \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, 0);                   \
    size_t i, count = 0;                                                       \
    for (i = 0; i < table->size; ++i) {                                        \
      assert(((occupied_bits >> 1) & table->neighbors[i]) == 0);               \
      occupied_bits = (occupied_bits >> 1) | table->neighbors[i];              \
      if (occupied_bits & 1) ++count;                                          \
    }                                                                          \
    return count;                                                              \
  }                                                                            \
                                                                               \
  void TABOE_FN(fn_pfx, keys)(const TABOE_NAME(tab_t) * table, key_t * out) {  \
    if (table == NULL || out == NULL) return;                                  \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, 0);                   \
    size_t i, j;                                                               \
    for (i = 0, j = 0; i < table->size; ++i) {                                 \
      assert(((occupied_bits >> 1) & table->neighbors[i]) == 0);               \
      occupied_bits = (occupied_bits >> 1) | table->neighbors[i];              \
      if (occupied_bits & 1) out[j++] = table->keys[table->indices[i]];        \
    }                                                                          \
  }                                                                            \
                                                                               \
  void TABOE_FN(fn_pfx, values)(const TABOE_NAME(tab_t) * table,               \
                                val_t * out) {                                 \
    if (table == NULL || out == NULL) return;                                  \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, 0);                   \
    size_t i, j;                                                               \
    for (i = 0, j = 0; i < table->size; ++i) {                                 \
      assert(((occupied_bits >> 1) & table->neighbors[i]) == 0);               \
      occupied_bits = (occupied_bits >> 1) | table->neighbors[i];              \
      if (occupied_bits & 1) out[j++] = table->values[table->indices[i]];      \
    }                                                                          \
  }                                                                            \
                                                                               \
  int TABOE_FN(fn_pfx, find)(const TABOE_NAME(tab_t) * table, val_t * value,   \
                             int (*compar)(const void *, const void *),        \
                             key_t *out) {                                     \
    if (table == NULL) return 0;                                               \
    bit_t occupied_bits = TABOE_FN(fn_pfx, check)(table, 0);                   \
    size_t i;                                                                  \
    for (i = 0; i < table->size; ++i) {                                        \
      assert(((occupied_bits >> 1) & table->neighbors[i]) == 0);               \
      occupied_bits = (occupied_bits >> 1) | table->neighbors[i];              \
      size_t sequence_index = table->indices[i];                               \
      if ((occupied_bits & 1) &&                                               \
          compar(&table->values[sequence_index], value) == 0) {                \
        if (out != NULL) *out = table->keys[sequence_index];                   \
        return 1;                                                              \
      }                                                                        \
    }                                                                          \
    return 0;                                                                  \
  }                                                                            \
                                                                               \
  void TABOE_FN(fn_pfx, clear)(TABOE_NAME(tab_t) * table) {                    \
    if (table == NULL) return;                                                 \
    size_t i;                                                                  \
    for (i = 0; i < table->size; ++i) table->neighbors[i] = 0;                 \
    table->count = 0;                                                          \
  }                                                                            \
                                                                               \
  void TABOE_FN(fn_pfx, destroy)(TABOE_NAME(tab_t) * table) {                  \
    if (table == NULL) return;                                                 \
    free(table->keys);                                                         \
    free(table->indices);                                                      \
    free(table->values);                                                       \
    free(table->neighbors);                                                    \
    free(table);                                                               \
  }

#endif
