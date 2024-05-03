#ifndef __ARRLIST_H__
#define __ARRLIST_H__

#define ARR_DECL(elem_type, name) \
  size_t name##_len, name##_cap;  \
  elem_type *name
#define ARR_STAT(elem_type, name)       \
  static size_t name##_len, name##_cap; \
  static elem_type *name
#define ARR_DEFN(elem_type, name, init_cap)                            \
  size_t name##_len = 0;                                               \
  size_t name##_cap = (init_cap);                                      \
  elem_type *name = (assert((name##_cap) != 0),                        \
                     assert(((name##_cap) & ((name##_cap) - 1)) == 0), \
                     malloc((name##_cap) * sizeof(elem_type)))
#define ARR_INIT(name, init_cap)                      \
  do {                                                \
    name##_len = 0;                                   \
    name##_cap = (init_cap);                          \
    assert((name##_cap) != 0);                        \
    assert(((name##_cap) & ((name##_cap) - 1)) == 0); \
    name = malloc((name##_cap) * sizeof(*(name)));    \
  } while (0)
#define ARR_PUSH(name, elem)                                        \
  do {                                                              \
    if ((name##_len) >= (name##_cap))                               \
      name = realloc((name), (name##_cap <<= 1) * sizeof(*(name))); \
    assert((name##_cap > 0));                                       \
    assert((name) != NULL);                                         \
    name[(name##_len)++] = (elem);                                  \
  } while (0)
#define ARR_POP(name)                                               \
  do {                                                              \
    assert((name##_len) > 0);                                       \
    if (--(name##_len) < (name##_cap) >> 2)                         \
      name = realloc((name), (name##_cap >>= 1) * sizeof(*(name))); \
    assert((name) != NULL);                                         \
  } while (0)
#define ARR_PUT(name, index, elem)        \
  do {                                    \
    size_t _temp_arrlist = (index);       \
    assert((name##_len) > _temp_arrlist); \
    (name)[_temp_arrlist] = (elem);       \
  } while (0)
#define ARR_RESIZE(name, len, elem)                                            \
  do {                                                                         \
    size_t _len_arrlist = (len);                                               \
    if ((name##_cap) < _len_arrlist) {                                         \
      do (name##_cap) <<= 1, assert((name##_cap) > 0);                         \
      while ((name##_cap) < _len_arrlist);                                     \
      name = realloc((name), (name##_cap) * sizeof(*(name)));                  \
      assert((name) != NULL);                                                  \
    }                                                                          \
    if ((name##_len) < _len_arrlist) {                                         \
      size_t _i_arrlist;                                                       \
      for (_i_arrlist = (name##_len); _i_arrlist < _len_arrlist; ++_i_arrlist) \
        (name)[_i_arrlist] = (elem);                                           \
    }                                                                          \
    (name##_len) = _len_arrlist;                                               \
  } while (0)
#define ARR_PEEK(name) ((name)[(name##_len) - 1])
#define ARR_GET(name, index) ((name)[(index)])
#define ARR_CAP(name) (name##_cap)
#define ARR_LEN(name) (name##_len)
#define ARR_EMPTY(name) ((name##_len) == 0)
#define ARR_DESTROY(name) \
  do {                    \
    free(name);           \
    name = NULL;          \
  } while (0)
#endif
