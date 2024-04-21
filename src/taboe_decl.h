#ifndef TABOE_DECL_H
#define TABOE_DECL_H

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

#define TABOE_DECL(tab_t, fn_pfx, key_t, val_t, bit_t)                        \
  tab_t *TABOE_FN(fn_pfx, init)(size_t size);                                 \
  void TABOE_FN(fn_pfx, put)(tab_t * table, key_t key, val_t value);          \
  int TABOE_FN(fn_pfx, get)(const tab_t *table, key_t key, val_t *out);       \
  int TABOE_FN(fn_pfx, get_key)(const tab_t *table, key_t key, key_t *out);   \
  int TABOE_FN(fn_pfx, at)(const tab_t *table, size_t index, val_t *out);     \
  int TABOE_FN(fn_pfx, key_at)(const tab_t *table, size_t index, key_t *out); \
  size_t TABOE_FN(fn_pfx, size)(const tab_t *table);                          \
  size_t TABOE_FN(fn_pfx, count)(const tab_t *table);                         \
  void TABOE_FN(fn_pfx, keys)(const tab_t *table, key_t *out);                \
  void TABOE_FN(fn_pfx, values)(const tab_t *table, val_t *out);              \
  int TABOE_FN(fn_pfx, find)(const tab_t *table, val_t *value,                \
                             int (*compar)(const void *, const void *),       \
                             key_t *out);                                     \
  void TABOE_FN(fn_pfx, clear)(TABOE_NAME(tab_t) * table);                    \
  void TABOE_FN(fn_pfx, destroy)(tab_t * table);

#endif
