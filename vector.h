#ifndef __VECTOR_H__
#    define __VECTOR_H__

#    include <stdint.h>
#    include <stddef.h>

struct vector {
    size_t size;
    size_t stack_size;
    uintptr_t *values;
};

void vector_put (struct vector *vector_, const size_t index, void *value);
void *vector_get (const struct vector *vector_, const size_t index);
void *vector_remove (struct vector *vector_, const size_t index);
void vector_append (struct vector *vector_, void *value);
void vector_push (struct vector *vector_, void *value);
void *vector_pop (struct vector *vector_);
void *vector_peek (const struct vector *vector_);
int vector_empty (struct vector *vector_);
void vector_expand (struct vector *vector_);
struct vector *vector_init (const size_t size);
void vector_free (struct vector *vector_);
#endif
