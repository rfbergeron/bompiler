#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "vector.h"

void vector_put (struct vector *vector_, const size_t index, void *value) {
    assert (index < vector_->size);
    *((vector_->values) + index) = (uintptr_t) value;
}

void *vector_get (struct vector *vector_, const size_t index) {
    assert (index < vector_->size);
    return (void *) *((vector_->values) + index);
}

void *vector_remove (struct vector *vector_, const size_t index) {
    assert (index < vector_->size);
    uintptr_t ret = *((vector_->values) + index);

    *((vector_->values) + index) = (uintptr_t) NULL;
    return (void *) ret;
}

void vector_push (struct vector *vector_, void *value) {
    if (vector_->stack_size == vector_->size)
        vector_expand (vector_);
    *((vector_->values) + vector_->stack_size) = (uintptr_t) value;
    ++(vector_->stack_size);
}

void *vector_pop (struct vector *vector_) {
    if (vector_empty (vector_))
        return NULL;
    void *ret = (void *) *(vector_->values);

    --(vector_->stack_size);
    for (size_t i = 0; i < vector_->stack_size; ++i) {
        *((vector_->values) + vector_->stack_size) =
            *((vector_->values) + vector_->stack_size + 1);
    }
    return ret;
}

void *vector_peek (struct vector *vector_) {
    return (void *) *((vector_->values) + vector_->stack_size);
}

int vector_empty (struct vector *vector_) {
    return (vector_->stack_size == 0);
}

void vector_expand (struct vector *vector_) {
    uintptr_t *old_values = vector_->values;
    size_t old_size = vector_->size;

    vector_->size *= 2;
    vector_->values = (uintptr_t *) calloc (sizeof (uintptr_t), vector_->size);
    memcpy (vector_->values, old_values, old_size);
    free (old_values);
}

struct vector *vector_init (const size_t size_) {
    struct vector *ret = malloc (sizeof (struct vector));

    ret->size = size_;
    ret->stack_size = 0;
    ret->values = (uintptr_t *) calloc (sizeof (uintptr_t), size_);
    return ret;
}

void vector_free (struct vector *vector_) {
    free (vector_->values);
    free (vector_);
}
