#include "vector.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "auxlib.h"

void vector_put (struct vector *vector_, const size_t index, void *value) {
    assert (vector_ != NULL);
    assert (index < vector_->size);
    assert (index >= 0);
    DEBUGS ('v',
            "Putting pointer %p at index %u; original value: %p",
            value,
            vector_->stack_size,
            *((vector_->values) + vector_->stack_size));
    *((vector_->values) + index) = value;
}

void *vector_get (struct vector *vector_, const size_t index) {
    assert (vector_ != NULL);
    assert (index < vector_->size);
    assert (index >= 0);
    return *((vector_->values) + index);
}

void *vector_remove (struct vector *vector_, const size_t index) {
    assert (vector_ != NULL);
    assert (index < vector_->size);
    assert (index >= 0);
    void *ret = *((vector_->values) + index);

    *((vector_->values) + index) = NULL;
    return ret;
}

void vector_push (struct vector *vector_, void *value) {
    assert (vector_ != NULL);
    if (vector_->stack_size == vector_->size) vector_expand (vector_);
    DEBUGS ('v',
            "Pushing pointer %p to index %u; original value: %p",
            value,
            vector_->stack_size,
            *((vector_->values) + vector_->stack_size));
    *((vector_->values) + vector_->stack_size) = value;
    ++(vector_->stack_size);
}

void *vector_pop (struct vector *vector_) {
    assert (vector_ != NULL);
    if (vector_empty (vector_)) return NULL;
    void *ret = *(vector_->values);

    --(vector_->stack_size);
    for (size_t i = 0; i < vector_->stack_size; ++i) {
        *((vector_->values) + vector_->stack_size) =
                *((vector_->values) + vector_->stack_size + 1);
    }
    return ret;
}

void *vector_peek (struct vector *vector_) {
    assert (vector_ != NULL);
    return *((vector_->values) + vector_->stack_size);
}

int vector_empty (struct vector *vector_) {
    assert (vector_ != NULL);
    return (vector_->stack_size == 0);
}

void vector_expand (struct vector *vector_) {
    assert (vector_ != NULL);
    void **old_values = vector_->values;
    size_t old_size = vector_->size;

    DEBUGE (
            'v',
            for (size_t i = 0; i < old_size; ++i) {
                DEBUGS ('v',
                        "Old vector value: %p index: %u",
                        *(old_values + i),
                        i);
            });

    vector_->size *= 2;
    vector_->values = calloc (sizeof (void *), vector_->size);
    memcpy (vector_->values, old_values, old_size * sizeof (void *));
    free (old_values);

    DEBUGE (
            'v',
            for (size_t i = 0; i < vector_->size; ++i) {
                DEBUGS ('v',
                        "New vector value: %p index: %u",
                        *(vector_->values + i),
                        i);
            });
}

struct vector *vector_init (const size_t size_) {
    struct vector *ret = calloc (sizeof (struct vector), 1);

    ret->size = size_;
    ret->stack_size = 0;
    ret->values = (void **) calloc (sizeof (void *), size_);
    return ret;
}

void vector_free (struct vector *vector_) {
    assert (vector_ != NULL);
    free (vector_->values);
    free (vector_);
}
