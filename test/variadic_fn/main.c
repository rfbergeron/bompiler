#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

typedef struct array {
  int *elements;
  size_t size;
  size_t cap;
} Array;

Array *array_init(size_t cap, size_t count, ...) {
  Array *array = malloc(sizeof(*array));
  array->size = 0;
  array->cap = 1;
  while (array->cap < cap || array->cap < count) array->cap <<= 1;
  array->elements = malloc(array->cap * sizeof(*(array->elements)));

  va_list elements;
  va_start(elements, count);
  size_t i;
  for (i = 0; i < count; ++i)
    array->elements[array->size++] = va_arg(elements, int);

  va_end(elements);
  return array;
}

Array *array_append(Array *array, size_t count, ...) {
  va_list elements;
  va_start(elements, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    int element = va_arg(elements, int);
    if (array->size >= array->cap)
      array->elements = realloc(array->elements,
              (array->cap <<= 1) * sizeof(*(array->elements)));
    array->elements[array->size++] = element;
  }

  va_end(elements);
  return array;
}

Array *array_print(Array *array) {
  size_t i;
  for (i = 0; i < array->size; ++i)
    if (i == 0)
      printf("[%i", array->elements[i]);
    else
      printf(", %i", array->elements[i]);
  printf("]\n");
  return array;
}

void array_destroy(Array *array) {
  free(array->elements);
  free(array);
}

int main(void) {
  array_destroy(array_print(array_append(array_init(4, 3, 36, 60, 120),
                  6, 6, 12, 4, 2, 240, 360)));
  return 0;
}
