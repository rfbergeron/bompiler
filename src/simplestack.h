#ifndef SIMPLE_STACK
#define SIMPLE_STACK
#include <stdlib.h>
#include <string.h>

#define DECLARE_STACK(stack_name, stack_type)                                 \
  struct stack_name {                                                         \
    stack_type *stack;                                                        \
    size_t size;                                                              \
    size_t max_size;                                                          \
  };                                                                          \
  void stack_name##_init(struct stack_name *stack, size_t max_size) {         \
    stack->max_size = max_size;                                               \
    stack->size = 0;                                                          \
    stack->stack = malloc(max_size * sizeof(stack_type));                     \
  }                                                                           \
  void stack_name##_destroy(struct stack_name *stack) { free(stack->stack); } \
  void stack_name##_push(struct stack_name *stack, stack_type value) {        \
    if (stack->size >= stack->max_size) {                                     \
      size_t new_max_size = stack->max_size * 2;                              \
      stack_type *new_stack = malloc(new_max_size * sizeof(stack_type));      \
      memcpy(new_stack, stack->stack, stack->size * sizeof(stack_type));      \
      free(stack->stack);                                                     \
      stack->max_size = new_max_size;                                         \
      stack->stack = new_stack;                                               \
    }                                                                         \
    stack->stack[stack->size] = value;                                        \
    ++stack->size;                                                            \
  }                                                                           \
  void stack_name##_pop(struct stack_name *stack) {                           \
    --stack->size;                                                            \
    if (stack->size <= stack->max_size / 4) {                                 \
      size_t new_max_size = stack->max_size / 2;                              \
      stack_type *new_stack = malloc(new_max_size * sizeof(stack_type));      \
      memcpy(new_stack, stack->stack, new_max_size * sizeof(stack_type));     \
      free(stack->stack);                                                     \
      stack->max_size = new_max_size;                                         \
      stack->stack = new_stack;                                               \
    }                                                                         \
  }                                                                           \
  stack_type stack_name##_top(struct stack_name *stack) {                     \
    return stack->stack[stack->size];                                         \
  }                                                                           \
  void stack_name##_replace(struct stack_name *stack, stack_type value) {     \
    stack->stack[stack->size] = value;                                        \
  }                                                                           \
  stack_type stack_name##_postfix_inc(struct stack_name *stack) {             \
    return (stack->stack[stack->size])++;                                     \
  }                                                                           \
  size_t stack_name##_size(struct stack_name *stack) { return stack->size; }

#endif
