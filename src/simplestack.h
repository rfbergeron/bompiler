#ifndef SIMPLE_STACK
#define SIMPLE_STACK
#include <stdlib.h>
#include <string.h>

#define DECLARE_STACK(stack_name, stack_type) \
  struct stack_name {                         \
    stack_type *stack;                        \
    size_t count;                             \
    size_t size;                              \
  };
#define PROC_STACK(stack_name, stack_type)                                    \
  void stack_name##_init(struct stack_name *stack, size_t size) {             \
    stack->size = size;                                                       \
    stack->count = 0;                                                         \
    stack->stack = malloc(size * sizeof(stack_type));                         \
  }                                                                           \
  void stack_name##_destroy(struct stack_name *stack) { free(stack->stack); } \
  void stack_name##_push(struct stack_name *stack, stack_type value) {        \
    if (stack->count >= stack->size) {                                        \
      size_t new_size = stack->size * 2;                                      \
      stack_type *new_stack = malloc(new_size * sizeof(stack_type));          \
      memcpy(new_stack, stack->stack, stack->count * sizeof(stack_type));     \
      free(stack->stack);                                                     \
      stack->size = new_size;                                                 \
      stack->stack = new_stack;                                               \
    }                                                                         \
    stack->stack[stack->count++] = value;                                     \
  }                                                                           \
  stack_type stack_name##_pop(struct stack_name *stack) {                     \
    if (stack->count <= stack->size / 4) {                                    \
      size_t new_size = stack->size / 2;                                      \
      stack_type *new_stack = malloc(new_size * sizeof(stack_type));          \
      memcpy(new_stack, stack->stack, new_size * sizeof(stack_type));         \
      free(stack->stack);                                                     \
      stack->size = new_size;                                                 \
      stack->stack = new_stack;                                               \
    }                                                                         \
    return stack->stack[--stack->count];                                      \
  }                                                                           \
  stack_type stack_name##_top(struct stack_name *stack) {                     \
    return stack->stack[stack->count - 1];                                    \
  }                                                                           \
  void stack_name##_replace(struct stack_name *stack, stack_type value) {     \
    stack->stack[stack->count - 1] = value;                                   \
  }                                                                           \
  stack_type stack_name##_postfix_inc(struct stack_name *stack) {             \
    return (stack->stack[stack->count - 1])++;                                \
  }                                                                           \
  size_t stack_name##_count(struct stack_name *stack) { return stack->count; }

#endif
