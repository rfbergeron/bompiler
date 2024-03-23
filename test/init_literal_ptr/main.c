#include <stddef.h>

#define HELLO_MSG "Hello, World!\n"
char hello[] = HELLO_MSG;

struct bar {
  char *str;
  size_t size;
} baz = { HELLO_MSG, sizeof(HELLO_MSG) };

int main (void) {
  struct bar baz = { HELLO_MSG, sizeof(HELLO_MSG) };
  return 0;
}
