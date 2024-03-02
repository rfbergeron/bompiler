#include <stdio.h>

int (*fn_ptr)(const char *) = puts;

int main(int argc, char **argv) {
  (void)fn_ptr("Hello, world!");
  return 0;
}
