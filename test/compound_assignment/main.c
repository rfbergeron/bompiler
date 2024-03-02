#include <stdio.h>

int main(void) {
  char *bing = NULL;
  int foo = 69;
  unsigned long bong = 6;
  unsigned char bang = 128;
  static long baz;
  printf("bing: %p, foo: %i, bong: %lu, bang: %u, baz: %li\n",
          bing, foo, bong, bang, baz);
  printf("baz -= foo: %li\n", baz -= foo);
  printf("baz %%= bang: %li\n", baz %= bang);
  printf("bing += foo: %p\n", bing += foo);
  printf("foo <<= bong: %i\n", foo <<= bong);
  printf("foo >>= 7: %i\n", foo >>= 7);
  printf("bang /= 24: %u\n", bang /= 24);
  printf("foo ^= foo: %i\n", foo ^= foo);
  return 0;
}
