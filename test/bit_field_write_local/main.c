#include "defs.h"
#include <stdio.h>

int main(void) {
  struct foo f;
  f.a = 4;
  f.b = 127;
  f.d = 32;
  f.e = 16;
  struct bing i;
  int bar;
  bar = f.a;
  printf("f.a: %i\n", f.a);
  printf("bar: %i\n", bar);
  bar = f.b;
  printf("f.b: %i\n", f.b);
  printf("bar: %i\n", bar);
  bar = f.d;
  printf("f.d: %i\n", f.d);
  printf("bar: %i\n", bar);
  bar = f.e;
  printf("f.e: %i\n", f.e);
  printf("bar: %i\n", bar);

  f.a += 1;
  printf("f.a: %i\n", f.a);
  ++f.a;
  printf("f.a: %i\n", f.a);
  f.a++;
  printf("f.a: %i\n", f.a);

  printf("f.b: %i\n", f.b);
  f.b += 256;
  printf("f.b: %i\n", f.b);
  f.b *= 2;
  printf("f.b: %i\n", f.b);
  f.b += 100;
  printf("f.b: %i\n", f.b);
  f.b /= 15;
  printf("f.b: %i\n", f.b);

  return 0;
}
