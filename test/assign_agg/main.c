#include <stdio.h>

struct agg {
  int one;
  int two;
  const void *three;
};

static const char *baz = "baz";

int main(void) {
  struct agg foo, bar;
  foo.one = 1;
  foo.two = 2;
  foo.three = baz;
  printf("foo: { %i, %i, %p }\n", foo.one, foo.two, foo.three);
  bar = foo;
  printf("bar: { %i, %i, %p }\n", bar.one, bar.two, bar.three);
  return 0;
}
