#define NULL 0

struct agg {
  int one;
  int two;
  void *three;
};

int main(int argc, char **argv) {
  struct agg foo, bar;
  foo.one = 1;
  foo.two = 2;
  foo.three = NULL;
  bar = foo;
  return 0;
}
