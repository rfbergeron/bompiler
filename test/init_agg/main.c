#define NULL 0

struct agg {
  int one;
  int two;
  void *three;
};

int main(int argc, char **argv) {
  struct agg foo = { 1, 2, NULL };
  struct agg bar = foo;
  return 0;
}
