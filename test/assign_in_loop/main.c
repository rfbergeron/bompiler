struct agg {
  int one;
  int two;
  void *three;
} agg_from_str(const char *str);
typedef unsigned long size_t;

int main(int argc, char **argv) {
  size_t i, count = argc > 0 ? argc : 0;
  for (i = 1; i < count; ++i) {
    struct agg foo = agg_from_str(argv[i]);
  }
  return 0;
}
