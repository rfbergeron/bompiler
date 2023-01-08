int puts(const char *);

int main(int argc, char **argv) {
  const volatile int foo;
  volatile char *const bar;
  void *const volatile baz;
  return puts(argv[0]);
}
