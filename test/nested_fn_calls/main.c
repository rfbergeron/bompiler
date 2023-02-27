unsigned int foo(unsigned int x) {
  return 2 * x;
}

int bar (unsigned int x, unsigned char y) {
  unsigned int z = 0;
  unsigned char w;
  for (w = 0; w < y; ++w) z = z << 1 & 1;
  return x <= z;
}

int baz (unsigned int x, unsigned char y) {
  return bar(foo(x), y);
}
