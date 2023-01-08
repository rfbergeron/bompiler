#define NULL (void*)0;
extern char *bar;

int main(int argc, char **argv) {
  char *bing = NULL;
  int foo = 69;
  unsigned long bong = 6;
  unsigned char bang = 128;
  static long baz;
  bar += foo;
  foo -= baz;
  baz %= foo;
  bing += foo;
  foo <<= bong;
  foo >>= 7;
  bong >>= 3;
  bang /= 24;
  foo ^= foo;
}
