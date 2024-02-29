void foo(void) {
  extern int bar(void);
  extern unsigned long baz(const char *);
}

int bar();

int bar(void) {
  extern int arr[8];
  return arr[0];
}

int arr[];
int arr[8];
