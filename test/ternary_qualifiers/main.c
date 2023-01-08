extern int *t0;
extern int *t1;
extern int *const t2;
extern int *volatile t3;
extern int *const volatile t4;
extern int *const volatile t5;
extern void foo(int *const volatile);

int main(int argc, char **argv) {
  foo(argc != 0 ? t0 : t1);
  foo(argc != 0 ? t1 : t2);
  foo(argc != 0 ? t1 : t3);
  foo(argc != 0 ? t1 : t4);
  foo(argc != 0 ? t2 : t3);
  foo(argc != 0 ? t3 : t4);
  foo(argc != 0 ? t2 : t4);
  foo(argc != 0 ? t4 : t5);
  return 0;
}
