void foo(void);
int fn_param_fn(void (*)(void));

int main(int argc, char **argv) {
  return fn_param_fn(foo);
}
