int array_param_fn(char []);

int main(int argc, char **argv) {
  return array_param_fn(argv[argc - 1]);
}
