int printf (const char *format, ...);

int main(int argc, char **argv) {
  return printf("Arg count: %i, Exec name: %s, Second arg: %s\n, Last Arg: %s",
          argc, argv[0], argv[1], argv[argc - 1]);
}
