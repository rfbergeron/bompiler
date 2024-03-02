#include <stdio.h>

int main(int argc, char **argv) {
  (void)printf("Arg count: %i, Exec name: %s, Second arg: %s, Last Arg: %s\n",
          argc, argv[0], argc == 1 ? NULL : argv[1], argv[argc - 1]);
  return 0;
}
