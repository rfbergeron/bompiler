#include <stdint.h>
extern intptr_t hmm;

int main (int argc, char **argv) {
  int huh = ((int(*)(void))(0xDEADBEEF))();
  return ((int(*)(void))hmm)();
}
