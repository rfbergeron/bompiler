#define NULL ((void*)0)
typedef unsigned long size_t;

struct struc {
  void *begin;
  void *end;
};

void *malloc(size_t size);

int main(int argc, char **argv) {
  size_t struc_size = sizeof(struct struc);
  struct struc **arrs = malloc(sizeof(struct struc *) * 10);
  size_t i;
  for (i = 0; i < 10; ++i) {
    arrs[i] = malloc(sizeof(struct struc));
  }
  (struct { int *nums; size_t size; }*)arrs[0];
  (int (*)(void))NULL;
  return 0;
}
