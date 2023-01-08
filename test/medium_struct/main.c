#define NULL (void*) 0
struct medium {
  const char *s;
  unsigned long length;
};

void *malloc(unsigned long size);
int strlen(const char *str);
int puts(const char *str);

struct medium make_medium (unsigned long size) {
  struct medium ret;
  ret.s = malloc(size * sizeof(char));
  ret.length = size;
  return ret;
}

int medium_to_str (struct medium med) {
  if (med.length < strlen(med.s)) return -1;
  return puts(med.s);
}

int main(int argc, char **argv) {
  struct medium med = make_medium(10);
  return medium_to_str(med);
}
