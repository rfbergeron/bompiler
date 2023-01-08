int puts(const char *str);
int puts_null(const char *str);

int main(int argc, char **argv) {
  return (argc ? puts : puts_null)(argv[0]);
}
