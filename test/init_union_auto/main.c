typedef unsigned long size_t;
#define NULL (void*)0
union a {
  int x;
  char y[4];
  short z[2];
};
union b {
  struct {
    char *data;
    size_t len;
    size_t cap;
  } str;
  signed long signed_num;
  unsigned long unsigned_num;
  struct {
    void *data;
    int tag;
  } obj;
};
char arr[10];
char brr[20];
char crr[30];

int main (int argc, char **argv) {
  union a bing = {6};
  union a bong = {{6}};
  union b foo = {arr, 0, 10};
  union b bar = {{brr, 0, 20}};
  union b baz = {{{NULL}, {0}, {0}}};
  union b fizz = {{crr}, 0, 30};
  union b buzz = {crr, {0}, {30}};
}
