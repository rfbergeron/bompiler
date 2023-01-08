struct testing {
  int distance;
  int *begin;
  int *end;
  int *middle;
};
static int arr[12];

int main(int argc, char **argv) {
  struct testing t = { (arr + 12) - arr, arr, arr + 12, arr + (12 / 2) };
}
