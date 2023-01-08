int arr[3] = { 2, 1, 0 };

int main(int argc, char **argv) {
  int i;
  for (i = 0; i < 6; ++i) {
    int j;
    int selected_index = i;
    for (j = i; j < 6; ++j) {
      if (arr[j] < arr[i]) selected_index = j;
    }
    int temp = arr[i];
    arr[i] = arr[selected_index];
    arr[selected_index] = temp;
  }
  return 0;
}
