#include <stdio.h>

static int arr[] = { 2, 1, 0 , 4, 6, 5};
static const size_t ARR_SIZE = sizeof(arr) / sizeof(*arr);

int main(void) {
  size_t i;
  for (i = 0; i < ARR_SIZE; ++i) {
    size_t j;
    size_t selected_index = i;
    for (j = i; j < ARR_SIZE; ++j) {
      if (arr[j] < arr[i]) selected_index = j;
    }
    int temp = arr[i];
    arr[i] = arr[selected_index];
    arr[selected_index] = temp;
  }

  fputs("Sorted array: [", stdout);
  for (i = 0; i < ARR_SIZE; ++i) {
    if (i == 0)
      printf("%i", arr[i]);
    else
      printf(", %i", arr[i]);
  }

  fputs("]\n", stdout);
  return 0;
}
