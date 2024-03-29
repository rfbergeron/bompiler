#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

struct medium {
  int a[3];
};

struct large {
  int a[5];
};

union larger {
  struct {
    int tag;
    int left;
    int right;
  } a;
  struct {
    int tag;
    const char *id;
    ptrdiff_t disp;
  } b;
  struct {
    int tag;
    int cond;
    int left;
    int right;
    int cond_is_bool;
  } c;
};


int main(void) {
  struct medium arr[32];
  struct large brr[64];
  char crr[256];
  union larger drr[128];
  const int **mat = NULL;
  static const size_t ARR_SIZE = sizeof(arr) / sizeof(arr[0]);
  static const size_t BRR_SIZE = sizeof(brr) / sizeof(brr[0]);
  static const size_t CRR_SIZE = sizeof(crr) / sizeof(crr[0]);
  static const size_t DRR_SIZE = sizeof(drr) / sizeof(drr[0]);

  size_t i;
  for (i = 0; i < 8; ++i) {
    int r = rand();
    size_t arr_ix = r % ARR_SIZE;
    printf("arr index: %lu; arr base pointer: %p; arr element pointer: %p; difference: %li; reverse difference: %li\n",
            arr_ix, arr, arr + arr_ix, (arr + arr_ix) - arr, arr - (arr + arr_ix));
    size_t brr_ix = r % BRR_SIZE;
    printf("brr index: %lu; brr base pointer: %p; brr element pointer: %p; difference: %li; reverse difference: %li\n",
            brr_ix, brr, brr + brr_ix, (brr + brr_ix) - brr, brr - (brr + brr_ix));
    size_t crr_ix = r % CRR_SIZE;
    printf("crr index: %lu; crr base pointer: %p; crr element pointer: %p; difference: %li; reverse difference: %li\n",
            crr_ix, crr, crr + crr_ix, (crr + crr_ix) - crr, crr - (crr + crr_ix));
    size_t drr_ix = r % DRR_SIZE;
    printf("drr index: %lu; drr base pointer: %p; drr element pointer: %p; difference: %li; reverse difference: %li\n",
            drr_ix, drr, drr + drr_ix, (drr + drr_ix) - drr, drr - (drr + drr_ix));
    size_t mat_ix = r;
    printf("mat index: %lu; mat base pointer: %p; mat element pointer: %p; difference: %li; reverse difference: %li\n",
            mat_ix, mat, mat + mat_ix, (mat + mat_ix) - mat, mat - (mat + mat_ix));
  }

  return EXIT_SUCCESS;
}
