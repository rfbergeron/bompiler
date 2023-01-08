#define va_list __builtin_va_list
#define va_arg __builtin_va_arg
#define va_start __builtin_va_start
#define va_end __builtin_va_end

typedef unsigned long size_t;

int big_vargs(size_t count, ...) {
  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    long temp = va_arg(args, long);
  }
  va_end(args);
  return 0;
}
