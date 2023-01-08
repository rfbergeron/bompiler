#define va_start __builtin_va_start
#define va_end __builtin_va_end
typedef __builtin_va_list va_list;
typedef unsigned long size_t;

int vsprintf(char *str, const char *format, va_list ap);

int foo(size_t count, ...) {
  char buffer[1024];
  va_list args;
  va_start(args, count);
  return vsprintf(buffer, "count: %lu, args: %p, %p, %p, %p",
          args);
  va_end(args);
}
