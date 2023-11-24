#include "debug.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

/* not ideal but I'm the only one that'll ever be using it so whatever */
#define MAX_MESSAGE_LEN 1024
#define FILENAME_WIDTH 15
#define LINENR_DIGITS 4
#define STRINGY(arg) #arg
#define STRINGIFY(arg) STRINGY(arg)
static const char *DEBUG_FMT = "DEBUG(%c) %" STRINGIFY(
    FILENAME_WIDTH) "s:%-" STRINGIFY(LINENR_DIGITS) "d %s\n";

static int flags[UCHAR_MAX + 1] = {0};

void debug_set_flags(const char *initflags) {
  size_t i, flags_len = strlen(initflags);
  for (i = 0; i < flags_len; ++i) {
    char flag = initflags[i];

    if (flag == '@') {
      size_t j;
      for (j = 0; j < UCHAR_MAX + 1; ++j) {
        flags[j] = 1;
      }
    } else {
      flags[(unsigned char)flag] = 1;
    }
  }
}

int debug_get_flag(char flag) { return flags[(unsigned char)flag]; }

void debug_where(char flag, const char *file, int line, const char *msg, ...) {
  static char full_msg[MAX_MESSAGE_LEN];
  va_list args;
  va_start(args, msg);
  vsprintf(full_msg, msg, args);
  fprintf(stderr, DEBUG_FMT, flag, file, line, full_msg);
  va_end(args);
}
