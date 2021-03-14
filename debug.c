#include "debug.h"

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

/* not ideal but I'm the only one that'll ever be using it so whatever */
#define MAX_MESSAGE_LEN 1024
#define FILENAME_WIDTH 11
#define LINENR_DIGITS 3

int flags[UCHAR_MAX + 1] = {0};

void debug_set_flags(const char *initflags) {
  size_t i;
  for (i = 0; i < strlen(initflags); ++i) {
    const unsigned char flag = *(initflags + i);

    if (flag == '@') {
      size_t j;
      for (j = 0; i < UCHAR_MAX + 1; ++i) {
        flags[i] = 1;
      }
    } else {
      flags[flag] = 1;
    }
  }
}

int debug_get_flag(char flag) {
  /* WARNING: Don't TRACE this function or the stack will blow up. */
  return flags[(unsigned char)flag];
}

void debug_where(char flag, const char *file, int line,
                 const char *pretty_function, const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  char full_msg[MAX_MESSAGE_LEN];

  vsprintf(full_msg, msg, args);
  warnx("DEBUG(%c) %s[%d] \n%s %s\n", flag, file, line, pretty_function,
        full_msg);
  va_end(args);
}

void debug_where_short(char flag, const char *file, int line, const char *msg,
                       ...) {
  va_list args;
  va_start(args, msg);
  char full_msg[MAX_MESSAGE_LEN];
  vsnprintf(full_msg, MAX_MESSAGE_LEN, msg, args);
  char spec_msg[MAX_MESSAGE_LEN];
  snprintf(spec_msg, MAX_MESSAGE_LEN, "DEBUG(%%c) %%-%us[%%%ud] %%s",
           FILENAME_WIDTH, LINENR_DIGITS);
  /* warnx ("DEBUG(%c) %s[%d] %s", flag, file, line, full_msg); */
  warnx(spec_msg, flag, file, line, full_msg);
  va_end(args);
}
