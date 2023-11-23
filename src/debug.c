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
#define LINENR_DIGITS 4

int flags[UCHAR_MAX + 1] = {0};

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
  /* TODO(Robert): check size without snprintf */
  char full_msg[MAX_MESSAGE_LEN];
  vsprintf(full_msg, msg, args);
  char spec_msg[MAX_MESSAGE_LEN];
  sprintf(spec_msg, "DEBUG(%%c) %%-%us[%%%ud] %%s", FILENAME_WIDTH,
          LINENR_DIGITS);
  /* warnx ("DEBUG(%c) %s[%d] %s", flag, file, line, full_msg); */
  warnx(spec_msg, flag, file, line, full_msg);
  va_end(args);
}
