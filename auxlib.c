#include "auxlib.h"

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

// not ideal but I'm the only one that'll ever be using it so whatever
#define MAX_MESSAGE_LEN 1024

int flags[UCHAR_MAX + 1] = {0};

void debug_set_flags (const char *initflags) {
    for (size_t i = 0; i < strlen (initflags); ++i) {
        const unsigned char flag = *(initflags + i);

        if (flag == '@') {
            for (size_t i = 0; i < UCHAR_MAX + 1; ++i) {
                flags[i] = 1;
            }
        } else {
            flags[flag] = 1;
        }
    }
}

// getflag -
//    Check to see if a certain flag is on.

int debug_get_flag (char flag) {
    // WARNING: Don't TRACE this function or the stack will blow up.
    return flags[(unsigned char) flag];
}

void debug_where (char flag,
                  const char *file,
                  int line,
                  const char *pretty_function,
                  const char *msg,
                  ...) {
    va_list args;

    va_start (args, msg);
    char full_msg[MAX_MESSAGE_LEN];

    vsprintf (full_msg, msg, args);
    warnx ("DEBUG(%c) %s[%d] \n%s %s\n",
           flag,
           file,
           line,
           pretty_function,
           full_msg);
    va_end (args);
}

void debug_where_short (char flag,
                        const char *file,
                        int line,
                        const char *msg,
                        ...) {
    va_list args;

    va_start (args, msg);
    char full_msg[MAX_MESSAGE_LEN];

    vsprintf (full_msg, msg, args);
    warnx ("DEBUG(%c) %s[%d] %s\n", flag, file, line, full_msg);
    va_end (args);
}
