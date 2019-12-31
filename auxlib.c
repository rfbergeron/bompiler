#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#include "auxlib.h"

int flags[UCHAR_MAX + 1] = { 0 };

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

void debug_where (char flag, const char *file, int line,
                  const char *pretty_function, const char *msg) {
    fprintf (stderr, "./oc: DEBUG(%c) %s[%d] \n%s %s\n", flag, file, line,
             pretty_function, msg);
}

void debug_where_short (char flag, const char *file, int line, const char *msg) {
    fprintf (stderr, "./oc: DEBUG(%c) %s[%d] %s\n", flag, file, line, msg);
}
