#ifndef __AUXLIB_H__
#    define __AUXLIB_H__

#    include <limits.h>
#    include <stdio.h>
#    include <string.h>
#    include <stdarg.h>

//
// DESCRIPTION
//    Auxiliary library containing miscellaneous useful things.
//

//
// Error message and exit status utility.
//

/*struct exec {
   static string execname;
   static int exit_status;
};

struct exit_error: public exception {
   exit_error (int);
};*/

FILE *error ();

void eprint_status (const char *command, int status);

// Print the status returned by wait(2) from a subprocess.

// set_flags -
//    Takes a string argument, and sets a flag for each char in the
//    string.  As a special case, '@', sets all flags.
// get_flag -
//    Used by the DEBUGF macro to check to see if a flag has been set.
//    Not to be called by user code.

void debug_set_flags (const char *optflags);
int debug_get_flag (char flag);
void debug_where (char flag, const char *file, int line,
                  const char *pretty_function, const char *msg);
void debug_where_short (char flag, const char *file, int line, const char *msg);

// DEBUGF -
//    Macro which expands into debug code.  First argument is a
//    debug flag char, second argument is output code that can
//    be sandwiched between <<.  Beware of operator precedence.
//    Example:
//       DEBUGF ('u', "foo = " << foo);
//    will print two words and a newline if flag 'u' is  on.
//    Traces are preceded by filename, line number, and function.
//
// DEBUGH -
//    DEBUGF, but shortens debug statments by not printing the
//    long __PRETTY_FUNCTION__ macro

#    ifdef NDEBUG
#        define DEBUGF(FLAG,CODE) ;
#        define DEBUGS(FLAG,STMT) ;
#        define DEBUGH(FLAG,CODE) ;
#    else
#        define DEBUGF(FLAG,CODE) { \
             if (debug_get_flag (FLAG)) { \
                 debug_where (FLAG, __FILE__, __LINE__, \
                        __PRETTY_FUNCTION__, CODE); \
             } \
         }
#        define DEBUGS(FLAG,STMT) { \
             if (debug_get_flag (FLAG)) { \
                 debug_where (FLAG, __FILE__, __LINE__, \
                        __PRETTY_FUNCTION__, ""); \
                 STMT; \
             } \
         }
#        define DEBUGH(FLAG,CODE) { \
             if (debug_get_flag (FLAG)) { \
                 debug_where_short (FLAG, __FILE__, __LINE__, CODE); \
             } \
         }
#    endif

//
// Support for stub messages.
//
#    define STUB(CODE) { \
         debug::where (__FILE__, __LINE__, __PRETTY_FUNCTION__); \
         cerr << CODE << endl; \
     }

#endif
