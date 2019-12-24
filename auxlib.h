// $Id: auxlib.h,v 1.5 2019-08-22 17:14:14-07 - - $

#ifndef __AUXLIB_H__
#define __AUXLIB_H__

#include <bitset>
#include <climits>
#include <iostream>
#include <string>
using namespace std;

#include <stdarg.h>

//
// DESCRIPTION
//    Auxiliary library containing miscellaneous useful things.
//

//
// Error message and exit status utility.
//

struct exec {
   static string execname;
   static int exit_status;
};

struct exit_error: public exception {
   exit_error (int);
};

ostream& error();

void eprint_status (const char* command, int status);
// Print the status returned by wait(2) from a subprocess.

// debug -
//    static class for maintaining global debug flags.
// setflags -
//    Takes a string argument, and sets a flag for each char in the
//    string.  As a special case, '@', sets all flags.
// getflag -
//    Used by the DEBUGF macro to check to see if a flag has been set.
//    Not to be called by user code.

class debug {
   private:
      using flagset = bitset<UCHAR_MAX + 1>;
      static flagset flags;
   public:
      static void setflags (const string& optflags);
      static bool getflag (char flag);
      static void where (char flag, const char* file, int line,
                         const char* pretty_function);
      static void where_short (char flag, const char* file, int line);
};

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

#ifdef NDEBUG
#define DEBUGF(FLAG,CODE) ;
#define DEBUGS(FLAG,STMT) ;
#define DEBUGH(FLAG,CODE) ;
#else
#define DEBUGF(FLAG,CODE) { \
           if (debug::getflag (FLAG)) { \
              debug::where (FLAG, __FILE__, __LINE__, \
                                 __PRETTY_FUNCTION__); \
              cerr << CODE << endl; \
           } \
        }
#define DEBUGS(FLAG,STMT) { \
           if (debug::getflag (FLAG)) { \
              debug::where (FLAG, __FILE__, __LINE__, \
                                 __PRETTY_FUNCTION__); \
              STMT; \
           } \
        }
#define DEBUGH(FLAG,CODE) { \
           if (debug::getflag (FLAG)) { \
              debug::where_short(FLAG, __FILE__, __LINE__); \
              cerr << CODE << endl; \
           } \
        }
#endif

//
// Support for stub messages.
//
#define STUB(CODE) { \
           debug::where (__FILE__, __LINE__, __PRETTY_FUNCTION__); \
           cerr << CODE << endl; \
        }

#endif

