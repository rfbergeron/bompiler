// $Id: auxlib.cpp,v 1.6 2019-08-22 17:14:14-07 - - $

#include <cassert>
#include <cerrno>
#include <cstring>
#include <iomanip>
#include <iostream>
using namespace std;

#include "auxlib.h"

string exec::execname;
int exec::exit_status = EXIT_SUCCESS;

exit_error::exit_error (int status) {
   if (exec::exit_status < status) exec::exit_status = status;
}

ostream& error() {
   exec::exit_status = EXIT_FAILURE;
   return cerr << exec::execname << ": ";
}

void cerr_signal (int signal) {
   cerr << signal;
   const char* sigstr = strsignal (signal);
   if (sigstr != nullptr) cerr << " (" << sigstr << ")";
}

void eprint_status (const char* command, int status) {
   if (status == 0) return; 
   ostringstream hexcode;
   hexcode << "0x" << hex << uppercase << setfill('0')
           << setw(4) << status;
   cerr << command << ": status " << hexcode.str();
   if (WIFEXITED (status)) {
      cerr << ", exit " << WEXITSTATUS (status);
   }
   if (WIFSIGNALED (status)) {
      cerr << ", Terminated via signal ";
      cerr_signal (WTERMSIG (status));
      #ifdef WCOREDUMP
      if (WCOREDUMP (status)) cerr << ", core dumped";
      #endif
   }
   if (WIFSTOPPED (status)) {
      cerr << ", Stopped via signal ";
      cerr_signal (WSTOPSIG (status));
   }
   if (WIFCONTINUED (status)) {
      cerr << ", Continued";
   }
   cerr << endl;
}

debug::flagset debug::flags {};

void debug::setflags (const string& initflags) {
   for (const unsigned char flag: initflags) {
      if (flag == '@') flags.set();
                  else flags.set (flag, true);
   }
}

// getflag -
//    Check to see if a certain flag is on.

bool debug::getflag (char flag) {
   // WARNING: Don't TRACE this function or the stack will blow up.
   return flags.test (static_cast<unsigned char> (flag));
}

void debug::where (char flag, const char* file, int line,
                        const char* pretty_function) {
   cerr << exec::execname << ": DEBUG(" << flag << ") "
        << file << "[" << line << "] " << endl
        << pretty_function << endl;
}

void debug::where_short (char flag, const char* file, int line) {
   cerr << exec::execname << ": DEBUG(" << flag << ") "
        << file << "[" << line << "] " << endl;
}


