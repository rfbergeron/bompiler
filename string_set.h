// $Id: string_set.h,v 1.1 2019-08-21 15:01:36-07 - - $

#ifndef __STRING_SET__
#define __STRING_SET__

#include <iostream>
#include <string>
#include <unordered_set>
using namespace std;

class string_set {
   private:
      static unordered_set<string> set;
   public:
      string_set();
      static const string* intern (const char*);
      static void dump (ostream&);
};

#endif

