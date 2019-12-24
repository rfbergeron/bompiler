// $Id: string_set.cpp,v 1.1 2019-08-21 15:01:36-07 - - $

#include <iomanip>
using namespace std;

#include "string_set.h"

unordered_set<string> string_set::set;

string_set::string_set() {
   set.max_load_factor (0.5);
}

string_set set;

const string* string_set::intern (const char* string) {
   auto handle = set.insert (string);
   return &*handle.first;
}

void string_set::dump (ostream& out) {
   static unordered_set<string>::hasher hash_fn
               = string_set::set.hash_function();
   size_t max_bucket_size = 0;
   for (size_t bucket = 0; bucket < set.bucket_count(); ++bucket) {
      bool need_index = true;
      size_t curr_size = set.bucket_size (bucket);
      if (max_bucket_size < curr_size) max_bucket_size = curr_size;
      for (auto itor = set.cbegin (bucket);
           itor != set.cend (bucket); ++itor) {
         if (need_index) {
            out << "string_set[" << setw(4) << bucket << "]: ";
         }else {
            out << "           " << setw(7) << "";
         }
         need_index = false;
         const string* str = &*itor;
         out << setw(20) << hash_fn(*str) << " " << str << "->\""
             << *str << "\"" << endl;
      }
   }
   out << "load_factor = " << set.load_factor() << endl;
   out << "bucket_count = " << set.bucket_count() << endl;
   out << "max_bucket_size = " << max_bucket_size << endl;
}

