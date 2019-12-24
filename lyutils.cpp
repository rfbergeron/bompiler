// $Id: lyutils.cpp,v 1.13 2019-09-20 16:23:59-07 - - $

#include <cassert>
#include <cctype>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
using namespace std;

#include "auxlib.h"
#include "lyutils.h"
#include "astree.h"
#include "symtable.h"

bool lexer::interactive = true;
location lexer::loc = {0, 1, 0};
size_t lexer::last_yyleng = 0;
vector<string> lexer::filenames;
vector<size_t> lexer::include_linenrs;
astree* parser::root = nullptr;
extern ofstream tokfile;

size_t lexer::get_filenr() {
   return lexer::loc.filenr;
}

const string* lexer::filename (int filenr) {
   return &lexer::filenames.at(filenr);
}

size_t lexer::include_linenr (int filenr) {
   return lexer::include_linenrs.at(filenr);
}

void lexer::newfilename (const string& filename) {
   lexer::loc.filenr = lexer::filenames.size();
   lexer::filenames.push_back (filename);
   lexer::include_linenrs.push_back (lexer::loc.linenr + 1);
}

void lexer::advance() {
   if (not interactive) {
      if (lexer::loc.offset == 0) {
         cout << ";" << setw(3) << lexer::loc.filenr << ","
              << setw(3) << lexer::loc.linenr << ": ";
      }
      cout << yytext;
   }
   lexer::loc.offset += last_yyleng;
   last_yyleng = yyleng;
}

void lexer::newline() {
   ++lexer::loc.linenr;
   lexer::loc.offset = 0;
}

void lexer::badchar (unsigned char bad) {
   ostringstream buffer;
   if (isgraph (bad)) buffer << bad;
                 else buffer << "\\" << setfill('0') << setw(3)
                             << static_cast<unsigned> (bad);
   lexer::error() << "invalid source character ("
                  << buffer.str() << ")" << endl;
}

void lexer::include() {
   size_t linenr;
   size_t filename_size = strlen (yytext) + 1;
   unique_ptr<char[]> filename = make_unique<char[]> (filename_size);
   int scan_rc = sscanf (yytext, "# %zu \"%[^\"]\"",
                         &linenr, filename.get());
   if (scan_rc != 2) {
      ::error() << "invalid directive, ignored: " << yytext << endl;
   } else {
      if (yy_flex_debug) {
         cerr << "--included # " << linenr << " \"" << filename.get()
              << "\"" << endl;
      }
      tokfile << "# " << setw(2) << linenr << " "
              << filename.get() << endl;
      lexer::loc.linenr = linenr - 1;
      lexer::newfilename (filename.get());
   }
}

int lexer::token (int symbol) {
   yylval = new astree (symbol, lexer::loc, yytext);
   tokfile << "  " << setw(2) << yylval->loc.filenr << "  " << setw(3)
           << yylval->loc.linenr << "." << setw(3) << setfill('0') 
           << yylval->loc.offset << " " << setw(3) << setfill(' ')
           << yylval->symbol << " " << setw(13) << left 
           << parser::get_tname(yylval->symbol) << right << " "
           << *(yylval->lexinfo) << endl;
   return symbol;
}

int lexer::badtoken (int symbol) {
   lexer::error() << "invalid token (" << yytext << ")" << endl;
   return lexer::token (symbol);
}

void lexer::fatal_error (const char* msg) {
   error() << msg << endl;
   throw exit_error (EXIT_FAILURE);
}

ostream& lexer::error() {
   assert (not lexer::filenames.empty());
   return ::error() << lexer::filename (loc.filenr) << ":"
          << loc.linenr << "." << loc.offset << ": ";
}

void lexer::dump_filenames (ostream& out) {
   for (size_t index = 0; index < filenames.size(); ++index) {
      out << "filenames[" << setw(2) << index << "] = \""
          << filenames[index] << "\"" << endl;
   }
}

void yyerror (const char* message) {
   lexer::error() << message << endl;
}

