#ifndef __UTILS_H__
#define __UTILS_H__

// Lex and Yacc interface utility.

#include <string>
#include <vector>
#include <fstream>
using namespace std;

#include "auxlib.h"
#include "astree.h"

#define YYEOF 0

// forward declarations
struct astree;
struct location;

extern FILE* yyin;
extern char* yytext; 
extern int yy_flex_debug;
extern int yydebug;
extern int yyleng; 
extern ofstream tokfile;

int yylex();
int yylex_destroy();
int yyparse();
void yyerror (const char* message);

class lexer {
   private:
      static location loc;
      static size_t last_yyleng;
      static vector<string> filenames;
      static vector<size_t> include_linenrs;
   public:
      static bool interactive;
      static size_t get_filenr();
      static const string* filename (int filenr);
      static size_t include_linenr (int filenr);
      static void newfilename (const string& filename);
      static void advance();
      static void newline();
      static void badchar (unsigned char bad);
      static void include();
      static int token (int symbol);
      static int badtoken (int symbol);
      static void fatal_error (const char* msg);
      static ostream& error();
      static void dump_filenames (ostream&);
};

struct parser {
   static astree* root;
   static astree* newroot();
   static const char* get_tname (int symbol);
   static astree* make_function (astree* type, astree* id,
         astree* paren, astree* params, astree* block);
   static astree* make_type_id (astree* type, astree* id,
         astree* expr = nullptr);
   static astree* make_struct (astree* parent,
         astree* structure_id, astree* structure_body = nullptr);
};

#define YYSTYPE_IS_DECLARED
typedef astree* YYSTYPE;
#include "yyparse.h"

#endif

