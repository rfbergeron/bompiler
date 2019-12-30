#ifndef __UTILS_H__
#    define __UTILS_H__

// flex and bison interface utilities

#    include <string.h>
#    include <stdio.h>
#    include <stdlib.h>
#    include <stddef.h>

#    include "auxlib.h"
#    include "astree.h"

#    define YYEOF 0

// forward declarations
struct astree;
struct location;

extern FILE *yyin;
extern char *yytext;
extern int yy_flex_debug;
extern int yydebug;
extern int yyleng;
extern FILE *tokfile;
int lexer_interactive;
struct astree *root;

int yylex ();
int yylex_destroy ();
int yyparse ();
void yyerror (const char *message);

size_t lexer_get_fileno ();
const char **lexer_filename (int fileno);
size_t lexer_include_lineno (int fileno);
void lexer_new_filename (const char **filename);
void lexer_advance ();
void lexer_newline ();
void lexer_badchar (unsigned char bad);
void lexer_include ();
int lexer_token (int symbol);
int lexer_badtoken (int symbol);
void lexer_fatal_error (const char *msg);
void lexer_error (const char *msg);
void lexer_dump_filenames (FILE * out);
void lexer_init_global_vars ();

#    define YYSTYPE_IS_DECLARED
typedef struct astree *YYSTYPE;

//#    include "yyparse.h"

#endif
