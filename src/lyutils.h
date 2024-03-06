#ifndef __UTILS_H__
#define __UTILS_H__

/* flex and bison interface utilities */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"

/* #define YYEOF 0 */

typedef struct location {
  size_t filenr;
  size_t linenr;
  size_t offset;
  size_t blocknr;
} Location;

#define LOC_EMPTY_VALUE \
  { 0, 0, 0, 0 }

extern const Location LOC_EMPTY;

extern FILE *yyin;
extern char *yytext;
extern int yy_flex_debug;
extern int yydebug;
extern int yyleng;
extern FILE *tokfile;
extern int lexer_interactive;
extern struct astree *parser_root;
extern struct astree *bcc_yyval;

int yylex();
int yylex_destroy();
int yyparse();
void yyerror(const char *message);

size_t lexer_get_filenr();
Location lexer_get_loc(void);
const char *lexer_filename(int filenr);
size_t lexer_include_linenr(int filenr);
void lexer_new_filename(const char *filename);
void lexer_advance();
void lexer_newline();
void lexer_include();
int lexer_bad_char(char bad);
int lexer_token(int tok_kind);
int lexer_ident(void);
int lexer_iteration(int tok_kind);
int lexer_switch(void);
int lexer_case(void);
int lexer_default(void);
int lexer_break(void);
int lexer_continue(void);
int lexer_if(void);
int lexer_bad_token(int tok_kind);
void lexer_dump_filenames(FILE *out);
void lexer_init_globals();
void lexer_free_globals();

int location_to_string(const Location *loc, char *buf);
const char *parser_get_tname(int tok_kind);
void parser_init_globals(void);
void parser_destroy_globals(void);

#define YYSTYPE_IS_DECLARED
typedef struct astree *YYSTYPE;

#include "yyparse.h"

#endif
