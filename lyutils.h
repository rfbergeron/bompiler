#ifndef __UTILS_H__
#define __UTILS_H__

// flex and bison interface utilities

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "astree.h"
#include "attributes.h"
#include "debug.h"

//#define YYEOF 0

typedef struct ASTree ASTree;

extern FILE *yyin;
extern char *yytext;
extern int yy_flex_debug;
extern int yydebug;
extern int yyleng;
extern FILE *tokfile;
extern int lexer_interactive;
extern ASTree *parser_root;

int yylex();
int yylex_destroy();
int yyparse();
void yyerror(const char *message);

size_t lexer_get_filenr();
const char *lexer_filename(int filenr);
size_t lexer_include_linenr(int filenr);
void lexer_new_filename(const char *filename);
void lexer_advance();
void lexer_newline();
void lexer_bad_char(unsigned char bad);
void lexer_include();
int lexer_token(int symbol);
int lexer_bad_token(int symbol);
void lexer_fatal_error(const char *msg);
void lexer_error(const char *msg);
void lexer_dump_filenames(FILE *out);
void lexer_init_globals();
void lexer_free_globals();

const char *parser_get_tname(int symbol);
ASTree *parser_make_root();
ASTree *parser_make_type_id(ASTree *type, ASTree *id, ASTree *expr);
ASTree *parser_make_function(ASTree *type, ASTree *id, ASTree *paren,
                             ASTree *params, ASTree *block);
ASTree *parser_make_struct(ASTree *parent, ASTree *structure_id,
                           ASTree *structure_body);

#define YYSTYPE_IS_DECLARED
typedef ASTree *YYSTYPE;

#include "yyparse.h"

#endif
