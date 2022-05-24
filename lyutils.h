#ifndef __UTILS_H__
#define __UTILS_H__

/* flex and bison interface utilities */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "astree.h"
#include "attributes.h"
#include "debug.h"

/* #define YYEOF 0 */

typedef struct astree ASTree;

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
ASTree *parser_new_sym(ASTree *tree, int new_symbol);
ASTree *parser_make_spec_list(ASTree *first_specifier);
ASTree *parser_make_declaration(ASTree *spec_list);
ASTree *parser_make_param_list(ASTree *left_paren, ASTree *spec_list,
                               ASTree *declarator);
ASTree *parser_make_type_name(ASTree *first_child);
ASTree *parser_make_cast(ASTree *left_paren, ASTree *spec_list,
                         ASTree *type_name, ASTree *expr);
ASTree *parser_make_label(ASTree *ident, ASTree *stmt);
ASTree *parse_sizeof(ASTree *sizeof_, ASTree *spec_list, ASTree *declarator);
void parser_cleanup(size_t count, ...);

#define YYSTYPE_IS_DECLARED
typedef ASTree *YYSTYPE;

#include "yyparse.h"

#endif
