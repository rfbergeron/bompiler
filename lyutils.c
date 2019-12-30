#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <err.h>

#include "auxlib.h"
#include "lyutils.h"
#include "astree.h"
//#include "symtable.h"
#include "vector.h"

extern FILE *tokfile;
size_t lexer_last_yyleng;
struct location lexer_loc;
struct vector *lexer_filenames;
struct vector *lexer_include_linenos;
struct astree *parser_root;

size_t lexer_get_fileno () {
    return lexer_loc.fileno;
}

const char **lexer_filename (int fileno) {
    return (vector_get (lexer_filenames, (size_t) fileno));
}

size_t lexer_include_lineno (int fileno) {
    // this oughtta be fixed; don't want to be casting size_t's to pointers but
    // dynamically allocating all that sounds like a pain in the ass
    return (size_t) vector_get (lexer_include_linenos, (size_t) fileno);
}

void lexer_newfilename (const char *filename) {
    lexer_loc.fileno = lexer_filenames->size;
    vector_push (lexer_filenames, (void *) filename);
    vector_push (lexer_include_linenos, (void *) lexer_loc.lineno + 1);
}

void lexer_advance () {
    if (!lexer_interactive) {
        if (lexer_loc.offset == 0) {
            printf (";%3d,%3d: ", lexer_loc.fileno, lexer_loc.lineno);
        }
        printf ("%s", yytext);
    }
    lexer_loc.offset += lexer_last_yyleng;
    lexer_last_yyleng = yyleng;
}

void lexer_newline () {
    ++lexer_loc.lineno;
    lexer_loc.offset = 0;
}

void lexer_bad_char (unsigned char bad) {
    // max size is len of error msg + parens + 3 digits for char as int + nul
    size_t bufsize = strlen ("Invalid source character (") + 3 + 1 + 1;
    char *buffer = (char *) calloc (sizeof (char), bufsize);

    if (isgraph (bad))
        sprintf (buffer, "Invalid source character (%s)\n", &bad);
    else
        sprintf (buffer, "Invalid source character (\\%3d)\n",
                 (unsigned *) &bad);

    lexer_error (buffer);
    free (buffer);
}

void lexer_include () {
    size_t lineno;
    size_t filename_size = strlen (yytext) + 1;
    char *filename = (char *) malloc (sizeof (char) * filename_size);
    int scan_rc = sscanf (yytext, "# %zu \"%[^\"]\"",
                          &lineno, filename);

    if (scan_rc != 2) {
        fprintf (stderr, "Invalid directive, ignored: %s\n", yytext);
    } else {
        if (yy_flex_debug) {
            fprintf (stderr, "--included # %d \"%s\"\n", lineno, filename);
        }
        fprintf (tokfile, "# %2d %s\n", lineno, filename);
        lexer_loc.lineno = lineno - 1;
        lexer_newfilename (filename);
    }
    free (filename);
}

int lexer_token (int symbol) {
    yylval = astree_init (symbol, lexer_loc, yytext);
    fprintf (tokfile, "%2d  %3d.%3d %3d %-13s %s\n", yylval->loc.fileno,
             yylval->loc.lineno, yylval->loc.offset, yylval->symbol,
             parser_get_tname (yylval->symbol), *(yylval->lexinfo));
    return symbol;
}

int lexer_badtoken (int symbol) {
    size_t bufsize = strlen ("Invalid token (") + strlen (yytext) + 3;
    char *buffer = (char *) malloc (sizeof (char) * bufsize);

    sprintf (buffer, "Invalid token (%s)\n", yytext);
    lexer_error (buffer);
    free (buffer);
    return lexer_token (symbol);
}

void lexer_fatal_error (const char *msg) {
    errx (1, "%s", msg);
}

void lexer_error (const char *message) {
    assert (!vector_empty (lexer_filenames));
    fprintf (stderr, "%s:%d.%d: %s", lexer_filename (lexer_loc.fileno),
             lexer_loc.lineno, lexer_loc.offset, message);
}

void lexer_dump_filenames (FILE * out) {
    for (size_t index = 0; index < lexer_filenames->size; ++index) {
        fprintf (out, "filenames[%2d] = \"%s\"\n",
                 index, (char *) vector_get (lexer_filenames, index));
    }
}

void lexer_init_global_vars () {
    lexer_interactive = 0;
    lexer_loc = (struct location) { 0, 1, 0 };
    lexer_filenames = vector_init (10);
    lexer_include_linenos = vector_init (10);
}

void yyerror (const char *message) {
    lexer_error (message);
}
