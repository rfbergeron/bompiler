#include "lyutils.h"

#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arrlist.h"
#include "astree.h"
#include "debug.h"
#include "state.h"
#include "symtable.h"

extern FILE *tokfile;
size_t lexer_last_yyleng;
Location lexer_loc = {0, 1, 0, 0};
int lexer_interactive;
ASTree *parser_root;
ASTree *bcc_yyval;

ARR_STAT(char *, lexer_filenames);
ARR_STAT(size_t, lexer_include_linenrs);
const Location LOC_EMPTY = LOC_EMPTY_VALUE;
static char error_buffer[1024];

static int lexer_print_error(const char *message) {
  assert(!ARR_EMPTY(lexer_filenames));
  return fprintf(stderr, "%s:%lu.%lu: %s\n", lexer_filename(lexer_loc.filenr),
                 lexer_loc.linenr, lexer_loc.offset, message);
}

static void push_linenr(size_t linenr) {
  ARR_PUSH(lexer_include_linenrs, linenr);
}

size_t lexer_get_filenr() { return lexer_loc.filenr; }

Location lexer_get_loc(void) { return lexer_loc; }

const char *lexer_filename(int filenr) {
  assert((size_t)filenr < ARR_LEN(lexer_filenames));
  return ARR_GET(lexer_filenames, filenr);
}

size_t lexer_include_linenr(int filenr) {
  assert((size_t)filenr < ARR_LEN(lexer_include_linenrs));
  return ARR_GET(lexer_include_linenrs, filenr);
}

void lexer_new_filename(const char *filename) {
  lexer_loc.filenr = ARR_LEN(lexer_filenames);
  char *filename_copy = malloc((strlen(filename) + 1) * sizeof(char *));
  strcpy(filename_copy, filename);
  ARR_PUSH(lexer_filenames, filename_copy);
  push_linenr(lexer_loc.linenr + 1);
}

void lexer_advance() {
  if (lexer_interactive) {
    if (lexer_loc.offset == 0) {
      printf(";%3lu,%3lu: ", lexer_loc.filenr, lexer_loc.linenr);
    }
    printf("%s", yytext);
  }
  lexer_loc.offset += lexer_last_yyleng;
  lexer_last_yyleng = yyleng;
}

void lexer_newline() {
  ++lexer_loc.linenr;
  lexer_loc.offset = 0;
}

int lexer_bad_char(char bad) {
  lexical_error = 1;
  if (isgraph(bad))
    (void)sprintf(error_buffer, "Invalid source character (%c)", bad);
  else
    (void)sprintf(error_buffer, "Invalid source character (%#.2x)",
                  ((unsigned int)bad) & 0xff);

  (void)lexer_print_error(error_buffer);
  return TOK_LEX_ERROR;
}

void lexer_include() {
  size_t linenr;
  char filename[1024];
  int scan_rc = sscanf(yytext, "# %lu \"%[^\"]\"", &linenr, filename);

  if (scan_rc != 2) {
    fprintf(stderr, "Invalid directive, ignored: %s\n", yytext);
  } else {
    if (yy_flex_debug) {
      fprintf(stderr, "--included # %lu \"%s\"\n", linenr, filename);
    }
    fprintf(tokfile, "# %2lu %s\n", linenr, filename);
    lexer_loc.linenr = linenr - 1;
    lexer_new_filename(filename);
  }
}

int lexer_token(int tok_kind) {
  PFDBG2('l', "Found token with code: %p, length: %p", tok_kind, yyleng);
  yylval = astree_init(tok_kind, lexer_loc, yytext);
  fprintf(tokfile, "%2lu  %3lu.%3lu %3d %-13s %s\n", yylval->loc.filenr,
          yylval->loc.linenr, yylval->loc.offset, yylval->tok_kind,
          parser_get_tname(yylval->tok_kind), yylval->lexinfo);
  /* fprintf (tokfile, "%p: [%p]->%s, length: %u\n", symbol, yytext, yytext,
   * yyleng);
   */
  return tok_kind;
}

int lexer_ident(void) {
  PFDBG1('l', "Determining appropriate token type for identifier %s", yytext);
  Symbol *symbol = NULL;
  (void)state_get_symbol(state, yytext, &symbol);
  if (symbol == NULL || symbol->storage != STORE_TYPEDEF ||
      (bcc_yyval->tok_kind == TOK_SPEC_LIST &&
       !type_is_none(bcc_yyval->type))) {
    return lexer_token(TOK_IDENT);
  } else {
    return lexer_token(TOK_TYPEDEF_NAME);
  }
}

int lexer_bad_token(int tok_kind) {
  lexical_error = 1;
  (void)sprintf(error_buffer, "Invalid token %s (%s)",
                parser_get_tname(tok_kind), yytext);
  (void)lexer_print_error(error_buffer);
  return lexer_token(TOK_LEX_ERROR);
}

void lexer_dump_filenames(FILE *out) {
  size_t index;
  for (index = 0; index < ARR_LEN(lexer_filenames); ++index)
    fprintf(out, "filenames[%2lu] = \"%s\"\n", index,
            ARR_GET(lexer_filenames, index));
}

void lexer_init_globals() {
  ARR_INIT(lexer_include_linenrs, 8);
  ARR_INIT(lexer_filenames, 8);
}

void lexer_free_globals() {
  size_t index;
  for (index = 0; index < ARR_LEN(lexer_filenames); ++index)
    free(ARR_GET(lexer_filenames, index));
  ARR_DESTROY(lexer_filenames);
  ARR_DESTROY(lexer_include_linenrs);
}

void yyerror(const char *message) {
  if (!lexical_error) lexer_print_error(message);
}

int location_to_string(const Location *loc, char *buf) {
  return sprintf(buf, "%lu, %lu, %lu, %lu", loc->filenr, loc->linenr,
                 loc->offset, loc->blocknr);
}
