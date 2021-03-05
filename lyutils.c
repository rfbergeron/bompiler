#include "lyutils.h"

#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "astree.h"
#include "attributes.h"
#include "badlib/badllist.h"
#include "debug.h"
//#include "symtable.h"

extern FILE *tokfile;
size_t lexer_last_yyleng;
Location lexer_loc;
int lexer_interactive;
ASTree *parser_root;
struct llist *lexer_filenames;
struct {
  size_t *data;
  size_t count;
  size_t size;
} lexer_include_linenrs;

/* internal functions */
void push_linenr(size_t linenr) {
  if (lexer_include_linenrs.count == lexer_include_linenrs.size) {
    size_t new_size = lexer_include_linenrs.size * 2;
    size_t *new_data = malloc(lexer_include_linenrs.size * sizeof(*new_data));
    memcpy(new_data, lexer_include_linenrs.data, lexer_include_linenrs.size);
    free(lexer_include_linenrs.data);
    lexer_include_linenrs.size = new_size;
  }
  lexer_include_linenrs.data[lexer_include_linenrs.count++] = linenr;
}
/* external functions */
size_t lexer_get_filenr() { return lexer_loc.filenr; }

const char *lexer_filename(int filenr) {
  return llist_get(lexer_filenames, (size_t)filenr);
}

size_t lexer_include_linenr(int filenr) {
  return lexer_include_linenrs.data[(size_t)filenr];
}

void lexer_new_filename(const char *filename) {
  lexer_loc.filenr = llist_size(lexer_filenames);
  llist_push_front(lexer_filenames, (char *)filename);
  push_linenr(lexer_loc.linenr + 1);
}

void lexer_advance() {
  if (lexer_interactive) {
    if (lexer_loc.offset == 0) {
      printf(";%3d,%3d: ", lexer_loc.filenr, lexer_loc.linenr);
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

void lexer_bad_char(unsigned char bad) {
  char buffer[1024];

  if (isgraph(bad))
    sprintf(buffer, "Invalid source character (%s)\n", &bad);
  else
    sprintf(buffer, "Invalid source character (\\%3d)\n", (unsigned *)&bad);

  lexer_error(buffer);
}

void lexer_include() {
  size_t linenr;
  char filename[1024];
  int scan_rc = sscanf(yytext, "# %zu \"%[^\"]\"", &linenr, filename);

  if (scan_rc != 2) {
    fprintf(stderr, "Invalid directive, ignored: %s\n", yytext);
  } else {
    if (yy_flex_debug) {
      fprintf(stderr, "--included # %d \"%s\"\n", linenr, filename);
    }
    fprintf(tokfile, "# %2d %s\n", linenr, filename);
    lexer_loc.linenr = linenr - 1;
    lexer_new_filename(filename);
  }
}

int lexer_token(int symbol) {
  DEBUGS('l', "Found token with code: %p, length: %p", symbol, yyleng);
  yylval = astree_init(symbol, lexer_loc, yytext);
  fprintf(tokfile, "%2d  %3d.%3d %3d %-13s %s\n", yylval->loc.filenr,
          yylval->loc.linenr, yylval->loc.offset, yylval->symbol,
          parser_get_tname(yylval->symbol), yylval->lexinfo);
  // fprintf (tokfile, "%p: [%p]->%s, length: %u\n", symbol, yytext, yytext,
  // yyleng);
  return symbol;
}

int lexer_bad_token(int symbol) {
  char buffer[1024];
  sprintf(buffer, "Invalid token (%s)\n", yytext);
  lexer_error(buffer);
  return lexer_token(symbol);
}

void lexer_fatal_error(const char *msg) { errx(1, "%s", msg); }

void lexer_error(const char *message) {
  assert(llist_size(lexer_filenames) != 0);
  fprintf(stderr, "%s:%d.%d: %s", lexer_filename(lexer_loc.filenr),
          lexer_loc.linenr, lexer_loc.offset, message);
}

void lexer_dump_filenames(FILE *out) {
  for (size_t index = 0; index < llist_size(lexer_filenames); ++index) {
    fprintf(out, "filenames[%2d] = \"%s\"\n", index,
            (const char *)llist_get(lexer_filenames, index));
  }
}

void lexer_init_globals() {
  lexer_interactive = 0;
  lexer_loc = (Location){0, 1, 0};
  lexer_filenames = malloc(sizeof(*lexer_filenames));
  llist_init(lexer_filenames, NULL, NULL);
  lexer_include_linenrs.count = 0;
  lexer_include_linenrs.size = 10;
  lexer_include_linenrs.data =
      malloc(lexer_include_linenrs.size * sizeof(size_t));
}

void lexer_free_globals() {
  llist_destroy(lexer_filenames);
  free(lexer_include_linenrs.data);
}

void yyerror(const char *message) { lexer_error(message); }
