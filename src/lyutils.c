#include "lyutils.h"

#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "astree.h"
#include "badllist.h"
#include "debug.h"
#include "state.h"
#include "symtable.h"

extern FILE *tokfile;
size_t lexer_last_yyleng;
Location lexer_loc = {0, 1, 0, 0};
int lexer_interactive = 0;
ASTree *parser_root;
ASTree *bcc_yyval;
struct llist lexer_filenames;
struct {
  size_t *data;
  size_t count;
  size_t size;
} lexer_include_linenrs = {NULL, 0, 10};
const Location LOC_EMPTY = LOC_EMPTY_VALUE;

/* internal functions */
void push_linenr(size_t linenr) {
  if (lexer_include_linenrs.count >= lexer_include_linenrs.size) {
    lexer_include_linenrs.size *= 2;
    lexer_include_linenrs.data =
        realloc(lexer_include_linenrs.data,
                lexer_include_linenrs.size * sizeof(size_t));
  }
  lexer_include_linenrs.data[lexer_include_linenrs.count++] = linenr;
}

/* external functions */
size_t lexer_get_filenr() { return lexer_loc.filenr; }

Location lexer_get_loc(void) { return lexer_loc; }

const char *lexer_filename(int filenr) {
  return llist_get(&lexer_filenames, (size_t)filenr);
}

size_t lexer_include_linenr(int filenr) {
  return lexer_include_linenrs.data[(size_t)filenr];
}

void lexer_new_filename(const char *filename) {
  lexer_loc.filenr = llist_size(&lexer_filenames);
  char *filename_copy = malloc((strlen(filename) + 1) * sizeof(char *));
  strcpy(filename_copy, filename);
  llist_push_front(&lexer_filenames, filename_copy);
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

void lexer_bad_char(unsigned char bad) {
  char buffer[1024];

  if (isgraph(bad))
    sprintf(buffer, "Invalid source character (%s)\n", &bad);
  else
    sprintf(buffer, "Invalid source character (\\%3u)\n",
            ((unsigned int)bad) & 0xFF);

  lexer_error(buffer);
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

int lexer_token(int symbol) {
  PFDBG2('l', "Found token with code: %p, length: %p", symbol, yyleng);
  yylval = astree_init(symbol, lexer_loc, yytext);
  fprintf(tokfile, "%2lu  %3lu.%3lu %3d %-13s %s\n", yylval->loc.filenr,
          yylval->loc.linenr, yylval->loc.offset, yylval->symbol,
          parser_get_tname(yylval->symbol), yylval->lexinfo);
  /* fprintf (tokfile, "%p: [%p]->%s, length: %u\n", symbol, yytext, yytext,
   * yyleng);
   */
  return symbol;
}

/* TODO(Robert): determine if types will always have been normalized by this
 * point and we can just use `typespec_is_*` functions instead of examining
 * flags directly
 */
int lexer_ident(void) {
  PFDBG1('l', "Determining appropriate token type for identifier %s", yytext);
  SymbolValue *symval = NULL;
  (void)state_get_symbol(state, yytext, yyleng, &symval);
  if (symval == NULL || !type_is_typedef(symval->type) ||
      (bcc_yyval->symbol == TOK_SPEC_LIST && !type_is_none(bcc_yyval->type))) {
    return lexer_token(TOK_IDENT);
  } else {
    return lexer_token(TOK_TYPEDEF_NAME);
  }
}

int lexer_iteration(int symbol) {
  (void)lexer_token(symbol);
  PFDBG1('l', "Creating jump stack entries for token %s", yytext);
  size_t id = state_next_jump_id(state);
  yylval->jump_id = id;
  state_push_break_id(state, id);
  state_push_continue_id(state, id);
  return symbol;
}

int lexer_switch(void) {
  (void)lexer_token(TOK_SWITCH);
  PFDBG1('l', "Creating jump stack entries for token %s", yytext);
  size_t id = state_next_jump_id(state);
  yylval->jump_id = id;
  state_push_break_id(state, id);
  state_push_selection(state, id);
  return TOK_SWITCH;
}

int lexer_if(void) {
  (void)lexer_token(TOK_IF);
  yylval->jump_id = state_next_jump_id(state);
  return TOK_IF;
}

int lexer_bad_token(int symbol) {
  char buffer[1024];
  sprintf(buffer, "Invalid token (%s)\n", yytext);
  lexer_error(buffer);
  return lexer_token(symbol);
}

void lexer_fatal_error(const char *msg) { errx(1, "%s", msg); }

void lexer_error(const char *message) {
  assert(llist_size(&lexer_filenames) != 0);
  fprintf(stderr, "%s:%lu.%lu: %s\n", lexer_filename(lexer_loc.filenr),
          lexer_loc.linenr, lexer_loc.offset, message);
}

void lexer_dump_filenames(FILE *out) {
  size_t index;
  for (index = 0; index < llist_size(&lexer_filenames); ++index) {
    fprintf(out, "filenames[%2lu] = \"%s\"\n", index,
            (const char *)llist_get(&lexer_filenames, index));
  }
}

/* TODO(Robert): add filename comparator to prevent duplicates */
void lexer_init_globals() {
  lexer_include_linenrs.data =
      malloc(lexer_include_linenrs.size * sizeof(size_t));
  llist_init(&lexer_filenames, free, NULL);
}

void lexer_free_globals() {
  llist_destroy(&lexer_filenames);
  free(lexer_include_linenrs.data);
  llist_destroy(&lexer_filenames);
}

void yyerror(const char *message) { lexer_error(message); }

int location_to_string(const Location *loc, char *buf, size_t size) {
  (void)size;
  /* TODO(Robert): check size without using snprintf */
  return sprintf(buf, "%lu, %lu, %lu, %lu", loc->filenr, loc->linenr,
                 loc->offset, loc->blocknr);
}
