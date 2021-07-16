/* clang-format off */
/* *INDENT-OFF* */
%{
#include <assert.h>

#include "lyutils.h"
#include "astree.h"
#include "debug.h"
#include "attributes.h"
/* #include "symtable.h" */
%}

%debug
%defines
%define parse.error verbose
%token-table
%verbose

%destructor { fprintf(stderr, "Cleanup\n"); } <>
%printer { assert (yyoutput == stderr); astree_dump($$, stderr); } <>

/* TODO(Robert): postfix increment/decrement should get
 * their own token codes to make assembly generation easier.
 */

/* dummy tokens used for precedence */
%token PREC_PREFIX PREC_POSTFIX
%token PREC_DEREF PREC_ADDROF PREC_TERNARY PREC_COMMA
/* tokens constructed by parser */
%token TOK_ROOT TOK_BLOCK TOK_CALL TOK_CAST TOK_INDEX TOK_PARAM TOK_FUNCTION
%token TOK_TYPE_ID TOK_INT_SPEC
%token TOK_POS TOK_NEG
/* tokens constructed by lexer */
%token TOK_VOID TOK_INT TOK_SHORT TOK_LONG TOK_CHAR TOK_UNSIGNED TOK_SIGNED
%token TOK_CONST TOK_VOLATILE TOK_RESTRICT
%token TOK_IF TOK_ELSE TOK_WHILE TOK_RETURN TOK_STRUCT TOK_UNION
%token TOK_ARROW TOK_EQ TOK_NE TOK_LE TOK_GE TOK_SHL TOK_SHR TOK_INC TOK_DEC TOK_AND TOK_OR
%token TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON

/* %left PREC_COMMA */
%right '=' /*TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ */
/* %right PREC_TERNARY */
%left TOK_OR
%left TOK_AND
%left '|'
%left '^'
%left '&'
%left TOK_EQ TOK_NE
%left TOK_LE TOK_GE '<' '>'
%left TOK_SHR TOK_SHL
%left '+' '-'
%left '*' '/' '%'
//%right PREC_PREFIX TOK_POS TOK_NEG '!' '~' TOK_CAST PREC_DEREF PREC_ADDROF
%right TOK_CAST PREC_PREFIX
/*%left PREC_POSTFIX TOK_CALL TOK_ARROW '.' TOK_INDEX */
%right PREC_POSTFIX

/* precedence of TOK_ELSE doesn't matter because it doesn't co-occur with operators, but it does need to be right-associative */
%right TOK_ELSE

%%
program       : %empty                                                            { parser_root = parser_make_root(); $$ = parser_root; }
              | program topdecl                                                   { $$ = astree_adopt($1, $2, NULL, NULL); }
              | program error '}'                                                 { $$ = $1; parser_cleanup (2, $2, $3); }
              | program error ';'                                                 { $$ = $1; parser_cleanup (2, $2, $3); }
              ;
topdecl       : fndecl block                                                      { $$ = astree_adopt($1, $2, NULL, NULL); }
              | fndecl ';'                                                        { $$ = $1; astree_destroy($2); }
              | vardecl ';'                                                       { $$ = $1; parser_cleanup(1, $2); }
              | ';'                                                               { $$ = NULL; astree_destroy($1); }
              ;
fndecl        : declarator '(' param_list ')'                                     { $$ = parser_make_function($1, $2, $3); astree_destroy ($4); }
              ;
vardecl       : declarator '=' expr                                               { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); }
              | declarator                                                        { $$ = $1; }
              ;
param_list    : %empty                                                            { $$ = NULL; }
              | TOK_VOID                                                          { $$ = NULL; astree_destroy($1); }
              | param_list ',' declarator                                         { $$ = astree_twin($1, $3); astree_destroy($2); }
              | declarator                                                        { $$ = $1; }
              ;
declarator    : type       TOK_IDENT                                              { $$ = parser_make_type_id($1, $2); }
              ;
typespec_list : typespec_list typespec                                            { $$ = astree_twin($1, $2); }
              | typespec                                                          { $$ = $1; }
              ;
typespec      : TOK_LONG                                                          { $$ = $1; }
              | TOK_SIGNED                                                        { $$ = $1; }
              | TOK_UNSIGNED                                                      { $$ = $1; }
              | TOK_SHORT                                                         { $$ = $1; }
              | TOK_INT                                                           { $$ = $1; }
              | TOK_CHAR                                                          { $$ = $1; }
              ;
block         : '{' stmt_list '}'                                                 { $$ = astree_adopt_sym($1, TOK_BLOCK, $2, NULL); parser_cleanup (1, $3); }
              ;
stmt_list     : %empty                                                            { $$ = NULL; }
              | stmt_list stmt                                                    { $$ = astree_twin($1, $2); }
              ;
stmt          : block                                                             { $$ = $1; }
              | while                                                             { $$ = $1; }
              | ifelse                                                            { $$ = $1; }
              | return                                                            { $$ = $1; }
              | expr ';'                                                          { $$ = $1; parser_cleanup (1, $2); }
              | vardecl ';'                                                       { $$ = $1; parser_cleanup (1, $2); }
              | ';'                                                               { $$ = NULL; astree_destroy($1); }
              ;
while         : TOK_WHILE '(' expr ')' stmt                                       { $$ = astree_adopt($1, $3, $5, NULL); parser_cleanup (2, $2, $4); }
              ;
ifelse        : TOK_IF '(' expr ')' stmt TOK_ELSE stmt                            { $$ = astree_adopt($1, $3, $5, $7); parser_cleanup (3, $2, $4, $6); }
              | TOK_IF '(' expr ')' stmt %prec TOK_ELSE                           { $$ = astree_adopt($1, $3, $5, NULL); parser_cleanup (2, $2, $4); }
              ;
return        : TOK_RETURN expr ';'                                               { $$ = astree_adopt($1, $2, NULL, NULL); parser_cleanup (1, $3); }
              | TOK_RETURN ';'                                                    { $$ = $1; parser_cleanup (1, $2); }
              ;
expr          : expr '+' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '-' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '/' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '*' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '%' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '>' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '<' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_LE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_GE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_EQ expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_NE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_OR expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_AND expr                                                 { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '|' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '&' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '^' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_SHR expr                                                 { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr TOK_SHL expr                                                 { $$ = astree_adopt($2, $1, $3, NULL); }
              | expr '=' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
              | unary_expr                                                        { $$ = $1; }
              ;
unary_expr    : postfix_expr                                                      { $$ = $1; }
              | TOK_INC unary_expr %prec PREC_PREFIX                              { $$ = astree_adopt($1, $2, NULL, NULL); }
              | TOK_DEC unary_expr %prec PREC_PREFIX                              { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '!' unary_expr                                                    { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '~' unary_expr                                                    { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '+' unary_expr %prec TOK_CAST                                     { $$ = astree_adopt_sym($1, TOK_POS, $2, NULL); }
              | '-' unary_expr %prec TOK_CAST                                     { $$ = astree_adopt_sym($1, TOK_NEG, $2, NULL); }
              | paren_toks unary_expr %prec TOK_CAST                              { $$ = parser_make_cast($1, $2); }
              ;
postfix_expr  : primary_expr                                                      { $$ = $1; }
              | primary_expr TOK_INC %prec PREC_POSTFIX                           { $$ = astree_adopt($2, $1, NULL, NULL); }
              | primary_expr TOK_DEC %prec PREC_POSTFIX                           { $$ = astree_adopt($2, $1, NULL, NULL); }
              | primary_expr '(' arg_list ')'                                     { $$ = astree_adopt_sym($2, TOK_CALL, $1, $3); parser_cleanup (1, $4); }
              ;
arg_list      : %empty                                                            { $$ = NULL; }
              | arg_list ',' expr                                                 { $$ = astree_twin($1, $3); astree_destroy($2); }
              | expr                                                              { $$ = $1; }
              ;
primary_expr  : TOK_IDENT                                                         { $$ = $1; }
              | constant                                                          { $$ = $1; }
              | paren_toks   %prec TOK_CAST                                       { $$ = $1; }
              ;
paren_toks    : '(' expr ')'   %prec TOK_CAST                                     { $$ = $2; parser_cleanup(2, $1, $3); }
              | '(' type ')'   %prec TOK_CAST                                     { $$ = $2; parser_cleanup(2, $1, $3); }
              ;
type          : TOK_VOID                                                          { $$ = $1;}
              | typespec_list                                                     { $$ = parser_make_int_spec($1); }
              ;
constant      : TOK_INTCON                                                        { $$ = $1; }
              | TOK_CHARCON                                                       { $$ = $1; }
              | TOK_STRINGCON                                                     { $$ = $1; }
              ;
%%

/* functions, structures, typeids, allocs can have their type
 * assigned when they adopt their children
 */

/* *INDENT-ON* */
    /* clang-format on */

    const char *parser_get_tname(int symbol) {
  return yytname[YYTRANSLATE(symbol)];
}

ASTree *parser_make_root() {
  DEBUGS('p', "Initializing AST, root token code: %d", TOK_ROOT);
  DEBUGS('p', "Translation of token code: %s", parser_get_tname(TOK_ROOT));
  Location root_loc = {lexer_get_filenr(), 0, 0};
  ASTree *root = astree_init(TOK_ROOT, root_loc, "_root");
  return root;
}

ASTree *parser_make_function(ASTree *type_id, ASTree *paren, ASTree *params) {
  ASTree *function = astree_init(TOK_FUNCTION, type_id->loc, "_function");
  return astree_adopt(function, type_id,
                      astree_adopt_sym(paren, TOK_PARAM, params, NULL), NULL);
}

ASTree *parser_make_type_id(ASTree *type, ASTree *id) {
  ASTree *type_id = astree_init(TOK_TYPE_ID, type->loc, "_type_id");
  return astree_adopt(type_id, type, id, NULL);
}

ASTree *parser_make_struct(ASTree *parent, ASTree *structure_id,
                           ASTree *structure_body) {
  return astree_adopt(parent, structure_id, structure_body, NULL);
}

/* due to grammar ambiguities regarding identifiers, the parser cannot (with
 * shift/reduce conflicts) distinguish between an expression surrounded by
 * parentheses and a cast operation with only one token of lookahead when a
 * TOK_IDENT is all that occurs between the parentheses.
 */
ASTree *parser_make_cast(ASTree *type, ASTree *expr) {
  ASTree *cast = astree_init(TOK_CAST, type->loc, "_cast");
  return astree_adopt(cast, type, expr, NULL);
}

ASTree *parser_make_int_spec(ASTree *list) {
  ASTree *int_spec = astree_init(TOK_INT_SPEC, list->loc, "_int_spec");
  return astree_adopt(int_spec, list, NULL, NULL);
}

void parser_cleanup(size_t count, ...) {
  va_list args;

  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *tree = va_arg(args, ASTree *);

    if (tree != NULL) {
      DEBUGS('t', "  ANNIHILATING: %d, %s", tree->symbol,
             parser_get_tname(tree->symbol));
      astree_destroy(tree);
    }
  }
  va_end(args);
}

int is_defined_token(int symbol) { return YYTRANSLATE(symbol) > YYUNDEF; }
