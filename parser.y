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

%destructor { astree_destroy($$); } <>
%destructor { ; } program
%printer {
  char nodestr[1024];
  astree_to_string($$, nodestr, 1024);
  fprintf(yyoutput, "%p->%s", $$, nodestr);
} <>

/* TODO(Robert): postfix increment/decrement should get
 * their own token codes to make assembly generation easier.
 */

/* dummy tokens used for precedence */
/* %token PREC_PREFIX PREC_POSTFIX */
/* %token PREC_TERNARY PREC_COMMA */
/* tokens constructed by checker */
%token TOK_TYPE_ERROR TOK_AUTO_CONV TOK_AUTO_PROM
/* tokens constructed by parser */
%token TOK_TYPE_ID TOK_SPEC TOK_DECLARATION TOK_DECLARATOR TOK_ROOT TOK_CAST TOK_FUNCTION TOK_LABEL
/* tokens assigned to exsting nodes */
%token TOK_POS TOK_NEG TOK_POST_INC TOK_POST_DEC TOK_INDIRECTION TOK_ADDROF TOK_CALL TOK_SUBSCRIPT
%token TOK_BLOCK TOK_PARAM TOK_POINTER TOK_ARRAY TOK_INIT_LIST
/* tokens constructed by lexer */
%token TOK_VOID TOK_INT TOK_SHORT TOK_LONG TOK_CHAR TOK_UNSIGNED TOK_SIGNED
%token TOK_CONST TOK_VOLATILE TOK_RESTRICT TOK_TYPEDEF TOK_SIZEOF
%token TOK_IF TOK_ELSE TOK_SWITCH TOK_DO TOK_WHILE TOK_FOR TOK_STRUCT TOK_UNION TOK_ENUM
%token TOK_RETURN TOK_CONTINUE TOK_BREAK TOK_GOTO TOK_CASE TOK_DEFAULT
%token TOK_ARROW TOK_EQ TOK_NE TOK_LE TOK_GE TOK_SHL TOK_SHR TOK_AND TOK_OR TOK_INC TOK_DEC
%token TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON

/* precedence of TOK_ELSE doesn't matter because it doesn't co-occur with operators, but it does need to be right-associative */
%right TOK_ELSE

/* %left PREC_COMMA */
/* %right '=' TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ */
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
/* %right PREC_PREFIX TOK_POS TOK_NEG '!' '~' TOK_CAST PREC_INDIRECTION PREC_ADDROF */
/* %right TOK_CAST PREC_PREFIX */
/* %left PREC_POSTFIX TOK_CALL TOK_ARROW '.' TOK_SUBSCRIPT */
/* %right PREC_POSTFIX TOK_POST_DEC TOK_POST_INC */

%%
program       : topdecl                                                           { $$ = parser_root = astree_adopt(parser_make_root(), $1, NULL, NULL); }
              | program topdecl                                                   { $$ = astree_adopt($1, $2, NULL, NULL); }
              | program error '}'                                                 { $$ = $1; astree_destroy($3); }
              | program error ';'                                                 { $$ = $1; astree_destroy($3); }
              ;
topdecl       : declaration ';'                                                   { $$ = $1; astree_destroy($2); }
              | typespec_list declarator block                                    { $$ = astree_adopt(parser_make_declaration($1, $2), $3, NULL, NULL); }
              | ';'                                                               { $$ = NULL; astree_destroy($1); }
              ;
declaration   : typespec_list init_decls                                          { $$ = parser_make_declaration($1, $2); }
              | typespec_list                                                     { $$ = parser_make_declaration($1, NULL); }
              ;
typespec_list : typespec_list typespec                                            { $$ = astree_adopt($1, $2, NULL, NULL); }
              | typespec                                                          { $$ = parser_make_spec($1); }
              ;
typespec      : TOK_LONG                                                          { $$ = $1; }
              | TOK_SIGNED                                                        { $$ = $1; }
              | TOK_UNSIGNED                                                      { $$ = $1; }
              | TOK_SHORT                                                         { $$ = $1; }
              | TOK_INT                                                           { $$ = $1; }
              | TOK_CHAR                                                          { $$ = $1; }
              | TOK_VOID                                                          { $$ = $1; }
              | TOK_TYPEDEF                                                       { $$ = $1; }
              | struct_spec                                                       { $$ = $1; }
              | union_spec                                                        { $$ = $1; }
              | enum_spec                                                         { $$ = $1; }
              ;
struct_spec   : TOK_STRUCT TOK_IDENT '{' struct_decl_list '}'                     { $$ = astree_adopt($1, $2, $4, NULL); astree_destroy($3); astree_destroy($5); }
              | TOK_STRUCT '{' struct_decl_list '}'                               { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); astree_destroy($4); }
              | TOK_STRUCT TOK_IDENT                                              { $$ = astree_adopt($1, $2, NULL, NULL); }
              ;
union_spec    : TOK_UNION TOK_IDENT '{' struct_decl_list '}'                      { $$ = astree_adopt($1, $2, $4, NULL); astree_destroy($3); astree_destroy($5); }
              | TOK_UNION '{' struct_decl_list '}'                                { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); astree_destroy($4); }
              | TOK_UNION TOK_IDENT                                               { $$ = astree_adopt($1, $2, NULL, NULL); }
              ;
enum_spec     : TOK_ENUM TOK_IDENT '{' enum_list '}'                              { $$ = astree_adopt($1, $2, $4, NULL); astree_destroy($3); astree_destroy($5); }
              | TOK_ENUM TOK_IDENT '{' enum_list ',' '}'                          { $$ = astree_adopt($1, $2, $4, NULL); astree_destroy($3); astree_destroy($5); astree_destroy($6); }
              | TOK_ENUM '{' enum_list '}'                                        { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); astree_destroy($4); }
              | TOK_ENUM '{' enum_list ',' '}'                                    { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); astree_destroy($4); astree_destroy($5); }
              | TOK_ENUM TOK_IDENT                                                { $$ = astree_adopt($1, $2, NULL, NULL); }
              ;
struct_decl_list
              : struct_decl_list struct_decl ';'                                  { $$ = astree_twin($1, $2); astree_destroy($3); }
              | struct_decl ';'                                                   { $$ = $1; astree_destroy($2); }
              ;
struct_decl   : struct_decl ',' declarator                                        { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); }
              | typespec_list declarator                                          { $$ = parser_make_declaration($1, $2); }
              ;
enum_list     : enum_list ',' enumerator                                          { $$ = astree_twin($1, $3); astree_destroy($2); }
              | enumerator                                                        { $$ = $1; }
              ;
enumerator    : TOK_IDENT                                                         { $$ = $1; }
              | TOK_IDENT '=' cond_expr                                           { $$ = astree_adopt($2, $1, $3, NULL); }
              ;
init_decls    : init_decls ',' init_decl                                          { $$ = astree_twin($1, $3); astree_destroy($2); }
              | init_decl                                                         { $$ = $1; }
              ;
init_decl     : declarator '=' initializer                                        { $$ = astree_twin($1, $3); astree_destroy($2); }
              | declarator                                                        { $$ = $1; }
              ;
initializer   : assign_expr                                                       { $$ = $1; }
              | '{' init_list '}'                                                 { $$ = astree_adopt_sym($1, TOK_INIT_LIST, $2, NULL); astree_destroy($3); }
              | '{' init_list ',' '}'                                             { $$ = astree_adopt_sym($1, TOK_INIT_LIST, $2, NULL); astree_destroy($3); astree_destroy($4); }
              ;
init_list     : init_list ',' initializer                                         { $$ = astree_twin($1, $3); astree_destroy($2); }
              | initializer                                                       { $$ = $1; }
              ;
declarator    : pointer direct_decl                                               { $$ = parser_make_declarator($1, $2); }
              | direct_decl                                                       { $$ = parser_make_declarator(NULL, $1); }
              ;
direct_decl   : TOK_IDENT                                                         { $$ = $1; }
              | '(' declarator ')'                                                { $$ = $2; astree_destroy($1); astree_destroy($3); }
              | direct_decl '[' ']'                                               { $$ = parser_make_array($1, $2, NULL); astree_destroy($3); }
              | direct_decl '[' cond_expr   ']'                                   { $$ = parser_make_array($1, $2, $3); astree_destroy($4); }
              | direct_decl '(' param_list ')'                                    { $$ = parser_make_function($1, $2, $3); astree_destroy($4); }
              | direct_decl '(' ')'                                               { $$ = parser_make_function($1, $2, NULL); astree_destroy($3); }
              | direct_decl '(' TOK_VOID ')'                                      { $$ = parser_make_function($1, $2, NULL); astree_destroy($3); astree_destroy($4); }
              ;
abstract_decl : pointer dir_abs_decl                                              { $$ = parser_make_declarator($1, $2); }
              | dir_abs_decl                                                      { $$ = parser_make_declarator(NULL, $1); }
              | pointer                                                           { $$ = parser_make_declarator($1, NULL); }
              ;
dir_abs_decl  : '(' abstract_decl ')'                                             { $$ = $2; astree_destroy($1); astree_destroy($3); }
              | dir_abs_decl '[' ']'                                              { $$ = parser_make_array($1, $2, NULL); astree_destroy($3); }
              | dir_abs_decl '[' cond_expr   ']'                                  { $$ = parser_make_array($1, $2, $3); astree_destroy($4); }
              | dir_abs_decl '(' param_list ')'                                   { $$ = parser_make_function($1, $2, $3); astree_destroy($4); }
              | dir_abs_decl '(' ')'                                              { $$ = parser_make_function($1, $2, NULL); astree_destroy($3); }
              | dir_abs_decl '(' TOK_VOID ')'                                     { $$ = parser_make_function($1, $2, NULL); astree_destroy($3); astree_destroy($4); }
              | '[' ']'                                                           { $$ = parser_make_array(NULL, $1, NULL); astree_destroy($2); }
              | '[' cond_expr   ']'                                               { $$ = parser_make_array(NULL, $1, $2); astree_destroy($3); }
              ;
pointer       : pointer '*'                                                       { $$ = astree_twin($1, astree_adopt_sym($2, TOK_POINTER, NULL, NULL)); }
              | '*'                                                               { $$ = astree_adopt_sym($1, TOK_POINTER, NULL, NULL);; }
              ;
param_list    : param_list ',' typespec_list declarator                           { $$ = astree_twin($1, parser_make_declaration($3, $4)); astree_destroy($2); }
              | typespec_list declarator                                          { $$ = parser_make_declaration($1, $2); }
              ;
block         : '{' block_content '}'                                             { $$ = astree_adopt_sym($1, TOK_BLOCK, $2, NULL); astree_destroy($3); }
              | '{' '}'                                                           { $$ = astree_adopt_sym($1, TOK_BLOCK, NULL, NULL); astree_destroy($2); }
              ;
block_content : block_content stmt                                                { $$ = astree_twin($1, $2); }
              | block_content declaration ';'                                     { $$ = astree_twin($1, $2); astree_destroy($3); }
              | stmt                                                              { $$ = $1; }
              | declaration ';'                                                   { $$ = $1; astree_destroy($2); }
              ;
stmt          : block                                                             { $$ = $1; }
              | dowhile                                                           { $$ = $1; }
              | while                                                             { $$ = $1; }
              | for                                                               { $$ = $1; }
              | ifelse                                                            { $$ = $1; }
              | switch                                                            { $$ = $1; }
              | return                                                            { $$ = $1; }
              | labelled_stmt                                                     { $$ = $1; }
              | expr ';'                                                          { $$ = $1; parser_cleanup (1, $2); }
              | TOK_CONTINUE ';'                                                  { $$ = $1; astree_destroy($2); }
              | TOK_BREAK ';'                                                     { $$ = $1; astree_destroy($2); }
              | TOK_GOTO TOK_IDENT ';'                                            { $$ = astree_adopt($1, $2, NULL, NULL); astree_destroy($3); }
              | ';'                                                               { $$ = NULL; astree_destroy($1); }
              ;
labelled_stmt : TOK_IDENT ':' stmt                                                { $$ = parser_make_label($1, $3); astree_destroy ($2); }
              | TOK_DEFAULT ':' stmt                                              { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy ($2); }
              | TOK_CASE cond_expr   ':' stmt                                     { $$ = astree_adopt($1, $2, $4, NULL); astree_destroy ($3); }
              ;
for           : TOK_FOR for_exprs stmt                                            { $$ = astree_adopt($1, $2, $3, NULL); }
              ;
for_exprs     : '(' ';' ';' ')'                                                   { $$ = astree_adopt($1, $2, $3, $4); }
              | '(' expr ';' ';' ')'                                              { $$ = astree_adopt($1, $2, $3, $4); astree_destroy($5); }
              | '(' ';' expr ';' ')'                                              { $$ = astree_adopt($1, $2, $3, $4); astree_destroy($5); }
              | '(' ';' ';' expr ')'                                              { $$ = astree_adopt($1, $2, $3, $4); astree_destroy($5); }
              | '(' expr ';' expr ';' ')'                                         { $$ = astree_adopt($1, $2, $4, $5); astree_destroy($3); astree_destroy($6); }
              | '(' expr ';' ';' expr ')'                                         { $$ = astree_adopt($1, $2, $4, $5); astree_destroy($3); astree_destroy($6); }
              | '(' ';' expr ';' expr ')'                                         { $$ = astree_adopt($1, $2, $3, $5); astree_destroy($4); astree_destroy($6); }
              | '(' expr ';' expr ';' expr ')'                                    { $$ = astree_adopt($1, $2, $4, $6); astree_destroy($3); astree_destroy($5); astree_destroy($7); }
              ;
dowhile       : TOK_DO stmt TOK_WHILE '(' expr ')' ';'                            { $$ = astree_adopt($1, $2, $5, NULL); parser_cleanup (4, $3, $4, $6, $7); }
              ;
while         : TOK_WHILE '(' expr ')' stmt                                       { $$ = astree_adopt($1, $3, $5, NULL); parser_cleanup (2, $2, $4); }
              ;
ifelse        : TOK_IF '(' expr ')' stmt TOK_ELSE stmt                            { $$ = astree_adopt($1, $3, $5, $7); parser_cleanup (3, $2, $4, $6); }
              | TOK_IF '(' expr ')' stmt %prec TOK_ELSE                           { $$ = astree_adopt($1, $3, $5, NULL); parser_cleanup (2, $2, $4); }
              ;
switch        : TOK_SWITCH '(' expr ')' stmt                                      { $$ = astree_adopt($1, $3, $5, NULL); astree_destroy($2); astree_destroy($4); }
              ;
return        : TOK_RETURN expr ';'                                               { $$ = astree_adopt($1, $2, NULL, NULL); parser_cleanup (1, $3); }
              | TOK_RETURN ';'                                                    { $$ = $1; parser_cleanup (1, $2); }
              ;
expr          : assign_expr                                                       { $$ = $1; }
              | expr ',' assign_expr                                              { $$ = astree_adopt($2, $1, $3, NULL); }
              ;
assign_expr   : assign_expr '=' cond_expr                                         { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_ADDEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_SUBEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_MULEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_DIVEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_REMEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_ANDEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_OREQ cond_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_XOREQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_SHREQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | assign_expr TOK_SHLEQ cond_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | cond_expr                                                         { $$ = $1; }
              ;
cond_expr     : binary_expr '?' expr ':' cond_expr                                { $$ = astree_adopt($2, $1, $3, $5); astree_destroy($4); }
              | binary_expr                                                       { $$ = $1; }
              ;
binary_expr   : binary_expr '+' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '-' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '/' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '*' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '%' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '>' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '<' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_LE binary_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_GE binary_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_EQ binary_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_NE binary_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_OR binary_expr                                    { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_AND binary_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '|' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '&' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr '^' binary_expr                                       { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_SHR binary_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | binary_expr TOK_SHL binary_expr                                   { $$ = astree_adopt($2, $1, $3, NULL); }
              | cast_expr                                                         { $$ = $1; }
              ;
cast_expr     : '(' typespec_list ')' unary_expr                                  { $$ = parser_make_cast($2, NULL, $4); parser_cleanup(2, $1, $3); }
              | '(' typespec_list abstract_decl ')' unary_expr                    { $$ = parser_make_cast($2, $3, $5); parser_cleanup(2, $1, $4); }
              | unary_expr                                                        { $$ = $1; }
              ;
unary_expr    : postfix_expr                                                      { $$ = $1; }
              | TOK_INC unary_expr                                                { $$ = astree_adopt($1, $2, NULL, NULL); }
              | TOK_DEC unary_expr                                                { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '!' unary_expr                                                    { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '~' unary_expr                                                    { $$ = astree_adopt($1, $2, NULL, NULL); }
              | '+' unary_expr                                                    { $$ = astree_adopt_sym($1, TOK_POS, $2, NULL); }
              | '-' unary_expr                                                    { $$ = astree_adopt_sym($1, TOK_NEG, $2, NULL); }
              | '*' unary_expr                                                    { $$ = astree_adopt_sym($1, TOK_INDIRECTION, $2, NULL); }
              | '&' unary_expr                                                    { $$ = astree_adopt_sym($1, TOK_ADDROF, $2, NULL); }
              | TOK_SIZEOF unary_expr                                             { $$ = astree_adopt($1, $2, NULL, NULL); }
              | TOK_SIZEOF '(' typespec_list ')'                                  { $$ = astree_adopt($1, $3, NULL, NULL); astree_destroy($2); astree_destroy($4); }
              | TOK_SIZEOF '(' typespec_list abstract_decl ')'                    { $$ = astree_adopt($1, $3, $4, NULL); astree_destroy($2); astree_destroy($5); }
              ;
postfix_expr  : primary_expr                                                      { $$ = $1; }
              | postfix_expr TOK_INC                                              { $$ = astree_adopt_sym($2, TOK_POST_INC, $1, NULL); }
              | postfix_expr TOK_DEC                                              { $$ = astree_adopt_sym($2, TOK_POST_DEC, $1, NULL); }
              | postfix_expr '(' arg_list ')'                                     { $$ = astree_adopt_sym($2, TOK_CALL, $1, $3); parser_cleanup (1, $4); }
              | postfix_expr '[' expr ']'                                         { $$ = astree_adopt_sym($2, TOK_SUBSCRIPT, $1, $3); astree_destroy($4); }
              | postfix_expr '.' TOK_IDENT                                        { $$ = astree_adopt($2, $1, $3, NULL); }
              | postfix_expr TOK_ARROW TOK_IDENT                                  { $$ = astree_adopt($2, $1, $3, NULL); }
              ;
arg_list      : %empty                                                            { $$ = NULL; }
              | arg_list ',' assign_expr                                          { $$ = astree_twin($1, $3); astree_destroy($2); }
              | assign_expr                                                       { $$ = $1; }
              ;
primary_expr  : TOK_IDENT                                                         { $$ = $1; }
              | constant                                                          { $$ = $1; }
              | '(' expr ')'                                                      { $$ = $2; parser_cleanup(2, $1, $3); }
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

ASTree *parser_make_declaration(ASTree *spec, ASTree *decls) {
  ASTree *declaration = astree_init(TOK_DECLARATION, spec->loc, "_declaration");
  return astree_adopt(declaration, spec, decls, NULL);
}

ASTree *parser_make_spec(ASTree *list) {
  ASTree *spec = astree_init(TOK_SPEC, list->loc, "_spec");
  return astree_adopt(spec, list, NULL, NULL);
}

/* Children of the declarator are adopted in an order that makes it easy to read
 * out the type. Iterating over the declarator's children from left to right,
 * ignoring the identifier, gives an accurate reading of the type. For this to
 * be the case, pointers should be the last children of a declarator.
 */
ASTree *parser_make_declarator(ASTree *pointer, ASTree *direct_decl) {
  ASTree *declarator = astree_init(
      TOK_DECLARATOR, pointer ? pointer->loc : direct_decl->loc, "_declarator");
  return astree_adopt(declarator, direct_decl, pointer, NULL);
}

ASTree *parser_make_function(ASTree *direct_decl, ASTree *paren,
                             ASTree *params) {
  return astree_twin(direct_decl,
                     astree_adopt_sym(paren, TOK_FUNCTION, params, NULL));
}

ASTree *parser_make_array(ASTree *direct_decl, ASTree *bracket,
                          ASTree *length) {
  return astree_twin(direct_decl,
                     astree_adopt_sym(bracket, TOK_ARRAY, length, NULL));
}

ASTree *parser_make_type_id(ASTree *type, ASTree *id) {
  ASTree *type_id = astree_init(TOK_TYPE_ID, type->loc, "_type_id");
  return astree_adopt(type_id, type, id, NULL);
}

ASTree *parser_make_cast(ASTree *spec_list, ASTree *abstract_declaration,
                         ASTree *expr) {
  ASTree *cast = astree_init(TOK_CAST, spec_list->loc, "_cast");
  return astree_adopt(cast, spec_list, abstract_declaration, expr);
}

ASTree *parser_make_label(ASTree *ident, ASTree *stmt) {
  ASTree *label = astree_init(TOK_LABEL, ident->loc, "_label");
  return astree_adopt(label, ident, stmt, NULL);
}

void parser_cleanup(size_t count, ...) {
  va_list args;

  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *tree = va_arg(args, ASTree *);

    if (tree != NULL) {
      astree_destroy(tree);
    }
  }
  va_end(args);
}

int is_defined_token(int symbol) { return YYTRANSLATE(symbol) > YYUNDEF; }
