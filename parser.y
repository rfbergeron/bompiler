%{
#include <assert.h>

#include "lyutils.h"
#include "astree.h"
#include "symtable.h"
%}

%debug
%defines
%define parse.error verbose
%token-table
%verbose

%destructor { astree_destroy (1, $$); } <>
%printer { assert (yyoutput == stderr); astree_dump_node ($$, stderr); } <>

%token TOK_VOID TOK_INT TOK_STRING TOK_TYPE_ID
%token TOK_IF TOK_ELSE TOK_WHILE TOK_RETURN TOK_STRUCT
%token TOK_NULLPTR TOK_ARRAY TOK_ARROW TOK_ALLOC TOK_PTR
%token TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE TOK_NOT
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON
%token TOK_ROOT TOK_BLOCK TOK_CALL TOK_PARAM TOK_FUNCTION
%token TOK_INDEX

%right '='
%left  TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE
%left  '+' '-'
%left  '*' '/' '%'
%right TOK_POS TOK_NEG TOK_NOT
%left TOK_ARROW TOK_INDEX
%right TOK_ELSE


%%

start       : program                                                           { $$ = $1 = NULL; }
            ;
program     : program structdef                                                 { $$ = astree_adopt($1, $2, NULL, NULL); }
            | program function                                                  { $$ = astree_adopt($1, $2, NULL, NULL); }
            | program vardecl                                                   { $$ = astree_adopt($1, $2, NULL, NULL); }
            | program error '}'                                                 { $$ = $1; }
            | program error ';'                                                 { $$ = $1; }
            | %empty                                                            { $$ = parser_make_root ();}
            ;
structdef   : TOK_STRUCT TOK_IDENT '{' structbody '}' ';'                       { $$ = astree_adopt($1, $2, $4, NULL); }
            | TOK_STRUCT TOK_IDENT '{' '}' ';'                                  { $$ = astree_adopt($1, $2, NULL, NULL); }
            ;
structbody  : type TOK_IDENT ';'                                                { $$ = parser_make_type_id($1, $2, NULL); }
            | structbody type TOK_IDENT ';'                                     { $$ = astree_buddy_up($1, parser_make_type_id($2, $3, NULL)); }
            ;
function    : type TOK_IDENT '(' parameters ')' block                           { $$ = parser_make_function($1, $2, $3, $4, $6); }
            | type TOK_IDENT '(' ')' block                                      { $$ = parser_make_function($1, $2, $3, NULL, $5); }
            ;
parameters  : type TOK_IDENT                                                    { $$ = parser_make_type_id($1, $2, NULL); }
            | parameters ',' type TOK_IDENT                                     { $$ = astree_buddy_up($1, parser_make_type_id($3, $4, NULL)); }
            ;
type        : plaintype                                                         { $$ = $1; }
            | TOK_VOID                                                          { $$ = $1; }
            | TOK_ARRAY '<' plaintype '>'                                       { $$ = astree_adopt($1, $3, NULL, NULL); }
            ;
plaintype   : TOK_INT                                                           { $$ = $1; }
            | TOK_STRING                                                        { $$ = $1; }
            | TOK_PTR '<' TOK_STRUCT TOK_IDENT '>'                              { $$ = astree_adopt($1, $4, NULL, NULL); }
            ;
block       : '{' statements '}'                                                { $$ = astree_adopt_sym($1, TOK_BLOCK, $2, NULL); }
            | '{' '}'                                                           { $$ = astree_adopt_sym($1, TOK_BLOCK, NULL, NULL); }
            | ';'                                                               { $$ = NULL; }
            ;
statements  : statement                                                         { $$ = $1; }
            | statements statement                                              { $$ = astree_buddy_up($1, $2); }
statement   : vardecl                                                           { $$ = $1; }
            | block                                                             { $$ = $1; }
            | while                                                             { $$ = $1; }
            | ifelse                                                            { $$ = $1; }
            | return                                                            { $$ = $1; }
            | expr ';'                                                          { $$ = $1; }
            ;
vardecl     : type TOK_IDENT '=' expr ';'                                       { $$ = parser_make_type_id($1, $2, $4); }
            | type TOK_IDENT ';'                                                { $$ = parser_make_type_id($1, $2, NULL); }
            ;
while       : TOK_WHILE '(' expr ')' statement                                  { $$ = astree_adopt($1, $3,$5, NULL); }
            ;
ifelse      : TOK_IF '(' expr ')' statement TOK_ELSE statement                  { $$ = astree_adopt($1, $3, $5, $7); }
            | TOK_IF '(' expr ')' statement %prec TOK_ELSE                      { $$ = astree_adopt($1, $3, $5, NULL); }
            ;
return      : TOK_RETURN expr ';'                                               { $$ = astree_adopt($1, $2, NULL, NULL); }
            | TOK_RETURN ';'                                                    { $$ = $1; }
            ;
expr        : expr '+' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '-' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '/' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '*' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '%' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '=' expr                                                     { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr TOK_EQ expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr TOK_NE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '<' expr %prec TOK_LT                                        { $$ = astree_adopt_sym($2, TOK_LT, $1, $3); }
            | expr TOK_LE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
            | expr '>' expr %prec TOK_GT                                        { $$ = astree_adopt_sym($2, TOK_GT, $1, $3); }
            | expr TOK_GE expr                                                  { $$ = astree_adopt($2, $1, $3, NULL); }
            | '+' expr %prec TOK_POS                                            { $$ = astree_adopt_sym($1, TOK_POS, $2, NULL); }
            | '-' expr %prec TOK_NEG                                            { $$ = astree_adopt_sym($1, TOK_NEG, $2, NULL); }
            | TOK_NOT expr                                                      { $$ = astree_adopt($1, $2, NULL, NULL); }
            | allocator                                                         { $$ = $1; }
            | call                                                              { $$ = $1; }
            | '(' expr ')'                                                      { $$ = $2; }
            | variable                                                          { $$ = $1; }
            | constant                                                          { $$ = $1; }
            ;
exprs       : expr                                                              { $$ = $1; }
            | exprs ',' expr                                                    { $$ = astree_buddy_up($1, $3); }
            ;
allocator   : TOK_ALLOC '<' TOK_STRING '>' '(' expr ')'                         { $$ = astree_adopt($1, $3, $6, NULL); }
            | TOK_ALLOC '<' TOK_STRUCT TOK_IDENT '>' '(' ')'                    { $$ = astree_adopt($1, $4, NULL, NULL); }
            | TOK_ALLOC '<' TOK_ARRAY '<' plaintype '>' '>' '(' expr ')'        { $$ = astree_adopt($1, astree_adopt($3, $5, NULL, NULL), $9, NULL); }
            ;
call        : TOK_IDENT '(' exprs ')'                                           { $$ = astree_adopt_sym($2, TOK_CALL, $1, $3); }
            | TOK_IDENT '(' ')'                                                 { $$ = astree_adopt_sym($2, TOK_CALL, $1, NULL); }
            ;
variable    : TOK_IDENT                                                         { $$ = $1; }
            | expr '[' expr ']' %prec TOK_INDEX                                 { $$ = astree_adopt_sym($2, TOK_INDEX, $1, $3); }
            | expr TOK_ARROW TOK_IDENT                                          { $$ = astree_adopt($2, $1, $3, NULL); }
            ;
constant    : TOK_INTCON                                                        { $$ = $1; }
            | TOK_CHARCON                                                       { $$ = $1; }
            | TOK_STRINGCON                                                     { $$ = $1; }
            | TOK_NULLPTR                                                       { $$ = $1; }
            ;
%%

// functions, structures, typeids, allocs can have their type
// assigned when they adopt their children

const char * parser_get_tname (int symbol) {
    return yytname [YYTRANSLATE (symbol)];
}

struct astree* parser_make_root () {
    return astree_init (TOK_ROOT, (struct location) {lexer_get_fileno(), 0, 0}, "");
}

struct astree* parser_make_function(struct astree *type, struct astree *id,
        struct astree *paren, struct astree *params, struct astree *block) {
    struct astree* function = astree_init (TOK_FUNCTION, type->loc, "");
    struct astree* type_id = parser_make_type_id(type, id, NULL);
    return astree_adopt (function, type_id, astree_adopt_sym (paren, TOK_PARAM,
            params, NULL), block);
}

struct astree* parser_make_type_id(struct astree *type, struct astree *id,
        struct astree *expr) {
    struct astree* type_id = astree_init (TOK_TYPE_ID, type->loc, "");
    return astree_adopt (type_id, type, id, expr);
}

struct astree* parser_make_struct(struct astree *parent, struct astree *structure_id,
       struct astree *structure_body) {
    parent->type_id = structure_id->lexinfo;
    return astree_adopt (parent, structure_id, structure_body, NULL);
}

int is_defined_token (int symbol) {
    return YYTRANSLATE (symbol) > YYUNDEFTOK;
}
