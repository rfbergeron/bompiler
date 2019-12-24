%{

#include <cassert>

#include "lyutils.h"
#include "astree.h"
#include "symtable.h"

%}

%debug
%defines
%define parse.error verbose
%token-table
%verbose

%destructor { destroy ($$); } <>
%printer { assert (yyoutput == stderr); astree::dump (cerr, $$); } <>

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
%precedence TOK_POS TOK_NEG TOK_NOT
%precedence TOK_ARROW '['
%precedence TOK_ELSE


%%

start       : program                                                           { $$ = $1 = nullptr; }
            ;
program     : program structdef                                                 { $$ = $1->adopt($2); }
            | program function                                                  { $$ = $1->adopt($2); }
            | program vardecl                                                   { $$ = $1->adopt($2); }
            | program error '}'                                                 { $$ = $1; }
            | program error ';'                                                 { $$ = $1; }
            | %empty                                                            { $$ = parser::newroot(); }
            ;
structdef   : TOK_STRUCT TOK_IDENT '{' structbody '}' ';'                       { $$ = $1->adopt($2, $4); }
            | TOK_STRUCT TOK_IDENT '{' '}' ';'                                  { $$ = $1->adopt($2); }
            ;
structbody  : type TOK_IDENT ';'                                                { $$ = parser::make_type_id($1, $2); }
            | structbody type TOK_IDENT ';'                                     { $$ = $1->buddy_up(parser::make_type_id($2, $3)); }
            ;
function    : type TOK_IDENT '(' parameters ')' block                           { $$ = parser::make_function($1, $2, $3, $4, $6); }
            | type TOK_IDENT '(' ')' block                                      { $$ = parser::make_function($1, $2, $3, nullptr, $5); }
            ;
parameters  : type TOK_IDENT                                                    { $$ = parser::make_type_id($1, $2); }
            | parameters ',' type TOK_IDENT                                     { $$ = $1->buddy_up(parser::make_type_id($3, $4)); }
            ;
type        : plaintype                                                         { $$ = $1; }
            | TOK_VOID                                                          { $$ = $1; }
            | TOK_ARRAY '<' plaintype '>'                                       { $$ = $1->adopt($3); }
            ;
plaintype   : TOK_INT                                                           { $$ = $1; }
            | TOK_STRING                                                        { $$ = $1; }
            | TOK_PTR '<' TOK_STRUCT TOK_IDENT '>'                              { $$ = $1->adopt($4); }
            ;
block       : '{' statements '}'                                                { $$ = $1->adopt_sym(TOK_BLOCK, $2); }
            | '{' '}'                                                           { $$ = $1->adopt_sym(TOK_BLOCK, nullptr); }
            | ';'                                                               { $$ = nullptr; }
            ;
statements  : statement                                                         { $$ = $1; }
            | statements statement                                              { $$ = $1->buddy_up($2); }
statement   : vardecl                                                           { $$ = $1; }
            | block                                                             { $$ = $1; }
            | while                                                             { $$ = $1; }
            | ifelse                                                            { $$ = $1; }
            | return                                                            { $$ = $1; }
            | expr ';'                                                          { $$ = $1; }
            ;
vardecl     : type TOK_IDENT '=' expr ';'                                       { $$ = parser::make_type_id($1,$2, $4); }
            | type TOK_IDENT ';'                                                { $$ = parser::make_type_id($1,$2); }
            ;
while       : TOK_WHILE '(' expr ')' statement                                  { $$ = $1->adopt($3,$5); }
            ;
ifelse      : TOK_IF '(' expr ')' statement TOK_ELSE statement                  { $$ = $1->adopt($3, $5, $7); }
            | TOK_IF '(' expr ')' statement %prec TOK_ELSE                      { $$ = $1->adopt($3, $5); }
            ;
return      : TOK_RETURN expr ';'                                               { $$ = $1->adopt($2); }
            | TOK_RETURN ';'                                                    { $$ = $1; }
            ;
expr        : expr '+' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr '-' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr '/' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr '*' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr '%' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr '=' expr                                                     { $$ = $2->adopt($1, $3); }
            | expr TOK_EQ expr                                                  { $$ = $2->adopt($1, $3); }
            | expr TOK_NE expr                                                  { $$ = $2->adopt($1, $3); }
            | expr '<' expr %prec TOK_LT                                        { $$ = $2->adopt_sym(TOK_LT, $1, $3); }
            | expr TOK_LE expr                                                  { $$ = $2->adopt($1, $3); }
            | expr '>' expr %prec TOK_GT                                        { $$ = $2->adopt_sym(TOK_GT, $1, $3); }
            | expr TOK_GE expr                                                  { $$ = $2->adopt($1, $3); }
            | '+' expr %prec TOK_POS                                            { $$ = $1->adopt_sym(TOK_POS, $2); }
            | '-' expr %prec TOK_NEG                                            { $$ = $1->adopt_sym(TOK_NEG, $2); }
            | TOK_NOT expr                                                      { $$ = $1->adopt($2); }
            | allocator                                                         { $$ = $1; }
            | call                                                              { $$ = $1; }
            | '(' expr ')'                                                      { $$ = $2; }
            | variable                                                          { $$ = $1; }
            | constant                                                          { $$ = $1; }
            ;
exprs       : expr                                                              { $$ = $1; }
            | exprs ',' expr                                                    { $$ = $1->buddy_up($3); }
            ;
allocator   : TOK_ALLOC '<' TOK_STRING '>' '(' expr ')'                         { $$ = $1->adopt($3, $6); }
            | TOK_ALLOC '<' TOK_STRUCT TOK_IDENT '>' '(' ')'                    { $$ = $1->adopt($4); }
            | TOK_ALLOC '<' TOK_ARRAY '<' plaintype '>' '>' '(' expr ')'        { $$ = $1->adopt($3->adopt($5), $9); }
            ;
call        : TOK_IDENT '(' exprs ')'                                           { $$ = $2->adopt_sym(TOK_CALL, $1, $3); }
            | TOK_IDENT '(' ')'                                                 { $$ = $2->adopt_sym(TOK_CALL, $1); }
            ;
variable    : TOK_IDENT                                                         { $$ = $1; }
            | expr '[' expr ']' %prec TOK_INDEX                                 { $$ = $2->adopt_sym(TOK_INDEX, $1, $3); }
            | expr TOK_ARROW TOK_IDENT                                          { $$ = $2->adopt($1, $3); }
            ;
constant    : TOK_INTCON                                                        { $$ = $1; }
            | TOK_CHARCON                                                       { $$ = $1; }
            | TOK_STRINGCON                                                     { $$ = $1; }
            | TOK_NULLPTR                                                       { $$ = $1; }
            ;
%%

// functions, structures, typeids, allocs can have their type
// assigned when they adopt their children

astree* parser::newroot() {
   parser::root = new astree(TOK_ROOT, {lexer::get_filenr(), 0, 0}, "");
   return parser::root;
}
const char * parser::get_tname (int symbol) {
   return yytname [YYTRANSLATE (symbol)];
}

astree* parser::make_function(astree* type, astree* id, astree* paren,
      astree* params, astree* block) {
   astree* function = new astree(TOK_FUNCTION, type->loc,
         astree::NOINFO);
   astree* type_id = parser::make_type_id(type, id);
   return function->adopt(type_id, paren->adopt_sym(TOK_PARAM, params),
         block);
}

astree* parser::make_type_id(astree* type, astree* id, astree* expr) {
   astree* type_id = new astree(TOK_TYPE_ID, type->loc, astree::NOINFO);
   return type_id->adopt(type, id, expr);
}

astree* parser::make_struct(astree* parent, astree* structure_id,
       astree* structure_body) {
    //structure_id->attributes.set((size_t)attr::TYPEID);
    parent->type_id=structure_id->lexinfo;
    return parent->adopt(structure_id, structure_body);
}

bool is_defined_token (int symbol) {
   return YYTRANSLATE (symbol) > YYUNDEFTOK;
}
