/* clang-format off */
/* *INDENT-OFF* */
%{
#include <assert.h>

#include "lyutils.h"
#include "astree.h"
#include "debug.h"
#include "attributes.h"
#include "symtable.h"
#include "tchk_expr.h"
#include "tchk_stmt.h"
#include "tchk_decl.h"
#include "bcc_err.h"
#include "state.h"
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

/* dummy tokens used for precedence */
/* %token PREC_PREFIX PREC_POSTFIX */
/* %token PREC_TERNARY PREC_COMMA */
/* tokens constructed by checker */
%token TOK_TYPE_ERROR TOK_AUTO_CONV TOK_AUTO_PROM
/* tokens constructed by parser */
%token TOK_DECLARATION TOK_ROOT TOK_TYPE_NAME TOK_SPEC_LIST
/* tokens assigned to exsting nodes */
%token TOK_POS TOK_NEG TOK_POST_INC TOK_POST_DEC TOK_INDIRECTION TOK_ADDROF TOK_CALL TOK_SUBSCRIPT
%token TOK_BLOCK TOK_ARRAY TOK_CAST TOK_POINTER TOK_LABEL TOK_INIT_LIST TOK_PARAM_LIST
/* tokens constructed by lexer */
%token TOK_VOID TOK_INT TOK_SHORT TOK_LONG TOK_CHAR TOK_UNSIGNED TOK_SIGNED
%token TOK_CONST TOK_VOLATILE TOK_TYPEDEF TOK_STATIC TOK_EXTERN TOK_AUTO TOK_REGISTER
%token TOK_IF TOK_ELSE TOK_SWITCH TOK_DO TOK_WHILE TOK_FOR TOK_STRUCT TOK_UNION TOK_ENUM
%token TOK_RETURN TOK_CONTINUE TOK_BREAK TOK_GOTO TOK_CASE TOK_DEFAULT
%token TOK_ARROW TOK_SIZEOF TOK_EQ TOK_NE TOK_LE TOK_GE TOK_SHL TOK_SHR TOK_AND TOK_OR TOK_INC TOK_DEC
%token TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON TOK_TYPEDEF_NAME

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

/* NOTE: Pointers are parsed with left recursion due to limitations I have
 * imposed on the syntax tree. This should be fixed somehow in the future, but
 * for the moment it is the only way I can see to parse types in the order they
 * are meant to be read.
 */
%%
program             : %empty                                            { $$ = bcc_yyval = parser_root = parser_make_root(); }
                    | program topdecl                                   { $$ = bcc_yyval = parser_root = validate_topdecl($1, finalize_declaration($2)); }
                    | program error '}'                                 { $$ = bcc_yyval = $1; astree_destroy($3); }
                    | program error ';'                                 { $$ = bcc_yyval = $1; astree_destroy($3); }
                    ;
topdecl             : declarations ';'                                  { $$ = bcc_yyval = $1; astree_destroy($2); }
                    | function_def '}'                                  { $$ = bcc_yyval = finalize_function($1); parser_cleanup(1, $2); }
                    | ';'                                               { $$ = bcc_yyval = &EMPTY_EXPR; astree_destroy($1); }
                    ;
function_def        : typespec_list declarator '{'                      { $$ = bcc_yyval = define_function(parser_make_declaration($1), $2, parser_new_sym($3, TOK_BLOCK)); }
                    | typespec_list abs_declarator '{'                  { $$ = bcc_yyval = define_function(parser_make_declaration($1), $2, parser_new_sym($3, TOK_BLOCK)); }
                    | function_def stmt                                 { $$ = bcc_yyval = validate_fnbody_content($1, $2); }
                    | function_def declarations ';'                     { $$ = bcc_yyval = validate_fnbody_content($1, $2); parser_cleanup(1, $3); }
                    ;
declarations        : typespec_list declarator                          { $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* make_declaration, declare_symbol */
                    | typespec_list abs_declarator                      { $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* make_declaration, declare_symbol */
                    | typespec_list declarator '=' initializer          { $$ = bcc_yyval = define_symbol(parser_make_declaration($1), $2, $3, $4); } /* make_declaration, define_symbol */
                    | typespec_list abs_declarator '=' initializer      { $$ = bcc_yyval = define_symbol(parser_make_declaration($1), $2, $3, $4); } /* make_declaration, define_symbol */
                    | declarations ',' declarator                       { $$ = bcc_yyval = declare_symbol($1, $3); parser_cleanup(1, $2); } /* declare_symbol */
                    | declarations ',' abs_declarator                   { $$ = bcc_yyval = declare_symbol($1, $3); parser_cleanup(1, $2); } /* declare_symbol */
                    | declarations ',' declarator '=' initializer       { $$ = bcc_yyval = define_symbol($1, $3, $4, $5); parser_cleanup(1, $2); } /* define_symbol */
                    | declarations ',' abs_declarator '=' initializer   { $$ = bcc_yyval = define_symbol($1, $3, $4, $5); parser_cleanup(1, $2); } /* define_symbol */
                    ;
typespec_list       : typespec_list typespec                            { $$ = bcc_yyval = validate_typespec($1, $2); } /* validate_typespec */
                    | typespec                                          { $$ = bcc_yyval = parser_make_spec_list($1); } /* make_spec_list, validate_typespec */
                    ;
typespec            : TOK_LONG                                          { $$ = bcc_yyval = $1; }
                    | TOK_SIGNED                                        { $$ = bcc_yyval = $1; }
                    | TOK_UNSIGNED                                      { $$ = bcc_yyval = $1; }
                    | TOK_SHORT                                         { $$ = bcc_yyval = $1; }
                    | TOK_INT                                           { $$ = bcc_yyval = $1; }
                    | TOK_CHAR                                          { $$ = bcc_yyval = $1; }
                    | TOK_VOID                                          { $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF                                       { $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF_NAME                                  { $$ = bcc_yyval = $1; }
                    | TOK_REGISTER                                      { $$ = bcc_yyval = $1; }
                    | TOK_AUTO                                          { $$ = bcc_yyval = $1; }
                    | TOK_STATIC                                        { $$ = bcc_yyval = $1; }
                    | TOK_EXTERN                                        { $$ = bcc_yyval = $1; }
                    | struct_spec                                       { $$ = bcc_yyval = $1; }
                    | enum_spec                                         { $$ = bcc_yyval = $1; }
                    ;
struct_spec         : struct_def '}'                                    { $$ = bcc_yyval = finalize_tag_def($1); astree_destroy($2); } /* cleanup intermediate products */
                    | struct_or_union any_ident                         { $$ = bcc_yyval = validate_tag_decl($1, $2); } /* declare tag if necessary */
                    ;
struct_def          : struct_or_union any_ident '{'                     { $$ = bcc_yyval = validate_tag_def($1, $2, $3); }
                    | struct_or_union '{'                               { $$ = bcc_yyval = validate_unique_tag($1, $2); }
                    | struct_def struct_decl ';'                        { $$ = bcc_yyval = define_struct_member($1, $2); parser_cleanup(1, $3); } /* define struct member */
                    ;
struct_or_union     : TOK_STRUCT                                        { $$ = bcc_yyval = $1; }
                    | TOK_UNION                                         { $$ = bcc_yyval = $1; }
                    ;
struct_decl         : struct_decl ',' declarator                        { $$ = bcc_yyval = declare_symbol($1, $3); astree_destroy($2); } /* define struct member */
                    | struct_decl ',' abs_declarator                    { $$ = bcc_yyval = declare_symbol($1, $3); astree_destroy($2); } /* define struct member */
                    | typespec_list declarator                          { $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* define struct member */
                    | typespec_list abs_declarator                      { $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* define struct member */
                    ;
enum_spec           : enum_def '}'                                      { $$ = bcc_yyval = finalize_tag_def($1); astree_destroy($2); } /* create tag and iterate over enums */
                    | TOK_ENUM any_ident                                { $$ = bcc_yyval = validate_tag_decl($1, $2); } /* do nothing */
                    ;
enum_def            : TOK_ENUM any_ident '{' any_ident                  { $$ = bcc_yyval = define_enumerator(validate_tag_def($1, $2, $3), $4, NULL, NULL); }
                    | TOK_ENUM any_ident '{' any_ident '=' cond_expr    { $$ = bcc_yyval = define_enumerator(validate_tag_def($1, $2, $3), $4, $5, $6); }
                    | TOK_ENUM '{' any_ident                            { $$ = bcc_yyval = define_enumerator(validate_unique_tag($1, $2), $3, NULL, NULL); }
                    | TOK_ENUM '{' any_ident '=' cond_expr              { $$ = bcc_yyval = define_enumerator(validate_unique_tag($1, $2), $3, $4, $5); }
                    | enum_def ',' any_ident                            { $$ = bcc_yyval = define_enumerator($1, $3, NULL, NULL); parser_cleanup(1, $2); }
                    | enum_def ',' any_ident '=' cond_expr              { $$ = bcc_yyval = define_enumerator($1, $3, $4, $5); parser_cleanup(1, $2); }
                    ;
initializer         : assign_expr                                       { $$ = bcc_yyval = $1; } /* do nothing */
                    | init_list '}'                                     { $$ = bcc_yyval = $1; astree_destroy($2); } /* do nothing */
                    | init_list ',' '}'                                 { $$ = bcc_yyval = $1; astree_destroy($2); astree_destroy($3); } /* do nothing */
                    ;
init_list           : init_list ',' initializer                         { $$ = bcc_yyval = astree_adopt($1, 1, $3); astree_destroy($2); } /* adopt */
                    | '{' initializer                                   { $$ = bcc_yyval = astree_adopt(parser_new_sym($1, TOK_INIT_LIST), 1, $2); } /* adopt */
                    ;
declarator          : pointer declarator                                { $$ = bcc_yyval = define_pointer($2, $1); } /* define_pointer */
                    | TOK_IDENT                                         { $$ = bcc_yyval = validate_declarator($1); } /* validate_declarator */
                    | '(' declarator ')'                                { $$ = bcc_yyval = $2; parser_cleanup(2, $1, $3); } /* do nothing */
                    | declarator direct_decl                            { $$ = bcc_yyval = define_dirdecl($1, $2); } /* validate_dirdecl */
                    ;
abs_declarator      : pointer abs_declarator                            { $$ = bcc_yyval = define_pointer($2, $1); } /* define_pointer */
                    | '(' abs_declarator ')'                            { $$ = bcc_yyval = $2; parser_cleanup(2, $1, $3); } /* do nothing */
                    | abs_declarator direct_decl                        { $$ = bcc_yyval = define_dirdecl($1, $2); } /* validate_dirdecl */
                    | %empty                                            { $$ = bcc_yyval = parser_make_type_name(); }
                    ;
pointer             : '*'                                               { $$ = bcc_yyval = parser_new_sym($1, TOK_POINTER); }
                    | pointer TOK_CONST                                 { $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    | pointer TOK_VOLATILE                              { $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    ;
direct_decl         : '[' ']'                                           { $$ = bcc_yyval = parser_new_sym($1, TOK_ARRAY); astree_destroy($2); } /* do nothing */
                    | '[' cond_expr   ']'                               { $$ = bcc_yyval = validate_array_size(parser_new_sym($1, TOK_ARRAY), $2); astree_destroy($3); } /* validate_array_size */
                    | param_list ')'                                    { $$ = bcc_yyval = finalize_param_list($1); astree_destroy($2); } /* exit parameter table */
                    | '(' ')'                                           { $$ = bcc_yyval = parser_new_sym($1, TOK_PARAM_LIST); astree_destroy($2); } /* create param table */
                    | '(' TOK_VOID ')'                                  { $$ = bcc_yyval = parser_new_sym($1, TOK_PARAM_LIST); parser_cleanup(2, $2, $3); } /* create param table */
                    ;
param_list          : '(' typespec_list declarator                      { $$ = bcc_yyval = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | '(' typespec_list abs_declarator                  { $$ = bcc_yyval = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | param_list ',' typespec_list declarator           { $$ = bcc_yyval = validate_param($1, parser_make_declaration($3), $4); astree_destroy($2); } /* define_param */
                    | param_list ',' typespec_list abs_declarator       { $$ = bcc_yyval = validate_param($1, parser_make_declaration($3), $4); astree_destroy($2); } /* define_param */
                    ;
block               : block_content '}'                                 { $$ = bcc_yyval = finalize_block($1); astree_destroy($2); }
                    ;
block_content       : '{'                                               { $$ = bcc_yyval = validate_block(parser_new_sym($1, TOK_BLOCK)); }
                    | block_content stmt                                { $$ = bcc_yyval = validate_block_content($1, $2); }
                    | block_content declarations ';'                    { $$ = bcc_yyval = validate_block_content($1, finalize_declaration($2)); parser_cleanup(1, $3); }
                    ;
stmt                : block                                             { $$ = bcc_yyval = $1; }
                    | dowhile                                           { $$ = bcc_yyval = $1; }
                    | while                                             { $$ = bcc_yyval = $1; }
                    | for                                               { $$ = bcc_yyval = $1; }
                    | ifelse                                            { $$ = bcc_yyval = $1; }
                    | switch                                            { $$ = bcc_yyval = $1; }
                    | return                                            { $$ = bcc_yyval = $1; }
                    | labelled_stmt                                     { $$ = bcc_yyval = $1; }
                    | TOK_CONTINUE ';'                                  { $$ = bcc_yyval = validate_continue($1); astree_destroy($2); }
                    | TOK_BREAK ';'                                     { $$ = bcc_yyval = validate_break($1); astree_destroy($2); }
                    | TOK_GOTO any_ident ';'                            { $$ = bcc_yyval = validate_goto($1, $2); astree_destroy($3); }
                    | stmt_expr                                         { $$ = bcc_yyval = $1; }
                    ;
stmt_expr           : expr ';'                                          { $$ = bcc_yyval = $1; astree_destroy($2); }
                    | ';'                                               { $$ = bcc_yyval = $1; }
                    ;
labelled_stmt       : any_ident ':' stmt                                { $$ = bcc_yyval = validate_label(parser_make_label($1), $1, $3); parser_cleanup(1, $2); }
                    | TOK_DEFAULT ':' stmt                              { $$ = bcc_yyval = validate_default($1, $3); parser_cleanup(1, $2); }
                    | TOK_CASE cond_expr   ':' stmt                     { $$ = bcc_yyval = validate_case($1, $2, $4); parser_cleanup(1, $3); }
                    ;
for                 : TOK_FOR '(' stmt_expr stmt_expr ')' stmt          { $$ = bcc_yyval = validate_for($1, $3, $4, parser_new_sym($5, ';'), $6); parser_cleanup(1, $2); }
                    | TOK_FOR '(' stmt_expr stmt_expr expr ')' stmt     { $$ = bcc_yyval = validate_for($1, $3, $4, $5, $7); parser_cleanup(2, $2, $6); }
                    ;
dowhile             : TOK_DO stmt TOK_WHILE '(' expr ')' ';'            { $$ = bcc_yyval = validate_do($1, $2, $5); parser_cleanup (4, $3, $4, $6, $7); }
                    ;
while               : TOK_WHILE '(' expr ')' stmt                       { $$ = bcc_yyval = validate_while($1, $3, $5); parser_cleanup (2, $2, $4); }
                    ;
ifelse              : TOK_IF '(' expr ')' stmt TOK_ELSE stmt            { $$ = bcc_yyval = validate_ifelse($1, $3, $5, $7); parser_cleanup (3, $2, $4, $6); }
                    | TOK_IF '(' expr ')' stmt %prec TOK_ELSE           { $$ = bcc_yyval = validate_ifelse($1, $3, $5, &EMPTY_EXPR); parser_cleanup (2, $2, $4); }
                    ;
switch              : TOK_SWITCH '(' expr ')' stmt                      { $$ = bcc_yyval = validate_switch($1, $3, $5); parser_cleanup(2, $2, $4); }
                    ;
switch_expr         : expr                                              { $$ = bcc_yyval = validate_switch_expr($1); }
                    ;
return              : TOK_RETURN expr ';'                               { $$ = bcc_yyval = validate_return($1, $2); parser_cleanup (1, $3); }
                    | TOK_RETURN ';'                                    { $$ = bcc_yyval = validate_return($1, &EMPTY_EXPR); parser_cleanup (1, $2); }
                    ;
expr                : assign_expr                                       { $$ = bcc_yyval = $1; }
                    | expr ',' assign_expr                              { $$ = bcc_yyval = validate_comma($2, $1, $3); }
                    ;
assign_expr         : assign_expr '=' cond_expr                         { $$ = bcc_yyval = validate_assignment($2, $1, $3); }
                    | cond_expr                                         { $$ = bcc_yyval = $1; }
                    ;
cond_expr           : binary_expr '?' expr ':' cond_expr                { $$ = bcc_yyval = validate_conditional($2, $1, $3, $5); astree_destroy($4); }
                    | binary_expr                                       { $$ = bcc_yyval = $1; }
                    ;
binary_expr         : binary_expr '+' binary_expr                       { $$ = bcc_yyval = validate_addition($2, $1, $3); }
                    | binary_expr '-' binary_expr                       { $$ = bcc_yyval = validate_addition($2, $1, $3); }
                    | binary_expr '/' binary_expr                       { $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '*' binary_expr                       { $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '%' binary_expr                       { $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '>' binary_expr                       { $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr '<' binary_expr                       { $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_LE binary_expr                    { $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_GE binary_expr                    { $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_EQ binary_expr                    { $$ = bcc_yyval = validate_equality($2, $1, $3); }
                    | binary_expr TOK_NE binary_expr                    { $$ = bcc_yyval = validate_equality($2, $1, $3); }
                    | binary_expr TOK_OR binary_expr                    { $$ = bcc_yyval = validate_logical($2, $1, $3); }
                    | binary_expr TOK_AND binary_expr                   { $$ = bcc_yyval = validate_logical($2, $1, $3); }
                    | binary_expr '|' binary_expr                       { $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr '&' binary_expr                       { $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr '^' binary_expr                       { $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr TOK_SHR binary_expr                   { $$ = bcc_yyval = validate_shift($2, $1, $3); }
                    | binary_expr TOK_SHL binary_expr                   { $$ = bcc_yyval = validate_shift($2, $1, $3); }
                    | cast_expr                                         { $$ = bcc_yyval = $1; }
                    ;
cast_expr           : '(' typespec_list abs_declarator')' unary_expr    { $$ = bcc_yyval = parser_make_cast($1, $2, $3, $5); parser_cleanup(1, $4); }
                    | unary_expr                                        { $$ = bcc_yyval = $1; }
                    ;
unary_expr          : postfix_expr                                      { $$ = bcc_yyval = $1; }
                    | TOK_INC unary_expr                                { $$ = bcc_yyval = validate_increment($1, $2); }
                    | TOK_DEC unary_expr                                { $$ = bcc_yyval = validate_increment($1, $2); }
                    | '!' unary_expr                                    { $$ = bcc_yyval = validate_not($1, $2); }
                    | '~' unary_expr                                    { $$ = bcc_yyval = validate_complement($1, $2); }
                    | '+' unary_expr                                    { $$ = bcc_yyval = validate_negation(parser_new_sym($1, TOK_POS), $2); }
                    | '-' unary_expr                                    { $$ = bcc_yyval = validate_negation(parser_new_sym($1, TOK_NEG), $2); }
                    | '*' unary_expr                                    { $$ = bcc_yyval = validate_indirection(parser_new_sym($1, TOK_INDIRECTION), $2); }
                    | '&' unary_expr                                    { $$ = bcc_yyval = validate_addrof(parser_new_sym($1, TOK_ADDROF), $2); }
                    | TOK_SIZEOF unary_expr                             { $$ = bcc_yyval = validate_sizeof($1, $2); }
                    | TOK_SIZEOF '(' typespec_list abs_declarator')'    { $$ = bcc_yyval = parse_sizeof($1, $3, $4); parser_cleanup(2, $2, $5); }
                    ;
postfix_expr        : primary_expr                                      { $$ = bcc_yyval = $1; }
                    | postfix_expr TOK_INC                              { $$ = bcc_yyval = validate_increment(parser_new_sym($2, TOK_POST_INC), $1); }
                    | postfix_expr TOK_DEC                              { $$ = bcc_yyval = validate_increment(parser_new_sym($2, TOK_POST_DEC), $1); }
                    | call                                              { $$ = bcc_yyval = $1; }
                    | postfix_expr '[' expr ']'                         { $$ = bcc_yyval = validate_subscript(parser_new_sym($2, TOK_SUBSCRIPT), $1, $3); parser_cleanup(1, $4); }
                    | postfix_expr '.' any_ident                        { $$ = bcc_yyval = validate_reference($2, $1, $3); }
                    | postfix_expr TOK_ARROW any_ident                  { $$ = bcc_yyval = validate_arrow($2, $1, $3); }
                    ;
call                : postfix_expr '(' ')'                              { $$ = bcc_yyval = finalize_call(validate_call($1, parser_new_sym($2, TOK_CALL))); parser_cleanup(1, $3);}
                    | call_args ')'                                     { $$ = bcc_yyval = finalize_call($1); parser_cleanup(1, $2); }
                    ;
call_args           : postfix_expr '(' assign_expr                      { $$ = bcc_yyval = validate_arg(validate_call($1, parser_new_sym($2, TOK_CALL)), $3); }
                    | call_args ',' assign_expr                         { $$ = bcc_yyval = validate_arg($1, $3); parser_cleanup(1, $2); }
                    ;
primary_expr        : TOK_IDENT                                         { $$ = bcc_yyval = validate_ident($1); }
                    | constant                                          { $$ = bcc_yyval = $1; }
                    | '(' expr ')'                                      { $$ = bcc_yyval = $2; parser_cleanup(2, $1, $3); }
                    ;
constant            : TOK_INTCON                                        { $$ = bcc_yyval = validate_intcon($1); }
                    | TOK_CHARCON                                       { $$ = bcc_yyval = validate_charcon($1); }
                    | TOK_STRINGCON                                     { $$ = bcc_yyval = validate_stringcon($1); }
                    ;
any_ident           : TOK_IDENT                                         { $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF_NAME                                  { $$ = bcc_yyval = parser_new_sym($1, TOK_IDENT); }
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
  DEBUGS('t', "Making symbol table");
  root->symbol_table = symbol_table_init(TRANS_UNIT_TABLE);
  state_push_table(state, root->symbol_table);
  return root;
}

ASTree *parser_new_sym(ASTree *tree, int new_symbol) {
  tree->symbol = new_symbol;
  return tree;
}

ASTree *parser_make_spec_list(ASTree *first_specifier) {
  ASTree *spec_list =
      astree_init(TOK_SPEC_LIST, first_specifier->loc, "_spec_list");
  TypeSpec *type = calloc(1, sizeof(*type));
  typespec_init(type);
  spec_list->type = type;
  return validate_typespec(spec_list, first_specifier);
}

ASTree *parser_make_declaration(ASTree *spec_list) {
  ASTree *declaration = astree_init(TOK_DECLARATION, LOC_EMPTY, "_declaration");
  spec_list = validate_typespec_list(spec_list);
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    return astree_propogate_errnode(declaration, spec_list);
  }
  return astree_adopt(declaration, 1, spec_list);
}

ASTree *parser_make_param_list(ASTree *left_paren, ASTree *spec_list,
                               ASTree *declarator) {
  /* all error handling done in validate_param */
  ASTree *param_list =
      validate_param_list(parser_new_sym(left_paren, TOK_PARAM_LIST));
  /* create temporary type object on the heap to hold the parameter auxspec */
  ASTree *declaration = parser_make_declaration(spec_list);
  return validate_param(param_list, declaration, declarator);
}

/* For the sake of code reusability, the tree structure of a type name is the
 * same as that of a full declaration, with the exception being that in the
 * place of the node with the TOK_IDENT symbol, there is instead a node with the
 * TOK_TYPE_NAME symbol. This way, the code for processing declaration can also
 * be used to validate type names, with a little tweaking.
 */
ASTree *parser_make_type_name(void) {
  ASTree *type_name = astree_init(TOK_TYPE_NAME, lexer_get_loc(), "_type_name");
  SymbolValue *symval =
      symbol_value_init(&type_name->loc, state_get_sequence(state));
  type_name->type = &symval->type;
  return type_name;
}

ASTree *parser_make_cast(ASTree *left_paren, ASTree *spec_list,
                         ASTree *type_name, ASTree *expr) {
  ASTree *cast = parser_new_sym(left_paren, TOK_CAST);
  ASTree *declaration = parser_make_declaration(spec_list);
  return validate_cast(cast, declare_symbol(declaration, type_name), expr);
}

ASTree *parser_make_label(ASTree *ident) {
  return astree_init(TOK_LABEL, ident->loc, "_label");
}

ASTree *parse_sizeof(ASTree *sizeof_, ASTree *spec_list, ASTree *declarator) {
  ASTree *declaration = parser_make_declaration(spec_list);
  ASTree *declaration_or_err = declare_symbol(declaration, declarator);
  return validate_sizeof(sizeof_, declaration_or_err);
}

void parser_cleanup(size_t count, ...) {
  DEBUGS('p', "Cleaning up %lu astree nodes", count);
  va_list args;
  va_start(args, count);
  size_t i;
  for (i = 0; i < count; ++i) {
    ASTree *tree = va_arg(args, ASTree *);
    astree_destroy(tree);
  }
  va_end(args);
}

int is_defined_token(int symbol) { return YYTRANSLATE(symbol) > YYUNDEF; }
