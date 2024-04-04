/* clang-format off */
%{
/* clang-format on */
#include <assert.h>

#include "astree.h"
#include "debug.h"
#include "lyutils.h"
#include "state.h"
#include "symtable.h"
#include "tchk_decl.h"
#include "tchk_expr.h"
#include "tchk_stmt.h"
/* TODO(Robert): redo `init.c` (again) to allow it to insert semantic nodes;
 * either that or live with the fact that initializers work a little funny
 */
#include "conversions.h"

#define ERRCHK \
  if (semantic_error || lexical_error) YYABORT

  static ASTree *parser_make_root(void);
  static ASTree *parser_make_empty(const Location loc);
  static ASTree *parser_new_sym(ASTree * tree, int new_symbol);
  static ASTree *parser_make_joiner(ASTree * stringcon);
  static ASTree *parser_join_strings(ASTree * joiner);
  static ASTree *parser_make_decl_specs(ASTree * first_specifier);
  static ASTree *parser_make_declaration(ASTree * decl_specs);
  static ASTree *parser_make_param_list(
      ASTree * left_paren, ASTree * decl_specs, ASTree * declarator);
  static ASTree *parser_make_type_name(void);
  static ASTree *parser_make_cast(ASTree * left_paren, ASTree * decl_specs,
                                  ASTree * type_name, ASTree * expr);
  static ASTree *parser_make_label(ASTree * ident);
  static ASTree *parser_make_attributes_list(void);
  static ASTree *parse_pretty_function(ASTree * pretty_function);
  static ASTree *parse_sizeof(ASTree * sizeof_, ASTree * decl_specs,
                              ASTree * declarator);
  static ASTree *parse_va_arg(ASTree * va_arg_, ASTree * expr,
                              ASTree * decl_specs, ASTree * declarator);
  static void parser_cleanup(size_t count, ...);
  /* clang-format off */
%}

%debug
%defines
%define parse.error verbose
%token-table
%verbose

%printer {
  static char nodestr[1024];
  astree_to_string($$, nodestr);
  fprintf(yyoutput, "%p->%s", $$, nodestr);
} <>

/* dummy tokens used for precedence */
/* %token PREC_PREFIX PREC_POSTFIX */
/* %token PREC_TERNARY PREC_COMMA */
/* tokens constructed by checker */
%token TOK_SCAL_CONV TOK_PTR_CONV TOK_DISP_CONV TOK_CEXPR_CONV TOK_RVAL_CONV
/* tokens constructed by parser */
%token TOK_DECLARATION TOK_ROOT TOK_TYPE_NAME TOK_SPEC_LIST TOK_ATTR_LIST TOK_EMPTY
/* tokens assigned to exsting nodes */
%token TOK_POS TOK_NEG TOK_POST_INC TOK_POST_DEC TOK_INDIRECTION TOK_ADDROF TOK_CALL TOK_SUBSCRIPT
%token TOK_BLOCK TOK_ARRAY TOK_CAST TOK_POINTER TOK_LABEL TOK_INIT_LIST TOK_PARAM_LIST
/* tokens constructed by lexer */
%token TOK_VA_ARG TOK_VA_START TOK_VA_END TOK_ELLIPSIS TOK_ATTR TOK_ASM TOK_EXTNSN TOK_PRTY_FN
%token TOK_VOID TOK_INT TOK_SHORT TOK_LONG TOK_CHAR TOK_UNSIGNED TOK_SIGNED TOK_FLOAT TOK_DOUBLE
%token TOK_CONST TOK_VOLATILE TOK_TYPEDEF TOK_STATIC TOK_EXTERN TOK_AUTO TOK_REGISTER TOK_RESTRICT
%token TOK_IF TOK_ELSE TOK_SWITCH TOK_DO TOK_WHILE TOK_FOR TOK_STRUCT TOK_UNION TOK_ENUM
%token TOK_RETURN TOK_CONTINUE TOK_BREAK TOK_GOTO TOK_CASE TOK_DEFAULT
%token TOK_ARROW TOK_SIZEOF TOK_EQ TOK_NE TOK_LE TOK_GE TOK_SHL TOK_SHR TOK_AND TOK_OR TOK_INC TOK_DEC
%token TOK_SUBEQ TOK_ADDEQ TOK_MULEQ TOK_DIVEQ TOK_REMEQ TOK_ANDEQ TOK_OREQ TOK_XOREQ TOK_SHREQ TOK_SHLEQ
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON TOK_TYPEDEF_NAME
%token TOK_LEX_ERROR

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
program             : %empty                                            { parser_init_globals(); $$ = bcc_yyval = parser_root; }
                    | program topdecl                                   { ERRCHK; $$ = bcc_yyval = parser_root = validate_topdecl($1, finalize_declaration($2)); }
                    ;
topdecl             : declarations ';'                                  { ERRCHK; $$ = bcc_yyval = $1; astree_destroy($2); }
                    | function_def '}'                                  { ERRCHK; $$ = bcc_yyval = finalize_function($1); parser_cleanup(1, $2); }
                    ;
function_def        : decl_specs declarator '{'                         { ERRCHK; $$ = bcc_yyval = define_function(parser_make_declaration($1), $2, parser_new_sym($3, TOK_BLOCK)); }
                    | function_def stmt                                 { ERRCHK; $$ = bcc_yyval = validate_fnbody_content($1, $2); }
                    | function_def declarations ';'                     { ERRCHK; $$ = bcc_yyval = validate_fnbody_content($1, finalize_declaration($2)); parser_cleanup(1, $3); }
                    ;
declarations        : decl_specs                                        { ERRCHK; $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), parser_make_type_name()); }
                    | declaration_list                                  { ERRCHK; $$ = bcc_yyval = $1; }
                    | definition                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
declaration_list    : decl_specs declarator                             { ERRCHK; $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); }
                    | declaration_list ',' declarator                   { ERRCHK; $$ = bcc_yyval = declare_symbol($1, $3); parser_cleanup(1, $2); }
                    | definition ',' declarator                         { ERRCHK; $$ = bcc_yyval = declare_symbol($1, $3); parser_cleanup(1, $2); }
                    ;
definition          : declaration_list '=' initializer                  { ERRCHK; $$ = bcc_yyval = define_symbol($1, $2, $3); }
                    ;
decl_specs          : decl_specs decl_spec                              { ERRCHK; $$ = bcc_yyval = validate_decl_spec($1, $2); } /* validate_decl_spec */
                    | decl_spec                                         { ERRCHK; $$ = bcc_yyval = parser_make_decl_specs($1); } /* make_spec_list, validate_decl_spec */
                    | attr_spec_list                                    { ERRCHK; $$ = bcc_yyval = parser_make_decl_specs($1); } /* validate decl_spec frees attributes */
                    ;
decl_spec           : TOK_LONG                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_SIGNED                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_UNSIGNED                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_SHORT                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_INT                                           { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_CHAR                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_VOID                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_FLOAT                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_DOUBLE                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF                                       { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF_NAME                                  { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_REGISTER                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_AUTO                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_STATIC                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_EXTERN                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_CONST                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_VOLATILE                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_RESTRICT                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | struct_spec                                       { ERRCHK; $$ = bcc_yyval = $1; }
                    | enum_spec                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
struct_spec         : struct_def '}'                                    { ERRCHK; $$ = bcc_yyval = finalize_tag_def($1); astree_destroy($2); } /* cleanup intermediate products */
                    | struct_or_union any_ident                         { ERRCHK; $$ = bcc_yyval = validate_tag_decl($1, $2); } /* declare tag if necessary */
                    ;
struct_def          : struct_or_union any_ident '{'                     { ERRCHK; $$ = bcc_yyval = validate_tag_def($1, $2, $3); }
                    | struct_or_union '{'                               { ERRCHK; $$ = bcc_yyval = validate_unique_tag($1, $2); }
                    | struct_def struct_decl ';'                        { ERRCHK; $$ = bcc_yyval = define_record_member($1, $2); parser_cleanup(1, $3); } /* define struct member */
                    ;
struct_or_union     : TOK_STRUCT                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_UNION                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
struct_decl         : struct_decl ',' declarator                        { ERRCHK; $$ = bcc_yyval = declare_symbol($1, $3); astree_destroy($2); } /* define struct member */
                    | struct_decl ',' abs_declarator                    { ERRCHK; $$ = bcc_yyval = declare_symbol($1, $3); astree_destroy($2); } /* define struct member */
                    | decl_specs declarator                             { ERRCHK; $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* define struct member */
                    | decl_specs abs_declarator                         { ERRCHK; $$ = bcc_yyval = declare_symbol(parser_make_declaration($1), $2); } /* define struct member */
                    ;
enum_spec           : enum_def '}'                                      { ERRCHK; $$ = bcc_yyval = finalize_tag_def($1); parser_cleanup(1, $2); } /* create tag and iterate over enums */
                    | enum_def ',' '}'                                  { ERRCHK; $$ = bcc_yyval = finalize_tag_def($1); parser_cleanup(2, $2, $3); } /* create tag and iterate over enums */
                    | TOK_ENUM any_ident                                { ERRCHK; $$ = bcc_yyval = validate_tag_decl($1, $2); } /* do nothing */
                    ;
enum_def            : TOK_ENUM any_ident '{' any_ident                  { ERRCHK; $$ = bcc_yyval = define_enumerator(validate_tag_def($1, $2, $3), $4, NULL, NULL); }
                    | TOK_ENUM any_ident '{' any_ident '=' cond_expr    { ERRCHK; $$ = bcc_yyval = define_enumerator(validate_tag_def($1, $2, $3), $4, $5, $6); }
                    | TOK_ENUM '{' any_ident                            { ERRCHK; $$ = bcc_yyval = define_enumerator(validate_unique_tag($1, $2), $3, NULL, NULL); }
                    | TOK_ENUM '{' any_ident '=' cond_expr              { ERRCHK; $$ = bcc_yyval = define_enumerator(validate_unique_tag($1, $2), $3, $4, $5); }
                    | enum_def ',' any_ident                            { ERRCHK; $$ = bcc_yyval = define_enumerator($1, $3, NULL, NULL); parser_cleanup(1, $2); }
                    | enum_def ',' any_ident '=' cond_expr              { ERRCHK; $$ = bcc_yyval = define_enumerator($1, $3, $4, $5); parser_cleanup(1, $2); }
                    ;
asm_spec            : TOK_ASM '(' TOK_STRINGCON TOK_STRINGCON ')'       { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 2, $3, $4); parser_cleanup(2, $2, $5); }
                    ;
attr_spec_list      : %empty                                            { ERRCHK; $$ = bcc_yyval = parser_make_attributes_list(); }
                    | attr_spec_list attr_spec                          { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    ;
attr_spec           : TOK_ATTR '(' '(' ')' ')'                          { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(4, $2, $3, $4, $5); }
                    | attrs ')' ')'                                     { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(2, $2, $3); }
                    ;
attrs               : TOK_ATTR '(' '(' attr                             { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $4); parser_cleanup(2, $2, $3); }
                    | attrs ',' attr                                    { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    ;
attr                : TOK_IDENT                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_IDENT '(' ')'                                 { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(2, $2, $3); }
                    | attr_fn ')'                                       { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(1, $2); }
                    ;
attr_fn             : TOK_IDENT '(' TOK_IDENT                           { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    | TOK_IDENT '(' TOK_INTCON                          { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    | TOK_IDENT '(' TOK_STRINGCON                       { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    | attr_fn ',' TOK_IDENT                             { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    | attr_fn ',' TOK_INTCON                            { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    | attr_fn ',' TOK_STRINGCON                         { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); parser_cleanup(1, $2); }
                    ;
initializer         : assign_expr                                       { ERRCHK; $$ = bcc_yyval = $1->tok_kind == TOK_STRINGCON ? $1 : tchk_ptr_conv($1, 1); } /* do nothing */
                    | init_list '}'                                     { ERRCHK; $$ = bcc_yyval = $1; astree_destroy($2); } /* do nothing */
                    | init_list ',' '}'                                 { ERRCHK; $$ = bcc_yyval = $1; astree_destroy($2); astree_destroy($3); } /* do nothing */
                    ;
init_list           : init_list ',' initializer                         { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $3); astree_destroy($2); } /* adopt */
                    | '{' initializer                                   { ERRCHK; $$ = bcc_yyval = astree_adopt(parser_new_sym($1, TOK_INIT_LIST), 1, $2); } /* adopt */
                    ;
declarator          : pointer declarator                                { ERRCHK; $$ = bcc_yyval = define_pointer($2, $1); } /* define_pointer */
                    | TOK_IDENT attr_spec_list                          { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(1, $2); }
                    | '(' declarator attr_spec_list ')'                 { ERRCHK; $$ = bcc_yyval = $2; parser_cleanup(3, $1, $3, $4); } /* do nothing */
                    | declarator direct_decl attr_spec_list             { ERRCHK; $$ = bcc_yyval = define_dirdecl($1, $2); parser_cleanup(1, $3); } /* validate_dirdecl */
                    ;
abs_declarator      : pointer abs_declarator                            { ERRCHK; $$ = bcc_yyval = define_pointer($2, $1); } /* define_pointer */
                    | '(' abs_declarator ')'                            { ERRCHK; $$ = bcc_yyval = $2; parser_cleanup(2, $1, $3); } /* do nothing */
                    | abs_declarator direct_decl                        { ERRCHK; $$ = bcc_yyval = define_dirdecl($1, $2); } /* validate_dirdecl */
                    | %empty                                            { ERRCHK; $$ = bcc_yyval = parser_make_type_name(); }
                    ;
pointer             : '*'                                               { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_POINTER); }
                    | pointer TOK_CONST                                 { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    | pointer TOK_VOLATILE                              { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    | pointer TOK_RESTRICT                              { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    ;
direct_decl         : '[' ']'                                           { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_ARRAY); astree_destroy($2); } /* do nothing */
                    | '[' cond_expr ']'                                 { ERRCHK; $$ = bcc_yyval = validate_array_size(parser_new_sym($1, TOK_ARRAY), $2); astree_destroy($3); }
                    | fn_direct_decl                                    { ERRCHK; $$ = bcc_yyval = $1; }
                    | fn_direct_decl asm_spec                           { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(1, $2); }
                    ;
fn_direct_decl      : param_list ')'                                    { ERRCHK; $$ = bcc_yyval = finalize_param_list($1, NULL); parser_cleanup(1, $2); } /* exit parameter table */
                    | param_list ',' TOK_ELLIPSIS ')'                   { ERRCHK; $$ = bcc_yyval = finalize_param_list($1, $3); parser_cleanup(2, $2, $4); } /* exit parameter table */
                    | '(' ')'                                           { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_PARAM_LIST); parser_cleanup(1, $2); }
                    | '(' TOK_VOID ')'                                  { ERRCHK; $$ = bcc_yyval = astree_adopt(parser_new_sym($1, TOK_PARAM_LIST), 1, $2); parser_cleanup(1, $3); }
                    ;
param_list          : '(' decl_specs declarator                         { ERRCHK; $$ = bcc_yyval = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | '(' decl_specs abs_declarator                     { ERRCHK; $$ = bcc_yyval = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | param_list ',' decl_specs declarator              { ERRCHK; $$ = bcc_yyval = validate_param($1, parser_make_declaration($3), $4); astree_destroy($2); } /* define_param */
                    | param_list ',' decl_specs abs_declarator          { ERRCHK; $$ = bcc_yyval = validate_param($1, parser_make_declaration($3), $4); astree_destroy($2); } /* define_param */
                    ;
block               : block_content '}'                                 { ERRCHK; $$ = bcc_yyval = finalize_block($1); astree_destroy($2); }
                    ;
block_content       : '{'                                               { ERRCHK; $$ = bcc_yyval = validate_block(parser_new_sym($1, TOK_BLOCK)); }
                    | block_content stmt                                { ERRCHK; $$ = bcc_yyval = validate_block_content($1, $2); }
                    | block_content declarations ';'                    { ERRCHK; $$ = bcc_yyval = validate_block_content($1, finalize_declaration($2)); parser_cleanup(1, $3); }
                    ;
stmt                : block                                             { ERRCHK; $$ = bcc_yyval = $1; }
                    | dowhile                                           { ERRCHK; $$ = bcc_yyval = $1; }
                    | while                                             { ERRCHK; $$ = bcc_yyval = $1; }
                    | for                                               { ERRCHK; $$ = bcc_yyval = $1; }
                    | ifelse                                            { ERRCHK; $$ = bcc_yyval = $1; }
                    | switch                                            { ERRCHK; $$ = bcc_yyval = $1; }
                    | return                                            { ERRCHK; $$ = bcc_yyval = $1; }
                    | labelled_stmt                                     { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_CONTINUE ';'                                  { ERRCHK; $$ = bcc_yyval = validate_continue($1); astree_destroy($2); }
                    | TOK_BREAK ';'                                     { ERRCHK; $$ = bcc_yyval = validate_break($1); astree_destroy($2); }
                    | TOK_GOTO any_ident ';'                            { ERRCHK; $$ = bcc_yyval = validate_goto($1, $2); astree_destroy($3); }
                    | stmt_expr                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
stmt_expr           : expr ';'                                          { ERRCHK; $$ = bcc_yyval = $1; astree_destroy($2); }
                    | ';'                                               { ERRCHK; $$ = bcc_yyval = parser_make_empty($1->loc); astree_destroy($1); }
                    ;
labelled_stmt       : any_ident ':' stmt                                { ERRCHK; $$ = bcc_yyval = validate_label(parser_make_label($1), $1, $3); parser_cleanup(1, $2); }
                    | TOK_DEFAULT ':' stmt                              { ERRCHK; $$ = bcc_yyval = validate_default($1, $3); parser_cleanup(1, $2); }
                    | TOK_CASE cond_expr   ':' stmt                     { ERRCHK; $$ = bcc_yyval = validate_case($1, $2, $4); parser_cleanup(1, $3); }
                    ;
for_reinit          : expr ')'                                          { ERRCHK; $$ = bcc_yyval = $1; parser_cleanup(1, $2); }
                    | ')'                                               { ERRCHK; $$ = bcc_yyval = parser_make_empty($1->loc); parser_cleanup(1, $1); }
                    ;
for                 : TOK_FOR '(' stmt_expr stmt_expr for_reinit stmt   { ERRCHK; $$ = bcc_yyval = validate_for($1, $3, $4, $5, $6); parser_cleanup(1, $2); }
                    ;
dowhile             : TOK_DO stmt TOK_WHILE '(' expr ')' ';'            { ERRCHK; $$ = bcc_yyval = validate_do($1, $2, $5); parser_cleanup (4, $3, $4, $6, $7); }
                    ;
while               : TOK_WHILE '(' expr ')' stmt                       { ERRCHK; $$ = bcc_yyval = validate_while($1, $3, $5); parser_cleanup (2, $2, $4); }
                    ;
ifelse              : TOK_IF '(' expr ')' stmt TOK_ELSE stmt            { ERRCHK; $$ = bcc_yyval = validate_ifelse($1, $3, $5, $7); parser_cleanup (3, $2, $4, $6); }
                    | TOK_IF '(' expr ')' stmt %prec TOK_ELSE           { ERRCHK; $$ = bcc_yyval = validate_ifelse($1, $3, $5, parser_make_empty(lexer_get_loc())); parser_cleanup (2, $2, $4); }
                    ;
switch              : TOK_SWITCH '(' switch_expr ')' stmt               { ERRCHK; $$ = bcc_yyval = validate_switch($1, $3, $5); parser_cleanup(2, $2, $4); }
                    ;
switch_expr         : expr                                              { ERRCHK; $$ = bcc_yyval = validate_switch_expr($1); }
                    ;
return              : TOK_RETURN expr ';'                               { ERRCHK; $$ = bcc_yyval = validate_return($1, $2); parser_cleanup (1, $3); }
                    | TOK_RETURN ';'                                    { ERRCHK; $$ = bcc_yyval = validate_return($1, parser_make_empty($2->loc)); parser_cleanup (1, $2); }
                    ;
expr                : assign_expr                                       { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_EXTNSN assign_expr                            { ERRCHK; $$ = bcc_yyval = $2; parser_cleanup(1, $1); }
                    | expr ',' assign_expr                              { ERRCHK; $$ = bcc_yyval = validate_comma($2, $1, $3); }
                    | expr ',' TOK_EXTNSN assign_expr                   { ERRCHK; $$ = bcc_yyval = validate_comma($2, $1, $4); parser_cleanup(1, $3); }
                    ;
assign_expr         : unary_expr assign_op assign_expr                  { ERRCHK; $$ = bcc_yyval = validate_assignment($2, $1, $3); }
                    | cond_expr                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
assign_op           : '='                                               { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_ADDEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_SUBEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_MULEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_DIVEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_REMEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_ANDEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_OREQ                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_XOREQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_SHREQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_SHLEQ                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
cond_expr           : binary_expr '?' expr ':' cond_expr                { ERRCHK; $$ = bcc_yyval = validate_conditional($2, $1, $3, $5); astree_destroy($4); }
                    | binary_expr                                       { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
binary_expr         : binary_expr '+' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_addition($2, $1, $3); }
                    | binary_expr '-' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_addition($2, $1, $3); }
                    | binary_expr '/' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '*' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '%' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_multiply($2, $1, $3); }
                    | binary_expr '>' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr '<' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_LE binary_expr                    { ERRCHK; $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_GE binary_expr                    { ERRCHK; $$ = bcc_yyval = validate_relational($2, $1, $3); }
                    | binary_expr TOK_EQ binary_expr                    { ERRCHK; $$ = bcc_yyval = validate_equality($2, $1, $3); }
                    | binary_expr TOK_NE binary_expr                    { ERRCHK; $$ = bcc_yyval = validate_equality($2, $1, $3); }
                    | binary_expr TOK_OR binary_expr                    { ERRCHK; $$ = bcc_yyval = validate_logical($2, $1, $3); }
                    | binary_expr TOK_AND binary_expr                   { ERRCHK; $$ = bcc_yyval = validate_logical($2, $1, $3); }
                    | binary_expr '|' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr '&' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr '^' binary_expr                       { ERRCHK; $$ = bcc_yyval = validate_bitwise($2, $1, $3); }
                    | binary_expr TOK_SHR binary_expr                   { ERRCHK; $$ = bcc_yyval = validate_shift($2, $1, $3); }
                    | binary_expr TOK_SHL binary_expr                   { ERRCHK; $$ = bcc_yyval = validate_shift($2, $1, $3); }
                    | cast_expr                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
cast_expr           : '(' decl_specs abs_declarator')' unary_expr       { ERRCHK; $$ = bcc_yyval = parser_make_cast($1, $2, $3, $5); parser_cleanup(1, $4); }
                    | unary_expr                                        { ERRCHK; $$ = bcc_yyval = $1; }
                    ;
unary_expr          : postfix_expr                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_INC unary_expr                                { ERRCHK; $$ = bcc_yyval = validate_increment($1, $2); }
                    | TOK_DEC unary_expr                                { ERRCHK; $$ = bcc_yyval = validate_increment($1, $2); }
                    | unary_op cast_expr                                { ERRCHK; $$ = bcc_yyval = validate_unary($1, $2); }
                    | '&' cast_expr                                     { ERRCHK; $$ = bcc_yyval = validate_unary(parser_new_sym($1, TOK_ADDROF), $2); }
                    | TOK_SIZEOF unary_expr                             { ERRCHK; $$ = bcc_yyval = validate_sizeof($1, $2); }
                    | TOK_SIZEOF '(' decl_specs abs_declarator')'       { ERRCHK; $$ = bcc_yyval = parse_sizeof($1, $3, $4); parser_cleanup(2, $2, $5); }
                    ;
unary_op            : '!'                                               { ERRCHK; $$ = bcc_yyval = $1; }
                    | '~'                                               { ERRCHK; $$ = bcc_yyval = $1; }
                    | '+'                                               { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_POS); }
                    | '-'                                               { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_NEG); }
                    | '*'                                               { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_INDIRECTION); }
                    ;
postfix_expr        : primary_expr                                      { ERRCHK; $$ = bcc_yyval = $1; }
                    | postfix_expr TOK_INC                              { ERRCHK; $$ = bcc_yyval = validate_increment(parser_new_sym($2, TOK_POST_INC), $1); }
                    | postfix_expr TOK_DEC                              { ERRCHK; $$ = bcc_yyval = validate_increment(parser_new_sym($2, TOK_POST_DEC), $1); }
                    | call                                              { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_VA_START '(' expr ',' TOK_IDENT ')'           { ERRCHK; $$ = bcc_yyval = validate_va_start($1, $3, $5); parser_cleanup(3, $2, $4, $6); }
                    | TOK_VA_END '(' expr ')'                           { ERRCHK; $$ = bcc_yyval = validate_va_end($1, $3); parser_cleanup(2, $2, $4); }
                    | TOK_VA_ARG '(' expr ',' decl_specs abs_declarator ')'   { ERRCHK; $$ = bcc_yyval = parse_va_arg($1, $3, $5, $6); parser_cleanup(3, $2, $4, $7); }
                    | postfix_expr '[' expr ']'                         { ERRCHK; $$ = bcc_yyval = validate_subscript(parser_new_sym($2, TOK_SUBSCRIPT), $1, $3); parser_cleanup(1, $4); }
                    | postfix_expr '.' any_ident                        { ERRCHK; $$ = bcc_yyval = validate_reference($2, $1, $3); }
                    | postfix_expr TOK_ARROW any_ident                  { ERRCHK; $$ = bcc_yyval = validate_reference($2, $1, $3); }
                    ;
call                : postfix_expr '(' ')'                              { ERRCHK; $$ = bcc_yyval = finalize_call(validate_call($1, parser_new_sym($2, TOK_CALL))); parser_cleanup(1, $3);}
                    | call_args ')'                                     { ERRCHK; $$ = bcc_yyval = finalize_call($1); parser_cleanup(1, $2); }
                    ;
call_args           : postfix_expr '(' assign_expr                      { ERRCHK; $$ = bcc_yyval = validate_arg(validate_call($1, parser_new_sym($2, TOK_CALL)), $3); }
                    | call_args ',' assign_expr                         { ERRCHK; $$ = bcc_yyval = validate_arg($1, $3); parser_cleanup(1, $2); }
                    | call_args ',' TOK_EXTNSN assign_expr              { ERRCHK; $$ = bcc_yyval = validate_arg($1, $4); parser_cleanup(2, $2, $3); }
                    ;
primary_expr        : TOK_IDENT                                         { ERRCHK; $$ = bcc_yyval = validate_ident($1); }
                    | constant                                          { ERRCHK; $$ = bcc_yyval = $1; }
                    | '(' expr ')'                                      { ERRCHK; $$ = bcc_yyval = $2; parser_cleanup(2, $1, $3); }
                    ;
constant            : TOK_INTCON                                        { ERRCHK; $$ = bcc_yyval = validate_intcon($1); }
                    | TOK_CHARCON                                       { ERRCHK; $$ = bcc_yyval = validate_charcon($1); }
                    | string_literal                                    { ERRCHK; $$ = bcc_yyval = validate_stringcon(parser_join_strings($1)); }
                    ;
string_literal      : TOK_STRINGCON                                     { ERRCHK; $$ = bcc_yyval = parser_make_joiner($1); }
                    | TOK_PRTY_FN                                       { ERRCHK; $$ = bcc_yyval = parser_make_joiner(parse_pretty_function($1)); }
                    | string_literal TOK_STRINGCON                      { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, $2); }
                    | string_literal TOK_PRTY_FN                        { ERRCHK; $$ = bcc_yyval = astree_adopt($1, 1, parse_pretty_function($2)); }
                    ;
any_ident           : TOK_IDENT                                         { ERRCHK; $$ = bcc_yyval = $1; }
                    | TOK_TYPEDEF_NAME                                  { ERRCHK; $$ = bcc_yyval = parser_new_sym($1, TOK_IDENT); }
                    ;
%%

/* define parser-specific functions in a separate source file */
#include "parser.c"
