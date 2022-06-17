/* clang-format off */
/* *INDENT-OFF* */
%{
#include <assert.h>

#include "lyutils.h"
#include "astree.h"
#include "debug.h"
#include "attributes.h"
#include "symtable.h"
#include "typecheck.h"
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

/* NOTE: Pointers are parsed with left recursion due to limitations I have
 * imposed on the syntax tree. This should be fixed somehow in the future, but
 * for the moment it is the only way I can see to parse types in the order they
 * are meant to be read.
 */
%%
program             : %empty                                            { $$ = parser_make_root(); }
                    | program topdecl                                   { $$ = validate_topdecl($1, finalize_declaration($2)); }
                    | program error '}'                                 { $$ = $1; astree_destroy($3); }
                    | program error ';'                                 { $$ = $1; astree_destroy($3); }
                    ;
topdecl             : declaration ';'                                   { $$ = $1; astree_destroy($2); }
                    | typespec_list declarator block                    { $$ = define_function(parser_make_declaration($1), $2, $3); }
                    | ';'                                               { $$ = &EMPTY_EXPR; astree_destroy($1); }
                    ;
declaration         : typespec_list                                     { $$ = parser_make_declaration($1); } /* fine as-is */
                    | declarations                                      { $$ = $1; } /* fine as-is */
                    ;
declarations        : typespec_list declarator                          { $$ = declare_symbol(parser_make_declaration($1), $2); } /* make_declaration, declare_symbol */
                    | typespec_list declarator '=' initializer          { $$ = define_symbol(parser_make_declaration($1), $2, $3, $4); } /* make_declaration, define_symbol */
                    | declarations ',' declarator                       { $$ = declare_symbol($1, $3); parser_cleanup(1, $2); } /* declare_symbol */
                    | declarations ',' declarator '=' initializer       { $$ = define_symbol($1, $3, $4, $5); parser_cleanup(1, $2); } /* define_symbol */
                    ;
typespec_list       : typespec_list typespec                            { $$ = validate_typespec($1, $2); } /* validate_typespec */
                    | typespec                                          { $$ = parser_make_spec_list($1); } /* make_spec_list, validate_typespec */
                    ;
typespec            : TOK_LONG                                          { $$ = $1; }
                    | TOK_SIGNED                                        { $$ = $1; }
                    | TOK_UNSIGNED                                      { $$ = $1; }
                    | TOK_SHORT                                         { $$ = $1; }
                    | TOK_INT                                           { $$ = $1; }
                    | TOK_CHAR                                          { $$ = $1; }
                    | TOK_VOID                                          { $$ = $1; }
                    | TOK_TYPEDEF                                       { $$ = $1; }
                    | struct_spec                                       { $$ = $1; }
                    | enum_spec                                         { $$ = $1; }
                    ;
struct_spec         : struct_def '}'                                    { $$ = finalize_tag_def($1); astree_destroy($2); } /* cleanup intermediate products */
                    | TOK_STRUCT TOK_IDENT                              { $$ = validate_tag_def($1, $2, NULL); } /* declare tag if necessary */
                    | TOK_UNION TOK_IDENT                               { $$ = validate_tag_def($1, $2, NULL); } /* declare tag if necessary */
                    ;
struct_def          : struct_or_union TOK_IDENT '{'                     { $$ = validate_tag_def($1, $2, $3); }
                    | struct_or_union '{'                               { $$ = validate_tag_def($1, NULL, $2); }
                    | struct_def struct_decl ';'                        { $$ = define_struct_member($1, $2); parser_cleanup(1, $3); } /* define struct member */
                    ;
struct_or_union     : TOK_STRUCT                                        { $$ = $1; }
                    | TOK_UNION                                         { $$ = $1; }
                    ;
struct_decl         : struct_decl ',' declarator                        { $$ = declare_symbol($1, $3); astree_destroy($2); } /* define struct member */
                    | typespec_list declarator                          { $$ = declare_symbol(parser_make_declaration($1), $2); } /* define struct member */
                    ;
enum_spec           : enum_def '}'                                      { $$ = finalize_tag_def($1); astree_destroy($2); } /* create tag and iterate over enums */
                    | TOK_ENUM TOK_IDENT                                { $$ = astree_adopt($1, 1, $2); } /* do nothing */
                    ;
enum_def            : TOK_ENUM TOK_IDENT '{' TOK_IDENT                  { $$ = define_enumerator(validate_tag_def($1, $2, $3), $4, NULL, NULL); }
                    | TOK_ENUM TOK_IDENT '{' TOK_IDENT '=' cond_expr    { $$ = define_enumerator(validate_tag_def($1, $2, $3), $4, $5, $6); }
                    | TOK_ENUM '{' TOK_IDENT                            { $$ = define_enumerator(validate_tag_def($1, NULL, $2), $3, NULL, NULL); }
                    | TOK_ENUM '{' TOK_IDENT '=' cond_expr              { $$ = define_enumerator(validate_tag_def($1, NULL, $2), $3, $4, $5); }
                    | enum_def ',' TOK_IDENT                            { $$ = define_enumerator($1, $3, NULL, NULL); parser_cleanup(1, $2); }
                    | enum_def ',' TOK_IDENT '=' cond_expr              { $$ = define_enumerator($1, $3, $4, $5); parser_cleanup(1, $2); }
                    ;
initializer         : assign_expr                                       { $$ = $1; } /* do nothing */
                    | init_list '}'                                     { $$ = $1; astree_destroy($2); } /* do nothing */
                    | init_list ',' '}'                                 { $$ = $1; astree_destroy($2); astree_destroy($3); } /* do nothing */
                    ;
init_list           : init_list ',' initializer                         { $$ = astree_adopt($1, 1, $3); astree_destroy($2); } /* adopt */
                    | '{' initializer                                   { $$ = astree_adopt(parser_new_sym($1, TOK_INIT_LIST), 1, $2); } /* adopt */
                    ;
declarator          : '*' declarator                                    { $$ = define_pointer($2, parser_new_sym($1, TOK_POINTER)); } /* define_pointer */
                    | TOK_IDENT                                         { $$ = validate_declarator($1); } /* validate_declarator */
                    | '(' declarator ')'                                { $$ = $2; parser_cleanup(2, $1, $3); } /* do nothing */
                    | declarator direct_decl                            { $$ = define_dirdecl($1, $2); } /* validate_dirdecl */
                    ;
abs_declarator      : '*' abs_declarator                                { $$ = define_pointer($2, parser_new_sym($1, TOK_POINTER)); } /* define_pointer */
                    | '*'                                               { $$ = parser_make_type_name(parser_new_sym($1, TOK_POINTER)); } /* create type name; define_pointer */
                    | direct_decl                                       { $$ = parser_make_type_name($1); } /* create type name; validate_dirdecl */
                    | '(' abs_declarator ')'                            { $$ = $2; parser_cleanup(2, $1, $3); } /* do nothing */
                    | abs_declarator direct_decl                        { $$ = define_dirdecl($1, $2); } /* validate_dirdecl */
                    ;
direct_decl         : '[' ']'                                           { $$ = parser_new_sym($1, TOK_ARRAY); astree_destroy($2); } /* do nothing */
                    | '[' cond_expr   ']'                               { $$ = validate_array_size($1, $2); astree_destroy($3); } /* validate_array_size */
                    | param_list ')'                                    { $$ = finalize_param_list($1); astree_destroy($2); } /* exit parameter table */
                    | '(' ')'                                           { $$ = parser_new_sym($1, TOK_PARAM_LIST); astree_destroy($2); } /* create param table */
                    | '(' TOK_VOID ')'                                  { $$ = parser_new_sym($1, TOK_PARAM_LIST); parser_cleanup(2, $2, $3); } /* create param table */
                    ;
param_list          : '(' typespec_list declarator                      { $$ = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | '(' typespec_list abs_declarator                  { $$ = parser_make_param_list($1, $2, $3); } /* create param table; define_param */
                    | '(' typespec_list                                 { $$ = parser_make_param_list($1, $2, parser_make_type_name($2)); } /* create param table; define_param */
                    | param_list ',' typespec_list declarator           { $$ = validate_param($1, $3, $4); astree_destroy($2); } /* define_param */
                    | param_list ',' typespec_list abs_declarator       { $$ = validate_param($1, $3, $4); astree_destroy($2); } /* define_param */
                    | param_list ',' typespec_list                      { $$ = validate_param($1, $3, parser_make_type_name($3)); astree_destroy($2); } /* define_param */
                    ;
block               : block_content '}'                                 { $$ = finalize_block($1); astree_destroy($2); }
                    ;
block_content       : '{'                                               { $$ = validate_block(parser_new_sym($1, TOK_BLOCK)); }
                    | block_content stmt                                { $$ = validate_block_content($1, $2); }
                    | block_content declaration ';'                     { $$ = validate_block_content($1, finalize_declaration($2)); parser_cleanup(1, $3); }
                    ;
stmt                : block                                             { $$ = $1; }
                    | dowhile                                           { $$ = $1; }
                    | while                                             { $$ = $1; }
                    | for                                               { $$ = $1; }
                    | ifelse                                            { $$ = $1; }
                    | switch                                            { $$ = $1; }
                    | return                                            { $$ = $1; }
                    | labelled_stmt                                     { $$ = $1; }
                    | expr ';'                                          { $$ = $1; parser_cleanup (1, $2); }
                    | TOK_CONTINUE ';'                                  { $$ = $1; astree_destroy($2); }
                    | TOK_BREAK ';'                                     { $$ = $1; astree_destroy($2); }
                    | TOK_GOTO TOK_IDENT ';'                            { $$ = astree_adopt($1, 1, $2); astree_destroy($3); }
                    | ';'                                               { $$ = &EMPTY_EXPR; astree_destroy($1); }
                    ;
labelled_stmt       : TOK_IDENT ':' stmt                                { $$ = validate_label(parser_make_label($1), $1, $3); parser_cleanup(1, $2); }
                    | TOK_DEFAULT ':' stmt                              { $$ = validate_default($1, $3); parser_cleanup(1, $2); }
                    | TOK_CASE cond_expr   ':' stmt                     { $$ = validate_case($1, $2, $4); parser_cleanup(1, $3); }
                    ;
for                 : TOK_FOR for_exprs stmt                            { $$ = validate_for($1, $2, $3); }
                    ;
for_exprs           : '(' ';' ';' ')'                                   { $$ = validate_for_exprs($1, &EMPTY_EXPR, &EMPTY_EXPR, &EMPTY_EXPR); parser_cleanup(3, $2, $3, $4); }
                    | '(' expr ';' ';' ')'                              { $$ = validate_for_exprs($1, $2, &EMPTY_EXPR, &EMPTY_EXPR); parser_cleanup(3, $3, $4, $5); }
                    | '(' ';' expr ';' ')'                              { $$ = validate_for_exprs($1, &EMPTY_EXPR, $3, &EMPTY_EXPR); parser_cleanup(3, $2, $4, $5); }
                    | '(' ';' ';' expr ')'                              { $$ = validate_for_exprs($1, &EMPTY_EXPR, &EMPTY_EXPR, $4); parser_cleanup(3, $2, $3, $5); }
                    | '(' expr ';' expr ';' ')'                         { $$ = validate_for_exprs($1, $2, $4, &EMPTY_EXPR); parser_cleanup(3, $3, $5, $6); }
                    | '(' expr ';' ';' expr ')'                         { $$ = validate_for_exprs($1, $2, &EMPTY_EXPR, $5); parser_cleanup(3, $3, $4, $6); }
                    | '(' ';' expr ';' expr ')'                         { $$ = validate_for_exprs($1, &EMPTY_EXPR, $3, $5); parser_cleanup(3, $2, $4, $6); }
                    | '(' expr ';' expr ';' expr ')'                    { $$ = validate_for_exprs($1, $2, $4, $6); parser_cleanup(3, $3, $5, $7); }
                    ;
dowhile             : TOK_DO stmt TOK_WHILE '(' expr ')' ';'            { $$ = validate_do($1, $2, $5); parser_cleanup (4, $3, $4, $6, $7); }
                    ;
while               : TOK_WHILE '(' expr ')' stmt                       { $$ = validate_while($1, $3, $5); parser_cleanup (2, $2, $4); }
                    ;
ifelse              : TOK_IF '(' expr ')' stmt TOK_ELSE stmt            { $$ = validate_ifelse($1, $3, $5, $7); parser_cleanup (3, $2, $4, $6); }
                    | TOK_IF '(' expr ')' stmt %prec TOK_ELSE           { $$ = validate_ifelse($1, $3, $5, &EMPTY_EXPR); parser_cleanup (2, $2, $4); }
                    ;
switch              : TOK_SWITCH '(' expr ')' stmt                      { $$ = validate_switch($1, $3, $5); parser_cleanup(2, $2, $4); }
                    ;
return              : TOK_RETURN expr ';'                               { $$ = validate_return($1, $2); parser_cleanup (1, $3); }
                    | TOK_RETURN ';'                                    { $$ = validate_return($1, &EMPTY_EXPR); parser_cleanup (1, $2); }
                    ;
expr                : assign_expr                                       { $$ = $1; }
                    | expr ',' assign_expr                              { $$ = validate_comma($2, $1, $3); }
                    ;
assign_expr         : assign_expr '=' cond_expr                         { $$ = validate_assignment($2, $1, $3); }
                    | cond_expr                                         { $$ = $1; }
                    ;
cond_expr           : binary_expr '?' expr ':' cond_expr                { $$ = validate_conditional($2, $1, $3, $5); astree_destroy($4); }
                    | binary_expr                                       { $$ = $1; }
                    ;
binary_expr         : binary_expr '+' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '-' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '/' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '*' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '%' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '>' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '<' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_LE binary_expr                    { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_GE binary_expr                    { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_EQ binary_expr                    { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_NE binary_expr                    { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_OR binary_expr                    { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_AND binary_expr                   { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '|' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '&' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr '^' binary_expr                       { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_SHR binary_expr                   { $$ = validate_binop($2, $1, $3); }
                    | binary_expr TOK_SHL binary_expr                   { $$ = validate_binop($2, $1, $3); }
                    | cast_expr                                         { $$ = $1; }
                    ;
cast_expr           : '(' typespec_list ')' unary_expr                  { $$ = parser_make_cast($1, $2, parser_make_type_name($2), $4); parser_cleanup(1, $3); }
                    | '(' typespec_list abs_declarator')' unary_expr    { $$ = parser_make_cast($1, $2, $3, $5); parser_cleanup(1, $4); }
                    | unary_expr                                        { $$ = $1; }
                    ;
unary_expr          : postfix_expr                                      { $$ = $1; }
                    | TOK_INC unary_expr                                { $$ = validate_unop($1, $2); }
                    | TOK_DEC unary_expr                                { $$ = validate_unop($1, $2); }
                    | '!' unary_expr                                    { $$ = validate_unop($1, $2); }
                    | '~' unary_expr                                    { $$ = validate_unop($1, $2); }
                    | '+' unary_expr                                    { $$ = validate_unop(parser_new_sym($1, TOK_POS), $2); }
                    | '-' unary_expr                                    { $$ = validate_unop(parser_new_sym($1, TOK_NEG), $2); }
                    | '*' unary_expr                                    { $$ = validate_indirection(parser_new_sym($1, TOK_INDIRECTION), $2); }
                    | '&' unary_expr                                    { $$ = validate_addrof(parser_new_sym($1, TOK_ADDROF), $2); }
                    | TOK_SIZEOF unary_expr                             { $$ = validate_sizeof($1, $2); }
                    | TOK_SIZEOF '(' typespec_list ')'                  { $$ = parse_sizeof($1, $3, parser_make_type_name($3)); parser_cleanup(2, $2, $4); }
                    | TOK_SIZEOF '(' typespec_list abs_declarator')'    { $$ = parse_sizeof($1, $3, $4); parser_cleanup(2, $2, $5); }
                    ;
postfix_expr        : primary_expr                                      { $$ = $1; }
                    | postfix_expr TOK_INC                              { $$ = validate_unop(parser_new_sym($2, TOK_POST_INC), $1); }
                    | postfix_expr TOK_DEC                              { $$ = validate_unop(parser_new_sym($2, TOK_POST_DEC), $1); }
                    | call                                              { $$ = $1; }
                    | postfix_expr '[' expr ']'                         { $$ = validate_subscript(parser_new_sym($2, TOK_SUBSCRIPT), $1, $3); parser_cleanup(1, $4); }
                    | postfix_expr '.' TOK_IDENT                        { $$ = validate_reference($2, $1, $3); }
                    | postfix_expr TOK_ARROW TOK_IDENT                  { $$ = validate_arrow($2, $1, $3); }
                    ;
call                : postfix_expr '(' ')'                              { $$ = finalize_call(validate_call($1, $2)); parser_cleanup(1, $3);}
                    | call_args ')'                                     { $$ = finalize_call($1); parser_cleanup(1, $2); }
                    ;
call_args           : postfix_expr '(' assign_expr                      { $$ = validate_arg(validate_call($1, $2), $3); }
                    | call_args ',' assign_expr                         { $$ = validate_arg($1, $3); parser_cleanup(1, $2); }
                    ;
primary_expr        : TOK_IDENT                                         { $$ = validate_ident($1); }
                    | constant                                          { $$ = $1; }
                    | '(' expr ')'                                      { $$ = $2; parser_cleanup(2, $1, $3); }
                    ;
constant            : TOK_INTCON                                        { $$ = validate_intcon($1); }
                    | TOK_CHARCON                                       { $$ = validate_charcon($1); }
                    | TOK_STRINGCON                                     { $$ = validate_stringcon($1); }
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
  parser_root = astree_init(TOK_ROOT, root_loc, "_root");
  DEBUGS('t', "Making symbol table");
  parser_root->symbol_table = symbol_table_init();
  state_push_table(state, parser_root->symbol_table);
  return parser_root;
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
  ASTree *declaration =
      astree_init(TOK_DECLARATION, spec_list->loc, "_declaration");
  if (spec_list->symbol == TOK_TYPE_ERROR) {
    return propogate_err(declaration, spec_list);
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
ASTree *parser_make_type_name(ASTree *first_child) {
  ASTree *type_name =
      astree_init(TOK_TYPE_NAME, first_child->loc, "_type_name");
  SymbolValue *symval =
      symbol_value_init(&type_name->loc, state_get_sequence(state));
  type_name->type = &symval->type;
  switch (first_child->symbol) {
    case TOK_SPEC_LIST:
      return type_name;
    case TOK_POINTER:
      return define_pointer(validate_declarator(type_name), first_child);
    case TOK_ARRAY:
    case TOK_PARAM_LIST:
      return define_dirdecl(validate_declarator(type_name), first_child);
    case TOK_TYPE_ERROR:
      ASTree *real_first_child = astree_get(first_child, 0);
      /* type specifier errors will be dealt with in parser_make_declaration */
      if (real_first_child->symbol == TOK_SPEC_LIST)
        return validate_declarator(type_name);
      else
        return propogate_err(validate_declarator(type_name), first_child);
    default:
      return create_terr(
          astree_adopt(validate_declarator(type_name), 1, first_child),
          BCC_TERR_UNEXPECTED_TOKEN, 1, first_child);
  }
}

ASTree *parser_make_cast(ASTree *left_paren, ASTree *spec_list,
                         ASTree *type_name, ASTree *expr) {
  ASTree *cast = parser_new_sym(left_paren, TOK_CAST);
  ASTree *declaration_or_err = parser_make_declaration(spec_list);
  return validate_cast(cast, declare_symbol(declaration_or_err, type_name),
                       expr);
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
  DEBUGS('p', "Cleaning up %zu astree nodes", count);
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
