%{

#include "lyutils.h"
#include "yyparse.h"

#define YY_USER_ACTION  { lexer_advance(); }
#define YY_FATAL_ERROR(msg) { lexer_fatal_error (msg); }

%}

%option 8bit
%option debug
%option nobackup
%option nodefault
%option noinput
%option nounput
%option noyywrap
%option warn
/*%option verbose*/

LETTER          [A-Za-z_]
DIGIT           [0-9]
HEXDIGIT        [0-9a-fA-F]
OCTDIGIT        [0-7]
MANTISSA        ({DIGIT}+\.?{DIGIT}*|\.{DIGIT}+)
EXPONENT        ([Ee][+-]?{DIGIT}+)
NUMBER          ({MANTISSA}{EXPONENT}?)
NOTNUMBER       ({MANTISSA}[Ee][+-]?)
IDENT           ({LETTER}({LETTER}|{DIGIT})*)
INTWIDTH        ([uU]|[lL]|[uU][lL]|[lL][uU])
DECCON          ([1-9]{DIGIT}*)
OCTCON          (0{OCTDIGIT}*)
HEXCON          (0[xX]{HEXDIGIT}+)
INTCON          ({DECCON}|{OCTCON}|{HEXCON})({INTWIDTH})?
OCTCHAR         (\\{OCTDIGIT}{OCTDIGIT}?{OCTDIGIT}?)
HEXCHAR         (\\x{HEXDIGIT}+)
ESCSEQUENCE     (\\[ntvbrfa\\?'"])
CHARCON         ('([^\\'\n]|{ESCSEQUENCE}|{OCTCHAR}|{HEXCHAR})')
STRINGCON       (\"([^\\"\n]|{ESCSEQUENCE}|{OCTCHAR}|{HEXCHAR})*\")
NOTCHAR         ('[^']*)
NOTSTRING       (\"[^"]*)
%%

"#".*\n         { lexer_include(); lexer_newline(); }
[ \t]+          { }
\n              { lexer_newline(); }

"void"          { return lexer_token (TOK_VOID); }
"int"           { return lexer_token (TOK_INT); }
"long"          { return lexer_token (TOK_LONG); }
"short"         { return lexer_token (TOK_SHORT); }
"char"          { return lexer_token (TOK_CHAR); }
"signed"        { return lexer_token (TOK_SIGNED); }
"unsigned"      { return lexer_token (TOK_UNSIGNED); }
"float"         { return lexer_token (TOK_FLOAT); }
"double"        { return lexer_token (TOK_DOUBLE); }
"static"        { return lexer_token (TOK_STATIC); }
"extern"        { return lexer_token (TOK_EXTERN); }
"auto"          { return lexer_token (TOK_AUTO); }
"register"      { return lexer_token (TOK_REGISTER); }
"const"         { return lexer_token (TOK_CONST); }
"volatile"      { return lexer_token (TOK_VOLATILE); }
"if"            { return lexer_if (); }
"else"          { return lexer_token (TOK_ELSE); }
"switch"        { return lexer_switch(); }
"do"            { return lexer_iteration (TOK_DO); }
"while"         { return lexer_iteration (TOK_WHILE); }
"for"           { return lexer_iteration (TOK_FOR); }
"return"        { return lexer_token (TOK_RETURN); }
"break"         { return lexer_break(); }
"continue"      { return lexer_continue(); }
"goto"          { return lexer_token (TOK_GOTO); }
"case"          { return lexer_case(); }
"default"       { return lexer_default(); }
"struct"        { return lexer_token (TOK_STRUCT); }
"union"         { return lexer_token (TOK_UNION); }
"enum"          { return lexer_token (TOK_ENUM); }
"typedef"       { return lexer_token (TOK_TYPEDEF); }
"sizeof"        { return lexer_token (TOK_SIZEOF); }
"__builtin_va_arg" { return lexer_token(TOK_VA_ARG); }
"__builtin_va_start" { return lexer_token(TOK_VA_START); }
"__builtin_va_end" { return lexer_token(TOK_VA_END); }
"__attribute__" { return lexer_token(TOK_ATTR); }
"__extension__" { return lexer_token(TOK_EXTNSN); }
"__asm__"       { return lexer_token(TOK_ASM); }
"__PRETTY_FUNCTION__" { return lexer_token(TOK_PRTY_FN); }
"__restrict"    { return lexer_token(TOK_RESTRICT); }
"\.\.\."        { return lexer_token(TOK_ELLIPSIS); }
{IDENT}         { return lexer_ident (); }
{STRINGCON}     { return lexer_token (TOK_STRINGCON); }
{INTCON}        { return lexer_token (TOK_INTCON); }
{CHARCON}       { return lexer_token (TOK_CHARCON); }

"->"            { return lexer_token (TOK_ARROW); }
"=="            { return lexer_token (TOK_EQ); }
"!="            { return lexer_token (TOK_NE); }
"<="            { return lexer_token (TOK_LE); }
">="            { return lexer_token (TOK_GE); }
">>"            { return lexer_token (TOK_SHR); }
"<<"            { return lexer_token (TOK_SHL); }
"++"            { return lexer_token (TOK_INC); }
"--"            { return lexer_token (TOK_DEC); }
"&&"            { return lexer_token (TOK_AND); }
"||"            { return lexer_token (TOK_OR); }
"-="            { return lexer_token (TOK_SUBEQ); }
"+="            { return lexer_token (TOK_ADDEQ); }
"*="            { return lexer_token (TOK_MULEQ); }
"/="            { return lexer_token (TOK_DIVEQ); }
"%="            { return lexer_token (TOK_REMEQ); }
"&="            { return lexer_token (TOK_ANDEQ); }
"|="            { return lexer_token (TOK_OREQ); }
"^="            { return lexer_token (TOK_XOREQ); }
">>="           { return lexer_token (TOK_SHREQ); }
"<<="           { return lexer_token (TOK_SHLEQ); }
"\."            { return lexer_token ('.'); }
"="             { return lexer_token ('='); }
"+"             { return lexer_token ('+'); }
"-"             { return lexer_token ('-'); }
"*"             { return lexer_token ('*'); }
"/"             { return lexer_token ('/'); }
"%"             { return lexer_token ('%'); }
"\^"            { return lexer_token ('^'); }
"("             { return lexer_token ('('); }
")"             { return lexer_token (')'); }
"["             { return lexer_token ('['); }
"]"             { return lexer_token (']'); }
"{"             { return lexer_token ('{'); }
"}"             { return lexer_token ('}'); }
","             { return lexer_token (','); }
";"             { return lexer_token (';'); }
":"             { return lexer_token (':'); }
"<"             { return lexer_token ('<'); }
">"             { return lexer_token ('>'); }
"|"             { return lexer_token ('|'); }
"&"             { return lexer_token ('&'); }
"~"             { return lexer_token ('~'); }
"!"             { return lexer_token ('!'); }
"?"             { return lexer_token ('?'); }

{NOTNUMBER}     { return lexer_bad_token (TOK_INTCON); }
{NOTCHAR}       { return lexer_bad_token (TOK_CHARCON); }
{NOTSTRING}     { return lexer_bad_token (TOK_STRINGCON); }
.               { lexer_bad_char (*yytext); }

%%

