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
DECCON          ([1-9]{DIGIT}*)
OCTCON          (0{OCTDIGIT}*)
HEXCON          (0[xX]{HEXDIGIT}+)
INTCON          ({DECCON}|{OCTCON}|{HEXCON})
CHARCON         ('([^\\'\n]|\\[\\'"0nt])')
STRINGCON       (\"([^\\"\n]|\\[\\'"0nt])*\")
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
"if"            { return lexer_token (TOK_IF); }
"else"          { return lexer_token (TOK_ELSE); }
"while"         { return lexer_token (TOK_WHILE); }
"return"        { return lexer_token (TOK_RETURN); }
"struct"        { return lexer_token (TOK_STRUCT); }
{IDENT}         { return lexer_token (TOK_IDENT); }
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
"<"             { return lexer_token ('<'); }
">"             { return lexer_token ('>'); }
"|"             { return lexer_token ('|'); }
"&"             { return lexer_token ('&'); }
"~"             { return lexer_token ('~'); }
"!"             { return lexer_token ('!'); }

{NOTNUMBER}     { return lexer_bad_token (TOK_INTCON); }
{NOTCHAR}       { return lexer_bad_token (TOK_CHARCON); }
{NOTSTRING}     { return lexer_bad_token (TOK_STRINGCON); }
.               { lexer_bad_char (*yytext); }

%%

