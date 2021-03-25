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
"string"        { return lexer_token (TOK_STRING); }
"if"            { return lexer_token (TOK_IF); }
"else"          { return lexer_token (TOK_ELSE); }
"while"         { return lexer_token (TOK_WHILE); }
"return"        { return lexer_token (TOK_RETURN); }
"struct"        { return lexer_token (TOK_STRUCT); }
"nullptr"       { return lexer_token (TOK_NULLPTR); }
"array"         { return lexer_token (TOK_ARRAY); }
{IDENT}         { return lexer_token (TOK_IDENT); }
{STRINGCON}     { return lexer_token (TOK_STRINGCON); }
{INTCON}        { return lexer_token (TOK_INTCON); }
{CHARCON}       { return lexer_token (TOK_CHARCON); }

"->"            { return lexer_token (TOK_ARROW); }
"=="            { return lexer_token (TOK_EQ); }
"!="            { return lexer_token (TOK_NE); }
"<="            { return lexer_token (TOK_LE); }
">="            { return lexer_token (TOK_GE); }
"="             { return lexer_token ('='); }
"+"             { return lexer_token ('+'); }
"-"             { return lexer_token ('-'); }
"*"             { return lexer_token ('*'); }
"/"             { return lexer_token ('/'); }
"%"             { return lexer_token ('%'); }
"^"             { return lexer_token ('^'); }
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
"!"             { return lexer_token ('!'); }

{NOTNUMBER}     { return lexer_bad_token (TOK_INTCON); }
{NOTCHAR}       { return lexer_bad_token (TOK_CHARCON); }
{NOTSTRING}     { return lexer_bad_token (TOK_STRINGCON); }
.               { lexer_bad_char (*yytext); }

%%

