/* $Id: lexer.l,v 1.4 2019-08-22 17:31:32-07 - - $ */

%{

#include "lyutils.h"
#include "yyparse.h"

#define YY_USER_ACTION  { lexer::advance(); }
#define YY_FATAL_ERROR(msg) { lexer::fatal_error (msg); }

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

"#".*\n         { lexer::include(); lexer::newline(); }
[ \t]+          { }
\n              { lexer::newline(); }

"void"          { return lexer::token (TOK_VOID); }
"int"           { return lexer::token (TOK_INT); }
"string"        { return lexer::token (TOK_STRING); }
"if"            { return lexer::token (TOK_IF); }
"else"          { return lexer::token (TOK_ELSE); }
"while"         { return lexer::token (TOK_WHILE); }
"return"        { return lexer::token (TOK_RETURN); }
"struct"        { return lexer::token (TOK_STRUCT); }
"nullptr"       { return lexer::token (TOK_NULLPTR); }
"array"         { return lexer::token (TOK_ARRAY); }
"alloc"         { return lexer::token (TOK_ALLOC); }
"ptr"           { return lexer::token (TOK_PTR); }
"not"           { return lexer::token (TOK_NOT); }
{IDENT}         { return lexer::token (TOK_IDENT); }
{STRINGCON}     { return lexer::token (TOK_STRINGCON); }
{INTCON}        { return lexer::token (TOK_INTCON); }
{CHARCON}       { return lexer::token (TOK_CHARCON); }

"->"            { return lexer::token (TOK_ARROW); }
"=="            { return lexer::token (TOK_EQ); }
"!="            { return lexer::token (TOK_NE); }
"<="            { return lexer::token (TOK_LE); }
">="            { return lexer::token (TOK_GE); }
"="             { return lexer::token ('='); }
"+"             { return lexer::token ('+'); }
"-"             { return lexer::token ('-'); }
"*"             { return lexer::token ('*'); }
"/"             { return lexer::token ('/'); }
"%"             { return lexer::token ('%'); }
"^"             { return lexer::token ('^'); }
"("             { return lexer::token ('('); }
")"             { return lexer::token (')'); }
"["             { return lexer::token ('['); }
"]"             { return lexer::token (']'); }
"{"             { return lexer::token ('{'); }
"}"             { return lexer::token ('}'); }
","             { return lexer::token (','); }
";"             { return lexer::token (';'); }
"<"             { return lexer::token ('<'); }
">"             { return lexer::token ('>'); }

{NOTNUMBER}     { return lexer::badtoken (TOK_INTCON); }
{NOTCHAR}       { return lexer::badtoken (TOK_CHARCON); }
{NOTSTRING}     { return lexer::badtoken (TOK_STRINGCON); }
.               { lexer::badchar (*yytext); }

%%

