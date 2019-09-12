
D			[0-9]
UN          [_]
L			[a-zA-Z_]
AN			[a-zA-Z_0-9]
H			[a-fA-F0-9]
UA          [A-Z_0-9]
DC          [a-z]
UC          [A-Z]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%{
#include <stdio.h>
#include "y.tab.h"
void count(void);
void comment(void);

%}

%%
"/*"			{ comment(); }

"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"char"			{ count(); return(CHAR); }
"const"			{ count(); return(CONST); }
"continue"		{ count(); return(CONTINUE); }
"default"		{ count(); return(DEFAULT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"float"			{ count(); return(FLOAT); }
"for"			{ count(); return(FOR); }
"goto"			{ count(); return(GOTO); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(INT); }
"uint"			{ count(); return(UINT); }
"long"			{ count(); return(LONG); }
"ulong"			{ count(); return(ULONG); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"ushort"		{ count(); return(USHORT); }
"sizeof"		{ count(); return(SIZEOF); }
"local"		    { count(); return(LOCAL); }
"type"          { count(); return(TYPE); }
"error"		    { count(); return(ERROR); }
"module"        { count(); return(MODULE); }
"as"            { count(); return(AS); }
"import"        { count(); return(IMPORT); }
"generic"		{ count(); return(GENERIC); }
"struct"		{ count(); return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"typedef"		{ count(); return(TYPEDEF); }
"union"			{ count(); return(UNION); }
"void"			{ count(); return(VOID); }
"volatile"		{ count(); return(VOLATILE); }
"while"			{ count(); return(WHILE); }
"throw"         { count(); return(THROW); }
"throws"        { count(); return(THROWS); }
"func"          { count(); return(FUNC); }
"nil"           { count(); return(NIL); }
"next"          { count(); return(NEXT); }
"in"            { count(); return(IN); }
"$for"          { count(); return(CTFOR); }
"$case"         { count(); return(CTCASE); }
"$switch"       { count(); return(CTSWITCH); }
"$default"      { count(); return(CTDEFAULT); }
"$if"           { count(); return(CTIF); }
"$elif"         { count(); return(CTELIF); }
"$else"         { count(); return(CTELSE); }

[_]*[A-Z]{UA}*   { count(); return(CONST_IDENT); }
[_]*[A-Z]{UA}*[a-z]{AN}* { count(); return(TYPE_IDENT); }
[_]*[a-z]{AN}*    { count(); return(IDENT); }
@{L}+[!]?            { count(); return(AT_IDENT); }
${L}+             { count(); return(CT_IDENT); }
#{L}+           { count(); return(HASH_IDENT); }
0[xX]{H}+{IS}?	{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
L?'(\\.|[^\\'])+'	{ count(); return(CONSTANT); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

L?\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

"..."			{ count(); return(ELLIPSIS); }
">>="			{ count(); return(RIGHT_ASSIGN); }
"<<="			{ count(); return(LEFT_ASSIGN); }
"+="			{ count(); return(ADD_ASSIGN); }
"-="			{ count(); return(SUB_ASSIGN); }
"*="			{ count(); return(MUL_ASSIGN); }
"/="			{ count(); return(DIV_ASSIGN); }
"%="			{ count(); return(MOD_ASSIGN); }
"&="			{ count(); return(AND_ASSIGN); }
"^="			{ count(); return(XOR_ASSIGN); }
"|="			{ count(); return(OR_ASSIGN); }
">>"			{ count(); return(RIGHT_OP); }
"<<"			{ count(); return(LEFT_OP); }
"++"			{ count(); return(INC_OP); }
"--"			{ count(); return(DEC_OP); }
"&&"			{ count(); return(AND_OP); }
"||"			{ count(); return(OR_OP); }
"<="			{ count(); return(LE_OP); }
">="			{ count(); return(GE_OP); }
"=="			{ count(); return(EQ_OP); }
"!="			{ count(); return(NE_OP); }
"::"            { count(); return(SCOPE); }
"?:"            { count(); return(ELVIS); }
";"			{ count(); return(';'); }
("{")		{ count(); return('{'); }
("}")		{ count(); return('}'); }
","			{ count(); return(','); }
":"			{ count(); return(':'); }
"="			{ count(); return('='); }
"("			{ count(); return('('); }
")"			{ count(); return(')'); }
("[")		{ count(); return('['); }
("]")		{ count(); return(']'); }
"."			{ count(); return('.'); }
"&"			{ count(); return('&'); }
"!"			{ count(); return('!'); }
"~"			{ count(); return('~'); }
"-"			{ count(); return('-'); }
"+"			{ count(); return('+'); }
"*"			{ count(); return('*'); }
"/"			{ count(); return('/'); }
"%"			{ count(); return('%'); }
"<"			{ count(); return('<'); }
">"			{ count(); return('>'); }
"^"			{ count(); return('^'); }
"|"			{ count(); return('|'); }
"?"			{ count(); return('?'); }

[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

int yywrap(void)
{
	return 1;
}


void comment(void)
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);

	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}


int column = 0;

void count(void)
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO;
}
