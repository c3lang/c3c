
D			[0-9]
UN          [_]
L			[a-zA-Z_]
AN			[a-zA-Z_0-9]
H			[a-fA-F0-9]
UA          [A-Z_0-9]
DC          [a-z]
UC          [A-Z]
E			[Ee][+-]?{D}+
P           [Pp][+-]?{D}+

%{
#include <stdio.h>
#include "grammar.tab.h"
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
"define"        { count(); return(DEFINE); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"float"			{ count(); return(FLOAT); }
"for"			{ count(); return(FOR); }
"foreach"		{ count(); return(FOREACH); }
"foreach_r"		{ count(); return(FOREACH_R); }
"fn"            { count(); return(FN); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(INT); }
"inline"		{ count(); return(INLINE); }
"ichar"         { count(); return(ICHAR); }
"uint"			{ count(); return(UINT); }
"long"			{ count(); return(LONG); }
"nextcase"      { count(); return(NEXTCASE); }
"ulong"			{ count(); return(ULONG); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"ushort"		{ count(); return(USHORT); }
"fault"		    { count(); return(FAULT); }
"module"        { count(); return(MODULE); }
"struct"		{ count(); return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"typedef"		{ count(); return(TYPEDEF); }
"union"			{ count(); return(UNION); }
"void"			{ count(); return(VOID); }
"while"			{ count(); return(WHILE); }
"null"          { count(); return(NUL); }
"$for"          { count(); return(CT_FOR); }
"$foreach"      { count(); return(CT_FOREACH); }
"$case"         { count(); return(CT_CASE); }
"$switch"       { count(); return(CT_SWITCH); }
"$default"      { count(); return(CT_DEFAULT); }
"$if"           { count(); return(CT_IF); }
"$else"         { count(); return(CT_ELSE); }
"$endif"        { count(); return(CT_ENDIF); }
"$endswitch"    { count(); return(CT_ENDSWITCH); }
"$endforeach"   { count(); return(CT_ENDFOREACH); }
"$endfor"       { count(); return(CT_ENDFOR); }

@[_]*[A-Z]{UA}*   { count(); return(AT_CONST_IDENT); }
#[_]*[A-Z]{UA}*   { count(); return(HASH_CONST_IDENT); }
$[_]*[A-Z]{UA}*   { count(); return(CT_CONST_IDENT); }
[_]*[A-Z]{UA}*   { count(); return(CONST_IDENT); }
@[_]*[A-Z]{UA}*[a-z]{AN}* { count(); return(AT_TYPE_IDENT); }
#[_]*[A-Z]{UA}*[a-z]{AN}* { count(); return(HASH_TYPE_IDENT); }
$[_]*[A-Z]{UA}*[a-z]{AN}* { count(); return(CT_TYPE_IDENT); }
[_]*[A-Z]{UA}*[a-z]{AN}* { count(); return(TYPE_IDENT); }
@[_]*[a-z]{AN}*    { count(); return(AT_IDENT); }
#[_]*[a-z]{AN}*    { count(); return(HASH_IDENT); }
$[_]*[a-z]{AN}*    { count(); return(CT_IDENT); }
[_]*[a-z]{AN}*    { count(); return(IDENT); }
0[xX]{H}+	{ count(); return(INTEGER); }
0[oO]{D}+?		{ count(); return(INTEGER); }
{D}+		{ count(); return(INTEGER); }

{D}+{E}		{ count(); return(REAL); }
0[xX]{H}+{P}	{ count(); return(REAL); }
{D}+"."{D}*({E})?	{ count(); return(REAL); }
0[xX]{H}+"."{H}*({P})? { count(); return(REAL); }

\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }
\'(\\.|[^\\'])*\'	{ count(); return(CHAR_LITERAL); }

"..."			{ count(); return(ELLIPSIS); }
".."			{ count(); return(DOTDOT); }
">>="			{ count(); return(SHR_ASSIGN); }
"<<="			{ count(); return(SHL_ASSIGN); }
"+="			{ count(); return(ADD_ASSIGN); }
"-="			{ count(); return(SUB_ASSIGN); }
"*="			{ count(); return(MUL_ASSIGN); }
"/="			{ count(); return(DIV_ASSIGN); }
"%="			{ count(); return(MOD_ASSIGN); }
"&="			{ count(); return(AND_ASSIGN); }
"^="			{ count(); return(XOR_ASSIGN); }
"|="			{ count(); return(OR_ASSIGN); }
">>"			{ count(); return(SHR_OP); }
"<<"			{ count(); return(SHL_OP); }
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
"{|"        { count(); return(FN_BLOCK_BEGIN); }
"|}"        { count(); return(FN_BLOCK_END); }
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
