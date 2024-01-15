%option yylineno

D			[0-9]
DU          [0-9_]
UN          [_]
L			[a-zA-Z_]
AN			[a-zA-Z_0-9]
H			[a-fA-F0-9]
HU          [a-fA-F0-9_]
UA          [A-Z_0-9]
O           [0-7]
B           [0-1]
DC          [a-z]
UC          [A-Z]
CONST       [_]*{UC}{UA}*
TYPE        [_]*{UC}{UA}*{DC}{AN}*
IDENTIFIER  [_]*{DC}{AN}*
E			[Ee][+-]?{D}+
P           [Pp][+-]?{D}+
B64         [ \t\v\n\f]?[A-Za-z0-9+/][ \t\v\n\fA-Za-z0-9+/=]+
HEX         [ \t\v\n\f]?[A-Fa-f0-9][ \t\v\n\fA-Fa-f0-9]+
INTTYPE     ([ui](8|16|32|64|128)|[Uu][Ll]?|[Ll])
REALTYPE    ([f](8|16|32|64|128)?)
INT         {D}(_*{D})*
HINT        {H}(_*{H})*
OINT        {O}(_*{O})*
BINT        {B}(_*{B})*

%x COMMENT RAW_STRING

%{
#include <stdio.h>
#include "grammar.tab.h"
void count(void);
int comment_level = 0;

%}

%%


"$alignof"      { count(); return(CT_ALIGNOF); }
"$and"          { count(); return(CT_AND); }
"$assert"       { count(); return(CT_ASSERT); }
"$assignable"   { count(); return(CT_ASSIGNABLE); }
"$case"         { count(); return(CT_CASE); }
"$default"      { count(); return(CT_DEFAULT); }
"$defined"      { count(); return(CT_DEFINED); }
"$echo"         { count(); return(CT_ECHO); }
"$else"         { count(); return(CT_ELSE); }
"$endfor"       { count(); return(CT_ENDFOR); }
"$endforeach"   { count(); return(CT_ENDFOREACH); }
"$endif"        { count(); return(CT_ENDIF); }
"$endswitch"    { count(); return(CT_ENDSWITCH); }
"$error"        { count(); return(CT_ERROR); }
"$eval"         { count(); return(CT_EVAL); }
"$evaltype"     { count(); return(CT_EVALTYPE); }
"$extnameof"    { count(); return(CT_EXTNAMEOF); }
"$feature"      { count(); return(CT_FEATURE); }
"$for"          { count(); return(CT_FOR); }
"$foreach"      { count(); return(CT_FOREACH); }
"$if"           { count(); return(CT_IF); }
"$is_const"     { count(); return(CT_IS_CONST); }
"$include"      { count(); return(CT_INCLUDE); }
"$nameof"       { count(); return(CT_NAMEOF); }
"$offsetof"     { count(); return(CT_OFFSETOF); }
"$qnameof"      { count(); return(CT_QNAMEOF); }
"$sizeof"       { count(); return(CT_SIZEOF); }
"$stringify"    { count(); return(CT_STRINGIFY); }
"$switch"       { count(); return(CT_SWITCH); }
"$typefrom"     { count(); return(CT_TYPEFROM); }
"$typeof"       { count(); return(CT_TYPEOF); }
"$vaarg"        { count(); return(CT_VAARG); }
"$vaconst"      { count(); return(CT_VACONST); }
"$vacount"      { count(); return(CT_VACOUNT); }
"$vaexpr"       { count(); return(CT_VAEXPR); }
"$varef"        { count(); return(CT_VAREF); }
"$vasplat"      { count(); return(CT_VASPLAT); }
"$vatype"       { count(); return(CT_VATYPE); }
"/*"			{ count(); BEGIN(COMMENT); }
<COMMENT>{
	"/*"        { count(); comment_level++; }
	"*"+"/"     { count(); if (comment_level) { comment_level--; } else { BEGIN(INITIAL); } }
	"*"+        { count(); }
	[^/*\n]+    { count(); }
	[/]         { count(); }
	\n          { count(); }
}
\/\/.*          { count(); }
"any"           { count(); return(ANY); }
"anyfault"      { count(); return(ANYFAULT); }
"asm"           { count(); return(ASM); }
"assert"        { count(); return(ASSERT); }
"bitstruct"     { count(); return(BITSTRUCT); }
"bool"          { count(); return(BOOL); }
"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"catch"         { count(); return(CATCH); }
"char"			{ count(); return(CHAR); }
"const"			{ count(); return(CONST); }
"continue"		{ count(); return(CONTINUE); }
"def"           { count(); return(DEF); }
"default"		{ count(); return(DEFAULT); }
"defer"         { count(); return(DEFER); }
"distinct"      { count(); return(DISTINCT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"extern"        { count(); return(EXTERN); }
"false"         { count(); return(FALSE); }
"fault"		    { count(); return(FAULT); }
"float"			{ count(); return(FLOAT); }
"bfloat16"      { count(); return(BFLOAT16); }
"float16"       { count(); return(FLOAT16); }
"float128"      { count(); return(FLOAT128); }
"fn"            { count(); return(FN); }
"for"			{ count(); return(FOR); }
"foreach"		{ count(); return(FOREACH); }
"foreach_r"		{ count(); return(FOREACH_R); }
"ichar"         { count(); return(ICHAR); }
"if"			{ count(); return(IF); }
"import"        { count(); return(IMPORT); }
"inline"		{ count(); return(INLINE); }
"int"			{ count(); return(INT); }
"int128"		{ count(); return(INT128); }
"interface"     { count(); return(INTERFACE); }
"iptr"          { count(); return(IPTR); }
"isz"           { count(); return(ISZ); }
"long"			{ count(); return(LONG); }
"macro"         { count(); return(MACRO); }
"module"        { count(); return(MODULE); }
"nextcase"      { count(); return(NEXTCASE); }
"null"          { count(); return(NUL); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"struct"		{ count(); return(STRUCT); }
"static"        { count(); return(STATIC); }
"switch"		{ count(); return(SWITCH); }
"tlocal"        { count(); return(TLOCAL); }
"true"          { count(); return(TRUE); }
"try"           { count(); return(TRY); }
"typeid"        { count(); return(TYPEID); }
"uint"			{ count(); return(UINT); }
"uint128"		{ count(); return(UINT128); }
"ulong"			{ count(); return(ULONG); }
"union"			{ count(); return(UNION); }
"uptr"          { count(); return(UPTR); }
"ushort"		{ count(); return(USHORT); }
"usz"           { count(); return(USZ); }
"var"           { count(); return(VAR); }
"void"			{ count(); return(VOID); }
"while"			{ count(); return(WHILE); }

@{CONST}        { count(); return(AT_CONST_IDENT); }
#{CONST}        { count(); return(HASH_CONST_IDENT); }
${CONST}        { count(); return(CT_CONST_IDENT); }
{CONST}         { count(); return(CONST_IDENT); }
@{TYPE}         { count(); return(AT_TYPE_IDENT); }
#{TYPE}         { count(); return(HASH_TYPE_IDENT); }
${TYPE}         { count(); return(CT_TYPE_IDENT); }
{TYPE}          { count(); return(TYPE_IDENT); }
@{IDENTIFIER}   { count(); return(AT_IDENT); }
#{IDENTIFIER}   { count(); return(HASH_IDENT); }
${IDENTIFIER}   { count(); return(CT_IDENT); }
{IDENTIFIER}    { count(); return(IDENT); }
0[xX]{HINT}{INTTYPE}?	{ count(); return(INTEGER); }
0[oO]{OINT}{INTTYPE}?	{ count(); return(INTEGER); }
0[bB]{BINT}{INTTYPE}?   { count(); return(INTEGER); }
{INT}{INTTYPE}?		    { count(); return(INTEGER); }
x\'{HEX}+\' { count(); return(BYTES); }
x\"{HEX}+\" { count(); return(BYTES); }
x\`{HEX}+\` { count(); return(BYTES); }
b64\'{B64}+\' { count(); return(BYTES); }
b64\"{B64}+\" { count(); return(BYTES); }
b64\`{B64}+\` { count(); return(BYTES); }

{INT}{E}?{REALTYPE}? { count(); return(REAL); }
0[xX]{HINT}{P}{REALTYPE}?	{ count(); return(REAL); }
{INT}"."{INT}{E}?{REALTYPE}?	{ count(); return(REAL); }
0[xX]{HINT}"."{HINT}{P}{REALTYPE}? { count(); return(REAL); }

\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }
\'(\\[ux]{HEX}+|\\.|[^\\'])\'	{ count(); return(CHAR_LITERAL); }

"`"             { count(); BEGIN(RAW_STRING); }
<RAW_STRING>{
	"``"        { count(); }
	"`"         { count(); BEGIN(INITIAL); return(STRING_LITERAL); }
	"[^`]"+        { count(); }
	<<EOF>>     { BEGIN(INITIAL); return(RAW_STRING); }
}

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
"??"            { count(); return(OPTELSE); }
"::"            { count(); return(SCOPE); }
"?:"            { count(); return(ELVIS); }
"=>"            { count(); return(IMPLIES); }
"[<"            { count(); return(LVEC); }
">]"            { count(); return(RVEC); }
"(<"            { count(); return(LGENPAR); }
">)"            { count(); return(RGENPAR); }
"$$"            { count(); return(BUILTIN); }
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
"{|"        { count(); return(LBRAPIPE); }
"|}"        { count(); return(RBRAPIPE); }
[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

int yywrap(void)
{
	return 1;
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
