%option yylineno
%option bison-locations
%option reentrant
%option bison-bridge

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
INT         {D}(_?{D})*
HINT        {H}(_?{H})*
OINT        {O}(_?{O})*
BINT        {B}(_?{B})*

%x COMMENT CONTRACT_DOC RAW_STRING

%{
#include <stdio.h>
#include "grammar.tab.h"

typedef struct {
	int comment_level;
} YY_Extra_Type;

#define YY_EXTRA_TYPE YY_Extra_Type

#define YY_USER_ACTION \
    yylloc->first_line = yylloc->last_line; \
    yylloc->first_column = yylloc->last_column; \
    for(int i = 0; yytext[i] != '\0'; i++) { \
        if(yytext[i] == '\n') { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
        } \
        else { \
            yylloc->last_column++; \
        } \
    }
%}

%%


"$alignof"      { return(CT_ALIGNOF); }
"$assert"       { return(CT_ASSERT); }
"$assignable"   { return(CT_ASSIGNABLE); }
"$case"         { return(CT_CASE); }
"$default"      { return(CT_DEFAULT); }
"$defined"      { return(CT_DEFINED); }
"$echo"         { return(CT_ECHO); }
"$else"         { return(CT_ELSE); }
"$endfor"       { return(CT_ENDFOR); }
"$endforeach"   { return(CT_ENDFOREACH); }
"$endif"        { return(CT_ENDIF); }
"$endswitch"    { return(CT_ENDSWITCH); }
"$error"        { return(CT_ERROR); }
"$eval"         { return(CT_EVAL); }
"$evaltype"     { return(CT_EVALTYPE); }
"$exec"         { return(CT_EXEC); }
"$extnameof"    { return(CT_EXTNAMEOF); }
"$feature"      { return(CT_FEATURE); }
"$for"          { return(CT_FOR); }
"$foreach"      { return(CT_FOREACH); }
"$if"           { return(CT_IF); }
"$is_const"     { return(CT_IS_CONST); }
"$include"      { return(CT_INCLUDE); }
"$nameof"       { return(CT_NAMEOF); }
"$offsetof"     { return(CT_OFFSETOF); }
"$qnameof"      { return(CT_QNAMEOF); }
"$sizeof"       { return(CT_SIZEOF); }
"$stringify"    { return(CT_STRINGIFY); }
"$switch"       { return(CT_SWITCH); }
"$typefrom"     { return(CT_TYPEFROM); }
"$typeof"       { return(CT_TYPEOF); }
"$vaarg"        { return(CT_VAARG); }
"$vaconst"      { return(CT_VACONST); }
"$vacount"      { return(CT_VACOUNT); }
"$vaexpr"       { return(CT_VAEXPR); }
"$varef"        { return(CT_VAREF); }
"$vasplat"      { return(CT_VASPLAT); }
"$vatype"       { return(CT_VATYPE); }
"/*"			{ BEGIN(COMMENT); }
<COMMENT>{
	"/*"        { yyg->yyextra_r.comment_level++; }
	"*"+"/"     { if (yyg->yyextra_r.comment_level) { yyg->yyextra_r.comment_level--; } else { BEGIN(INITIAL); } }
	"*"+        { }
	[^/*\n]+    { }
	[/]         { }
	\n          { }
}
"<*"			{ BEGIN(CONTRACT_DOC); }
<CONTRACT_DOC>{
	"*>"        { BEGIN(INITIAL); }
	"*"         { }
	[^*]+       { }
}
\/\/.*          { }
"any"           { return(ANY); }
"anyfault"      { return(ANYFAULT); }
"asm"           { return(ASM); }
"assert"        { return(ASSERT); }
"bitstruct"     { return(BITSTRUCT); }
"bool"          { return(BOOL); }
"break"			{ return(BREAK); }
"case"			{ return(CASE); }
"catch"         { return(CATCH); }
"char"			{ return(CHAR); }
"const"			{ return(CONST); }
"continue"		{ return(CONTINUE); }
"def"           { return(DEF); }
"default"		{ return(DEFAULT); }
"defer"         { return(DEFER); }
"distinct"      { return(DISTINCT); }
"do"			{ return(DO); }
"double"		{ return(DOUBLE); }
"else"			{ return(ELSE); }
"enum"			{ return(ENUM); }
"extern"        { return(EXTERN); }
"false"         { return(FALSE); }
"fault"		    { return(FAULT); }
"float"			{ return(FLOAT); }
"bfloat16"      { return(BFLOAT16); }
"float16"       { return(FLOAT16); }
"float128"      { return(FLOAT128); }
"fn"            { return(FN); }
"for"			{ return(FOR); }
"foreach"		{ return(FOREACH); }
"foreach_r"		{ return(FOREACH_R); }
"ichar"         { return(ICHAR); }
"if"			{ return(IF); }
"import"        { return(IMPORT); }
"inline"		{ return(INLINE); }
"int"			{ return(INT); }
"int128"		{ return(INT128); }
"interface"     { return(INTERFACE); }
"iptr"          { return(IPTR); }
"isz"           { return(ISZ); }
"long"			{ return(LONG); }
"macro"         { return(MACRO); }
"module"        { return(MODULE); }
"nextcase"      { return(NEXTCASE); }
"null"          { return(NUL); }
"return"		{ return(RETURN); }
"short"			{ return(SHORT); }
"struct"		{ return(STRUCT); }
"static"        { return(STATIC); }
"switch"		{ return(SWITCH); }
"tlocal"        { return(TLOCAL); }
"true"          { return(TRUE); }
"try"           { return(TRY); }
"typeid"        { return(TYPEID); }
"uint"			{ return(UINT); }
"uint128"		{ return(UINT128); }
"ulong"			{ return(ULONG); }
"union"			{ return(UNION); }
"uptr"          { return(UPTR); }
"ushort"		{ return(USHORT); }
"usz"           { return(USZ); }
"var"           { return(VAR); }
"void"			{ return(VOID); }
"while"			{ return(WHILE); }

@{CONST}        { return(AT_CONST_IDENT); }
#{CONST}        { return(HASH_CONST_IDENT); }
${CONST}        { return(CT_CONST_IDENT); }
{CONST}         { return(CONST_IDENT); }
@{TYPE}         { return(AT_TYPE_IDENT); }
#{TYPE}         { return(HASH_TYPE_IDENT); }
${TYPE}         { return(CT_TYPE_IDENT); }
{TYPE}          { return(TYPE_IDENT); }
@{IDENTIFIER}   { return(AT_IDENT); }
#{IDENTIFIER}   { return(HASH_IDENT); }
${IDENTIFIER}   { return(CT_IDENT); }
{IDENTIFIER}    { return(IDENT); }
0[xX]{HINT}{INTTYPE}?	{ return(INTEGER); }
0[oO]{OINT}{INTTYPE}?	{ return(INTEGER); }
0[bB]{BINT}{INTTYPE}?   { return(INTEGER); }
{INT}{INTTYPE}?		    { return(INTEGER); }
x\'{HEX}\' { return(BYTES); }
x\"{HEX}\" { return(BYTES); }
x\`{HEX}\` { return(BYTES); }
b64\'{B64}\' { return(BYTES); }
b64\"{B64}\" { return(BYTES); }
b64\`{B64}\` { return(BYTES); }

{INT}{REALTYPE} { return(REAL); }
{INT}{E}{REALTYPE}? { return(REAL); }
0[xX]{HINT}{P}{REALTYPE}?	{ return(REAL); }
{INT}"."{INT}{E}?{REALTYPE}?	{ return(REAL); }
0[xX]{HINT}"."{HINT}{P}{REALTYPE}? { return(REAL); }

\"(\\.|[^\\"])*\"	{ return(STRING_LITERAL); }
\'(\\[ux]{HEX}|\\.|[^\\']+)\'	{ return(CHAR_LITERAL); }

"`"             { BEGIN(RAW_STRING); }
<RAW_STRING>{
	"``"        { }
	"`"         { BEGIN(INITIAL); return(STRING_LITERAL); }
	"[^`]"+        { }
	<<EOF>>     { BEGIN(INITIAL); return(RAW_STRING); }
}

"!!"			{ return(BANGBANG); }
"..."			{ return(ELLIPSIS); }
".."			{ return(DOTDOT); }
"&&&"           { return(CT_AND_OP); }
"|||"           { return(CT_OR_OP); }
"+++"           { return(CT_CONCAT_OP); }
">>="			{ return(SHR_ASSIGN); }
"<<="			{ return(SHL_ASSIGN); }
"+="			{ return(ADD_ASSIGN); }
"-="			{ return(SUB_ASSIGN); }
"*="			{ return(MUL_ASSIGN); }
"/="			{ return(DIV_ASSIGN); }
"%="			{ return(MOD_ASSIGN); }
"&="			{ return(AND_ASSIGN); }
"^="			{ return(XOR_ASSIGN); }
"|="			{ return(OR_ASSIGN); }
">>"			{ return(SHR_OP); }
"<<"			{ return(SHL_OP); }
"++"			{ return(INC_OP); }
"--"			{ return(DEC_OP); }
"&&"			{ return(AND_OP); }
"||"			{ return(OR_OP); }
"<="			{ return(LE_OP); }
">="			{ return(GE_OP); }
"=="			{ return(EQ_OP); }
"!="			{ return(NE_OP); }
"??"            { return(OPTELSE); }
"::"            { return(SCOPE); }
"?:"            { return(ELVIS); }
"=>"            { return(IMPLIES); }
"[<"            { return(LVEC); }
">]"            { return(RVEC); }
"(<"            { return(LGENPAR); }
">)"            { return(RGENPAR); }
"$$"            { return(BUILTIN); }
";"			{ return(';'); }
("{")		{ return('{'); }
("}")		{ return('}'); }
","			{ return(','); }
":"			{ return(':'); }
"="			{ return('='); }
"("			{ return('('); }
")"			{ return(')'); }
("[")		{ return('['); }
("]")		{ return(']'); }
"."			{ return('.'); }
"&"			{ return('&'); }
"!"			{ return('!'); }
"~"			{ return('~'); }
"-"			{ return('-'); }
"+"			{ return('+'); }
"*"			{ return('*'); }
"/"			{ return('/'); }
"%"			{ return('%'); }
"<"			{ return('<'); }
">"			{ return('>'); }
"^"			{ return('^'); }
"|"			{ return('|'); }
"?"			{ return('?'); }
"{|"        { return(LBRAPIPE); }
"|}"        { return(RBRAPIPE); }
[ \t\v\n\f]		{ }
.			{ /* ignore bad characters */ }

%%

int yywrap(yyscan_t yyscanner)
{
	return 1;
}
