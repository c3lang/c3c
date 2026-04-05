
// typedefs and aliases
typedef MustInitTypedef @mustinit = String[];

typedef CanNoInitTypedef = MustInitTypedef;
CanNoInitTypedef b @noinit;

alias MyAliasedType = MustInitTypedef;
MyAliasedType c @noinit; // #error: 'MyAliasedType' (MustInitTypedef) requires initialization ('@mustinit'); remove '@noinit' or change the type.
alias DoubleAlias = MyAliasedType;
DoubleAlias _c @noinit; // #error: 'DoubleAlias' (MustInitTypedef) requires initialization ('@mustinit'); remove '@noinit' or change the type.

alias AliasedMustInit @mustinit = float; // #error: '@mustinit' is not a valid alias attribute.


// simple unions & bit/structs
struct TypeA
{
	int a;
}
TypeA e @noinit;
struct TypeB @mustinit
{
	int b;
}
TypeB f @noinit; // #error: 'TypeB' requires initialization ('@mustinit'); remove '@noinit' or change the type.

bitstruct BitA : char
{
	char a : 1..7;
}
BitA g @noinit;
bitstruct BitB : char @mustinit
{
	char b : 1..7;
}
BitB h @noinit; // #error: 'BitB' requires initialization ('@mustinit'); remove '@noinit' or change the type.

union UnionTrioA
{
	uint foo;
	long bar;
	double baz;
}
UnionTrioA i @noinit;
union UnionTrioB @mustinit
{
	uint foo;
	long bar;
	double baz;
}
UnionTrioB j @noinit; // #error: 'UnionTrioB' requires initialization ('@mustinit'); remove '@noinit' or change the type.


struct Container
{
	UnionTrioB a;
}
Container jk @noinit; // #error: 'Container' contains the type 'UnionTrioB' which requires initialization ('@mustinit')

typedef InVecA @mustinit = float;
typedef InVecB = float;
InVecA[<4>] k @noinit; // #error: InVecA[<4>]' contains the type 'InVecA' which requires initialization ('@mustinit');
InVecB[<4>] l @noinit;

