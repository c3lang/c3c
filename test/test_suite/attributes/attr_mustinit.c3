
// typedefs and aliases
typedef MustInitTypedef @mustinit = String[];
MustInitTypedef a @noinit; // #error: `@noinit` cannot be applied to

typedef CanNoInitTypedef = MustInitTypedef;
CanNoInitTypedef b @noinit;

alias MyAliasedType = MustInitTypedef;
MyAliasedType c @noinit; // #error: `@noinit` cannot be applied to
alias DoubleAlias = MyAliasedType;
DoubleAlias _c @noinit; // #error: `@noinit` cannot be applied to

alias AliasedMustInit @mustinit = float;
AliasedMustInit d @noinit; // #error: `@noinit` cannot be applied to

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
TypeB f @noinit; // #error: `@noinit` cannot be applied to

bitstruct BitA : char
{
	char a : 1..7;
}
BitA g @noinit;
bitstruct BitB : char @mustinit
{
	char b : 1..7;
}
BitB h @noinit; // #error: `@noinit` cannot be applied to

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
UnionTrioB j @noinit; // #error: `@noinit` cannot be applied to

// vectors
alias InVecA @mustinit = float;
alias InVecB = float;
InVecA[<4>] k @noinit; // #error: `@noinit` cannot be applied to
InVecB[<4>] l @noinit;

