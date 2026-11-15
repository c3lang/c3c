module test;

constset Lone : int { A = 1 }
constset Ltwo : inline Lone { B = 2 }
constset Lthree : inline Ltwo { C = 3 }
constset Other : int { D = 4 }

typedef Mone = int;
typedef Mtwo = inline Mone;
typedef Mthree = inline Mtwo;

// An inline constset converts to any constset it inlines, also when there is another one in between,
// the same as an inline distinct type does.
// Each check is in its own function so that one failure doesn't hide the others.
fn void to_intermediate_parent()
{
	Lthree x = Lthree.C;
	Ltwo y = x;
}

// These already work, and must keep working.
fn void to_direct_parent()
{
	Ltwo x = Ltwo.B;
	Lone y = x;
}

fn void to_root()
{
	Lthree x = Lthree.C;
	Lone y = x;
}

fn void distinct_chain_for_comparison()
{
	Mthree x = (Mthree)3;
	Mtwo y = x;
	Mone z = x;
}

// And the conversion only goes towards the parents.
fn void unrelated_constset()
{
	Lthree x = Lthree.C;
	Other y = x; // #error: Implicitly casting
}

fn void parent_to_child()
{
	Lone x = Lone.A;
	Ltwo y = x; // #error: Implicitly casting
}
