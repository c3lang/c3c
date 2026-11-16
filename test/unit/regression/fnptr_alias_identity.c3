// Function pointer types written separately (aliases, other modules) with the same signature are one type.
module fnptr_alias_identity;
import fnptr_alias_identity::other;

alias Cmp = fn int(int, int);
alias Cmp2 = fn int(int, int);

fn int sub(int a, int b) => a - b;
fn int apply(Cmp f, int a, int b) => f(a, b);

fn void same_signature_aliases() @test
{
	Cmp2 h = &sub;
	assert(apply(h, 5, 2) == 3);
	assert(other::apply3(h, 7, 1) == 6);
	other::Op k = h;
	Cmp back = k;
	assert(back(9, 4) == 5);
	Cmp2[2] table = { &sub, k };
	assert(table[1](3, 1) == 2);
}

module fnptr_alias_identity::other;
alias Op = fn int(int, int);
fn int apply3(Op f, int a, int b) => f(a, b);
