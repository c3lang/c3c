module generic_terminating_recursion;
import generic_terminating_recursion::a, generic_terminating_recursion::c;

fn void terminating_generic_recursion() @test
{
	Aa{int, int*} v = { 1, null };
	Yy{int, int*} swapped;
	Cc{int, 2} w = { 3 };
	Up{int, 2} up;
	Up{int*, 1} up2;
	assert(v.a == 1 && w.t == 3);
	assert($Typeof(swapped.a)::typeid == int*::typeid);
	assert($Typeof(up.t)::typeid == int*::typeid);
	assert($Typeof(up2.t)::typeid == int**::typeid);
}

module generic_terminating_recursion::a <Ta, Tb>;
struct Aa { Ta a; Tb b; }
alias Yy = Aa{Tb, Tb};

module generic_terminating_recursion::c <Tt, NN>;
struct Cc { Tt t; }
alias Up @if(NN > 0) = Cc{Tt*, NN - 1};
