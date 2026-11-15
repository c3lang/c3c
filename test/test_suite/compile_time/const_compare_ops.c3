faultdef FOO, BAR;
int x;
int y;
bitstruct Bs : uint
{
	uint a : 0..7;
	uint b : 8..15;
}
bitstruct Bb : char { bool x; bool y; }
const Bs B1 = { .a = 1 };
const Bs B2 = { .a = 1, .b = 2 };
const Bs B3 = { .b = 2, .a = 1 };
const Bb C1 = { .x = true };
const Bb C2 = { .x = true, .y = true };

fn void main()
{
	$assert(!(B1 == B2));
	$assert(!(B2 == B1));
	$assert(B1 != B2);
	$assert(B2 == B3);
	$assert(!(B2 != B3));
	$assert(!(C1 == C2));
	$assert(C2 != C1);
	$assert(!(FOO != FOO));
	$assert(FOO == FOO);
	$assert(FOO != BAR);
	$assert(!(FOO == BAR));
	$assert(&x == &x);
	$assert(!(&x != &x));
	$assert(&x != &y);
	$assert(!(&x == &y));
}
