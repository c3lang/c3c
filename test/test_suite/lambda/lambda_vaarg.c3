alias SegFaulter = fn void(char);
alias SegFaulter2 = fn void(...);

struct Foo
{
	SegFaulter func;
	SegFaulter2 func2;
}

fn void main()
{
	Foo sa = {};
	sa.func = fn (...) {}; // #error: doesn't match the required type
	sa.func2 = fn (...) {}; // #error: may not use C-style vaargs. Consequently
	var $f = fn (...) {}; // #error: may not use C-style
}