module main;

import std, mod;

fn int main(int argc, char** argv)
{
	io::printfn("%s", mod::afn{5}());
	return 0;
}

module mod<SIZE>;

fn int afn() @if(SIZE > 10) {
	return SIZE;
}

fn int afn() @feat(env::DARWIN && SIZE < 10) // #error:  One or more feature names were expected here, like '@feat(MY_FEATURE)'. Arbitrary expressions are only valid when using
{
	return SIZE / 2;
}