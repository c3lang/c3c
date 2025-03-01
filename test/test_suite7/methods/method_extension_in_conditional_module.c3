module std::core::env;
const bool FOO = true;

module foo @if(env::FOO);
struct Foo { int a; }

module bar;
import std, foo;

fn void Foo.baz(self) {}

fn int main()
{
	return 0;
}
