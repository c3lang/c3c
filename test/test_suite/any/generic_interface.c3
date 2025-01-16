module foo(<Type>);

interface Baz
{}

module test;
import foo;

fn void main()
{
	Baz(<int>) x;
}
