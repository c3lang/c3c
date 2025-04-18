module test;

struct Baz { int a; }

fn void main()
{
	Baz x = Baz { 1 }; // #error: is not a generic type
}