enum Foo
{
	ABC
}
typedef Abc = distinct int;
fn void main()
{
	Abc d = Foo.ABC; // #error: cannot implicitly be converted
}
