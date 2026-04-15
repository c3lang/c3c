struct Foo { int a; }
fn void a()
{
	typeid a = $typeof(Foo.a); // #error:  You cannot take the typeid of a compile time type
}