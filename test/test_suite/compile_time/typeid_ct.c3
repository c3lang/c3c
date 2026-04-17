struct Foo { int a; }
fn void main()
{
	typeid t = $typeof(Foo.a)::typeid; // #error:  Cannot get typeid of 'member_ref'
}

fn void main2()
{
	$typeof(Foo.a)::name; // #error: 'name' is not a valid property for 'member_ref'
}