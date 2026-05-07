struct Foo { int a; }
fn void main()
{
	typeid t = $Typeof(Foo.a)::typeid; // #error:  Cannot get typeid of 'member_ref'
}

fn void main2()
{
	$Typeof(Foo.a)::name; // #error: 'name' is not a valid property for 'member_ref'
}