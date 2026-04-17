struct Foo
{
	int a;
}
fn void main()
{
    Foo::has_tag.get_tag; // #error: Type functions have neither methods nor fields.
}