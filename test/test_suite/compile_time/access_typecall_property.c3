struct Foo
{
	int a;
}
fn void main()
{
    Foo.has_tagof.tagof; // #error: Type functions have neither methods nor fields.
}