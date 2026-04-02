module std::test;
macro foo()
{
	var h = $$str_hash(~""); // #error:  Cannot bit negate 'String'
}
fn int main()
{
	@in($$asin(foo()), 1, 2, );
}