module std::test;
fn void test()
{
	bool a = $$atan($defined(void{ } ).$name.@short_name); // #error: A user-defined generic type was expected here, but the type was 'void'
}