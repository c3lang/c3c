module test;

macro void something(int a = ...)
{
	int a = 3; // #error: 'a' would shadow a previous declaration
}

fn void test1()
{
	something();
}
fn void test2()
{
	something(3);
}
