

extern fn void foo(int x, ...);

fn void test(void*... y)
{
	foo(1, ...y); // #error: Only a value with a length known at compile time can be splatted
}