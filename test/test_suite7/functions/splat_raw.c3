

extern fn void foo(int x, ...);

fn void test(void*... y)
{
	foo(1, ...y); // #error: raw varargs
}