fn void test1()
{
	char i = 1;
	void* x = (void*)i; // #error: You cannot convert an integer smaller than pointer size to a pointer, try first widening it to pointer size, for example write '(void*)(uptr)some_value' instead.

}

fn void test2()
{
	char i = 1;
	void* x = i; // #error: You cannot implicitly convert an integer to a pointer, you may use an explicit cast if you first widen the value to pointer size, e.g. '(void*)(uptr)some_value'.

}