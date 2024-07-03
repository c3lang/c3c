module struct_alignment @test;

struct Test @align(16) { void* foo; }

struct Test2
{
	Test test;
	uint a;
}

fn void nested_struct()
{
	Test2* array;
	assert((uptr)&array[1] - (uptr)array == 32);
	assert((uptr)&array[1] - (uptr)array == Test2.sizeof);
	assert(Test2.sizeof == 32);
	assert(Test.sizeof == 16);
}