module vector_pointer_to_integer_cast @test;

fn void vector_pointer_to_integer_cast()
{
	int[2] values = { 1, 2 };
	void*[<2>] pointers = { &values[0], &values[1] };
	uptr[<2>] addresses = (uptr[<2>])pointers;
	assert(addresses[0] == (uptr)&values[0]);
	assert(addresses[1] == (uptr)&values[1]);
}
