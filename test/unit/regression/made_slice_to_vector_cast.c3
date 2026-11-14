module made_slice_to_vector_cast @test;

fn void made_slice_to_vector_cast()
{
	int[4] values = { 1, 2, 3, 4 };
	int* pointer = &values[0];
	int[<4>] vector = (int[<4>])(int[])(int[4]*)pointer;
	assert(vector == { 1, 2, 3, 4 });
}
