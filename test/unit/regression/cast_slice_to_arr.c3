module slice_to_arr @test;

fn void to_arr()
{
	int[] x = { 1, 2, 3, 4, 5 };
	int z = 2;
	int[2] y = x[z:2];
	assert(y == int[2]{ 3, 4 });
}

fn void to_vec()
{
	int[] x = { 1, 2, 3, 4, 5 };
	int z = 2;
	int[<2>] y = x[z:2];
	assert(y == { 3, 4 });
}