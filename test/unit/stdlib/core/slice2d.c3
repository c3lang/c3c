module test_slice @test;

fn void slice2d()
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 }};
	Slice2d {int} s = array::slice2d(&x);
	assert(s.len() == 2);
	assert(s.count() == 6);
	assert(s.get_xy(1, 1) == 5);
	assert(s.get_coord({1, 1}) == 5);
	s.set_coord({ 0, 1 }, 100);
	assert(x[1][0] == 100);
	s.set_xy(0, 1, 101);
	assert(x[1][0] == 101);
	Slice2d {int} s2 = s.slice(1, 2, 0, 2);
	assert(s2[0] == { 2, 3 });
	assert(s2.count() == 4);

}