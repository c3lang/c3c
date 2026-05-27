module test_slice @test;

import std;

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


fn void slice2d_default_xlen_respects_offset() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);
	assert(s.xlen == 3);
	assert(s.ylen == 2);

	Slice2d{int} sub = s.slice(1);
	assert(sub.xlen == 2, "expected xlen=2 (source xlen 3 minus x=1), got %d", sub.xlen);
	assert(sub.ylen == 2);
	assert(sub.get_xy(0, 0) == 2);  // x[0][1]
	assert(sub.get_xy(1, 0) == 3);  // x[0][2]
	assert(sub.get_xy(0, 1) == 5);  // x[1][1]
	assert(sub.get_xy(1, 1) == 6);  // x[1][2]

	Slice2d{int} sub_y = s.slice(0, 0, 1);
	assert(sub_y.ylen == 1, "expected ylen=1 (source ylen 2 minus y=1), got %d", sub_y.ylen);
	assert(sub_y.xlen == 3);
	assert(sub_y.get_xy(0, 0) == 4);  // x[1][0]
	assert(sub_y.get_xy(2, 0) == 6);  // x[1][2]

	Slice2d{int} sub_neg = s.slice(1, -1);
	assert(sub_neg.xlen == 1, "expected xlen=1 (3 - 1 - 1), got %d", sub_neg.xlen);
	assert(sub_neg.get_xy(0, 0) == 2);  // x[0][1]

	Slice2d{int} full = s.slice();
	assert(full.xlen == 3);
	assert(full.ylen == 2);
}