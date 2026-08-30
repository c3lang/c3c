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

fn void slice2d_reslice_negative_ylen_offset() @test
{
	int[3][3] grid = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
	Slice2d{int} s = array::slice2d(&grid);

	Slice2d{int} sub = s.slice(0, 0, 1, -1);
	assert(sub.ylen == 1, "expected ylen=1 (3 - 1 - 1), got %d", sub.ylen);
	assert(sub.xlen == 3);
	assert(sub.get_xy(0, 0) == 4);
	assert(sub.get_xy(2, 0) == 6);
}

fn void slice2d_array_slice2d_explicit_bounds() @test
{
	int[4][3] grid = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } };
	Slice2d{int} s = array::slice2d(&grid, 1, 2, 1, 2);
	assert(s.xlen == 2);
	assert(s.ylen == 2);
	assert(s.get_xy(0, 0) == 6);
	assert(s.get_xy(1, 0) == 7);
	assert(s.get_xy(0, 1) == 10);
	assert(s.get_xy(1, 1) == 11);
}

fn void slice2d_each_visits_all_coords_with_correct_values() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);

	int count = 0;
	s.@each(; sz[<2>] coord, int val)
	{
		assert(val == x[coord.y][coord.x]);
		count++;
	};
	assert(count == 6);
}

fn void slice2d_each_ref_allows_mutation() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);

	s.@each_ref(; sz[<2>] coord, int* val)
	{
		*val = *val * 10;
	};
	assert(x[0][0] == 10);
	assert(x[0][2] == 30);
	assert(x[1][2] == 60);
}

fn void slice2d_get_row_returns_correct_rows() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);

	assert(s.get_row(0) == { 1, 2, 3 });
	assert(s[0] == { 1, 2, 3 });
	assert(s.get_row(1) == { 4, 5, 6 });
	assert(s[1] == { 4, 5, 6 });
}

fn void slice2d_get_xy_ref_and_get_coord_ref_write_through() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);

	int* p = s.get_xy_ref(2, 1);
	*p = 999;
	assert(x[1][2] == 999);

	int* p2 = s.get_coord_ref({ 0, 0 });
	*p2 = -1;
	assert(x[0][0] == -1);
}

fn void slice2d_set_through_reslice_writes_back_to_original() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);
	Slice2d{int} sub = s.slice(1, 2, 1, 1);

	sub.set_xy(0, 0, 42);
	assert(x[1][1] == 42);
	sub.set_coord({ 1, 0 }, 43);
	assert(x[1][2] == 43);
}

fn void slice2d_double_reslice_composes_offsets() @test
{
	int[4][4] grid = {
		{ 0, 1, 2, 3 },
		{ 4, 5, 6, 7 },
		{ 8, 9, 10, 11 },
		{ 12, 13, 14, 15 },
	};
	Slice2d{int} s = array::slice2d(&grid);
	Slice2d{int} once = s.slice(1, 3, 1, 3);
	assert(once.get_xy(0, 0) == 5);

	Slice2d{int} twice = once.slice(1, 2, 1, 2);
	assert(twice.xlen == 2);
	assert(twice.ylen == 2);
	assert(twice.get_xy(0, 0) == 10);
	assert(twice.get_xy(1, 0) == 11);
	assert(twice.get_xy(0, 1) == 14);
	assert(twice.get_xy(1, 1) == 15);
}

fn void slice2d_full_reslice_matches_original_content() @test
{
	int[3][2] x = { { 1, 2, 3 }, { 4, 5, 6 } };
	Slice2d{int} s = array::slice2d(&x);
	Slice2d{int} full = s.slice();

	assert(full.xlen == s.xlen);
	assert(full.ylen == s.ylen);
	for (int yy = 0; yy < s.ylen; yy++)
	{
		for (int xx = 0; xx < s.xlen; xx++)
		{
			assert(full.get_xy(xx, yy) == s.get_xy(xx, yy));
		}
	}
}

struct Slice2dTestPoint
{
	int x;
	int y;
}

fn void slice2d_works_with_struct_type() @test
{
	Slice2dTestPoint[2][2] grid = {
		{ { 1, 1 }, { 2, 2 } },
		{ { 3, 3 }, { 4, 4 } },
	};
	Slice2d{Slice2dTestPoint} s = array::slice2d(&grid);

	assert(s.get_xy(1, 1).x == 4);
	s.set_xy(0, 1, { 99, 99 });
	assert(grid[1][0].x == 99);
}