module foreach_r_enum_index;
import std::collections::enummap;

enum FrColor { RED, GREEN, BLUE }

struct FrByColor
{
	int[3] values;
}

macro int FrByColor.get(self, FrColor c) @operator([]) => self.values[c.ordinal];
macro usz FrByColor.len(self) @operator(len) => 3;

fn void foreach_r_over_enummap() @test
{
	EnumMap{FrColor, int} m;
	m[FrColor.RED] = 1;
	m[FrColor.GREEN] = 2;
	m[FrColor.BLUE] = 3;
	FrColor[3] keys;
	int[3] values;
	int n = 0;
	foreach_r (c, v : m)
	{
		keys[n] = c;
		values[n] = v;
		n++;
	}
	assert(n == 3);
	assert(keys == { FrColor.BLUE, FrColor.GREEN, FrColor.RED });
	assert(values == { 3, 2, 1 });
}

fn void foreach_r_over_enum_indexed_operator() @test
{
	FrByColor b = { { 10, 20, 30 } };
	FrColor[3] keys;
	int[3] values;
	int n = 0;
	foreach_r (c, v : b)
	{
		keys[n] = c;
		values[n] = v;
		n++;
	}
	assert(n == 3);
	assert(keys == { FrColor.BLUE, FrColor.GREEN, FrColor.RED });
	assert(values == { 30, 20, 10 });
}
