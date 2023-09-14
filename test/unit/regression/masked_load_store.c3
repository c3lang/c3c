import std;
fn void masked_load() @test
{
	long[<4>] val = { 1, 3, 5, 8 };
	long[<4>] res = mem::masked_load(&val, { true, false, true, false }, long[<4>] { 100, 200, 300, 400 });
	assert(res == { 1, 200, 5, 400 });
}

fn void masked_load_aligned() @test
{
	long[<4>] val = { 1, 3, 5, 8 };
	long[<4>] res = mem::@masked_load_aligned(&val, { true, false, true, false }, long[<4>] { 100, 200, 300, 400 }, 8);
	assert(res == { 1, 200, 5, 400 });
}

fn void masked_store() @test
{
	long[<4>] val = { 1, 3, 5, 8 };
	mem::masked_store(&val, long[<4>] { 11, 22, 33, 44 }, { true, false, false, true });
	assert(val == { 11, 3, 5, 44 });
}

fn void masked_store_aligned() @test
{
	long[<4>] val = { 1, 3, 5, 8 };
	mem::@masked_store_aligned(&val, long[<4>] { 11, 22, 33, 44 }, { true, false, false, true }, 8);
	assert(val == { 11, 3, 5, 44 });
}
