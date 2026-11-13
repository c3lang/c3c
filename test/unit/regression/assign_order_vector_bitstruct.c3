module assign_order_vector_bitstruct;

bitstruct AobBits : uint
{
	int a : 0..7;
	int b : 8..15;
}

struct AobHolder
{
	int[<4>] v;
	AobBits bits;
}

char[8] aob_log;
int aob_len;

fn int aob_l(int v = 0)
{
	aob_log[aob_len++] = 'L';
	return v;
}

fn int aob_r(int v = 5)
{
	aob_log[aob_len++] = 'R';
	return v;
}

AobHolder aob_holder;
int[4] aob_dst;
int[4] aob_src = { 1, 2, 3, 4 };

fn int[] aob_dst_slice()
{
	aob_log[aob_len++] = 'L';
	return aob_dst[..];
}

fn int[] aob_src_slice()
{
	aob_log[aob_len++] = 'R';
	return aob_src[..];
}

fn AobHolder* aob_ptr()
{
	aob_log[aob_len++] = 'L';
	return &aob_holder;
}

fn String aob_take()
{
	String s = (String)aob_log[:aob_len];
	aob_len = 0;
	return s;
}

fn void plain_assign_rhs_first() @test
{
	int[3] arr;
	int[<3>] vec;
	AobBits[2] bits;
	int[<4>][2] vecs;
	arr[aob_l()] = aob_r();
	assert(aob_take() == "RL");
	arr[aob_l()..aob_l(1)] = aob_r(6);
	assert(aob_take() == "RLL");
	assert(arr[0] == 6 && arr[1] == 6);
	vec[aob_l()] = aob_r();
	assert(aob_take() == "RL");
	assert(vec[0] == 5);
	bits[aob_l()].a = aob_r();
	assert(aob_take() == "RL");
	assert(bits[0].a == 5);
	vecs[aob_l()].xy = { aob_r(), 1 };
	assert(aob_take() == "RL");
	assert(vecs[0].x == 5 && vecs[0].y == 1);
	aob_ptr().v[aob_l(1)] = aob_r(7);
	assert(aob_take() == "RLL");
	assert(aob_holder.v[1] == 7);
	aob_ptr().bits.b = aob_r(3);
	assert(aob_take() == "RL");
	assert(aob_holder.bits.b == 3);
	aob_ptr().v.zw = { aob_r(8), 9 };
	assert(aob_take() == "RL");
	assert(aob_holder.v.z == 8 && aob_holder.v.w == 9);
	aob_dst_slice()[..] = aob_src_slice()[..];
	assert(aob_take() == "RL");
	assert(aob_dst == aob_src);
}

fn void compound_assign_lhs_first() @test
{
	int[3] arr;
	int[<3>] vec = { 1, 1, 1 };
	AobBits[2] bits;
	int[<4>][2] vecs;
	arr[aob_l()] += aob_r();
	assert(aob_take() == "LR");
	vec[aob_l()] += aob_r();
	assert(aob_take() == "LR");
	assert(vec[0] == 6);
	bits[aob_l()].a += aob_r();
	assert(aob_take() == "LR");
	assert(bits[0].a == 5);
	vecs[aob_l()].xy += { aob_r(), 1 };
	assert(aob_take() == "LR");
	assert(vecs[0].x == 5 && vecs[0].y == 1);
}

fn void assign_result_value() @test
{
	int[<3>] vec;
	AobBits bits;
	int[<4>] v4;
	assert((vec[aob_l(2)] = aob_r(4)) == 4);
	assert(aob_take() == "RL");
	assert((bits.a = aob_r(-3)) == -3);
	assert(aob_take() == "R");
	int[<2>] res = (v4.yx = { aob_r(1), 2 });
	assert(aob_take() == "R");
	assert(res == { 1, 2 } && v4.y == 1 && v4.x == 2);
}
