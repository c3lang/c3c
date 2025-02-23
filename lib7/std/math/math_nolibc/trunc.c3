module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double _trunc(double x) @weak @extern("trunc") @nostrip
{
	ulong i = bitcast(x, ulong);
	int e = (int)((i >> 52) & 0x7ff) - 0x3ff + 12;
	if (e >= 52 + 12) return x;
	if (e < 12) e = 1;
	ulong m = ((ulong)-1) >> e;
	if (i & m == 0) return x;
	force_eval_add(x, 0x1p120f);
	i &= ~m;
	return bitcast(i, double);
}

fn float _truncf(float x) @weak @extern("truncf") @nostrip
{
	uint i = bitcast(x, uint);
	int e = (int)((i >> 23) & 0xff) - 0x7f + 9;
	if (e >= 23 + 9) return x;
	if (e < 9) e = 1;
	uint m = ((uint)-1) >> e;
	if (i & m == 0) return x;
	force_eval_add(x, 0x1p120f);
	i &= ~m;
	return bitcast(i, float);
}

