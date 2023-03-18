module std::math::nolibc;

$if (!env::COMPILER_LIBC_AVAILABLE):

fn double frexp(double x, int* e) @extern("frexp") @weak
{
	ulong i = bitcast(x, ulong);
	int ee = (int)((i >> 52) & 0x7ff);

	if (!ee)
	{
		if (x)
		{
			x = frexp(x * 0x1p64, e);
			*e -= 64;
		}
		else
		{
			*e = 0;
		}
		return x;
	}
	if (ee == 0x7ff) return x;

	*e = ee - 0x3fe;
	i &= 0x800fffffffffffffu64;
	i |= 0x3fe0000000000000u64;
	return bitcast(i, double);
}

fn float frexpf(float x, int* e) @extern("frexpf") @weak
{
	uint i = bitcast(x, uint);
	int ee = (i >> 23) & 0xff;

	if (!ee)
	{
		if (x)
		{
			x = frexpf(x * 0x1p64, e);
			*e -= 64;
		}
		else
		{
			*e = 0;
		}
		return x;
	}
	if (ee == 0xff) return x;

	*e = ee - 0x7e;
	i &= 0x807fffffu32;
	i |= 0x3f000000u32;
	return bitcast(i, float);
}

$endif;