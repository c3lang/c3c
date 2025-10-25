module std::math::math_rt;

const double TOINT = 1 / math::DOUBLE_EPSILON;
const float TOINTF = (float)(1 / math::FLOAT_EPSILON);
macro force_eval_add(x, v)
{
	$typeof(x) temp @noinit;
	@volatile_store(temp, x + v);
}

fn double __roundeven(double x) @cname("roundeven") @weak @nostrip
{
	ulong u = bitcast(x, ulong);
	int e = (int)((u >> 52) & 0x7ff);
	if (e >= 0x3ff + 52) return x;
	bool signed = u >> 63 != 0;
	if (signed) x = -x;
	if (e < 0x3ff - 1)
	{
		/* raise inexact if x!=0 */
		force_eval_add(x, TOINT);
		return 0 * x;
	}
	double y = (x + TOINT) - TOINT - x;
	switch
	{
		case y > 0.5:
			y = y + x - 1;
		case y < -0.5:
			y = y + x + 1;
		case y == 0.5 || y == -0.5:
            if (u & 1)
            {
                y = x + (y > 0 ? y + 1 : y - 1);
                break;
            }
            nextcase;
		default:
			y = y + x;
	}
	if (signed) y = -y;
	return y;
}

fn float __roundevenf(float x) @cname("roundevenf") @weak @nostrip
{
	uint u = bitcast(x, uint);
	int e = (u >> 23) & 0xff;
	if (e >= 0x7f + 23) return x;
	bool signed = u >> 31 != 0;
	if (signed) x = -x;
	if (e < 0x7f - 1)
	{
		force_eval_add(x, TOINTF);
		return 0 * x;
	}
	float y = (x + TOINTF) - TOINTF - x;
	switch
	{
		case y > 0.5f:
			y = y + x - 1;
		case y < -0.5f:
			y = y + x + 1;
		case y == 0.5f || y == -0.5f:
            if (u & 1)
            {
                y = x + (y > 0.0f ? y + 1.0f : y - 1.0f);
                break;
            }
            nextcase;
		default:
			y = y + x;
	}
	if (signed) y = -y;
	return y;
}

fn double __powidf2(double a, int b) @cname("__powidf2") @weak @nostrip
{
	bool recip = b < 0;
    double r = 1;
    while (1)
    {
        if (b & 1) r *= a;
        b /= 2;
        if (b == 0) break;
        a *= a;
	}
    return recip ? 1 / r : r;
}