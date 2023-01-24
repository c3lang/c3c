module std::math::nolibc;


fn double floor(double x) @weak @extname("floor")
{
	const double TOINT = 1 / math::DOUBLE_EPSILON;
	ulong ui = bitcast(x, ulong);
	int e = (int)((ui >> 52) & 0x7ff);
	double y;
	if (e >= 0x3ff+52 || x == 0) return x;
	/* y = int(x) - x, where int(x) is an integer neighbor of x */
	y = ui >> 63 ? (x - TOINT) + TOINT - x : (x + TOINT) - TOINT - x;

	/* special case because of non-nearest rounding modes */
	if (e <= 0x3ff-1)
	{
		@volatile_load(y);
		return ui >> 63 ? -1 : 0;
	}
	return y > 0 ? x + y - 1 : x + y;
}