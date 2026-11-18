module const_half_float_rounding;
macro check(String name, double $v)
{
	double dv = $v;
	float16 rh = (float16)dv;
	bfloat rb = (bfloat)dv;
	double ch = (double)(float16)$v;
	double cb = (double)(bfloat)$v;
	bool ok = ((double)rh == ch || (ch != ch && (double)rh != (double)rh)) && ((double)rb == cb || (cb != cb && (double)rb != (double)rb));
	assert(ok, "%s: f16 const %g run %g, bf16 const %g run %g", name, ch, (double)rh, cb, (double)rb);
}
fn void const_matches_runtime() @test
{
	check("70000", 70000.0);
	check("65504", 65504.0);
	check("65519", 65519.0);
	check("65520", 65520.0);
	check("-70000", -70000.0);
	check("2049", 2049.0);
	check("2051", 2051.0);
	check("301", 301.0);
	check("0.1", 0.1);
	check("1e-5", 1e-5);
	check("6e-8", 6e-8);
	check("2.9e-8", 2.9e-8);
	check("3e-8", 3e-8);
	check("1e-40", 1e-40);
	check("3.4e38", 3.4e38);
	check("1e39", 1e39);
	check("-0.0", -0.0);
	$assert (float)(float16)70000.0 != 70000.0;
	$assert (float)(bfloat)301.0 == 300.0;
	$assert (int)(float16)2049.0 == 2048;
}
