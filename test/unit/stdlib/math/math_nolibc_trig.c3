module std::math::nolibc::trig_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { ulong x; ulong expected; }
struct FloatCase { uint x; uint expected; }

const DoubleCase[*] TAN_CASES = {
	{ 0x3ff0000000000000, 0x3ff8eb245cbee3a6 }, // macOS differs by 1 ulp // 0x1p+0
	{ 0xbff0000000000000, 0xbff8eb245cbee3a6 }, // macOS differs by 1 ulp // -0x1p+0
	{ 0x4000000000000000, 0xc0017af62e0950f8 }, // 0x1p+1
	{ 0x3fe0000000000000, 0x3fe17b4f5bf3474a }, // 0x1p-1
	{ 0x3fe921fb54442d18, 0x3fefffffffffffff }, // 0x1.921fb54442d18p-1
	{ 0x3ff921fb54442d18, 0x434d02967c31cdb5 }, // 0x1.921fb54442d18p+0
	{ 0x4008000000000000, 0xbfc23ef71254b86f }, // 0x1.8p+1
	{ 0x4059000000000000, 0xbfe2ca74d62b5d38 }, // 0x1.9p+6
	{ 0xc0f86a0000000000, 0x3fa250a9d503313d }, // macOS differs by 1 ulp // -0x1.86ap+16
	{ 0x4480f0cf064dd592, 0xbffa0f79c1b6b258 }, // macOS differs by 1 ulp // 0x1.0f0cf064dd592p+73
	{ 0x7e70000000000000, 0xbfc4a41d560c08cc }, // 0x1p+1000
	{ 0x0000000000000001, 0x0000000000000001 }, // 0x1p-1074
	{ 0x8000000000000000, 0x8000000000000000 }, // -0x0p+0
	{ 0x7ff0000000000000, 0x7ff8000000000000 }, // inf
};

const FloatCase[*] TANF_CASES = {
	{ 0x3f800000, 0x3fc75923 }, // 0x1p+0
	{ 0xbf800000, 0xbfc75923 }, // -0x1p+0
	{ 0x40f00000, 0x402d2f55 }, // 0x1.ep+2
	{ 0xc0f00000, 0xc02d2f55 }, // -0x1.ep+2
	{ 0xc3ba0000, 0xc05fa2d6 }, // macOS differs by 1 ulp // -0x1.74p+8
	{ 0x44318000, 0x387cde82 }, // 0x1.63p+9
	{ 0x47c35000, 0xbd12854f }, // 0x1.86ap+16
	{ 0xcdc84588, 0xbf369103 }, // -0x1.908b1p+28
	{ 0x7149f2ca, 0x3fa5943b }, // 0x1.93e594p+99
	{ 0x00000001, 0x00000001 }, // 0x1p-149
	{ 0x80000000, 0x80000000 }, // -0x0p+0
	{ 0x7f800000, 0x7fc00000 }, // inf
};

const FloatCase[*] SINF_CASES = {
	{ 0xc3ba0000, 0xbf761eb1 }, // -0x1.74p+8
	{ 0xc0e66666, 0xbf4b2dcf }, // -0x1.ccccccp+2
	{ 0xc2c80000, 0x3f01a12e }, // -0x1.9p+6
	{ 0x432a8000, 0x3f410063 }, // 0x1.55p+7
	{ 0xc32a8000, 0xbf410063 }, // -0x1.55p+7
	{ 0x4e6e6b28, 0x3f0bbc65 }, // macOS differs by 1 ulp // 0x1.dcd65p+29
	{ 0xce6e6b28, 0xbf0bbc65 }, // macOS differs by 1 ulp // -0x1.dcd65p+29
	{ 0x59800000, 0x3f5fccb5 }, // macOS differs by 1 ulp // 0x1p+52
	{ 0x3f000000, 0x3ef57744 }, // 0x1p-1
	{ 0x000116c2, 0x000116c2 }, // 0x1.16c2p-133
	{ 0x80000000, 0x80000000 }, // -0x0p+0
};

const FloatCase[*] COSF_CASES = {
	{ 0xc3ba0000, 0x3e8cde69 }, // -0x1.74p+8
	{ 0xc0e66666, 0x3f1bbcec }, // -0x1.ccccccp+2
	{ 0xc2c80000, 0x3f5cc0ee }, // -0x1.9p+6
	{ 0x4e6e6b28, 0x3f567fc6 }, // 0x1.dcd65p+29
	{ 0xce6e6b28, 0x3f567fc6 }, // -0x1.dcd65p+29
	{ 0x59800000, 0xbef89807 }, // 0x1p+52
	{ 0x3f000000, 0x3f60a940 }, // 0x1p-1
	{ 0x000116c2, 0x3f800000 }, // 0x1.16c2p-133
	{ 0x80000000, 0x3f800000 }, // -0x0p+0
};

const FloatCase[*] ASINF_CASES = {
	{ 0x3f490fdb, 0x3f67413c }, // 0x1.921fb6p-1
	{ 0x3f19999a, 0x3f24bc7e }, // 0x1.333334p-1
	{ 0xbf666666, 0xbf8f549b }, // -0x1.ccccccp-1
	{ 0x3f7d70a4, 0x3fb6f1e4 }, // 0x1.fae148p-1
	{ 0x3f800000, 0x3fc90fdb }, // macOS differs by 1 ulp // 0x1p+0
	{ 0xbf800000, 0xbfc90fdb }, // macOS differs by 1 ulp // -0x1p+0
	{ 0x00000001, 0x00000001 }, // 0x1p-149
	{ 0x80000000, 0x80000000 }, // -0x0p+0
};

const DoubleCase[*] ATANH_CASES = {
	{ 0x3e2c9aa2a7ef3724, 0x3e2c9aa2a7ef3725 }, // macOS differs by 1 ulp // 0x1.c9aa2a7ef3724p-29
	{ 0x3e10000000000000, 0x3e10000000000000 }, // 0x1p-30
	{ 0xbe2666a58eff3ffd, 0xbe2666a58eff3ffe }, // macOS differs by 1 ulp // -0x1.666a58eff3ffdp-29
	{ 0x3de0000000000000, 0x3de0000000000000 }, // 0x1p-33
	{ 0x3fd3333333333333, 0x3fd3cf2b50617c96 }, // 0x1.3333333333333p-2
	{ 0x3fe8000000000000, 0x3fef2272ae325a57 }, // 0x1.8p-1
	{ 0xbfeff7ced916872b, 0xc00e66cfde9c7c2d }, // -0x1.ff7ced916872bp-1
	{ 0x3ff0000000000000, 0x7ff0000000000000 }, // 0x1p+0
	{ 0xbff0000000000000, 0xfff0000000000000 }, // -0x1p+0
	{ 0x0000000000000001, 0x0000000000000001 }, // 0x1p-1074
	{ 0x8000000000000000, 0x8000000000000000 }, // -0x0p+0
};

const FloatCase[*] ATANHF_CASES = {
	{ 0x3164d515, 0x3164d515 }, // 0x1.c9aa2ap-29
	{ 0x30800000, 0x30800000 }, // 0x1p-30
	{ 0xb02aaaab, 0xb02aaaab }, // -0x1.555556p-31
	{ 0x2f000000, 0x2f000000 }, // 0x1p-33
	{ 0x3e99999a, 0x3e9e795b }, // 0x1.333334p-2
	{ 0x3f400000, 0x3f791396 }, // macOS differs by 1 ulp // 0x1.8p-1
	{ 0xbf7fbe77, 0xc073369a }, // -0x1.ff7ceep-1
	{ 0x3f800000, 0x7f800000 }, // 0x1p+0
	{ 0xbf800000, 0xff800000 }, // -0x1p+0
	{ 0x00000001, 0x00000001 }, // 0x1p-149
	{ 0x80000000, 0x80000000 }, // -0x0p+0
};

fn void tan_matches_musl()
{
	foreach (c : TAN_CASES)
	{
		ulong x = c.x;
		ulong got = bitcast(nolibc::tan(bitcast(@volatile_load(x), double)), ulong);
		if (math::is_nan(bitcast(c.expected, double)) && math::is_nan(bitcast(got, double))) continue;
		assert(got == c.expected, "tan(%a) = %x, expected %x", bitcast(c.x, double), got, c.expected);
	}
}

fn void tanf_matches_musl()
{
	foreach (c : TANF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::tanf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "tanf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void sinf_matches_musl()
{
	foreach (c : SINF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_sinf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "sinf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void cosf_matches_musl()
{
	foreach (c : COSF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_cosf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "cosf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void asinf_matches_musl()
{
	foreach (c : ASINF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_asinf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "asinf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void atanh_matches_musl()
{
	foreach (c : ATANH_CASES)
	{
		ulong x = c.x;
		ulong got = bitcast(nolibc::_atanh(bitcast(@volatile_load(x), double)), ulong);
		if (math::is_nan(bitcast(c.expected, double)) && math::is_nan(bitcast(got, double))) continue;
		assert(got == c.expected, "atanh(%a) = %x, expected %x", bitcast(c.x, double), got, c.expected);
	}
}

fn void atanhf_matches_musl()
{
	foreach (c : ATANHF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_atanhf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "atanhf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void sincos_matches_sin_and_cos()
{
	foreach (double x : { 1.0, -1.0, 2.0, -372.0, 0x1p-1074, 1e22, 0x1p1000, -0.0 })
	{
		double s;
		double c;
		nolibc::sincos(x, &s, &c);
		assert(bitcast(s, ulong) == bitcast(nolibc::sin(x), ulong), "sincos(%a).sin", x);
		assert(bitcast(c, ulong) == bitcast(nolibc::_cos(x), ulong), "sincos(%a).cos", x);
		$if !env::WIN32:
			double s2;
			double c2;
			nolibc::_sincos_std(x, &s2, &c2);
			assert(bitcast(s2, ulong) == bitcast(s, ulong) && bitcast(c2, ulong) == bitcast(c, ulong), "sincos symbol (%a)", x);
		$endif
	}
	foreach (float x : { 1.0f, -1.0f, -7.2f, -372.0f, 1e9f, -1e9f, 0x1p52f, 0x1p-149f, -0.0f })
	{
		float s;
		float c;
		nolibc::sincosf(x, &s, &c);
		assert(bitcast(s, uint) == bitcast(nolibc::_sinf(x), uint), "sincosf(%a).sin", x);
		assert(bitcast(c, uint) == bitcast(nolibc::_cosf(x), uint), "sincosf(%a).cos", x);
		$if !env::WIN32:
			float s2;
			float c2;
			nolibc::_sincosf_std(x, &s2, &c2);
			assert(bitcast(s2, uint) == bitcast(s, uint) && bitcast(c2, uint) == bitcast(c, uint), "sincosf symbol (%a)", x);
		$endif
	}
}

fn void out_of_domain_is_nan()
{
	assert(math::is_nan(nolibc::_asin(1.5)));
	assert(math::is_nan(nolibc::_asinf(-1.5f)));
	assert(math::is_nan(nolibc::_acos(-2.0)));
	assert(math::is_nan(nolibc::_acosf(2.0f)));
	// |x| > 1 where the high word minus 0x3ff00000 overlaps the low word
	double just_above_one = 0x1.0000100000001p+0;
	assert(math::is_nan(nolibc::_asin(@volatile_load(just_above_one))));
	assert(math::is_nan(nolibc::_acos(@volatile_load(just_above_one))));
	assert(math::is_nan(nolibc::_asin(-0x1.0000000000001p+0)));
	assert(math::is_nan(nolibc::_acos(0x1.0000000000001p+0)));
	assert(math::is_nan(nolibc::_atanh(1.5)));
	assert(math::is_nan(nolibc::_atanhf(-1.5f)));
	assert(math::is_nan(nolibc::tan(double::nan)));
	assert(math::is_nan(nolibc::tanf(-float::inf)));
}
