module std::math::nolibc::erf_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { ulong x; ulong expected; }
struct FloatCase { uint x; uint expected; }

const DoubleCase[*] ERF_CASES = {
	{ 0x0000000000000000, 0x0000000000000000 }, // 0x0p+0 -> 0x0p+0
	{ 0x8000000000000000, 0x8000000000000000 }, // -0x0p+0 -> -0x0p+0
	{ 0x3bc79ca10c924223, 0x3bcaa4a230244ae0 }, // 0x1.79ca10c924223p-67 -> 0x1.aa4a230244aep-67
	{ 0xbbc79ca10c924223, 0xbbcaa4a230244ae0 }, // -0x1.79ca10c924223p-67 -> -0x1.aa4a230244aep-67
	{ 0x0000000000000001, 0x0000000000000001 }, // 0x1p-1074 -> 0x1p-1074
	{ 0x3e2fffffffffffff, 0x3e320dd750429b6c }, // 0x1.fffffffffffffp-29 -> 0x1.20dd750429b6cp-28
	{ 0x3e30000000000000, 0x3e320dd750429b6d }, // 0x1p-28 -> 0x1.20dd750429b6dp-28
	{ 0x3fb999999999999a, 0x3fbcca5ea24fb334 }, // 0x1.999999999999ap-4 -> 0x1.cca5ea24fb334p-4
	{ 0x3fe0000000000000, 0x3fe0a7ef5c18edd2 }, // 0x1p-1 -> 0x1.0a7ef5c18edd2p-1
	{ 0xbfe0000000000000, 0xbfe0a7ef5c18edd2 }, // -0x1p-1 -> -0x1.0a7ef5c18edd2p-1
	{ 0x3feaffffffffffff, 0x3fe88d1cd474a2e0 }, // 0x1.affffffffffffp-1 -> 0x1.88d1cd474a2ep-1
	{ 0x3feb000000000000, 0x3fe88d1cd474a2e0 }, // 0x1.bp-1 -> 0x1.88d1cd474a2ep-1
	{ 0xbfeb000000000000, 0xbfe88d1cd474a2e0 }, // -0x1.bp-1 -> -0x1.88d1cd474a2ep-1
	{ 0x3ff0000000000000, 0x3feaf767a741088b }, // 0x1p+0 -> 0x1.af767a741088bp-1 macOS differs by 1 ulp
	{ 0xbff0000000000000, 0xbfeaf767a741088b }, // -0x1p+0 -> -0x1.af767a741088bp-1 macOS differs by 1 ulp
	{ 0x3ff3ffffffffffff, 0x3fed8865d98abe00 }, // 0x1.3ffffffffffffp+0 -> 0x1.d8865d98abep-1
	{ 0x3ff4000000000000, 0x3fed8865d98abe00 }, // 0x1.4p+0 -> 0x1.d8865d98abep-1
	{ 0xbff4000000000000, 0xbfed8865d98abe00 }, // -0x1.4p+0 -> -0x1.d8865d98abep-1
	{ 0x4000000000000000, 0x3fefd9ae142795e3 }, // 0x1p+1 -> 0x1.fd9ae142795e3p-1
	{ 0x4006db6cffffffff, 0x3fefff9031ef839b }, // 0x1.6db6cffffffffp+1 -> 0x1.fff9031ef839bp-1
	{ 0x4006db6d00000000, 0x3fefff9031ef839b }, // 0x1.6db6dp+1 -> 0x1.fff9031ef839bp-1
	{ 0x4008000000000000, 0x3fefffd1ac4135f9 }, // 0x1.8p+1 -> 0x1.fffd1ac4135f9p-1 macOS differs by 1 ulp
	{ 0xc008000000000000, 0xbfefffd1ac4135f9 }, // -0x1.8p+1 -> -0x1.fffd1ac4135f9p-1 macOS differs by 1 ulp
	{ 0x4017ffffffffffff, 0x3ff0000000000000 }, // 0x1.7ffffffffffffp+2 -> 0x1p+0
	{ 0x401799999999999a, 0x3fefffffffffffff }, // 0x1.799999999999ap+2 -> 0x1.fffffffffffffp-1 macOS differs by 1 ulp
	{ 0xc01799999999999a, 0xbfefffffffffffff }, // -0x1.799999999999ap+2 -> -0x1.fffffffffffffp-1 macOS differs by 1 ulp
	{ 0x4018000000000000, 0x3ff0000000000000 }, // 0x1.8p+2 -> 0x1p+0
	{ 0xc018000000000000, 0xbff0000000000000 }, // -0x1.8p+2 -> -0x1p+0
	{ 0x4024000000000000, 0x3ff0000000000000 }, // 0x1.4p+3 -> 0x1p+0
	{ 0x403be66666666666, 0x3ff0000000000000 }, // 0x1.be66666666666p+4 -> 0x1p+0
	{ 0x403c000000000000, 0x3ff0000000000000 }, // 0x1.cp+4 -> 0x1p+0
	{ 0x403e000000000000, 0x3ff0000000000000 }, // 0x1.ep+4 -> 0x1p+0
	{ 0xc03e000000000000, 0xbff0000000000000 }, // -0x1.ep+4 -> -0x1p+0
	{ 0x7e37e43c8800759c, 0x3ff0000000000000 }, // 0x1.7e43c8800759cp+996 -> 0x1p+0
	{ 0xfe37e43c8800759c, 0xbff0000000000000 }, // -0x1.7e43c8800759cp+996 -> -0x1p+0
	{ 0x7ff0000000000000, 0x3ff0000000000000 }, // inf -> 0x1p+0
	{ 0xfff0000000000000, 0xbff0000000000000 }, // -inf -> -0x1p+0
};

const FloatCase[*] ERFF_CASES = {
	{ 0x00000000, 0x00000000 }, // 0x0p+0 -> 0x0p+0
	{ 0x80000000, 0x80000000 }, // -0x0p+0 -> -0x0p+0
	{ 0x1e3ce508, 0x1e552511 }, // 0x1.79ca1p-67 -> 0x1.aa4a22p-67
	{ 0x9e3ce508, 0x9e552511 }, // -0x1.79ca1p-67 -> -0x1.aa4a22p-67
	{ 0x00000001, 0x00000001 }, // 0x1p-149 -> 0x1p-149
	{ 0x317fffff, 0x31906eba }, // 0x1.fffffep-29 -> 0x1.20dd74p-28
	{ 0x31800000, 0x31906eba }, // 0x1p-28 -> 0x1.20dd74p-28 macOS differs by 1 ulp
	{ 0x3dcccccd, 0x3de652f5 }, // 0x1.99999ap-4 -> 0x1.cca5eap-4
	{ 0x3f000000, 0x3f053f7b }, // 0x1p-1 -> 0x1.0a7ef6p-1
	{ 0xbf000000, 0xbf053f7b }, // -0x1p-1 -> -0x1.0a7ef6p-1
	{ 0x3f57ffff, 0x3f4468e6 }, // 0x1.affffep-1 -> 0x1.88d1ccp-1
	{ 0x3f580000, 0x3f4468e6 }, // 0x1.bp-1 -> 0x1.88d1ccp-1 macOS differs by 1 ulp
	{ 0xbf580000, 0xbf4468e6 }, // -0x1.bp-1 -> -0x1.88d1ccp-1 macOS differs by 1 ulp
	{ 0x3f800000, 0x3f57bb3d }, // 0x1p+0 -> 0x1.af767ap-1
	{ 0xbf800000, 0xbf57bb3d }, // -0x1p+0 -> -0x1.af767ap-1
	{ 0x3f9fffff, 0x3f6c432e }, // 0x1.3ffffep+0 -> 0x1.d8865cp-1
	{ 0x3fa00000, 0x3f6c432f }, // 0x1.4p+0 -> 0x1.d8865ep-1
	{ 0xbfa00000, 0xbf6c432f }, // -0x1.4p+0 -> -0x1.d8865ep-1
	{ 0x40000000, 0x3f7ecd71 }, // 0x1p+1 -> 0x1.fd9ae2p-1
	{ 0x4036db6c, 0x3f7ffc82 }, // 0x1.6db6d8p+1 -> 0x1.fff904p-1
	{ 0x4036db6d, 0x3f7ffc82 }, // 0x1.6db6dap+1 -> 0x1.fff904p-1
	{ 0x40400000, 0x3f7ffe8d }, // 0x1.8p+1 -> 0x1.fffd1ap-1
	{ 0xc0400000, 0xbf7ffe8d }, // -0x1.8p+1 -> -0x1.fffd1ap-1
	{ 0x40bfffff, 0x3f800000 }, // 0x1.7ffffep+2 -> 0x1p+0
	{ 0x40bccccd, 0x3f800000 }, // 0x1.79999ap+2 -> 0x1p+0
	{ 0xc0bccccd, 0xbf800000 }, // -0x1.79999ap+2 -> -0x1p+0
	{ 0x40c00000, 0x3f800000 }, // 0x1.8p+2 -> 0x1p+0
	{ 0xc0c00000, 0xbf800000 }, // -0x1.8p+2 -> -0x1p+0
	{ 0x41200000, 0x3f800000 }, // 0x1.4p+3 -> 0x1p+0
	{ 0x41df3333, 0x3f800000 }, // 0x1.be6666p+4 -> 0x1p+0
	{ 0x41e00000, 0x3f800000 }, // 0x1.cp+4 -> 0x1p+0
	{ 0x41f00000, 0x3f800000 }, // 0x1.ep+4 -> 0x1p+0
	{ 0xc1f00000, 0xbf800000 }, // -0x1.ep+4 -> -0x1p+0
	{ 0x7149f2ca, 0x3f800000 }, // 0x1.93e594p+99 -> 0x1p+0
	{ 0xf149f2ca, 0xbf800000 }, // -0x1.93e594p+99 -> -0x1p+0
	{ 0x7f800000, 0x3f800000 }, // inf -> 0x1p+0
	{ 0xff800000, 0xbf800000 }, // -inf -> -0x1p+0
};

fn void erf_matches_musl()
{
	foreach (c : ERF_CASES)
	{
		double x = bitcast(c.x, double);
		ulong got = bitcast(nolibc::_erf(@volatile_load(x)), ulong);
		assert(got == c.expected, "erf(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void erff_matches_musl()
{
	foreach (c : ERFF_CASES)
	{
		float x = bitcast(c.x, float);
		uint got = bitcast(nolibc::_erff(@volatile_load(x)), uint);
		assert(got == c.expected, "erff(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void erf_nan()
{
	double x = double::nan;
	float xf = float::nan;
	assert(math::is_nan(nolibc::_erf(@volatile_load(x))));
	assert(math::is_nan(nolibc::_erff(@volatile_load(xf))));
}

fn void erf_through_std_math()
{
	double x = 0.5;
	float xf = 0.5f;
	assert(bitcast(math::erf(@volatile_load(x)), ulong) == 0x3fe0a7ef5c18edd2);
	assert(bitcast(math::erf(@volatile_load(xf)), uint) == 0x3f053f7b);
}
