module std::math::nolibc::gamma_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { ulong x; ulong expected; }
struct FloatCase { uint x; uint expected; }
struct SignCase { double x; int sign; }

const DoubleCase[*] LGAMMA_CASES = {
	{ 0x3ff0000000000000, 0x0000000000000000 }, // 0x1p+0 -> 0x0p+0
	{ 0x4000000000000000, 0x0000000000000000 }, // 0x1p+1 -> 0x0p+0
	{ 0x3ff0000000000001, 0xbca2788cfc6fb618 }, // 0x1.0000000000001p+0 -> -0x1.2788cfc6fb618p-53 macOS differs by 1 ulp
	{ 0x3fefffffffffffff, 0x3c92788cfc6fb61a }, // 0x1.fffffffffffffp-1 -> 0x1.2788cfc6fb61ap-54
	{ 0x4000000000000001, 0x3cab0ee6072093d1 }, // 0x1.0000000000001p+1 -> 0x1.b0ee6072093d1p-53
	{ 0x3fffffffffffffff, 0xbc9b0ee6072093cd }, // 0x1.fffffffffffffp+0 -> -0x1.b0ee6072093cdp-54
	{ 0x3fe0000000000000, 0x3fe250d048e7a1bd }, // 0x1p-1 -> 0x1.250d048e7a1bdp-1 macOS differs by 1 ulp
	{ 0x3fd3333333333333, 0x3ff188637a6c4196 }, // 0x1.3333333333333p-2 -> 0x1.188637a6c4196p+0
	{ 0x3fe6666666666666, 0x3fd0b20c891cde74 }, // 0x1.6666666666666p-1 -> 0x1.0b20c891cde74p-2 macOS differs by 2 ulp
	{ 0x3feccccccccccccd, 0x3fb0fe0882e465b2 }, // 0x1.ccccccccccccdp-1 -> 0x1.0fe0882e465b2p-4 macOS differs by 1 ulp
	{ 0x3ff3333333333333, 0xbfb5db138c7d70c6 }, // 0x1.3333333333333p+0 -> -0x1.5db138c7d70c6p-4 macOS differs by 1 ulp
	{ 0x3ff762d86356be3f, 0xbfbf19b9bcc38a42 }, // 0x1.762d86356be3fp+0 -> -0x1.f19b9bcc38a42p-4 macOS differs by 1 ulp
	{ 0x3ff8000000000000, 0xbfbeeb95b094c191 }, // 0x1.8p+0 -> -0x1.eeb95b094c191p-4 macOS differs by 3 ulp
	{ 0x3ffccccccccccccd, 0xbfb2328d7d70b422 }, // 0x1.ccccccccccccdp+0 -> -0x1.2328d7d70b422p-4
	{ 0x4008000000000000, 0x3fe62e42fefa39ef }, // 0x1.8p+1 -> 0x1.62e42fefa39efp-1 macOS differs by 1 ulp
	{ 0x4012000000000000, 0x4003a140a3a623cb }, // 0x1.2p+2 -> 0x1.3a140a3a623cbp+1
	{ 0x401f99999999999a, 0x4020a6062d049d21 }, // 0x1.f99999999999ap+2 -> 0x1.0a6062d049d21p+3
	{ 0x4020000000000000, 0x40210ce1f32dcc2f }, // 0x1p+3 -> 0x1.10ce1f32dcc2fp+3
	{ 0x4024000000000000, 0x40299a8921a7f7d0 }, // 0x1.4p+3 -> 0x1.99a8921a7f7dp+3
	{ 0x4059000000000000, 0x40767225b4879463 }, // 0x1.9p+6 -> 0x1.67225b4879463p+8
	{ 0x4202a05f20000000, 0x4249a43710f467c1 }, // 0x1.2a05f2p+33 -> 0x1.9a43710f467c1p+37
	{ 0x4390000000000000, 0x43e399ecb712c481 }, // 0x1p+58 -> 0x1.399ecb712c481p+63
	{ 0x7e37e43c8800759c, 0x7ed017f38e7a1ab5 }, // 0x1.7e43c8800759cp+996 -> 0x1.017f38e7a1ab5p+1006
	{ 0x7fefffffffffffff, 0x7ff0000000000000 }, // 0x1.fffffffffffffp+1023 -> inf
	{ 0x3b80000000000000, 0x40489b5252dd9845 }, // 0x1p-71 -> 0x1.89b5252dd9845p+5
	{ 0x0010000000000000, 0x4086232bdd7abcd2 }, // 0x1p-1022 -> 0x1.6232bdd7abcd2p+9
	{ 0x0000000000000001, 0x40874385446d71c3 }, // 0x1p-1074 -> 0x1.74385446d71c3p+9
	{ 0x8000000000000001, 0x40874385446d71c3 }, // -0x1p-1074 -> 0x1.74385446d71c3p+9
	{ 0x3ddb7cdfd9d7bdbb, 0x4037069e2aa26ae4 }, // 0x1.b7cdfd9d7bdbbp-34 -> 0x1.7069e2aa26ae4p+4
	{ 0xbfe0000000000000, 0x3ff43f89a3f0edd6 }, // -0x1p-1 -> 0x1.43f89a3f0edd6p+0
	{ 0xbff8000000000000, 0x3feb858151820f85 }, // -0x1.8p+0 -> 0x1.b858151820f85p-1 macOS differs by 1 ulp
	{ 0xc004000000000000, 0xbfaccbf9f5ed0f20 }, // -0x1.4p+1 -> -0x1.ccbf9f5ed0f2p-5
	{ 0xbfb999999999999a, 0x4002f3a1ffcc7845 }, // -0x1.999999999999ap-4 -> 0x1.2f3a1ffcc7845p+1
	{ 0xc059200000000000, 0xc076ce6a5dbefb91 }, // -0x1.92p+6 -> -0x1.6ce6a5dbefb91p+8
	{ 0xbfeffffffff24190, 0x4037069e293f7ad8 }, // -0x1.ffffffff2419p-1 -> 0x1.7069e293f7ad8p+4 macOS differs by 175878315 ulp
	{ 0xc008000000225c18, 0x4032ee7732209a9c }, // -0x1.8000000225c18p+1 -> 0x1.2ee7732209a9cp+4
	{ 0xc202a05f20040000, 0xc249a43711051a32 }, // -0x1.2a05f2004p+33 -> -0x1.9a43711051a32p+37
	{ 0x0000000000000000, 0x7ff0000000000000 }, // 0x0p+0 -> inf
	{ 0x8000000000000000, 0x7ff0000000000000 }, // -0x0p+0 -> inf
	{ 0xbff0000000000000, 0x7ff0000000000000 }, // -0x1p+0 -> inf
	{ 0xc000000000000000, 0x7ff0000000000000 }, // -0x1p+1 -> inf
	{ 0xc059000000000000, 0x7ff0000000000000 }, // -0x1.9p+6 -> inf
	{ 0xc3b0000000000000, 0x7ff0000000000000 }, // -0x1p+60 -> inf
	{ 0x7ff0000000000000, 0x7ff0000000000000 }, // inf -> inf
	{ 0xfff0000000000000, 0x7ff0000000000000 }, // -inf -> inf
};

const FloatCase[*] LGAMMAF_CASES = {
	{ 0x3f800000, 0x00000000 }, // 0x1p+0 -> 0x0p+0
	{ 0x40000000, 0x00000000 }, // 0x1p+1 -> 0x0p+0
	{ 0x3f800001, 0xb393c466 }, // 0x1.000002p+0 -> -0x1.2788ccp-24
	{ 0x3f7fffff, 0x3313c469 }, // 0x1.fffffep-1 -> 0x1.2788d2p-25
	{ 0x40000001, 0x33d87733 }, // 0x1.000002p+1 -> 0x1.b0ee66p-24
	{ 0x3fffffff, 0xb358772f }, // 0x1.fffffep+0 -> -0x1.b0ee5ep-25
	{ 0x3f000000, 0x3f128682 }, // 0x1p-1 -> 0x1.250d04p-1
	{ 0x3e99999a, 0x3f8c431c }, // 0x1.333334p-2 -> 0x1.188638p+0 macOS differs by 1 ulp
	{ 0x3f333333, 0x3e859065 }, // 0x1.666666p-1 -> 0x1.0b20cap-2
	{ 0x3f666666, 0x3d87f047 }, // 0x1.ccccccp-1 -> 0x1.0fe08ep-4
	{ 0x3f99999a, 0xbdaed89e }, // 0x1.333334p+0 -> -0x1.5db13cp-4
	{ 0x3fbb16c3, 0xbdf8cdce }, // 0x1.762d86p+0 -> -0x1.f19b9cp-4
	{ 0x3fc00000, 0xbdf75cad }, // 0x1.8p+0 -> -0x1.eeb95ap-4 macOS differs by 1 ulp
	{ 0x3fe66666, 0xbd91946e }, // 0x1.ccccccp+0 -> -0x1.2328dcp-4
	{ 0x40400000, 0x3f317218 }, // 0x1.8p+1 -> 0x1.62e43p-1
	{ 0x40900000, 0x401d0a06 }, // 0x1.2p+2 -> 0x1.3a140cp+1 macOS differs by 1 ulp
	{ 0x40fccccd, 0x41053032 }, // 0x1.f9999ap+2 -> 0x1.0a6064p+3
	{ 0x41000000, 0x41086710 }, // 0x1p+3 -> 0x1.10ce2p+3
	{ 0x41200000, 0x414cd449 }, // 0x1.4p+3 -> 0x1.99a892p+3
	{ 0x42c80000, 0x43b3912e }, // 0x1.9p+6 -> 0x1.67225cp+8
	{ 0x501502f9, 0x524d21b8 }, // 0x1.2a05f2p+33 -> 0x1.9a437p+37 macOS differs by 1 ulp
	{ 0x5c800000, 0x5f1ccf66 }, // 0x1p+58 -> 0x1.399eccp+63
	{ 0x7149f2ca, 0x7456d09c }, // 0x1.93e594p+99 -> 0x1.ada138p+105
	{ 0x7f7fffff, 0x7f800000 }, // 0x1.fffffep+127 -> inf
	{ 0x34800000, 0x4173fce1 }, // 0x1p-22 -> 0x1.e7f9c2p+3
	{ 0x00800000, 0x42aeac50 }, // 0x1p-126 -> 0x1.5d58ap+6
	{ 0x00000001, 0x42ce8ed0 }, // 0x1p-149 -> 0x1.9d1dap+6
	{ 0x80000001, 0x42ce8ed0 }, // -0x1p-149 -> 0x1.9d1dap+6
	{ 0x3727c5ac, 0x413834eb }, // 0x1.4f8b58p-17 -> 0x1.7069d6p+3
	{ 0xbf000000, 0x3fa1fc4d }, // -0x1p-1 -> 0x1.43f89ap+0
	{ 0xbfc00000, 0x3f5c2c0b }, // -0x1.8p+0 -> 0x1.b85816p-1
	{ 0xc0200000, 0xbd665fc8 }, // -0x1.4p+1 -> -0x1.ccbf9p-5 macOS differs by 8 ulp
	{ 0xbdcccccd, 0x40179d10 }, // -0x1.99999ap-4 -> 0x1.2f3a2p+1
	{ 0xc2c90000, 0xc3b67352 }, // -0x1.92p+6 -> -0x1.6ce6a4p+8 macOS differs by 1 ulp
	{ 0xbf7fbe77, 0x40dd0fea }, // -0x1.ff7ceep-1 -> 0x1.ba1fd4p+2 macOS differs by 1 ulp
	{ 0xc0401062, 0x40a3ac8e }, // -0x1.8020c4p+1 -> 0x1.47591cp+2
	{ 0xc9742408, 0xcb438ca4 }, // -0x1.e8481p+19 -> -0x1.871948p+23
	{ 0x00000000, 0x7f800000 }, // 0x0p+0 -> inf
	{ 0x80000000, 0x7f800000 }, // -0x0p+0 -> inf
	{ 0xbf800000, 0x7f800000 }, // -0x1p+0 -> inf
	{ 0xc0000000, 0x7f800000 }, // -0x1p+1 -> inf
	{ 0xc2c80000, 0x7f800000 }, // -0x1.9p+6 -> inf
	{ 0xce800000, 0x7f800000 }, // -0x1p+30 -> inf
	{ 0x7f800000, 0x7f800000 }, // inf -> inf
	{ 0xff800000, 0x7f800000 }, // -inf -> inf
};

const DoubleCase[*] TGAMMA_CASES = {
	{ 0x3ff0000000000000, 0x3ff0000000000000 }, // 0x1p+0 -> 0x1p+0
	{ 0x4000000000000000, 0x3ff0000000000000 }, // 0x1p+1 -> 0x1p+0
	{ 0x4008000000000000, 0x4000000000000000 }, // 0x1.8p+1 -> 0x1p+1
	{ 0x4010000000000000, 0x4018000000000000 }, // 0x1p+2 -> 0x1.8p+2
	{ 0x4014000000000000, 0x4038000000000000 }, // 0x1.4p+2 -> 0x1.8p+4
	{ 0x4018000000000000, 0x405e000000000000 }, // 0x1.8p+2 -> 0x1.ep+6
	{ 0x4024000000000000, 0x4116260000000000 }, // 0x1.4p+3 -> 0x1.626p+18
	{ 0x4034000000000000, 0x437b02b930689000 }, // 0x1.4p+4 -> 0x1.b02b930689p+56
	{ 0x4037000000000000, 0x444e77526159f06c }, // 0x1.7p+4 -> 0x1.e77526159f06cp+69
	{ 0x4038000000000000, 0x4495e5c335f8a4cc }, // 0x1.8p+4 -> 0x1.5e5c335f8a4ccp+74 macOS differs by 2 ulp
	{ 0x3fe0000000000000, 0x3ffc5bf891b4ef6a }, // 0x1p-1 -> 0x1.c5bf891b4ef6ap+0 macOS differs by 1 ulp
	{ 0x3ff8000000000000, 0x3fec5bf891b4ef6d }, // 0x1.8p+0 -> 0x1.c5bf891b4ef6dp-1 macOS differs by 2 ulp
	{ 0x4004000000000000, 0x3ff544fa6d47b390 }, // 0x1.4p+1 -> 0x1.544fa6d47b39p+0
	{ 0x4024333333333333, 0x411bc1a30179e72c }, // 0x1.4333333333333p+3 -> 0x1.bc1a30179e72cp+18
	{ 0x403e800000000000, 0x468305adf049c818 }, // 0x1.e8p+4 -> 0x1.305adf049c818p+105
	{ 0x3ddb7cdfd9d7bdbb, 0x4202a05f1ffb61dc }, // 0x1.b7cdfd9d7bdbbp-34 -> 0x1.2a05f1ffb61dcp+33 macOS differs by 1 ulp
	{ 0x3c80000000000000, 0x4360000000000000 }, // 0x1p-55 -> 0x1p+55
	{ 0x0000000000000001, 0x7ff0000000000000 }, // 0x1p-1074 -> inf
	{ 0xbfe0000000000000, 0xc00c5bf891b4ef6b }, // -0x1p-1 -> -0x1.c5bf891b4ef6bp+1 macOS differs by 1 ulp
	{ 0xbff8000000000000, 0x4002e7fb0bcdf4f0 }, // -0x1.8p+0 -> 0x1.2e7fb0bcdf4fp+1 macOS differs by 2 ulp
	{ 0xc004000000000000, 0xbfee3ff812e32182 }, // -0x1.4p+1 -> -0x1.e3ff812e32182p-1
	{ 0xbfb999999999999a, 0xc0255f61032f6359 }, // -0x1.999999999999ap-4 -> -0x1.55f61032f6359p+3 macOS differs by 1 ulp
	{ 0xc025000000000000, 0xbe91b7b1db2e47f2 }, // -0x1.5p+3 -> -0x1.1b7b1db2e47f2p-22 macOS differs by 1 ulp
	{ 0xc065500000000000, 0x8017d2374dfcda78 }, // -0x1.55p+7 -> -0x1.7d2374dfcda78p-1022 macOS differs by 293 ulp
	{ 0xc066f00000000000, 0x0000000000000000 }, // -0x1.6fp+7 -> 0x0p+0
	{ 0xc067100000000000, 0x8000000000000000 }, // -0x1.71p+7 -> -0x0p+0
	{ 0xc067300000000000, 0x0000000000000000 }, // -0x1.73p+7 -> 0x0p+0
	{ 0xc08f440000000000, 0x8000000000000000 }, // -0x1.f44p+9 -> -0x0p+0
	{ 0xbc80000000000000, 0xc360000000000000 }, // -0x1p-55 -> -0x1p+55
	{ 0x4065733333333333, 0x7fec3adadc5107b0 }, // 0x1.5733333333333p+7 -> 0x1.c3adadc5107bp+1023 macOS differs by 1 ulp
	{ 0x406573d70a3d70a4, 0x7fef49ac9f1924cb }, // 0x1.573d70a3d70a4p+7 -> 0x1.f49ac9f1924cbp+1023 macOS differs by 1 ulp
	{ 0x4065766666666666, 0x7ff0000000000000 }, // 0x1.5766666666666p+7 -> inf
	{ 0x4065800000000000, 0x7ff0000000000000 }, // 0x1.58p+7 -> inf
	{ 0x4067000000000000, 0x7ff0000000000000 }, // 0x1.7p+7 -> inf
	{ 0x4069000000000000, 0x7ff0000000000000 }, // 0x1.9p+7 -> inf
	{ 0x7e37e43c8800759c, 0x7ff0000000000000 }, // 0x1.7e43c8800759cp+996 -> inf
	{ 0x0000000000000000, 0x7ff0000000000000 }, // 0x0p+0 -> inf
	{ 0x8000000000000000, 0xfff0000000000000 }, // -0x0p+0 -> -inf
	{ 0x7ff0000000000000, 0x7ff0000000000000 }, // inf -> inf
};

const FloatCase[*] TGAMMAF_CASES = {
	{ 0x3f800000, 0x3f800000 }, // 0x1p+0 -> 0x1p+0
	{ 0x40000000, 0x3f800000 }, // 0x1p+1 -> 0x1p+0
	{ 0x40400000, 0x40000000 }, // 0x1.8p+1 -> 0x1p+1
	{ 0x40800000, 0x40c00000 }, // 0x1p+2 -> 0x1.8p+2
	{ 0x40a00000, 0x41c00000 }, // 0x1.4p+2 -> 0x1.8p+4
	{ 0x40c00000, 0x42f00000 }, // 0x1.8p+2 -> 0x1.ep+6
	{ 0x41200000, 0x48b13000 }, // 0x1.4p+3 -> 0x1.626p+18
	{ 0x41a00000, 0x5bd815ca }, // 0x1.4p+4 -> 0x1.b02b94p+56
	{ 0x3f000000, 0x3fe2dfc5 }, // 0x1p-1 -> 0x1.c5bf8ap+0
	{ 0x3fc00000, 0x3f62dfc5 }, // 0x1.8p+0 -> 0x1.c5bf8ap-1
	{ 0x40200000, 0x3faa27d3 }, // 0x1.4p+1 -> 0x1.544fa6p+0
	{ 0x4121999a, 0x48de0d25 }, // 0x1.433334p+3 -> 0x1.bc1a4ap+18
	{ 0x41f40000, 0x74182d70 }, // 0x1.e8p+4 -> 0x1.305aep+105
	{ 0x2edbe6ff, 0x501502f9 }, // 0x1.b7cdfep-34 -> 0x1.2a05f2p+33
	{ 0x00000001, 0x7f800000 }, // 0x1p-149 -> inf
	{ 0xbf000000, 0xc062dfc5 }, // -0x1p-1 -> -0x1.c5bf8ap+1
	{ 0xbfc00000, 0x40173fd8 }, // -0x1.8p+0 -> 0x1.2e7fbp+1
	{ 0xc0200000, 0xbf71ffc1 }, // -0x1.4p+1 -> -0x1.e3ff82p-1
	{ 0xbdcccccd, 0xc12afb08 }, // -0x1.99999ap-4 -> -0x1.55f61p+3
	{ 0xc1280000, 0xb48dbd8f }, // -0x1.5p+3 -> -0x1.1b7b1ep-22
	{ 0xc2220000, 0x80000000 }, // -0x1.44p+5 -> -0x0p+0
	{ 0xc3388000, 0x80000000 }, // -0x1.71p+7 -> -0x0p+0
	{ 0xc3398000, 0x00000000 }, // -0x1.73p+7 -> 0x0p+0
	{ 0x42080000, 0x7cd10b14 }, // 0x1.1p+5 -> 0x1.a21628p+122
	{ 0x420c0000, 0x7f5e1bc5 }, // 0x1.18p+5 -> 0x1.bc378ap+127
	{ 0x420c28f6, 0x7f7fe95f }, // 0x1.1851ecp+5 -> 0x1.ffd2bep+127
	{ 0x420c6666, 0x7f800000 }, // 0x1.18ccccp+5 -> inf
	{ 0x42100000, 0x7f800000 }, // 0x1.2p+5 -> inf
	{ 0x43480000, 0x7f800000 }, // 0x1.9p+7 -> inf
	{ 0x7149f2ca, 0x7f800000 }, // 0x1.93e594p+99 -> inf
	{ 0x00000000, 0x7f800000 }, // 0x0p+0 -> inf
	{ 0x80000000, 0xff800000 }, // -0x0p+0 -> -inf
	{ 0x7f800000, 0x7f800000 }, // inf -> inf
};

// The sign of Gamma(x) from lgamma_r, for x and for (float)x with lgammaf_r.
const SignCase[*] SIGN_CASES = {
	{ 0x1p-1, 1 }, // lgamma: 0x1.250d048e7a1bdp-1
	{ 0x1.8p+1, 1 }, // lgamma: 0x1.62e42fefa39efp-1
	{ -0x1p-1, -1 }, // lgamma: 0x1.43f89a3f0edd6p+0
	{ -0x1.8p+0, 1 }, // lgamma: 0x1.b858151820f85p-1
	{ -0x1.4p+1, -1 }, // lgamma: -0x1.ccbf9f5ed0f2p-5
	{ -0x1.cp+1, 1 }, // lgamma: -0x1.4f1b0fe64a5d9p+0
	{ -0x0p+0, -1 }, // lgamma: inf
	{ 0x0p+0, 1 }, // lgamma: inf
	{ -0x1.4484bfeebc2ap-100, -1 }, // lgamma: 0x1.144f69ff9ffc4p+6
	{ -0x1p+0, 1 }, // lgamma: inf
	{ double::inf, 1 }, // lgamma: inf
	{ -double::inf, 1 }, // lgamma: inf
};

fn void lgamma_matches_musl()
{
	foreach (c : LGAMMA_CASES)
	{
		double x = bitcast(c.x, double);
		ulong got = bitcast(nolibc::_lgamma(@volatile_load(x)), ulong);
		assert(got == c.expected, "lgamma(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void lgammaf_matches_musl()
{
	foreach (c : LGAMMAF_CASES)
	{
		float x = bitcast(c.x, float);
		uint got = bitcast(nolibc::_lgammaf(@volatile_load(x)), uint);
		assert(got == c.expected, "lgammaf(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void tgamma_matches_musl()
{
	foreach (c : TGAMMA_CASES)
	{
		double x = bitcast(c.x, double);
		ulong got = bitcast(nolibc::_tgamma(@volatile_load(x)), ulong);
		assert(got == c.expected, "tgamma(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void tgammaf_matches_musl()
{
	foreach (c : TGAMMAF_CASES)
	{
		float x = bitcast(c.x, float);
		uint got = bitcast(nolibc::_tgammaf(@volatile_load(x)), uint);
		assert(got == c.expected, "tgammaf(%a) = %x, expected %x", x, got, c.expected);
	}
}

fn void lgamma_r_sign()
{
	foreach (c : SIGN_CASES)
	{
		double x = c.x;
		int sign = 7;
		nolibc::_lgamma_r(@volatile_load(x), &sign);
		assert(sign == c.sign, "lgamma_r(%a) sign = %d, expected %d", x, sign, c.sign);
		float xf = (float)c.x;
		sign = 7;
		nolibc::_lgammaf_r(@volatile_load(xf), &sign);
		assert(sign == c.sign, "lgammaf_r(%a) sign = %d, expected %d", xf, sign, c.sign);
	}
}

fn void gamma_nan()
{
	foreach (double x : { double::nan, -1.0, -2.0, -100.0, -0x1p60, -double::inf })
	{
		double v = x;
		assert(math::is_nan(nolibc::_tgamma(@volatile_load(v))), "tgamma(%a) should be nan", x);
		float vf = (float)x;
		assert(math::is_nan(nolibc::_tgammaf(@volatile_load(vf))), "tgammaf(%a) should be nan", x);
	}
	double n = double::nan;
	float nf = float::nan;
	assert(math::is_nan(nolibc::_lgamma(@volatile_load(n))));
	assert(math::is_nan(nolibc::_lgammaf(@volatile_load(nf))));
}

fn void gamma_through_std_math()
{
	double x = 0.5;
	float xf = 0.5f;
	assert(bitcast(math::lgamma(@volatile_load(x)), ulong) == 0x3fe250d048e7a1bd);
	assert(bitcast(math::lgamma(@volatile_load(xf)), uint) == 0x3f128682);
	assert(bitcast(math::tgamma(@volatile_load(x)), ulong) == 0x3ffc5bf891b4ef6a);
	assert(bitcast(math::tgamma(@volatile_load(xf)), uint) == 0x3fe2dfc5);
}
