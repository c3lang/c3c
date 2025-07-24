// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module metrohash_tests @test;

import std::hash::metro64;
import std::hash::metro128;


const char[] TEST_KEY = "012345678901234567890123456789012345678901234567890123456789012";


fn void metro64_offset()
{
	metro64::hash(TEST_KEY[1..]);
}

fn void metro128_offset()
{
	metro128::hash(TEST_KEY[1..]);
}

fn void metro64_vectors()
{
	ulong expected_0 = 0xad4b7006ae3d756b;
	ulong actual_0 = metro64::hash(TEST_KEY);
	test::@check(actual_0 == expected_0, "Hash mismatch (%x expected // %x actual).", expected_0, actual_0);

	ulong expected_1 = 0xdfb8b9f41c480d3b;
	ulong actual_1 = metro64::hash(TEST_KEY, 1);
	test::@check(actual_1 == expected_1, "Hash mismatch (%x expected // %x actual).", expected_1, actual_1);
}

fn void metro64_streamed()
{
	ulong expected = 0xdfb8b9f41c480d3b;

	MetroHash64 m;
	m.init(1);
	m.update(TEST_KEY[:13]);
	m.update(TEST_KEY[13:11]);
	m.update(TEST_KEY[24..]);

	ulong actual = m.final();

	test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
}


const ulong[66] VECTORS_SWEEP_64 = {
	0xe2f700c7be596c30, 0xd924f06e80703f5f, 0x0e407ae9f3b31eea, 0xb286855b22bb5a7c,
	0x413147f80d972772, 0xa6defbb4891b57ad, 0x0bf33d8a3a11377b, 0x5ef754dc5e155820,
	0x57817499be0ee747, 0x61410284964661e8, 0xdf14b67bf1cf84a1, 0xa34f9fc7d88adda1,
	0xeff25775757576a4, 0x5a3f096738c0f672, 0xcedb9bba97965231, 0xe2234b45b095d9f5,
	0x19cb856abaffafc5, 0x4c2385e5a329fe50, 0x0c1731f599c24394, 0x207d5d5069420af6,
	0xa00af52b3ee78ccf, 0x2649bb0315ed3705, 0x1e1e8cb19aebd947, 0x441c7ad9ede94456,
	0xde8fb76b48fe0795, 0xe28aff110a0485d3, 0x1c4be10ba94dea5e, 0xb345b8382fbcb14c,
	0xbd2083c97604113a, 0x53725cedc13b1f91, 0x6bde258654aabe35, 0x5571177c4f463a94,
	0x7893679fa856b4d8, 0xeb700288dd6ed4fb, 0x3f70383fca952a4a, 0x5b7a795ce3f141b0,
	0xa18b62d7c44d3718, 0x6e9e37eb8ef7bc49, 0x159b948172457d48, 0x113872acbfc4fc7f,
	0x114e2d0a2bbb1700, 0xfc3a6f8cae61d210, 0x627e43470bc34b5d, 0xfe08fbd0cb9abe73,
	0x89dd4e70b7c61b60, 0x6bf6d591e9c00425, 0x7bebba4795cbd4a3, 0x02dee5dde8549496,
	0x71e30b2b3c71393f, 0x2024d0a05633cc87, 0x6884bd684f1cb48b, 0xd8f23c050ee162c2,
	0xbb4425af0f4fd259, 0x7a63abf543efaf39, 0x6b6b919b7a44fefa, 0xedf8000398fe7486,
	0x7fa5131c2a164dad, 0x1831d78b576a433d, 0x8914114c29b11246, 0xd6f5b2b3c48239c7,
	0xc4d9392164f808fb, 0x98454695cda41767, 0x1463110024129443, 0x37e06b51f39b0db1,
	0xb231266aea3ac1d2, 0x9659fcfdeff62211
};

fn void metro64_sweep()
{
	char[66] v = { [0..65] = 'a' };

	for (usz x = 0; x < v.len; ++x)
	{
		ulong actual = metro64::hash(v[:x], 1337);

		test::@check(actual == VECTORS_SWEEP_64[x],
			"Hash mismatch (%x expected // %x actual).", VECTORS_SWEEP_64[x], actual);
	}
}


fn void metro128_vectors()
{
	uint128 expected_0 = 0x97a27450acb248059b9feda4bfe27cc7;
	uint128 actual_0 = metro128::hash(TEST_KEY);
	test::@check(actual_0 == expected_0, "Hash mismatch (%x expected // %x actual).", expected_0, actual_0);

	uint128 expected_1 = 0xefec147a868dd6bd7f9d1938b8cda345;
	uint128 actual_1 = metro128::hash(TEST_KEY, 1);
	test::@check(actual_1 == expected_1, "Hash mismatch (%x expected // %x actual).", expected_1, actual_1);
}

fn void metro128_streamed()
{
	uint128 expected = 0xefec147a868dd6bd7f9d1938b8cda345;

	MetroHash128 m;
	m.init(1);
	foreach (c : TEST_KEY) m.update({c});   // byte by byte

	uint128 actual = m.final();

	test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
}


const uint128[66] VECTORS_SWEEP_128 = {
	0xed66a903a5af8770c4bfd518077b1d4d, 0x9c04be2535e73e406be42706b98cddd4, 0xc082896b0e4704071863c4d6b79c5fd2, 0x93e5f0fabb995f1c567d1d00031ebdb2,
	0xf33e194121b0946dadef05404de5cc63, 0xd38db248561bf524962ed9a48a841a45, 0x7e1695c8838701a49091add6ca0b6da2, 0xe9d9b67eae87f20a0d1c7e19b6c7bc8e,
	0x8b7d6e334c2130f1f8104302054a6adf, 0x363e19909e59b57d6ea1a44071334801, 0xfc07f6db22caf91dfc07eb162e94e5b4, 0x17258d6fe6821c82b721567ad5cc845c,
	0x5759d0fbfdad344f7bc4b2eec33494f4, 0x8e599e8eea792d89cebdd9c11f888f59, 0xce942bf2e18597e63bf12a03ba95e122, 0xdb0d0635c453b26ba07664c37bc7f241,
	0xa9951a456d5c08c4c1564a4e111f88cc, 0x2ddea9673d7ab8e428607e268cc4af58, 0x623ce3f6fdd7f9c070dd915d89564be7, 0xa37787b74daafdebfafdf122b1f04b92,
	0x79c36fdf895491a5d8e2b9d7b27e830e, 0x1ca5545989d706abc51eb30db70733ff, 0xe7f2557aee5921dcad639a73840f1b82, 0x5b66b8cfe8c8381d34c2cb2682f8b3a9,
	0x555d28dc1f2cedbebea4de1c24664b07, 0xcd95e57621d4b3eba9a8a240a751f2f6, 0xab25e96dc41b344295e8d5a734d236bb, 0x0e835e0ac14d8bc0c6707af9cb04780b,
	0x28c74c57374a23e54a97831fea86e71f, 0x09c02b2cb852802664531074b43b24e5, 0xb23658cd2892c1b33179800ca748c093, 0x4f0999fd7417928c77f5169eb6605115,
	0xbc85b4db9fd3096abdcfc238c815e406, 0xf68f40c0ebcf9858a34f846d6442b2c4, 0xd2a4eaa7659c2ca1603d1fae214c5f6a, 0xe382f4280e70fe32c991eacd9a417644,
	0xae43523189c866e6b759f3da9abebb0e, 0x94a3f58c1f5a71bc9d6488c74154e8d7, 0x7e869c466cf2408a0a4c2758ac1c2a1a, 0x645e5babb2ddd637d1d616db16468c8f,
	0x668d5187f86a97172fa7dff866ab4307, 0x43761b3e2011d2b1defce3b2abd3220c, 0xcbe5c5febe9e9522f92eab2faff5a4d2, 0x57effbd664e86987a7e41d0139b0c1d4,
	0x90fc91743fc288d563c6059b099debf2, 0xe7fe3b7f9e2804dc4ca39486d1ff95f8, 0xd419e052bf7a8037581a7176d5e5c40f, 0xbdcf3e2e8e9bec8b5174ee35f5c77a90,
	0xa73b9edb918e873728cb61dbff14ae18, 0x6630e865ec83027c5e930f4ca1ebe300, 0xd44cc36826feb880572a83a046c159c8, 0x1e477dc003e907a1d424a4f84654ddd2,
	0xb498e2859fa073c28a988aa0a461f9ae, 0x05666028c9d1a1a7878cbde8a82e84c4, 0xc1dac1ea4f24c32e83522d0f921560f0, 0xcbb2a8a58dc91c1230aec1f3a5c398cb,
	0x7e76d0952c34286f5ccc2a9a30f65bb3, 0x0091c352079662facb5cd03255a6ecc7, 0xcc9d1fa3518a937b594da868ac1ea634, 0xfec1ae0bb45d5fd9bc0ed7c418c2c633,
	0x9e9cbd767281cdd3779b2e8506774cd4, 0x42be3cc544dc7ed64da7d695313d7802, 0x7f57bad2d44c1f47722c3029ba9f53f5, 0xbd574d95b4635562acc1d8c5633589dc,
	0x1761b98ffa140cfdc8e6ac36327b6080, 0xfddd7de5827fc61fc01b594181f887c1
};

fn void metro128_sweep()
{
	char[66] v = { [0..65] = 'a' };

	for (usz x = 0; x < v.len; ++x)
	{
		uint128 actual = metro128::hash(v[:x], 1337);

		test::@check(actual == VECTORS_SWEEP_128[x],
			"Hash mismatch (%x expected // %x actual).", VECTORS_SWEEP_128[x], actual);
	}
}
