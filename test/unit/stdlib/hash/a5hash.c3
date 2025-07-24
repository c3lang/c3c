// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module a5hash_tests @test;

import std::hash::a5hash;


fn void vector_1()
{
	char[] input = "This is a test of a5hash.";
	ulong expected = 0xb163640b41959e6b;

	ulong actual = a5hash::hash(input);

	test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
}

fn void vector_offset()
{
	char[] input = "This is a test of a5hash.";

	ulong actual = a5hash::hash(input[1..]);
}


fn void vector_2()
{
	char[] input = "7 chars";
	ulong expected = 0xe49a0cc72256bbac;

	ulong actual = a5hash::hash(input);

	test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
}


const ulong[] EXPECTED_SWEEP = {
	0xfa40305e7f876cde, 0xa462e33cc53262b4, 0x2373712194152d56, 0x948839e266ada547,
	0x6d0c1912787ad5b8, 0x6c234caa741dc983, 0x2d45a051cf4c6588, 0x16c4a4f081d55f34,
	0x2c06450d6f205485, 0x55296f9db1992971, 0x7329cd52328a9082, 0x74785ad80cb7e0cf,
	0x13288aea2281441a, 0x194ae1b6f33f8a83, 0x165e812426f0e087, 0x84981c9506adefb3,
	0x63270fe923b6935f, 0x42279ee502ecac49, 0x21d1c830488bc670, 0x4ea7876e46fdab41,
	0x8af2d66eff7dbae9, 0x8892e79538d4d132, 0x823916d272cfaa91, 0x4187aa86dc29e276,
	0xa2c8198dd1d883b0, 0x1f29c0e4fdcee024, 0xd27d762a99e59b08, 0x361f15e55087a978,
	0x30272a11795ab5d4, 0xffb5f1f42efa5c1a, 0xbc9e503290940862, 0x325c94b294da618f,
	0xa3da4b25911ac41f, 0x48b0e8e5c734e3bc, 0x5e7b0d5f607108b1, 0xaf44b82e7cc700c0,
	0x08184e9ed8940831, 0x16493a88bb9bc76c, 0x6a542a2614969994, 0x7ea3a4295a702672,
	0x4cdcae9d0feaae9a, 0xa51c82eb8201d45f, 0x4e4bce4bb46f20a4, 0xc4a97e28b2fa2993,
	0xe6d48cc40df3905e, 0x684abe59a2db9061, 0x766f289e1ab66393, 0x46f4ab742979a005,
	0xa2d0521bb9eb3653, 0xb41938068a89f9ae, 0x06c063a13b6c380d, 0xf53bf0e413522ab7,
	0x61fa9597bf50dc2e, 0x5911a437240cd52b, 0xc8929ab341f26bb6, 0x46c99c2cfcb00d14,
	0x22f46d19bf96ded8, 0xf63d8cf026448dcf, 0x7e6ab3b486536caa, 0xc2e53529793ce2a8,
	0xcf9f59fb91b7893b, 0xf95d2ae3f31aaf04, 0x423472f722383ea1, 0xc42aebbb3980132a,
	0x458efaa15efd35cb, 0xcd3e0989dc4e04ed, 0xa1c01cd5305af58a, 0x40bc73f12e21385f,
	0x8464509b2b5438ec, 0x961baaded287ad53, 0x22b0a89537728143, 0x7826002b97c764a1,
	0x25eed2c492550022, 0x833bb150f9e75741, 0xcc30d4982191208d, 0x1eaf0a962f3eedea,
	0xe98219e502cce0d5, 0x2bfe6f0253fc07c1, 0x2f8a14428430d003, 0x30e1aa29ee8b7bea,
};

fn void sweep()
{
	char[] input = { [0..EXPECTED_SWEEP.len] = '5' };

	foreach (i, expected : EXPECTED_SWEEP)
	{
		ulong actual = a5hash::hash(input[:i], 0x12ca6b4391e055fe);

		test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
	}
}
