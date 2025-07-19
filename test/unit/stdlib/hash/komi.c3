// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module komi_tests;


char[256] bulk;

fn void setup() @init
{
	for (usz i = 0; i < bulk.len; ++i) bulk[i] = (char)i;
}


// ==========================================================================
module komi_tests @test;

import std::hash::komi;


const char[][] INPUTS = {
	"This is a 32-byte testing string",
	"The cat is out of the bag",
	"A 16-byte string",
	"The new string",
	"7 chars",
};

const usz[] BULK_INTERVALS = { 3, 6, 8, 12, 20, 31, 32, 40, 47, 48, 56, 64, 72, 80, 112, 132, 256 };


const ulong[] EXPECTED_UNSEEDED = {
	0x05ad960802903a9d,
	0xd15723521d3c37b1,
	0x467caa28ea3da7a6,
	0xf18e67bc90c43233,
	0x2c514f6e5dcb11cb,
};

const ulong[] EXPECTED_BULK_UNSEEDED = {
	0x7a9717e9eea4be8b,
	0xa56469564c2ea0ff,
	0x00b4313a24431306,
	0x64c2ad96013f70fe,
	0x7a3888bc95545364,
	0xc77e02ed4b201b9a,
	0x256d74350303a1ba,
	0x59609c71697bb9df,
	0x36eb9e6a4c2c5e4b,
	0x8dd56c332850baa6,
	0xcbb722192b353999,
	0x90b07e2158f88cc0,
	0x24c9621701603741,
	0x1d4c1d97ca684334,
	0xd1a425d530652287,
	0x72623be342c20ab5,
	0x94c3dbdca59ddf57,
};

fn void unseeded_string_vectors()
{
	for (usz i = 0; i < INPUTS.len; ++i)
	{
		char[] input = INPUTS[i];
		ulong expected = EXPECTED_UNSEEDED[i];

		ulong actual = komi::hash(input);

		test::@check(actual == expected, "Hash mismatch for unseeded '%s' (%x expected // %x actual).", (ZString)input, expected, actual);
	}
}

fn void unseeded_bulk_vectors()
{
	$assert BULK_INTERVALS.len == EXPECTED_BULK_UNSEEDED.len
		: "BULK_INTERVALS length does not match the given BULK test set.";

	for (usz i = 0; i < BULK_INTERVALS.len; ++i)
	{
		char[] input = bulk[:BULK_INTERVALS[i]];
		ulong expected = EXPECTED_BULK_UNSEEDED[i];

		ulong actual = komi::hash(input);

		test::@check(actual == expected, "Hash mismatch for unseeded bulk interval %d (idx %d) (%x expected // %x actual).", BULK_INTERVALS[i], i, expected, actual);
	}
}


const ulong TEST_SEED = 0x0123456789abcdef;

const ulong[] EXPECTED_SEEDED = {
	0x6ce66a2e8d4979a5,
	0x5b1da0b43545d196,
	0x26af914213d0c915,
	0x62d9ca1b73250cb5,
	0x90ab7c9f831cd940,
};

const ulong[] EXPECTED_BULK_SEEDED = {
	0x84ae4eb65b96617e,
	0xaceebc32a3c0d9e4,
	0xdaa1a90ecb95f6f8,
	0xec8eb3ef4af380b4,
	0x07045bd31abba34c,
	0xd5f619fb2e62c4ae,
	0x5a336fd2c4c39abe,
	0x0e870b4623eea8ec,
	0xe552edd6bf419d1d,
	0x37d170ddcb1223e6,
	0x1cd89e708e5098b6,
	0x765490569ccd77f2,
	0x19e9d77b86d01ee8,
	0x25f83ee520c1d241,
	0xd6007417091cd4c0,
	0x3e49c2d3727b9cc9,
	0xb2b3405ee5d65f4c,
};

fn void seeded_string_vectors()
{
	for (usz i = 0; i < INPUTS.len; ++i)
	{
		char[] input = INPUTS[i];
		ulong expected = EXPECTED_SEEDED[i];

		ulong actual = komi::hash(input, TEST_SEED);

		test::@check(actual == expected,
			"Hash mismatch for seed 0x123456789abcdef '%s' (%x expected // %x actual).",
			(ZString)input, expected, actual);
	}
}

fn void seeded_offset()
{
	char[] x = "kepkoewkopkfpokfoewkfokweokefkfkkpoewkf";
	ulong actual = komi::hash(x[1..], TEST_SEED);
}

fn void seeded_bulk_vectors()
{
	$assert BULK_INTERVALS.len == EXPECTED_BULK_SEEDED.len
		: "BULK_INTERVALS length does not match the given BULK test set.";

	for (usz i = 0; i < BULK_INTERVALS.len; ++i)
	{
		char[] input = bulk[:BULK_INTERVALS[i]];
		ulong expected = EXPECTED_BULK_SEEDED[i];

		ulong actual = komi::hash(input, TEST_SEED);

		test::@check(actual == expected,
			"Hash mismatch for unseeded bulk interval %d (idx %d) (%x expected // %x actual).",
			BULK_INTERVALS[i], i, expected, actual);
	}
}


const ulong TEST_SEED_2 = 0x100;

const ulong[] EXPECTED_SEEDED_2 = {
	0x5f197b30bcec1e45,
	0xa761280322bb7698,
	0x11c31ccabaa524f1,
	0x3a43b7f58281c229,
	0xcff90b0466b7e3a2,
};

const ulong[] EXPECTED_BULK_SEEDED_2 = {
	0x8ab53f45cc9315e3,
	0xea606e43d1976ccf,
	0x889b2f2ceecbec73,
	0xacbec1886cd23275,
	0x57c3affd1b71fcdb,
	0x7ef6ba49a3b068c3,
	0x49dbca62ed5a1ddf,
	0x192848484481e8c0,
	0x420b43a5edba1bd7,
	0xd6e8400a9de24ce3,
	0xbea291b225ff384d,
	0x0ec94062b2f06960,
	0xfa613272ecd49985,
	0x76f0bb380bc207be,
	0x4afb4e08ca77c020,
	0x410f9c129ad88aea,
	0x066c7b25f4f569ae,
};

fn void seeded_string_vectors_2()
{
	for (usz i = 0; i < INPUTS.len; ++i)
	{
		char[] input = INPUTS[i];
		ulong expected = EXPECTED_SEEDED_2[i];

		ulong actual = komi::hash(input, TEST_SEED_2);

		test::@check(actual == expected,
			"Hash mismatch for seed 0x100 '%s' (%x expected // %x actual).",
			(ZString)input, expected, actual);
	}
}

fn void seeded_bulk_vectors_2()
{
	$assert BULK_INTERVALS.len == EXPECTED_BULK_SEEDED_2.len
		: "BULK_INTERVALS length does not match the given BULK test set.";

	for (usz i = 0; i < BULK_INTERVALS.len; ++i)
	{
		char[] input = bulk[:BULK_INTERVALS[i]];
		ulong expected = EXPECTED_BULK_SEEDED_2[i];

		ulong actual = komi::hash(input, TEST_SEED_2);

		test::@check(actual == expected,
			"Hash mismatch for unseeded bulk interval %d (idx %d) (%x expected // %x actual).",
			BULK_INTERVALS[i], i, expected, actual);
	}
}


const ulong[] EXPECTED_SWEEP = {
	0x5cdcdaf25a774bdf, 0x1e5c3d2098586a17, 0x138664c4f409a6aa, 0x6936cf53c986deb1,
	0x3417ed7d7a081094, 0xd38acf8461008782, 0xf19e49a8a7c77869, 0xb4c60ae1c52e8a13,
	0xc6e7ceffa4af2605, 0xc219152657fcc7b9, 0x9562052479b8007c, 0xb4395e5aec193f02,
	0x80d9987c7e56ca6f, 0x03a29a5f5d9918d3, 0x2d4988241df24218, 0x8d569336b00c6578,
	0x1975540b1f4ed2bc, 0x7265c30b704afb6e, 0x173f6f524900ec6d, 0xcc86a82757407a99,
	0xacc0654d841e5e31, 0x67345fcf0f031a01, 0x5914b9ebdc010c00, 0x1c61fe5bda86efb4,
	0x6e133aa91b2d9218, 0x84357177a1c7df4e, 0x8cdd00e42ceeea38, 0x0fa84a74c35fd8f3,
	0x9f33f6baf88e1b8b, 0xe3d86438fdc4bbb5, 0x9be24abc570ab17d, 0x2d76ab384b25e64d,
	0xecdc96ef224dc58f, 0x62940f1aa710e4ee, 0xf45e451deff06f7b, 0x9250f7c75be78b2d,
	0x600298c67b78935e, 0x35180e348bdd7e95, 0x101cf10a0c10c8cf, 0xea20c0ab77226b52,
	0x1182f1b40f5e68b8, 0xf12820779d3a6eef, 0x34c21125302e30bd, 0x9a61527aa22e1000,
	0x24a9c494a2cc820e, 0x56f3fc85980c2630, 0xeb260f799a9dab96, 0xddee1e0ca36342f9,
	0x2249255270abe787, 0x588d83ead4d6085d, 0x8833170d2fdc30ce, 0x9ac09ffc9290461e,
	0xabb332ab60963f6f, 0x9a5ae4156a1d7b76, 0x2466ca4ff4acf4a4, 0xde76c11cc56419ef,
	0x619f641aa58485ea, 0x34d208b1da1e5684, 0xff4ab73c565450e2, 0xb09fdca3ec15641c,
	0x4eaba3d39397930a, 0x1c5cad34c08a56bd, 0x182989a7334faf06, 0xefcc05ca09cde68e,
	0xac04e3c6d5439ab5, 0xfeeee75b73596fe3, 0x1d35e0da61e6888a, 0x55c89cc84bbb5faf,
	0x39981a0668198e8f, 0x17851ab16d9312ee, 0x0caec994194bd050, 0x90f47d9d7bfd3861,
	0x3c45c729da026626, 0xf0ab0708bac2eefb, 0x6c6c15ce7b59daa9, 0xf47920556419e57b,
	0x7d997797acc04e3a, 0x329c7665007f9d3f, 0x113774f05438d762, 0x4be67982859ac5f4,
};

fn void sweep()
{
	char[] input = { [0..EXPECTED_SWEEP.len] = 'z' };

	foreach (i, expected : EXPECTED_SWEEP)
	{
		ulong actual = komi::hash(input[:i], 0x0abab1234321);

		test::@check(actual == expected,
			"Hash mismatch for seed 0x100 '%s' (%x expected // %x actual).",
			(ZString)input, expected, actual);
	}
}
