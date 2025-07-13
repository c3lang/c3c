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

        test::@check(actual == expected, "Hash mismatch for seed 0x123456789abcdef '%s' (%x expected // %x actual).", (ZString)input, expected, actual);
    }
}

fn void seeded_bulk_vectors()
{
    for (usz i = 0; i < BULK_INTERVALS.len; ++i)
    {
        char[] input = bulk[:BULK_INTERVALS[i]];
        ulong expected = EXPECTED_BULK_SEEDED[i];

        ulong actual = komi::hash(input, TEST_SEED);

        test::@check(actual == expected, "Hash mismatch for unseeded bulk interval %d (idx %d) (%x expected // %x actual).", BULK_INTERVALS[i], i, expected, actual);
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

        test::@check(actual == expected, "Hash mismatch for seed 0x100 '%s' (%x expected // %x actual).", (ZString)input, expected, actual);
    }
}

fn void seeded_bulk_vectors_2()
{
    for (usz i = 0; i < BULK_INTERVALS.len; ++i)
    {
        char[] input = bulk[:BULK_INTERVALS[i]];
        ulong expected = EXPECTED_BULK_SEEDED_2[i];

        ulong actual = komi::hash(input, TEST_SEED_2);

        test::@check(actual == expected, "Hash mismatch for unseeded bulk interval %d (idx %d) (%x expected // %x actual).", BULK_INTERVALS[i], i, expected, actual);
    }
}
