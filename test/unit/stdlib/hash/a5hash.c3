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


fn void vector_2()
{
    char[] input = "7 chars";
    ulong expected = 0xe49a0cc72256bbac;

    ulong actual = a5hash::hash(input);

    test::@check(actual == expected, "Hash mismatch (%x expected // %x actual).", expected, actual);
}
