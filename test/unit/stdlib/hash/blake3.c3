// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module blake3_tests @test;

import std::hash::blake3;
import std::collections;


fn void basic()
{
    char[] expected = x'af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262';
    char[32] hash;
    Blake3 b @noinit;
    b.init();
    b.update("");
    b.final(hash[..], 32);
    test::@check(hash[..] == expected, "Mismatched hashes:\n\tGOT:      %s\n\tEXPECTED: %s\n", hash[..], expected);
}
