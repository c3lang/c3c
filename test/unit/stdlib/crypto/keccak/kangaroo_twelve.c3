// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module keccak_test::kangaroo_twelve @test;

import std::crypto::kangaroo_twelve;


fn void empty()
{
	test::@check(kangaroo_twelve::xof(128, 32, "", "") == x'1ac2d450fc3b4205d19da7bfca1b37513c0803577ac7167f06fe2ce1f0ef39e5');
	test::@check(kangaroo_twelve::xof(128, 64, "", "") == x'1ac2d450fc3b4205d19da7bfca1b37513c0803577ac7167f06fe2ce1f0ef39e54269c056b8c82e48276038b6d292966cc07a3d4645272e31ff38508139eb0a71');
	test::@check(kangaroo_twelve::xof(128, 10032, "", "")[^32..] == x'e8dc563642f7228c84684c898405d3a834799158c079b12880277a1d28e2ff6d');
}
