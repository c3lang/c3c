// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module entropy_tests @test;

import std::crypto::entropy, std::math;

fn void generates_when_supported()
{
	$if env::WIN32 || env::LINUX || env::BSD_FAMILY || env::DARWIN || env::ANDROID || env::EMSCRIPTEN:
		char[] cmp  = alloc::alloc_array(tmem, char, 1024);
		char[] buf  = alloc::alloc_array(tmem, char, 1024);
		char[] zero = alloc::alloc_array(tmem, char, 1024);
		cmp[..] = 0xA5;
		mem::zero_volatile(zero);
		foreach (i, v : math::iota(sz[1024]))
		{
			buf[:i] = 0xA5;
			test::eq(buf[:i], cmp[:i]);   // sanity
			entropy::fill(buf[:i])!!;
			// only run this when it's statistically improbable that the generated entropy could have ever returned 0xA5 repeating
			if (i > 11) test::ne(buf[:i], cmp[:i]);
			// also ensure this works too (for good measure)
			char[] res = entropy::make(tmem, i)!!;
			if (i > 11) test::ne(res[:i], zero[:i]);
		}
	$else
		char[10] x;
		test::@error(entropy::fill(x[..]), entropy::SECURE_RANDOM_UNAVAILABLE);
	$endif
}
