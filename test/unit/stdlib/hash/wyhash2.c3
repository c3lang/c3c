// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module wyhash2_tests @test;

import std::hash::wyhash2;


fn void simple_vector()
	=> test::@check(0xb4808df22d44ffcf == wyhash2::hash("abc"));

fn void offset_check()
{
	char[*] x = "0123293829";
	wyhash2::hash(x[1..]);
}

fn void simple_vector_seeded()
	=> test::@check(0x9c962ca4764da6f4 == wyhash2::hash("aax", 2));

fn void simple_vector_seeded_2()
	=> test::@check(0x49090566becc19bf == wyhash2::hash("aax", 0x9c962ca4764da6f4));

fn void longer_vector_seeded()
	=> test::@check(0x8b18145f8353c46d == wyhash2::hash("hi my name is:", 2));

fn void longer_vector_seeded_2()
	=> test::@check(0x2b8f7c0e2e562e63 == wyhash2::hash("hi my name is:", 0x8b18145f8353c46d));


<*
These constant vectors are easily confirmed with a quick Rust executable:

However, as of writing, the 0-length return value is different between the below method
(streaming the input) and using `wyhash2::wyhash_single`. So this relies on the function
retval instead of the streamer's, because all other function values match the streamer.

```rust
use core::hash::Hasher;
use wyhash2::WyHash;

fn main() {
	let arr: [u8; 16] = [b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', b'p'];
	let secret = 0;
	for i in 0..=16 {
		let mut hasher = WyHash::with_seed(secret);
		hasher.write(&arr[..i]);
		println!("Index {}: 0x{:x}", i, hasher.finish());
	}
}
```
*>
const ulong[17] VECTORS = {
	0x42bc986dc5eec4d3,
	0x6cf84e5a2465e867,
	0x172ba773b8ebb6d8,
	0xb4808df22d44ffcf,
	0x8cd6fedc542c39e1,
	0x89f29dfa6e5ab1e5,
	0x2d62e7827072fb65,
	0xce8a19cc22fbe893,
	0x3c36fed2521530c0,
	0x1958d0433e7579fa,
	0x787f681f01831617,
	0x7107735a3edb98ee,
	0xf4c24a45a41ea322,
	0x03779e9d9ed9ff12,
	0xd24ac6ffc05e0cb8,
	0x0b4153cef1f30b07,
	0x4ff3b52ca1e858d2,
};

fn void sweep()
{
	char[20] c;

	ulong actual = wyhash2::hash({});
	test::@check(actual == VECTORS[0], "Empty hash failed (%x expected // %x actual).", VECTORS[0], actual);

	for (usz x = 1; x <= 16; ++x)
	{
		c[x - 1] = 'a' + (char)(x - 1);

		actual = wyhash2::hash(c[:x]);

		test::@check(actual == VECTORS[x],
			"Failed on '%s', length %d (%x expected // %x actual).", (ZString)&c, x, VECTORS[x], actual);
	}
}
