// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module chacha20_benchmarks;

import std::crypto::chacha20;


fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(1024);
}

const char[] KEY = x'98bef1469be7269837a45bfbc92a5a6ac762507cf96443bf33b96b1bd4c6f8f6';
const char[] NONCE = x'44e792d63335abb1582e9253';
const uint COUNTER = 42;

char[] one_mb @align(ulong.sizeof) = { [0..1024*1024] = 0xA5 };

// This doesn't test both encryption + decryption, because it's a symmetric operation that shares
//   a single common data transformation. Testing one limb is enough.
fn void gogo_chacha20() @benchmark
{
	chacha20::encrypt_mut(one_mb[..], KEY, NONCE, COUNTER);
}

// Check what the speed of an unligned buffer looks like.
fn void gogo_chacha20_unaligned() @benchmark => @pool()
{
    char[] copy = mem::talloc_array(char, one_mb.len + 3);
    char[] im_off_slightly = copy[3..];
    copy[3..] = one_mb[..];
    assert((usz)im_off_slightly.ptr % usz.sizeof > 0);

    runtime::@start_benchmark();
	chacha20::encrypt_mut(im_off_slightly, KEY, NONCE, COUNTER);
    runtime::@end_benchmark();
}
