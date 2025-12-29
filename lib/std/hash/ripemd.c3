// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require @in(DIGEST_BITS, ...PERMISSIBLE_SIZES_BITS) : "Invalid DIGEST_BITS; must be one of {128, 160, 256, 320}."
*>
module std::hash::ripemd { DIGEST_BITS };


<* Unchanging block size. *>
const BLOCK_SIZE = 64;

const usz[4] PERMISSIBLE_SIZES_BITS = { 128, 160, 256, 320 };

<* RIPE-MD initial values. *>
const uint[10] H = {
	0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0,
	0x76543210, 0xfedcba98, 0x89abcdef, 0x01234567, 0x3c2d1e0f
};

<* RIPE-MD constant values. *>
const uint[9] K = {
	0x00000000, 0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xa953fd4e,
	0x50a28be6, 0x5c4dd124, 0x6d703ef3, 0x7a6d76e9
};

<* Ultimate digest's size in bytes. *>
const DIGEST_BYTES = DIGEST_BITS / 8;


struct RipeMd
{
	uint[10]	state;
	uint[16]	buffer;
	ulong		byte_count;
}


<*
 Compute the RIPE-MD hash of the given data.

 @param [in] data : "The data to hash."
*>
macro char[*] hash(char[] data)
{
	RipeMd r @noinit;
	r.init();
	r.update(data);
	return r.final();
}

<*
 Initialize the RIPE-MD hash context.
*>
fn void RipeMd.init(&self)
{
	mem::zero_volatile(@as_char_view(*self));
	self.state[..] = H[..];
	// I don't care about the reason, I want to strangle whoever thought this was a good idea.
	//   I get it, you have to set the lane initializers correctly, but damn I want my hours back.
	$if DIGEST_BITS == 256: self.state[4:4] = self.state[5:4]; $endif
}

<*
 Update the RIPE-MD hash context with the given data.

 @param [in] data : "The data to digest."
*>
fn void RipeMd.update(&self, char[] data)
{
	usz $bufsz = $sizeof(self.buffer);
	uint avail = $bufsz - (uint)(self.byte_count & 0x3f);
	self.byte_count += data.len;

	if (avail > data.len)
	{
		@as_char_view(self.buffer)[($bufsz - avail) : data.len] = data[..];
		return;
	}

	@as_char_view(self.buffer)[($bufsz - avail) : avail] = data[:avail];
	self.transform(self.buffer);
	data = data[avail..];

	for (bool is_aligned = 0 == (usz)data.ptr % uint.sizeof; data.len >= $bufsz; data = data[$bufsz..])
	{
		if (is_aligned) // when aligned, this optimization is a ~10-20% performance boost
		{
			self.transform(((uint*)data.ptr)[:$bufsz / uint.sizeof]);
		}
		else
		{
			@as_char_view(self.buffer)[:$bufsz] = data[:$bufsz];
			self.transform(self.buffer);
		}
	}

	@as_char_view(self.buffer)[:data.len] = data[..];
}

<*
 Finalize the RIPE-MD hash context and return the final hash value.
 This implicitly destroys the underlying hash structure for the sake of security.
*>
fn char[DIGEST_BYTES] RipeMd.final(&self)
{
	char[DIGEST_BYTES] result @align(uint.sizeof);
	static char[64] padding = { [0] = 0x80, [1..63] = 0x00 };

	// Napkin maffs.
	ulong bits = (ulong)self.byte_count << 3;
	uint index = (uint)self.byte_count & 0x3f;
	uint padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);

	// Update with padding, then append bit-length of digested message.
	self.update(padding[:padlen]);
	self.update(@as_char_view(bits));

	// Clean up implicitly.
	defer mem::zero_volatile(@as_char_view(*self));

	// Copy and return.
	((uint*)&result)[:DIGEST_BYTES / uint.sizeof] = self.state[:DIGEST_BYTES / uint.sizeof];
	return result;
}


// State-based permutation functions given as a function pointer to each computation lane.
fn uint f1(uint x, uint y, uint z) @inline @local => x ^ y ^ z;
fn uint f2(uint x, uint y, uint z) @inline @local => z ^ (x & (y ^ z));
fn uint f3(uint x, uint y, uint z) @inline @local => (x | ~y) ^ z;
fn uint f4(uint x, uint y, uint z) @inline @local => y ^ (z & (x ^ y));
fn uint f5(uint x, uint y, uint z) @inline @local => x ^ (y | ~z);

// The primary workhorse of the digest function. Behavior only changes slightly.
//   All 128/256 inputs will expand with 'e' as 0 - they don't use it anyway.
macro @round(#a, b, #c, d, e, #func, k, x, s) @local
{
	#a += #func(b, #c, d) + x + k;
	$switch:
		$case DIGEST_BITS == 128 ||| DIGEST_BITS == 256:
			#a = #a.rotl(s);
		$case DIGEST_BITS == 160 ||| DIGEST_BITS == 320:
			#a = #a.rotl(s) + e;
			#c = #c.rotl(10);
		$default:
			$error "Invalid digest bits";
	$endswitch
}

<*
 Apply a transformation to some input data, but DO NOT inline the contents of the macro within the parent call.

 @param in : "Data or message block to process."
*>
fn void RipeMd.transform(&self, uint[BLOCK_SIZE / uint.sizeof] in) @noinline => self.@transform(in);

<*
 @param in : "Data or message block to process."
*>
macro RipeMd.@transform(&self, uint[BLOCK_SIZE / uint.sizeof] in) @local
{
	uint aa, bb, cc, dd, ee, aaa, bbb, ccc, ddd, eee;
	uint[] state = self.state[..];

	// Run computations lanes based on the size of the resulting digest.
	//   The behavior is different enough for each that this can simply stay broken out into separate cases.
	//   Otherwise, lacing them all together is just not worth the effort and isn't as clear to other developers.
	//   (No shade, but see 'RustCrypto/hashes' RIPE-MD macros for an example of what I mean...)
	$switch DIGEST_BITS:
		$case 128:
			aa = state[0];
			bb = state[1];
			cc = state[2];
			dd = state[3];
			aaa = state[0];
			bbb = state[1];
			ccc = state[2];
			ddd = state[3];

			@round(aa, bb, cc, dd, 0, f1, K[0], in[0],  11);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[5],   8);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[1],  14);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[14],  9);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[2],  15);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[7],   9);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[3],  12);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[0],  11);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[4],   5);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[9],  13);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[5],   8);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[2],  15);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[6],   7);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[11], 15);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[7],   9);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[4],   5);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[8],  11);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[13],  7);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[9],  13);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[6],   7);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[10], 14);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[15],  8);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[11], 15);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[8],  11);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[12],  6);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[1],  14);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[13],  7);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[10], 14);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[14],  9);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[3],  12);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[15],  8);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[12],  6);

			@round(aa, bb, cc, dd, 0, f2, K[1], in[7],   7);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[6],   9);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[4],   6);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[11], 13);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[13],  8);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[3],  15);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[1],  13);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[7],   7);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[10], 11);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[0],  12);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[6],   9);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[13],  8);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[15],  7);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[5],   9);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[3],  15);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[10], 11);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[12],  7);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[14],  7);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[0],  12);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[15],  7);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[9],  15);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[8],  12);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[5],   9);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[12],  7);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[2],  11);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[4],   6);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[14],  7);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[9],  15);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[11], 13);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[1],  13);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[8],  12);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[2],  11);

			@round(aa, bb, cc, dd, 0, f3, K[2], in[3],  11);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[15],  9);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[10], 13);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[5],   7);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[14],  6);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[1],  15);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[4],   7);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[3],  11);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[9],  14);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[7],   8);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[15],  9);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[14],  6);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[8],  13);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[6],   6);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[1],  15);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[9],  14);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[2],  14);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[11], 12);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[7],   8);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[8],  13);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[0],  13);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[12],  5);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[6],   6);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[2],  14);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[13],  5);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[10], 13);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[11], 12);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[0],  13);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[5],   7);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[4],   7);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[12],  5);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[13],  5);

			@round(aa, bb, cc, dd, 0, f4, K[3], in[1],  11);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[8],  15);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[9],  12);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[6],   5);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[11], 14);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[4],   8);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[10], 15);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[1],  11);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[0],  14);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[3],  14);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[8],  15);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[11], 14);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[12],  9);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[15],  6);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[4],   8);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[0],  14);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[13],  9);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[5],   6);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[3],  14);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[12],  9);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[7],   5);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[2],  12);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[15],  6);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[13],  9);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[14],  8);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[9],  12);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[5],   6);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[7],   5);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[6],   5);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[10], 15);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[2],  12);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[14],  8);

			ddd += cc + state[1];
			state[1] = state[2] + dd + aaa;
			state[2] = state[3] + aa + bbb;
			state[3] = state[0] + bb + ccc;
			state[0] = ddd;

		$case 160:
			aa = state[0];
			bb = state[1];
			cc = state[2];
			dd = state[3];
			ee = state[4];
			aaa = state[0];
			bbb = state[1];
			ccc = state[2];
			ddd = state[3];
			eee = state[4];

			@round(aa, bb, cc, dd, ee, f1, K[0], in[0],  11);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[5],   8);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[1],  14);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[14],  9);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[2],  15);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[7],   9);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[3],  12);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[0],  11);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[4],   5);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[9],  13);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[5],   8);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[2],  15);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[6],   7);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[11], 15);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[7],   9);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[4],   5);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[8],  11);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[13],  7);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[9],  13);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[6],   7);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[10], 14);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[15],  8);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[11], 15);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[8],  11);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[12],  6);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[1],  14);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[13],  7);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[10], 14);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[14],  9);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[3],  12);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[15],  8);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[12],  6);

			@round(ee, aa, bb, cc, dd, f2, K[1], in[7],   7);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[6],   9);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[4],   6);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[11], 13);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[13],  8);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[3],  15);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[1],  13);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[7],   7);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[10], 11);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[0],  12);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[6],   9);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[13],  8);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[15],  7);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[5],   9);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[3],  15);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[10], 11);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[12],  7);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[14],  7);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[0],  12);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[15],  7);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[9],  15);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[8],  12);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[5],   9);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[12],  7);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[2],  11);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[4],   6);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[14],  7);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[9],  15);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[11], 13);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[1],  13);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[8],  12);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[2],  11);

			@round(dd, ee, aa, bb, cc, f3, K[2], in[3],  11);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[15],  9);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[10], 13);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[5],   7);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[14],  6);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[1],  15);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[4],   7);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[3],  11);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[9],  14);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[7],   8);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[15],  9);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[14],  6);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[8],  13);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[6],   6);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[1],  15);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[9],  14);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[2],  14);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[11], 12);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[7],   8);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[8],  13);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[0],  13);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[12],  5);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[6],   6);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[2],  14);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[13],  5);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[10], 13);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[11], 12);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[0],  13);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[5],   7);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[4],   7);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[12],  5);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[13],  5);

			@round(cc, dd, ee, aa, bb, f4, K[3], in[1],  11);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[8],  15);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[9],  12);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[6],   5);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[11], 14);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[4],   8);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[10], 15);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[1],  11);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[0],  14);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[3],  14);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[8],  15);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[11], 14);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[12],  9);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[15],  6);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[4],   8);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[0],  14);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[13],  9);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[5],   6);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[3],  14);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[12],  9);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[7],   5);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[2],  12);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[15],  6);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[13],  9);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[14],  8);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[9],  12);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[5],   6);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[7],   5);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[6],   5);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[10], 15);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[2],  12);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[14],  8);

			@round(bb, cc, dd, ee, aa, f5, K[4], in[4],   9);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[12],  8);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[0],  15);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[15],  5);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[5],   5);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[10], 12);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[9],  11);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[4],   9);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[7],   6);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[1],  12);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[12],  8);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[5],   5);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[2],  13);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[8],  14);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[10], 12);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[7],   6);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[14],  5);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[6],   8);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[1],  12);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[2],  13);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[3],  13);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[13],  6);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[8],  14);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[14],  5);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[11], 11);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[0],  15);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[6],   8);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[3],  13);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[15],  5);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[9],  11);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[13],  6);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[11], 11);

			ddd += cc + state[1];
			state[1] = state[2] + dd + eee;
			state[2] = state[3] + ee + aaa;
			state[3] = state[4] + aa + bbb;
			state[4] = state[0] + bb + ccc;
			state[0] = ddd;

		$case 256:
			aa = state[0];
			bb = state[1];
			cc = state[2];
			dd = state[3];
			aaa = state[4];
			bbb = state[5];
			ccc = state[6];
			ddd = state[7];

			@round(aa, bb, cc, dd, 0, f1, K[0], in[0],  11);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[5],   8);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[1],  14);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[14],  9);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[2],  15);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[7],   9);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[3],  12);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[0],  11);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[4],   5);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[9],  13);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[5],   8);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[2],  15);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[6],   7);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[11], 15);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[7],   9);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[4],   5);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[8],  11);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[13],  7);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[9],  13);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[6],   7);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[10], 14);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[15],  8);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[11], 15);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[8],  11);
			@round(aa, bb, cc, dd, 0, f1, K[0], in[12],  6);  @round(aaa, bbb, ccc, ddd, 0, f4, K[5], in[1],  14);
			@round(dd, aa, bb, cc, 0, f1, K[0], in[13],  7);  @round(ddd, aaa, bbb, ccc, 0, f4, K[5], in[10], 14);
			@round(cc, dd, aa, bb, 0, f1, K[0], in[14],  9);  @round(ccc, ddd, aaa, bbb, 0, f4, K[5], in[3],  12);
			@round(bb, cc, dd, aa, 0, f1, K[0], in[15],  8);  @round(bbb, ccc, ddd, aaa, 0, f4, K[5], in[12],  6);

			@swap(aa, aaa);

			@round(aa, bb, cc, dd, 0, f2, K[1], in[7],   7);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[6],   9);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[4],   6);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[11], 13);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[13],  8);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[3],  15);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[1],  13);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[7],   7);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[10], 11);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[0],  12);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[6],   9);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[13],  8);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[15],  7);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[5],   9);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[3],  15);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[10], 11);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[12],  7);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[14],  7);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[0],  12);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[15],  7);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[9],  15);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[8],  12);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[5],   9);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[12],  7);
			@round(aa, bb, cc, dd, 0, f2, K[1], in[2],  11);  @round(aaa, bbb, ccc, ddd, 0, f3, K[6], in[4],   6);
			@round(dd, aa, bb, cc, 0, f2, K[1], in[14],  7);  @round(ddd, aaa, bbb, ccc, 0, f3, K[6], in[9],  15);
			@round(cc, dd, aa, bb, 0, f2, K[1], in[11], 13);  @round(ccc, ddd, aaa, bbb, 0, f3, K[6], in[1],  13);
			@round(bb, cc, dd, aa, 0, f2, K[1], in[8],  12);  @round(bbb, ccc, ddd, aaa, 0, f3, K[6], in[2],  11);

			@swap(bb, bbb);

			@round(aa, bb, cc, dd, 0, f3, K[2], in[3],  11);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[15],  9);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[10], 13);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[5],   7);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[14],  6);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[1],  15);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[4],   7);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[3],  11);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[9],  14);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[7],   8);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[15],  9);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[14],  6);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[8],  13);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[6],   6);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[1],  15);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[9],  14);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[2],  14);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[11], 12);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[7],   8);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[8],  13);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[0],  13);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[12],  5);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[6],   6);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[2],  14);
			@round(aa, bb, cc, dd, 0, f3, K[2], in[13],  5);  @round(aaa, bbb, ccc, ddd, 0, f2, K[7], in[10], 13);
			@round(dd, aa, bb, cc, 0, f3, K[2], in[11], 12);  @round(ddd, aaa, bbb, ccc, 0, f2, K[7], in[0],  13);
			@round(cc, dd, aa, bb, 0, f3, K[2], in[5],   7);  @round(ccc, ddd, aaa, bbb, 0, f2, K[7], in[4],   7);
			@round(bb, cc, dd, aa, 0, f3, K[2], in[12],  5);  @round(bbb, ccc, ddd, aaa, 0, f2, K[7], in[13],  5);

			@swap(cc, ccc);

			@round(aa, bb, cc, dd, 0, f4, K[3], in[1],  11);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[8],  15);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[9],  12);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[6],   5);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[11], 14);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[4],   8);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[10], 15);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[1],  11);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[0],  14);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[3],  14);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[8],  15);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[11], 14);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[12],  9);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[15],  6);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[4],   8);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[0],  14);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[13],  9);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[5],   6);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[3],  14);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[12],  9);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[7],   5);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[2],  12);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[15],  6);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[13],  9);
			@round(aa, bb, cc, dd, 0, f4, K[3], in[14],  8);  @round(aaa, bbb, ccc, ddd, 0, f1, 0, in[9],  12);
			@round(dd, aa, bb, cc, 0, f4, K[3], in[5],   6);  @round(ddd, aaa, bbb, ccc, 0, f1, 0, in[7],   5);
			@round(cc, dd, aa, bb, 0, f4, K[3], in[6],   5);  @round(ccc, ddd, aaa, bbb, 0, f1, 0, in[10], 15);
			@round(bb, cc, dd, aa, 0, f4, K[3], in[2],  12);  @round(bbb, ccc, ddd, aaa, 0, f1, 0, in[14],  8);

			@swap(dd, ddd);

			state[0] += aa;
			state[1] += bb;
			state[2] += cc;
			state[3] += dd;
			state[4] += aaa;
			state[5] += bbb;
			state[6] += ccc;
			state[7] += ddd;

		$case 320:
			aa = state[0];
			bb = state[1];
			cc = state[2];
			dd = state[3];
			ee = state[4];
			aaa = state[5];
			bbb = state[6];
			ccc = state[7];
			ddd = state[8];
			eee = state[9];

			@round(aa, bb, cc, dd, ee, f1, K[0], in[0],  11);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[5],   8);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[1],  14);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[14],  9);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[2],  15);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[7],   9);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[3],  12);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[0],  11);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[4],   5);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[9],  13);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[5],   8);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[2],  15);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[6],   7);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[11], 15);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[7],   9);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[4],   5);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[8],  11);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[13],  7);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[9],  13);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[6],   7);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[10], 14);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[15],  8);
			@round(ee, aa, bb, cc, dd, f1, K[0], in[11], 15);  @round(eee, aaa, bbb, ccc, ddd, f5, K[5], in[8],  11);
			@round(dd, ee, aa, bb, cc, f1, K[0], in[12],  6);  @round(ddd, eee, aaa, bbb, ccc, f5, K[5], in[1],  14);
			@round(cc, dd, ee, aa, bb, f1, K[0], in[13],  7);  @round(ccc, ddd, eee, aaa, bbb, f5, K[5], in[10], 14);
			@round(bb, cc, dd, ee, aa, f1, K[0], in[14],  9);  @round(bbb, ccc, ddd, eee, aaa, f5, K[5], in[3],  12);
			@round(aa, bb, cc, dd, ee, f1, K[0], in[15],  8);  @round(aaa, bbb, ccc, ddd, eee, f5, K[5], in[12],  6);

			@swap(aa, aaa);

			@round(ee, aa, bb, cc, dd, f2, K[1], in[7],   7);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[6],   9);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[4],   6);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[11], 13);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[13],  8);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[3],  15);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[1],  13);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[7],   7);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[10], 11);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[0],  12);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[6],   9);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[13],  8);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[15],  7);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[5],   9);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[3],  15);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[10], 11);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[12],  7);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[14],  7);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[0],  12);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[15],  7);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[9],  15);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[8],  12);
			@round(dd, ee, aa, bb, cc, f2, K[1], in[5],   9);  @round(ddd, eee, aaa, bbb, ccc, f4, K[6], in[12],  7);
			@round(cc, dd, ee, aa, bb, f2, K[1], in[2],  11);  @round(ccc, ddd, eee, aaa, bbb, f4, K[6], in[4],   6);
			@round(bb, cc, dd, ee, aa, f2, K[1], in[14],  7);  @round(bbb, ccc, ddd, eee, aaa, f4, K[6], in[9],  15);
			@round(aa, bb, cc, dd, ee, f2, K[1], in[11], 13);  @round(aaa, bbb, ccc, ddd, eee, f4, K[6], in[1],  13);
			@round(ee, aa, bb, cc, dd, f2, K[1], in[8],  12);  @round(eee, aaa, bbb, ccc, ddd, f4, K[6], in[2],  11);

			@swap(bb, bbb);

			@round(dd, ee, aa, bb, cc, f3, K[2], in[3],  11);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[15],  9);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[10], 13);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[5],   7);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[14],  6);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[1],  15);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[4],   7);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[3],  11);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[9],  14);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[7],   8);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[15],  9);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[14],  6);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[8],  13);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[6],   6);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[1],  15);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[9],  14);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[2],  14);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[11], 12);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[7],   8);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[8],  13);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[0],  13);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[12],  5);
			@round(cc, dd, ee, aa, bb, f3, K[2], in[6],   6);  @round(ccc, ddd, eee, aaa, bbb, f3, K[7], in[2],  14);
			@round(bb, cc, dd, ee, aa, f3, K[2], in[13],  5);  @round(bbb, ccc, ddd, eee, aaa, f3, K[7], in[10], 13);
			@round(aa, bb, cc, dd, ee, f3, K[2], in[11], 12);  @round(aaa, bbb, ccc, ddd, eee, f3, K[7], in[0],  13);
			@round(ee, aa, bb, cc, dd, f3, K[2], in[5],   7);  @round(eee, aaa, bbb, ccc, ddd, f3, K[7], in[4],   7);
			@round(dd, ee, aa, bb, cc, f3, K[2], in[12],  5);  @round(ddd, eee, aaa, bbb, ccc, f3, K[7], in[13],  5);

			@swap(cc, ccc);

			@round(cc, dd, ee, aa, bb, f4, K[3], in[1],  11);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[8],  15);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[9],  12);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[6],   5);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[11], 14);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[4],   8);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[10], 15);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[1],  11);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[0],  14);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[3],  14);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[8],  15);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[11], 14);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[12],  9);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[15],  6);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[4],   8);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[0],  14);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[13],  9);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[5],   6);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[3],  14);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[12],  9);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[7],   5);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[2],  12);
			@round(bb, cc, dd, ee, aa, f4, K[3], in[15],  6);  @round(bbb, ccc, ddd, eee, aaa, f2, K[8], in[13],  9);
			@round(aa, bb, cc, dd, ee, f4, K[3], in[14],  8);  @round(aaa, bbb, ccc, ddd, eee, f2, K[8], in[9],  12);
			@round(ee, aa, bb, cc, dd, f4, K[3], in[5],   6);  @round(eee, aaa, bbb, ccc, ddd, f2, K[8], in[7],   5);
			@round(dd, ee, aa, bb, cc, f4, K[3], in[6],   5);  @round(ddd, eee, aaa, bbb, ccc, f2, K[8], in[10], 15);
			@round(cc, dd, ee, aa, bb, f4, K[3], in[2],  12);  @round(ccc, ddd, eee, aaa, bbb, f2, K[8], in[14],  8);

			@swap(dd, ddd);

			@round(bb, cc, dd, ee, aa, f5, K[4], in[4],   9);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[12],  8);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[0],  15);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[15],  5);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[5],   5);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[10], 12);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[9],  11);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[4],   9);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[7],   6);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[1],  12);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[12],  8);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[5],   5);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[2],  13);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[8],  14);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[10], 12);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[7],   6);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[14],  5);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[6],   8);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[1],  12);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[2],  13);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[3],  13);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[13],  6);
			@round(aa, bb, cc, dd, ee, f5, K[4], in[8],  14);  @round(aaa, bbb, ccc, ddd, eee, f1, 0, in[14],  5);
			@round(ee, aa, bb, cc, dd, f5, K[4], in[11], 11);  @round(eee, aaa, bbb, ccc, ddd, f1, 0, in[0],  15);
			@round(dd, ee, aa, bb, cc, f5, K[4], in[6],   8);  @round(ddd, eee, aaa, bbb, ccc, f1, 0, in[3],  13);
			@round(cc, dd, ee, aa, bb, f5, K[4], in[15],  5);  @round(ccc, ddd, eee, aaa, bbb, f1, 0, in[9],  11);
			@round(bb, cc, dd, ee, aa, f5, K[4], in[13],  6);  @round(bbb, ccc, ddd, eee, aaa, f1, 0, in[11], 11);

			@swap(ee, eee);

			state[0] += aa;
			state[1] += bb;
			state[2] += cc;
			state[3] += dd;
			state[4] += ee;
			state[5] += aaa;
			state[6] += bbb;
			state[7] += ccc;
			state[8] += ddd;
			state[9] += eee;

		$default:
			$error "Invalid digest size";
	$endswitch
}
