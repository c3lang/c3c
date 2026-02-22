// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// A C3 implementation of the Argon2 memory-hard function in accordance with RFC 9106.
//
module std::crypto::argon2;

import std::bits, std::encoding::base64, std::hash::blake2, std::io, std::math;
import std::thread;


<* Each Argon2 block is a 1024-byte array. *>
const usz BLOCK_SIZE = 1024;

<* See RFC 9106 - Argon2 uses this constant for its 'version' value `v`. *>
const char VERSION = 0x13;

<* Whether calls to Argon2 hashing will use parallelization with OS threads. *>
const bool USE_THREADING = !$feature(ARGON2_FORCE_SEQUENTIAL) && (env::POSIX || env::WIN32);

<* Used for attempts at automatic C3 SIMD optimization with 256-bit vectors. *>
typedef SimdVec @local = ulong[<4>] @simd;

<* Values for this enumeration are sourced from the spec as variable `y` in the input configuration. *>
constdef Argon2Algorithm : inline uint
{
	ARGON2_D = 0,
	ARGON2_I = 1,
	ARGON2_ID = 2,
}

<* Present multiple ways to view the same Argon2 block. *>
union Argon2Block @local @align(SimdVec.sizeof)
{
	SimdVec[BLOCK_SIZE / SimdVec.sizeof] vec;   // 1024 / 32 = 32
	uint128[BLOCK_SIZE / uint128.sizeof] z;   // 1024 / 16 = 64
	ulong[BLOCK_SIZE / ulong.sizeof] q;   // 1024 / 8 = 128
	uint[BLOCK_SIZE / uint.sizeof] d;   // 1024 / 4 = 256
	ushort[BLOCK_SIZE / ushort.sizeof] w;   // 1024 / 2 = 512
	char[BLOCK_SIZE] b;   // 1024
}

<*
 The Argon2 output, or "tag", is a string T bytes long.

 Argon2 has the following input parameters:
	*  Message string P, which is a password for password hashing
		applications.  It MUST have a length not greater than 2^(32)-1
		bytes.
	*  Nonce S, which is a salt for password hashing applications.  It
		MUST have a length not greater than 2^(32)-1 bytes.  16 bytes is
		RECOMMENDED for password hashing.  The salt SHOULD be unique for
		each password.
	*  Degree of parallelism p determines how many independent (but
		synchronizing) computational chains (lanes) can be run.  It MUST
		be an integer value from 1 to 2^(24)-1.
	*  Tag length T MUST be an integer number of bytes from 4 to 2^(32)-1.
	*  Memory size m MUST be an integer number of kibibytes from 8*p to
		2^(32)-1.  The actual number of blocks is m', which is m rounded
		down to the nearest multiple of 4*p.
	*  Number of passes t (used to tune the running time independently of
		the memory size) MUST be an integer number from 1 to 2^(32)-1.
	*  Version number v MUST be one byte 0x13.
	*  Secret value K is OPTIONAL.  If used, it MUST have a length not
		greater than 2^(32)-1 bytes.
	*  Associated data X is OPTIONAL.  If used, it MUST have a length not
		greater than 2^(32)-1 bytes.
	*  Type y MUST be 0 for Argon2d, 1 for Argon2i, or 2 for Argon2id.
*>
struct Argon2Options (Printable)
{
	Argon2Algorithm algorithm;   // y
	char[] salt;   // S (optional)
	char[] key;   // K (optional)
	char[] assoc;   // X (optional)
	uint lanes;   // p
	uint mem_size;   // m
	uint iterations;   // t
}

<* Implement `Printable` interface for the options data structure. *>
fn usz? Argon2Options.to_format(&self, Formatter* f) @dynamic
{
	return f.printf(
		"%s[m=%d,t=%d,p=%d,S=%h%s,K=%h%s,X=%h%s]",
		self.algorithm, self.mem_size, self.iterations, self.lanes,
		self.salt[:min(self.salt.len, 16)], self.salt.len > 16 ? "..." : "",
		self.key[:min(self.key.len, 16)], self.key.len > 16 ? "..." : "",
		self.assoc[:min(self.assoc.len, 16)], self.assoc.len > 16 ? "..." : "",
	);
}

<* Allows comparison between two sets of Argon2 options. *>
fn bool Argon2Options.eq(&self, Argon2Options other) @operator(==)
{
	// Do NOT memcmp the two structures, because slices can have the same content in different memory locations.
	return self.algorithm == other.algorithm
		&& self.salt[..] == other.salt[..]
		&& self.key[..] == other.key[..]
		&& self.assoc[..] == other.assoc[..]
		&& self.lanes == other.lanes
		&& self.mem_size == other.mem_size
		&& self.iterations == other.iterations;
}

<*
 A simple set of default Argon2 options to work from.
 You can use this in a struct-splat to quickly populate settings you don't wish to change.

 ```c3
 Argon2Options my_opts = { ...argon2::DEFAULT_OPTIONS, .salt = my_salt, .mem_size = 64 };
 ```
*>
const Argon2Options DEFAULT_OPTIONS = {
	.algorithm = ARGON2_ID,   // a safe default version of the algorithm for non-specific use cases
	.lanes = 1,   // no parallelism
	.mem_size = 4096,   // 4096 KiB, or 4 MiB
	.iterations = 3,   // 3 iterations
};

<* Force a value to be little-endian where required. *>
macro uint @le(val) @local => env::BIG_ENDIAN ??? bswap(val) : val;

<* Force a value to be little-endian where required and return it as a char view. *>
macro @le_c(val) @local => @as_char_view(env::BIG_ENDIAN ??? *&&bswap(val) : val);

<* The internal round function of Argon2's BLAKE2-like compression function.  *>
fn void _g(ulong* a, ulong *b, ulong* c, ulong* d) @local @inline
{
	*a += *b + 2ul * (uint)*a * (uint)*b;
	*d ^= *a;
	*d = (*d).rotr(32);
	*c += *d + 2ul * (uint)*c * (uint)*d;
	*b ^= *c;
	*b = (*b).rotr(24);
	*a += *b + 2ul * (uint)*a * (uint)*b;
	*d ^= *a;
	*d = (*d).rotr(16);
	*c += *d + 2ul * (uint)*c * (uint)*d;
	*b ^= *c;
	*b = (*b).rotr(63);
}

<* Apply a set of compression rounds to permute an `Argon2Block` in memory. *>
fn void Argon2Block.permute(&self, ulong[16] v) @inline
{
	_g(&self.q[v[0]], &self.q[v[4]], &self.q[v[8]],  &self.q[v[12]]);
	_g(&self.q[v[1]], &self.q[v[5]], &self.q[v[9]],  &self.q[v[13]]);
	_g(&self.q[v[2]], &self.q[v[6]], &self.q[v[10]], &self.q[v[14]]);
	_g(&self.q[v[3]], &self.q[v[7]], &self.q[v[11]], &self.q[v[15]]);
	_g(&self.q[v[0]], &self.q[v[5]], &self.q[v[10]], &self.q[v[15]]);
	_g(&self.q[v[1]], &self.q[v[6]], &self.q[v[11]], &self.q[v[12]]);
	_g(&self.q[v[2]], &self.q[v[7]], &self.q[v[8]],  &self.q[v[13]]);
	_g(&self.q[v[3]], &self.q[v[4]], &self.q[v[9]],  &self.q[v[14]]);
}

<* The core compression function used by Argon2. *>
fn Argon2Block* Argon2Block.compress(&self) @noinline
{
	$for ulong $i = 0; $i < 128; $i += 16:
		ulong[16] $indices = {};
		$foreach $x, $c : $indices: $indices[$x] = $i + $x; $endforeach
		self.permute($indices);
	$endfor
	$for ulong $i = 0; $i < 16; $i += 2:
		ulong[16] $indices = { 0, 1, 16, 17, 32, 33, 48, 49, 64, 65, 80, 81, 96, 97, 112, 113 };
		$foreach $x, $c : $indices: $indices[$x] = $i + $c; $endforeach
		self.permute($indices);
	$endfor
	return self;
}

<*
 XORs this block with another and returns its pointer for method chaining.

 @param [&in] other : "The block is be XOR'd with this instance."
*>
macro Argon2Block* Argon2Block.xor_with(&self, Argon2Block* other)
{
	array::@zip_into(self.z[..], other.z[..], fn (a, b) => a ^ b);
	return self;
}

<*
 Copies another block into this one and returns the destination pointer for method chaining.

 @param [&in] other : "The block to be copied into this instance."
*>
macro Argon2Block* Argon2Block.copy(&self, Argon2Block* other)
{
	self.z[..] = other.z[..];
	return self;
}

<* An extended BLAKE2 hash function which can apply overlapping permutations to fill a digest space indefinitely. *>
fn void h_prime(char[] digest, char[] input) @local
{
	usz hash_out_len = min(blake2::SIZE_512, digest.len);
	Blake2b h @noinit;
	h.init(hash_out_len);
	h.update(@le_c((uint)digest.len));   // LE32(lengthof T)
	h.update(input);   // LE32(A)
	h.final(digest[:hash_out_len]);

	if (digest.len <= blake2::SIZE_512) return;

	// In an overlapping fashion, walk the storage buffer and place a hash of the partial
	//   previous value as the stored value of the next.
	uint r = (uint)((digest.len + 31) >> 5) - 2;
	usz in = 0;
	usz out = 32;
	for (usz i = 1; i < r; i++, in += 32, out += 32)
	{
		digest[out:blake2::SIZE_512] = blake2::b_512(digest[in:blake2::SIZE_512])[..];
	}

	hash_out_len = digest.len - (32ul * r);
	if (hash_out_len == 0) return;

	Blake2b k @noinit;
	k.init(hash_out_len);
	k.update(digest[in:blake2::SIZE_512]);
	k.final(digest[out:hash_out_len]);
}

<* Stores a thread context that's passed to each processing invocation. *>
struct Argon2WorkerContext @local
{
	Argon2Block[] blocks;
	Argon2Options* opts_ref;
	uint lane;
	uint pass;
	uint slice;
	uint num_blocks;
	uint rows;
	uint cols;
	uint blocks_per_segment;
	uint pass_offset;
	uint slice_offset;
	bool use_constant_time;
}

<*
 Worker thread process for each thread, or just called repeatedly for synchronous Argon2.

 @param [&in] in_ctx : "The current `Argon2WorkerContext` for this slice of work."
*>
fn int process_lane_slice(void* in_ctx) @local
{
	Argon2WorkerContext* ctx = (Argon2WorkerContext*)in_ctx;

	Argon2Block scratch @noinit;
	Argon2Block index_block @noinit;
	uint index_ctr = 1;
	Argon2Block* segment_start = &ctx.blocks[ctx.lane * ctx.cols + ctx.slice_offset];

	for (uint block = ctx.pass_offset; block < ctx.blocks_per_segment; block++)
	{
		Argon2Block* current = segment_start + block;
		Argon2Block* previous = (!block && !ctx.slice_offset) ? (segment_start + ctx.cols - 1) : (segment_start + block - 1);

		ulong index_seed;
		if (!ctx.use_constant_time)
		{
			index_seed = previous.q[0];
		}
		else
		{
			if (block == ctx.pass_offset || (block % 128) == 0)
			{
				mem::zero_volatile(index_block.b[..]);
				index_block.q[0] = ctx.pass;
				index_block.q[1] = ctx.lane;
				index_block.q[2] = ctx.slice;
				index_block.q[3] = ctx.num_blocks;
				index_block.q[4] = ctx.opts_ref.iterations;
				index_block.q[5] = ctx.opts_ref.algorithm;
				index_block.q[6] = index_ctr;
				index_ctr++;

				scratch.copy(&index_block);
				index_block.compress().xor_with(&scratch);

				scratch.copy(&index_block);
				index_block.compress().xor_with(&scratch);
			}
			index_seed = index_block.q[block % 128];
		}
		uint next_slice = ((ctx.slice + 1) % 4) * ctx.blocks_per_segment;
		uint window_start = !ctx.pass ? 0 : next_slice;
		uint nb_segments = !ctx.pass ? ctx.slice : 3;
		uint window_size = nb_segments * ctx.blocks_per_segment + block - 1;
		// Find reference block
		ulong j1 = (uint)index_seed;   // block selector
		ulong j2 = (uint)(index_seed >> 32);   // lane selector
		ulong x = (j1 * j1) >> 32;
		ulong y = (ulong)(window_size * x) >> 32;
		ulong z = (ulong)(window_size - 1) - y;
		ulong ref = (window_start + z) % ctx.cols;
		uint index = (uint)(j2 % ctx.rows) * ctx.cols + (uint)ref;
		Argon2Block* reference = &ctx.blocks[index];

		scratch.copy(previous).xor_with(reference);
		if (ctx.pass == 0) { current.copy(&scratch); } else { current.xor_with(&scratch); }
		scratch.compress();
		current.xor_with(&scratch);
	}

	return 0;
}

<*
 Generate a resultant Argon2 hash based on the specified options. The size of the `outbuf` result slice
 determines the byte-length of the generated output tag.

 Setting the feature flag "ARGON2_FORCE_SEQUENTIAL" at compile-time will force this function to avoid
 the use of any native threading libraries where available.

 It should be noted that it's probably not well-advised to use the temp=allocator `tmem` when
 working with this construct. You should probably just use `mem`, since this function will clean
 up after itself anyway, and the caller's allocation of `outbuf` is done independently. Nevertheless,
 a custom allocator can always be provided based on the operating environment.

 @param [&inout] allocator : "The memory allocator to use."
 @param [out] outbuf : "The destination buffer for the resultant Argon2 tag. The size of this slice determines the tag's length."
 @param [in] input : "The data to consume and hash."
 @param options : "Chosen algorithm options for computing the resultant tag. See the `Argon2Options` struct for more information."

 @require outbuf.len >= 4 && outbuf.len < uint.max : "The tag length must be at least 4 bytes and no more than (2^32)-1"
 @require input.len < uint.max : "The input data (P) length must be less than 2^32 bytes."
 @require options.salt.len < uint.max : "The salt (S) length must be less than 2^32 bytes."
 @require options.lanes > 0 && options.lanes < uint.max : "Parallelism (p) must be greater than 0 and less than 2^32."
 @require options.mem_size >= options.lanes << 3 && options.mem_size < uint.max : "Memory size (m) must be at least 8*p and less than 2^32."
 @require options.iterations > 0 && options.iterations < uint.max : "Passes (t) must be greater than 0 and less than 2^32."
*>
fn void hash_raw(Allocator allocator, char[] outbuf, char[] input, Argon2Options options)
{
	char[64 + 2 * uint.sizeof] digest @noinit @align(uint.sizeof);

	$if $feature(ARGON2_FORCE_SEQUENTIAL):
		$echo "Argon2: forcing the use of sequential lane processing.";
	$endif

	// Sec 3.2, item 1 : H_0 is a Blake2b_512 hash of a concatenation of values.
	Blake2b h0 @noinit;
	h0.init(blake2::SIZE_512);
	h0.update(@le_c(options.lanes));   // LE32(p)
	h0.update(@le_c((uint)outbuf.len));   // LE32(T)
	h0.update(@le_c(options.mem_size));   // LE32(m)
	h0.update(@le_c(options.iterations));   // LE32(t)
	h0.update(@le_c((uint)VERSION));   // LE32(v)
	h0.update(@le_c((uint)options.algorithm));   // LE32(y)
	h0.update(@le_c((uint)input.len));   // LE32(lengthof P)
	h0.update(input);   // LE32(P)
	h0.update(@le_c((uint)options.salt.len));   // LE32(lengthof S)
	h0.update(options.salt);   // LE32(S)
	h0.update(@le_c((uint)options.key.len));   // LE32(lengthof K)
	h0.update(options.key);   // LE32(K)
	h0.update(@le_c((uint)options.assoc.len));   // LE32(lengthof X)
	h0.update(options.assoc);   // LE32(X)
	h0.final(digest[:64]);   // implicitly wipes the Blake2b context

	// For p lanes, the memory is organized in a matrix B[i][j] of blocks with p rows (lanes) and q = m' / p columns.
	uint m_prime = 4ul * options.lanes * (options.mem_size / (4ul * options.lanes));
	uint blocks_per_lane = m_prime / options.lanes;
	uint blocks_per_segment = blocks_per_lane / 4;   // each lane/row is divided into 4 slices, the intersection of a slice and lane is a "segment"

	Argon2Block[] blocks = ((Argon2Block*)(allocator::alloc_array_aligned(allocator, SimdVec, m_prime * BLOCK_SIZE / SimdVec.sizeof).ptr))[:m_prime];
	defer
	{
		foreach (&b : blocks) mem::zero_volatile(b.b[..]);
		allocator::free(allocator, blocks.ptr);
	}

	for (uint lane = 0; lane < options.lanes; lane++)
	{
		for (uint j = 0; j < 2; j++)
		{
			@unaligned_store(*(uint*)(&digest[64]), @le(j), 1);
			@unaligned_store(*(uint*)(&digest[68]), @le(lane), 1);
			h_prime(blocks[lane * blocks_per_lane + j].b[..], digest[..]);
		}
	}
	mem::zero_volatile(digest[..]);

	// Build a template context for all threads to derive their context details from.
	Argon2WorkerContext template_context = {
		.opts_ref = &options,
		.num_blocks = m_prime,
		.rows = options.lanes,
		.cols = blocks_per_lane,
		.blocks_per_segment = blocks_per_segment,
		.use_constant_time = options.algorithm != ARGON2_D,
		.blocks = blocks,
	};

	// Allocate one recycled context per lane. For synchronous operations, this is just looped over.
	//   In threaded environments, each thread gets one and a `join` awaits them all to finish for the current slice.
	//
	// NOTE: A ThreadPool instance could have been used here, but that requires a compile-time constant to determine its size,
	//       and the amount of threads to start should simply match the amount of lanes.
	//
	Argon2WorkerContext[] contexts = allocator::new_array(allocator, Argon2WorkerContext, options.lanes);
	Thread[] workers = allocator::new_array(allocator, Thread, options.lanes);
	defer allocator::free_all(allocator, contexts, workers);

	// bool use_constant_time = options.algorithm != ARGON2_D;
	for (uint pass = 0; pass < options.iterations; pass++)
	{
		template_context.pass = pass;
		for (uint slice = 0; slice < 4; slice++)
		{
			template_context.slice = slice;
			template_context.pass_offset = (pass == 0 && slice == 0) ? 2 : 0;
			template_context.slice_offset = slice * blocks_per_segment;

			if (slice == 2 && options.algorithm == ARGON2_ID) template_context.use_constant_time = false;

			// Initialize each lane's processing context for this slice.
			foreach (uint i, &context : contexts)
			{
				*context = { ...template_context, .lane = i, };

				$if USE_THREADING:
					workers[i].create(&process_lane_slice, context)!!;
				$else
					process_lane_slice(context);
				$endif
			}

			$if USE_THREADING:   // all threads must join after all lane-slices have finished processing
				foreach (w : workers) w.join()!!;
			$endif
		}
	}

	// Starting from the second lane, XOR each block at the end of a lane with the block from the end of the preceding lane.
	for (usz lane = 1; lane < options.lanes; lane++)
	{
		blocks[(lane + 1) * blocks_per_lane - 1].xor_with(&blocks[lane * blocks_per_lane - 1]);
	}

	Argon2Block scratch @noinit;
	defer mem::zero_volatile(scratch.b[..]);

	scratch.copy(&blocks[^1]);
	h_prime(outbuf, scratch.b[..]);
}

<* A simple alias for `hash_raw` implicitly using the temp allocator. *>
macro thash_raw(char[] outbuf, char[] input, Argon2Options options)
	=> hash_raw(tmem, outbuf, input, options);


<*
 Encode a hash result and an `Argon2Options` struct into a PHC String Format output.
 See: https://github.com/P-H-C/phc-string-format/blob/e8fbd333dcc9a8b0843fac6b33371cf157e91a48/phc-sf-spec.md

 @param [&inout] allocator : "Which allocator to use for building the output string."
 @param [in] hash_value : "The Argon2 output hash as a raw byte array. This function handles Base64 encoding on its own."
 @param options : "The options which were used upon creating the `hash_value`."
 @param [in] key_id : "An OPTIONAL byte array representing the ID of the pre-shared secret key to use. This is application-dependent and therefore not required."

 @require hash_value.len > 0 : "The `hash_value` input must have a length greater than zero."
*>
fn String encode(Allocator allocator, char[] hash_value, Argon2Options options, char[] key_id = {})
{
	String encoded_hash = base64::encode(allocator, hash_value, padding: base64::NO_PAD);
	String encoded_salt = options.salt.len ? base64::encode(allocator, options.salt, padding: base64::NO_PAD) : {};
	String encoded_key_id = key_id.len ? base64::encode(allocator, key_id, padding: base64::NO_PAD) : {};
	String encoded_assoc = options.assoc.len ? base64::encode(allocator, options.assoc, padding: base64::NO_PAD) : {};
	defer
	{
		allocator::free(allocator, encoded_hash);
		if (encoded_salt.len) allocator::free(allocator, encoded_salt);
		if (encoded_key_id.len) allocator::free(allocator, encoded_key_id);
		if (encoded_assoc.len) allocator::free(allocator, encoded_assoc);
	}

	// This could have been the encoded  lengths plus a few hundred, but it never hurts to be more explicit.
	usz min_size =
		9   // up to "$argon2id"
		+ 6   // "$v=19$"
		+ 13   // "m=111," - up to 10 decimal integers assumed possible, per spec
		+ 13   // same for "t="
		+ 13   // we just assume this is also 10 digits possible, despite the spec saying "up to 3 digits"
		+ 7   // ",keyid="
		+ 6   // ",data="
		+ 3   // extra "$" separators
		+ encoded_hash.len + encoded_salt.len + encoded_key_id.len + encoded_assoc.len
		+ 128;   // extra padding, just in case

	// Knowing the lengths of all encoded fields, plus the general size of the other identifiers,
	//   we should be able to form a safe capacity to use for the dynamic string.
	DString outstr;
	outstr.init(allocator, capacity: min_size);
	defer outstr.free();

	outstr.append("$argon2");
	switch (options.algorithm)
	{
		case ARGON2_D:  outstr.append("d");
		case ARGON2_I:  outstr.append("i");
		case ARGON2_ID: outstr.append("id");
	}
	outstr.append("$v=19$");   // this implementation only supports V = 0x13, according to spec, no version 0x10

	// The three parameters m,t,p MUST appear in that order and are REQUIRED. keyid and data are optional.
	io::fprintf(&outstr, "m=%d,", options.mem_size)!!;
	io::fprintf(&outstr, "t=%d,", options.iterations)!!;
	io::fprintf(&outstr, "p=%d",  options.lanes)!!;

	// While the spec says 0 to 8 bytes, we let the user determine their own use-case for this field, and assume that
	//   options.key already contains an encoded form of whatever Key ID they might be using.
	if (encoded_key_id.len) io::fprintf(&outstr, ",keyid=%s", encoded_key_id)!!;
	// Assumptions carry over for associated data too.
	if (encoded_assoc.len) io::fprintf(&outstr, ",data=%s", encoded_assoc)!!;

	// Remember: a salt is an _optional_ value here.
	if (encoded_salt.len) io::fprintf(&outstr, "$%s", encoded_salt)!!;

	// Lastly, append the hash Base64.
	io::fprintf(&outstr, "$%s", encoded_hash)!!;

	// Done! Return a copy to self-contain all other allocations.
	return outstr.copy_str(allocator);
}

<* A simple alias of `encode` implicitly using the temp allocator. *>
macro tencode(char[] hash_value, Argon2Options options, char key_id = {})
	=> encode(tmem, hash_value, options, key_id: key_id);


<*
 Perform an Argon2 hashing operation and encode its result as a PHC-SF string in one go.

 @param [&inout] allocator : "Which allocator to use for building the output string."
 @param [in] input : "The data to consume and hash."
 @param options : "Chosen algorithm options for computing the resultant tag. See the `Argon2Options` struct for more information."
 @param [in] key_id : "An identifier to use for an unconveyed private key."
 @param out_size : "The output size of the intermediate hash value. This is 32 bytes by default."

 @require out_size >= 4 && out_size < usz.max : "The hash's output size must be >= 4 bytes but less than usz.max."
*>
fn String hash(Allocator allocator, char[] input, Argon2Options options, char[] key_id = {}, usz out_size = 32)
{
	char[] result = allocator::alloc_array(allocator, char, out_size);
	defer allocator::free(allocator, result);
	hash_raw(allocator, result[..], input, options);
	return encode(allocator, result[..], options, key_id: key_id);
}

<* A simple alias of `hash` implicitly using the temp allocator. *>
macro thash(char[] input, Argon2Options options, char[] key_id = {}, usz out_size = 32)
	=> hash(tmem, input, options, key_id: key_id, out_size: out_size);


<* Hash parameter key names must only use `[a-z0-9-]` characters. *>
const AsciiCharset PARAMETER_NAMES_SET = ascii::@combine_sets(ascii::ALPHA_LOWER_SET, ascii::NUMBER_SET, ascii::@create_set("-"));

<* Hash parameter values must only use `[A-Za-z0-9/+.-]` characters. *>
const AsciiCharset PARAMETER_VALUES_SET = ascii::@combine_sets(ascii::ALPHANUMERIC_SET, ascii::@create_set("/+.-"));

<* Used to determine which options were already 'seen'. *>
bitstruct SeenOptions : char @local
{
	bool m : 0;
	bool t : 1;
	bool p : 2;
	bool key_id : 3;
	bool assoc : 4;
	char _unused : 5..7;
}

<* Represents a set of decoded information from an incoming Argon2 hash string. *>
struct Argon2Decoded (Printable)
{
	<* The decoded Argon2 options destructured from the string. *>
	inline Argon2Options opts;
	<* The decoded hash value as a byte-array. *>
	char[] hash;
	<* An application-specific key identifier conveyed with the hash string's options. *>
	char[] key_id;
	<* The version value of the hash. This implementation currently only supports 0x13, but this must be parsed nonetheless. *>
	char version;
	<* Handle to the allocator used to allocate the decoded Base64 values. Only intended to be used with `self.free` calls. *>
	Allocator allocator;
}

<* Implement `Printable` for deserialized Argon2 information. *>
fn usz? Argon2Decoded.to_format(&self, Formatter* f) @dynamic
{
	return f.printf(
		"hash=%h%s,key_id=%h%s,v=%d,opts=%s",
		self.hash[:min(self.hash.len, 32)], self.hash.len > 32 ? "..." : "",
		self.key_id[:min(self.key_id.len, 8)], self.key_id.len > 8 ? "..." : "",
		self.version,
		self.opts
	);
}

<* Allows comparisons between two decoded Argon2 hash strings. *>
fn bool Argon2Decoded.eq(&self, Argon2Decoded other) @operator(==)
{
	// NOTE: do not compare allocators (obviously).
	return self.opts == other.opts && self.hash == other.hash && self.key_id == other.key_id && self.version == other.version;
}

<* Release any memory acquired by duplicating Argon2 options into this decoded structure, then zeroize. *>
fn void Argon2Decoded.free(&self)
{
	if (self.allocator == tmem) return;   // no need to do this with the temp allocator
	allocator::free_all(self.allocator, self.hash.ptr, self.salt.ptr, self.key_id.ptr, self.assoc.ptr);
	mem::zero_volatile(@as_char_view(*self));
}

<* Various descriptive faults must be defined for decoding incoming PHC String Format values. *>
faultdef
	BAD_PARAMETER_KEY,
	BAD_PARAMETER_VALUE,
	DUPLICATE_PARAMETERS,
	INVALID_ALGORITHM,
	INVALID_BASE64,
	INVALID_FORMAT,
	INVALID_ASSOC,
	INVALID_HASH_ENCODING,
	INVALID_ITERATIONS,
	INVALID_KEY_ID,
	INVALID_MEM_SIZE,
	INVALID_PARALLELISM,
	INVALID_SALT_ENCODING,
	INVALID_VERSION,
	NOT_ENOUGH_PARAMETERS,
	MISSING_ITERATIONS,
	MISSING_MEM_SIZE,
	MISSING_PARALLELISM;

<* Helper macro for marking a parameter as seen or throwing if it was already seen. *>
macro @saw(#field) @local
{
	if (#field == true) return DUPLICATE_PARAMETERS~;
	#field = true;
}

<*
 Decode a PHC String Format hash, specifically for Argon2, into usable values.

 @param [in] hash_str : "The PHC-SF hash string to decipher."

 @require hash_str.len > 0 : "The hash string cannot be empty."
*>
fn Argon2Decoded? decode(Allocator allocator, String hash_str)
{
	Argon2Decoded result;
	char[] decoded_hash = {};
	char[] decoded_salt = {};
	char[] decoded_key_id = {};
	char[] decoded_assoc = {};
	defer allocator::free_all(allocator, decoded_hash, decoded_salt, decoded_key_id, decoded_assoc);

	if (hash_str[0] != '$') return INVALID_FORMAT~;
	hash_str = hash_str[1..];

	String[] outer_tokens = hash_str.split(allocator, "$", max: 5, skip_empty: false);
	defer allocator::free(allocator, outer_tokens);

	if (outer_tokens.len < 4) return INVALID_FORMAT~;   // there must be a minimum of 4 different tokens, up to 5
	foreach (token : outer_tokens) if (token.len < 3) return INVALID_FORMAT~;

	// The first three tokens must always be the same: algorithm, version, parameters/options
	switch (outer_tokens[0])
	{
		case "argon2d":  result.opts.algorithm = ARGON2_D;
		case "argon2i":  result.opts.algorithm = ARGON2_I;
		case "argon2id": result.opts.algorithm = ARGON2_ID;
		default: return INVALID_ALGORITHM~;
	}

	if (outer_tokens[1][:2] != "v=") return INVALID_VERSION~;
	foreach (c : outer_tokens[1][2..]) if (!ascii::NUMBER_SET.contains(c)) return INVALID_VERSION~;
	result.version = (outer_tokens[1][2..].to_integer(char) ?? INVALID_VERSION~)!;

	String[] option_tokens = outer_tokens[2].split(allocator, ",", max: 5, skip_empty: false);
	defer allocator::free(allocator, option_tokens);

	if (option_tokens.len < 3) return NOT_ENOUGH_PARAMETERS~;

	SeenOptions seen;
	foreach (token : option_tokens)
	{
		if (token.len < 3) return INVALID_FORMAT~;

		String[] key_val = token.split(allocator, "=", max: 2, skip_empty: false);
		defer allocator::free(allocator, key_val);

		String key = key_val[0];
		String value = key_val[1];

		if (!key.len || !value.len) return INVALID_FORMAT~;

		// No need to validate `key` here since the switch-case below does that for us already.
		foreach (c : value) if (!PARAMETER_VALUES_SET.contains(c)) return BAD_PARAMETER_VALUE~;

		// Technically we should be asserting the order of the keys as `m,t,p,keyid,data`, but as long as all data
		//   here is being conveyed, there's not a strong reason why we shouldn't just parse it anyway.
		switch (key)
		{
			case "m":
				@saw(seen.m)!;
				foreach (c : value) if (!ascii::NUMBER_SET.contains(c)) return INVALID_MEM_SIZE~;
				result.mem_size = (value.to_integer(uint) ?? INVALID_MEM_SIZE~)!;
				if (result.mem_size < 1 || result.mem_size == uint.max) return INVALID_MEM_SIZE~;
			case "t":
				@saw(seen.t)!;
				foreach (c : value) if (!ascii::NUMBER_SET.contains(c)) return INVALID_ITERATIONS~;
				result.iterations = (value.to_integer(uint) ?? INVALID_ITERATIONS~)!;
				if (result.iterations < 1 || result.iterations == uint.max) return INVALID_ITERATIONS~;
			case "p":
				@saw(seen.p)!;
				foreach (c : value) if (!ascii::NUMBER_SET.contains(c)) return INVALID_PARALLELISM~;
				result.lanes = (value.to_integer(uint) ?? INVALID_PARALLELISM~)!;
				if (result.lanes < 1 || result.lanes > char.max) return INVALID_PARALLELISM~;
			case "keyid":
				@saw(seen.key_id)!;
				decoded_key_id = (base64::decode(allocator, value, padding: base64::NO_PAD) ?? INVALID_KEY_ID~)!;
			case "data":
				@saw(seen.assoc)!;
				decoded_assoc = (base64::decode(allocator, value, padding: base64::NO_PAD) ?? INVALID_ASSOC~)!;
			default: return BAD_PARAMETER_KEY~;
		}
	}

	if (!seen.m) return MISSING_MEM_SIZE~;
	if (!seen.t) return MISSING_ITERATIONS~;
	if (!seen.p) return MISSING_PARALLELISM~;

	// The fourth token can be either the salt or the hash value.
	usz index = 3;
	if (outer_tokens.len > 4)
	{
		decoded_salt = (base64::decode(allocator, outer_tokens[index], padding: base64::NO_PAD) ?? INVALID_SALT_ENCODING~)!;
		index++;
	}

	// The final token is always the actual resultant hash value.
	decoded_hash = (base64::decode(allocator, outer_tokens[index], padding: base64::NO_PAD) ?? INVALID_HASH_ENCODING~)!;

	// Link the passed allocator to the result object, so it can be freed correctly later with `.free()`.
	result = {
		...result,
		.allocator = allocator,
		.hash = allocator::clone_slice(allocator, decoded_hash),
		.salt = allocator::clone_slice(allocator, decoded_salt),
		.key_id = allocator::clone_slice(allocator, decoded_key_id),
		.assoc = allocator::clone_slice(allocator, decoded_assoc),
	};
	return result;
}

<* A simple alias of `decode` implicitly using the temp allocator. *>
macro tdecode(String hash_str) => decode(tmem, hash_str);


<*
 Given a hash string and a password, verify that the password is correct.

 Note that this function SHOULD NOT use the `tmem` temp allocator if the `mem_size` of
 the hash is expected to be larger than the maximum pool size.

 @param [&inout] allocator : "The allocator to use. This function should never leak."
 @param [in] hash_str : "The PHC-SF hash string to decipher."
 @param [in] password : "The password value to use when attempting to reconstruct `hash_str`."
 @param [in] key : "A secret key that's added to the hash value. This cannot legally be conveyed by the hash string."
 @param $rethrows : "When `false`, removes the optional return from the function."
*>
macro verify(Allocator allocator, String hash_str, char[] password, char[] key = {}, bool $rethrows = true)
{
	$if $rethrows:
		Argon2Decoded c = decode(allocator, hash_str)!;
	$else
		Argon2Decoded c;
		if (catch @try(c, decode(allocator, hash_str))) return false;
	$endif
	c.key = key;
	defer c.free();
	return verify_impl(&c, hash_str, password);
}

<* A simple alias of `verify` implicitly using the temp allocator. *>
macro tverify(String hash_str, char[] password, char[] key = {}, bool $rethrows = true)
	=> verify(tmem, hash_str, password, key: key, $rethrows: $rethrows);

<*
 Given a hash string and a password, verify that the password is correct.

 Note that this function SHOULD NOT use the `tmem` temp allocator if the `mem_size` of
 the hash is expected to be larger than the maximum pool size.

 @param [&in] decoded_ctx : "The decoded hash string's information"
 @param [in] hash_str : "The PHC-SF hash string to decipher."
 @param [in] password : "The password value to use when attempting to reconstruct `hash_str`."
*>
fn bool verify_impl(Argon2Decoded* decoded_ctx, String hash_str, char[] password) @local => @pool()
{
	return hash_str == thash(password, decoded_ctx.opts, out_size: decoded_ctx.hash.len);
}
