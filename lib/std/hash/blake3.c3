// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// This is based on the original BLAKE3 reference implementation:
//   https://github.com/BLAKE3-team/BLAKE3/blob/master
//
module std::hash::blake3;


const BLOCK_SIZE = 64;
const CHUNK_SIZE = 1024;
const KEY_SIZE = 32;
const KEY_SIZE_WORDS = KEY_SIZE / uint.sizeof;
const OUT_SIZE = 32;
const MAX_DEPTH = 54;

const uint[8] IV = {
	0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
	0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};

const char[16][7] MESSAGE_SCHEDULE = {
	x'000102030405060708090a0b0c0d0e0f',
	x'0206030a0700040d010b0c05090e0f08',
	x'03040a0c0d02070e060509000b0f0801',
	x'0a070c090e030d0f04000b0205080106',
	x'0c0d090b0f0a0e080702050300010604',
	x'090e0b05080c0f010d03000a02060407',
	x'0b0f0500010908060e0a020c0304070d',
};


enum Blake3Flags : const inline char
{
	CHUNK_START			= 1 << 0,
	CHUNK_END			= 1 << 1,
	PARENT				= 1 << 2,
	ROOT				= 1 << 3,
	KEYED_HASH			= 1 << 4,
	DERIVE_KEY_CONTEXT	= 1 << 5,
	DERIVE_KEY_MATERIAL	= 1 << 6,
}

struct Blake3ChunkState @local
{
	uint[8]				cv;
	ulong				chunk_counter;
	char[BLOCK_SIZE]	buf;
	char				buf_len;
	char				blocks_compressed;
	char				flags;
}

struct Blake3Output @local
{
	uint[KEY_SIZE_WORDS]	input_cv;
	ulong					counter;
	char[BLOCK_SIZE]		block;
	char					block_len;
	char					flags;
}

struct Blake3
{
	uint[KEY_SIZE_WORDS]				key;
	Blake3ChunkState					chunk;
	char								cv_stack_len;
	char[(MAX_DEPTH + 1) * OUT_SIZE]	cv_stack;
}


macro char[*] hash(char[] data, char[] key = {}, ulong seek = 0, usz $out_size = 32)
{
	char[$out_size] result;
	Blake3 b @noinit;
	b.init(key);
	b.update(data);
	b.final(result[..], $out_size, seek);
	return result;
}

macro char[*] ctx_hash(char[] data, char[] context, ulong seek = 0, usz $out_size = 32)
{
	char[$out_size] result;
	Blake3 b = new_from_context(context);
	b.update(data);
	b.final(result[..], $out_size, seek);
	return result;
}

macro Blake3 new_from_context(char[] context)
{
	char[KEY_SIZE] context_based_key;
	Blake3 key_from_ctx @noinit;
	key_from_ctx.init(explicit_flags: Blake3Flags.DERIVE_KEY_CONTEXT);
	key_from_ctx.update(context);
	key_from_ctx.final(context_based_key[..], KEY_SIZE);

	Blake3 b @noinit;
	b.init(key: context_based_key[..], explicit_flags: Blake3Flags.DERIVE_KEY_MATERIAL);

	mem::zero_volatile(context_based_key[..]);
	return b;
}


fn void Blake3.merge_cv_stack(&self, ulong total_len) @local @inline
{
	usz post_merge_stack_len = (usz)@popcnt(total_len);
	for (; self.cv_stack_len > post_merge_stack_len; self.cv_stack_len--)
	{
		char* parent_node = &self.cv_stack[(self.cv_stack_len - 2) * OUT_SIZE];
		Blake3Output o = parent_output(parent_node[:OUT_SIZE], self.key[..], self.chunk.flags);
		o.chaining_value(parent_node);
	}
}

fn void Blake3.push_cv(&self, char* new_cv, ulong chunk_counter) @local @inline
{
	self.merge_cv_stack(chunk_counter);
	self.cv_stack[self.cv_stack_len * OUT_SIZE : OUT_SIZE] = new_cv[:OUT_SIZE];
	self.cv_stack_len++;
}

<*
 Initialize a BLAKE3 context.

 @param[in] key : "An optional key initializer to use."

 @require !key.len || key.len == KEY_SIZE : "An explicit initialization key must be of KEY_SIZE (32 bytes)."
*>
fn void Blake3.init(&self, char[] key = {}, char explicit_flags = 0)
{
	mem::zero_volatile(@as_char_view(*self));

	if (key.len)
	{
		foreach (i, &w : self.key) *w = @unaligned_load(*(uint*)&key[i * $sizeof(self.key[0])], 1);
		if (!explicit_flags) explicit_flags = Blake3Flags.KEYED_HASH;
	}
	else
	{
		self.key[..] = IV[..];
	}

	self.chunk.init(self.key[..], explicit_flags);
}

<*
 Reset the state of the hashing context, in case it should be reused without reloading the key value.
*>
fn void Blake3.reset(&self)
{
	self.chunk.reset(self.key[..], 0);
	self.cv_stack_len = 0;
}

<*
*>
fn void Blake3.update(&self, char[] input, bool use_tbb = false)
{
	if (!input.len) return;

	if (self.chunk.len())
	{
		usz take = min(CHUNK_SIZE - self.chunk.len(), input.len);
		self.chunk.update(input[:take]);
		input = input[take..];

		if (!input.len) return;

		char[KEY_SIZE] chunk_cv;
		Blake3Output o = self.chunk.output();
		o.chaining_value(&chunk_cv);
		self.push_cv(&chunk_cv, self.chunk.chunk_counter);
		self.chunk.reset(self.key[..], self.chunk.chunk_counter + 1);
	}

	while (input.len > CHUNK_SIZE)
	{
		usz subtree_len = @round_down_to_power_of_2(input.len);
		ulong count_so_far = self.chunk.chunk_counter * CHUNK_SIZE;

		while ((((ulong)(subtree_len - 1)) & count_so_far) != 0) subtree_len /= 2;

		ulong subtree_chunks = subtree_len / CHUNK_SIZE;
		if (subtree_len <= CHUNK_SIZE)
		{
			Blake3ChunkState chunk_state;
			chunk_state.init(self.key[..], self.chunk.flags);
			chunk_state.chunk_counter = self.chunk.chunk_counter;
			chunk_state.update(input[:subtree_len]);
			Blake3Output o = chunk_state.output();
			char[OUT_SIZE] cv;
			o.chaining_value(&cv);
			self.push_cv(&cv, chunk_state.chunk_counter);
		}
		else
		{
			char[2 * OUT_SIZE] cv_pair;
			compress_subtree_to_parent_node(input[:subtree_len], self.key[..], self.chunk.chunk_counter, self.chunk.flags, cv_pair[..], use_tbb);
			self.push_cv(cv_pair[:OUT_SIZE], self.chunk.chunk_counter);
			self.push_cv(cv_pair[OUT_SIZE..], self.chunk.chunk_counter + (subtree_chunks / 2));
		}
		self.chunk.chunk_counter += subtree_chunks;
		input = input[subtree_len..];
	}

	if (input.len)
	{
		self.chunk.update(input);
		self.merge_cv_stack(self.chunk.chunk_counter);
	}
}

<*
 Yield the results of the hash into a specified output buffer, at the specified length.

 @require into.len >= into_len : "The requested output size must be equal to or less than the size of the output slice."
*>
fn void Blake3.final(&self, char[] into, usz into_len, ulong seek = 0)
{
	if (!into_len) return;
	// defer self.reset(); // implicitly reset the hash context when finalizing

	if (!self.cv_stack_len)
	{
		Blake3Output o = self.chunk.output();
		o.root_bytes(seek, into[:into_len]);
		return;
	}

	Blake3Output o;
	usz cvs_remaining;
	if (self.chunk.len() > 0)
	{
		cvs_remaining = self.cv_stack_len;
		o = self.chunk.output();
	}
	else
	{
		cvs_remaining = (usz)self.cv_stack_len - 2;
		// TODO: block or key size here?
		o = parent_output(self.cv_stack[cvs_remaining * KEY_SIZE : BLOCK_SIZE], self.key[..], self.chunk.flags);
	}

	while (cvs_remaining)
	{
		cvs_remaining--;
		char[BLOCK_SIZE] parent_block;
		parent_block[:32] = self.cv_stack[cvs_remaining * 32 : 32];
		o.chaining_value(&parent_block[32]);
		o = parent_output(parent_block[..], self.key[..], self.chunk.flags);
	}

	o.root_bytes(seek, into[:into_len]);
}


<*
 Initialize a BLAKE3 chunk state.

 @param key
 @param flags
*>
fn void Blake3ChunkState.init(&self, uint[] key, char flags) @inline
{
	mem::zero_volatile(@as_char_view(*self));
	self.cv[..] = key[..];
	self.flags = flags;
}

<*
 Reset a BLAKE3 chunk state.

 @param key
 @param chunk_counter
*>
fn void Blake3ChunkState.reset(&self, uint[] key, ulong chunk_counter) @inline
{
	self.init(key, self.flags); // maintain its own flags
	self.chunk_counter = chunk_counter; // update chunk counter
}

<*
 Get bytes length of consumed data.
*>
fn usz Blake3ChunkState.len(&self) @operator(len) @inline => (BLOCK_SIZE * (usz)self.blocks_compressed) + (usz)self.buf_len;

<*
 Ingest an amount of bytes into the chunk's buffer. NOTE: Doesn't check for underflow.

 @param[in] data : "Data to ingest."
*>
fn usz Blake3ChunkState.fill_buf(&self, char[] data) @inline
{
	usz take = min(BLOCK_SIZE - (usz)self.buf_len, data.len);
	self.buf[self.buf_len:take] = data[:take];
	self.buf_len += (char)take;
	return take;
}

<*
 Determine whether to set the CHUNK_START flag.
*>
fn char Blake3ChunkState.maybe_start_flag(&self) @inline => !self.blocks_compressed ? Blake3Flags.CHUNK_START : 0;

<*
*>
fn void Blake3ChunkState.update(&self, char[] input)
{
	if (self.buf_len)
	{
		usz take = self.fill_buf(input);
		input = input[take..];
		if (input.len)
		{
			@compress_in_place(self.cv[..], self.buf[..], BLOCK_SIZE, self.chunk_counter, self.flags | self.maybe_start_flag());
			self.blocks_compressed++;
			self.buf_len = 0;
			self.buf[..] = {};
		}
	}
	for (; input.len > BLOCK_SIZE; self.blocks_compressed++, input = input[BLOCK_SIZE..])
	{
		@compress_in_place(self.cv[..], input[:BLOCK_SIZE], BLOCK_SIZE, self.chunk_counter, self.flags | self.maybe_start_flag());
	}
	self.fill_buf(input);
}

<*
 Convert the chunk state to an "output" type with the right flags.
*>
fn Blake3Output Blake3ChunkState.output(&self) @inline
	=> make_output(self.cv[..], self.buf[:self.buf_len], self.chunk_counter, self.flags | self.maybe_start_flag() | Blake3Flags.CHUNK_END);


macro Blake3Output make_output(uint[] key, char[] in_block, ulong counter, char flags)
{
	Blake3Output o;
	o.input_cv[..] = key[..];
	o.block[:in_block.len] = in_block[..];
	o.block_len = (char)in_block.len;
	o.counter = counter;
	o.flags = flags;
	return o;
}

macro Blake3Output parent_output(char[] block, uint[] key, char flags) => make_output(key, block, 0, flags | Blake3Flags.PARENT);

fn void Blake3Output.chaining_value(&self, char* cv)
{
	// TODO: Why doesn't this just compress_in_place the 'cv' pointer itself?
	uint[KEY_SIZE_WORDS] cv_words;
	@as_char_view(cv_words)[:KEY_SIZE] = cv[:KEY_SIZE];
	@compress_in_place(cv_words[..], self.block, self.block_len, self.counter, self.flags);
	cv[:KEY_SIZE] = @as_char_view(cv_words)[:KEY_SIZE];
}

fn void Blake3Output.root_bytes(&self, ulong seek, char[] into)
{
	if (!into.len) return;

	ulong output_block_counter = seek / BLOCK_SIZE;
	usz offset_within_block = seek % BLOCK_SIZE;
	char[BLOCK_SIZE] wide_buf;

	if (offset_within_block)
	{
		@compress_xof(self.input_cv[..], self.block, self.block_len, output_block_counter, self.flags | Blake3Flags.ROOT, wide_buf[..]);
		usz avail = BLOCK_SIZE - offset_within_block;
		usz bytes = min(into.len, avail);
		into[:bytes] = wide_buf[offset_within_block:bytes];
		into = into[bytes..];
		output_block_counter++;
	}
	if (into.len / BLOCK_SIZE)
	{
		@xof_many(self.input_cv[..], self.block, self.block_len, output_block_counter, self.flags | Blake3Flags.ROOT, into, into.len / BLOCK_SIZE);
	}
	output_block_counter += into.len / 64;
	into = into[(usz)(into.len & -64ll) ..];
	if (into.len)
	{
		@compress_xof(self.input_cv[..], self.block, self.block_len, output_block_counter, self.flags | Blake3Flags.ROOT, wide_buf[..]);
		into[..] = wide_buf[:into.len];
	}
}


// =================================================================================================
// =================================================================================================
// =================================================================================================
// WELCOME TO THE COMPUTATION GARDEN...
//
//     You wanna understand BLAKE3? You gotta get through us.
//
//        ༼ ºل͟º ༼ ºل͟º ༼ ºل͟º ༽ ºل͟º ༽ ºل͟º ༽
//
//
macro uint @popcnt(ulong #x) => (uint)#x.popcount();
macro uint @highest_one(ulong #x) => 63 ^ (uint)#x.clz();
macro ulong @round_down_to_power_of_2(ulong #x) => 1ul << @highest_one(#x | 1);

macro left_subtree_len(usz input_len) => @round_down_to_power_of_2((input_len - 1) / CHUNK_SIZE) * CHUNK_SIZE;


macro @g(#state, a, b, c, d, x, y)
{
	#state[a] += #state[b] + x;
	#state[d] = (#state[d] ^ #state[a]).rotr(16);
	#state[c] += #state[d];
	#state[b] = (#state[b] ^ #state[c]).rotr(12);
	#state[a] += #state[b] + y;
	#state[d] = (#state[d] ^ #state[a]).rotr(8);
	#state[c] += #state[d];
	#state[b] = (#state[b] ^ #state[c]).rotr(7);
}

macro @round(uint[] state, uint* msg, usz round)
{
	char* schedule = &MESSAGE_SCHEDULE[round];
	@g(state, 0, 4,  8, 12, msg[schedule[0] ], msg[schedule[1] ]);
	@g(state, 1, 5,  9, 13, msg[schedule[2] ], msg[schedule[3] ]);
	@g(state, 2, 6, 10, 14, msg[schedule[4] ], msg[schedule[5] ]);
	@g(state, 3, 7, 11, 15, msg[schedule[6] ], msg[schedule[7] ]);
	@g(state, 0, 5, 10, 15, msg[schedule[8] ], msg[schedule[9] ]);
	@g(state, 1, 6, 11, 12, msg[schedule[10]], msg[schedule[11]]);
	@g(state, 2, 7,  8, 13, msg[schedule[12]], msg[schedule[13]]);
	@g(state, 3, 4,  9, 14, msg[schedule[14]], msg[schedule[15]]);
}

macro @compress_pre(uint[] state, uint[] cv, char[BLOCK_SIZE] block, usz block_len, ulong counter, char flags)
{
	uint[16] block_words @noinit;
	foreach (i, &b : block_words) *b = @unaligned_load(*(uint*)&block[i * 4], 1);
	state[0:8] = cv[0:8];
	state[8:4] = IV[0:4];
	state[12] = (uint)counter;
	state[13] = (uint)(counter >> 32);
	state[14] = (uint)block_len;
	state[15] = (uint)flags;
	@round(state, &block_words[0], 0);
	@round(state, &block_words[0], 1);
	@round(state, &block_words[0], 2);
	@round(state, &block_words[0], 3);
	@round(state, &block_words[0], 4);
	@round(state, &block_words[0], 5);
	@round(state, &block_words[0], 6);
}

macro @compress_in_place(uint[] cv, char[BLOCK_SIZE] block, usz block_len, ulong counter, char flags)
{
	uint[16] state @noinit;
	@compress_pre(state[..], cv, block, block_len, counter, flags);
	for (usz i = 0; i < 8; i++) cv[i] = state[i] ^ state[i + 8];
}

macro @compress_xof(uint[] cv, char[BLOCK_SIZE] block, usz block_len, ulong counter, char flags, char[] out)
{
	uint[16] state @noinit;
	@compress_pre(state[..], cv, block, block_len, counter, flags);
	$for usz $i = 0; $i < 8; $i++: @unaligned_store(*(uint*)&out[4 * $i], state[$i] ^ state[$i + 8], 1); $endfor
	$for usz $i = 0; $i < 8; $i++: @unaligned_store(*(uint*)&out[4 * (8 + $i)], state[$i + 8] ^ cv[$i], 1); $endfor
}

macro @xof_many(uint[] cv, char[BLOCK_SIZE] block, usz block_len, ulong counter, char flags, char[] out, usz out_blocks)
{
	for (usz i = 0; i < out_blocks; i++, out = out[BLOCK_SIZE..]) @compress_xof(cv, block, block_len, counter + i, flags, out);
}

macro hash_one(char* input, usz blocks, uint[] key, ulong counter, char flags, char flags_start, char flags_end, char[] out)
{
	uint[8] cv;
	cv[..] = key[..];
	char block_flags = flags | flags_start;
	for (; blocks > 0; input += BLOCK_SIZE, blocks--, block_flags = flags)
	{
		if (blocks == 1) block_flags |= flags_end;
		@compress_in_place(cv[..], input[:BLOCK_SIZE], BLOCK_SIZE, counter, block_flags);
	}
	foreach (i, c : cv) @unaligned_store(*(uint*)&out[i * 4], c, 1);
}

macro hash_many(char*[] inputs, usz num_inputs, usz blocks, uint[] key, ulong counter, bool increment_counter, char flags, char flags_start, char flags_end, char* out)
{
	for(; num_inputs > 0; num_inputs--, inputs = inputs[1..], out += OUT_SIZE)
	{
		hash_one(inputs[0], blocks, key, counter, flags, flags_start, flags_end, out[:OUT_SIZE]);
		if (increment_counter) counter++;
	}
}


fn void compress_subtree_to_parent_node(char[] input, uint[] key, ulong chunk_counter, char flags, char[] out, bool use_tbb) @local @noinline
{
	char[2 * OUT_SIZE] cv_array;
	usz num_cvs = compress_subtree_wide(input, key, chunk_counter, flags, cv_array[..], use_tbb);
	assert(num_cvs <= 2);
	out[..] = cv_array[:2 * OUT_SIZE];
}

fn usz compress_subtree_wide(char[] input, uint[] key, ulong chunk_counter, char flags, char* out, bool use_tbb) @local @noinline
{
	// TODO: Currently using a static '1' for the SIMD degree.
	if (input.len <= 1 * CHUNK_SIZE) return compress_chunks_parallel(input, key, chunk_counter, flags, out);

	usz left_input_len = left_subtree_len(input.len);
	usz right_input_len = input.len - left_input_len;
	char* right_input = &input[left_input_len];
	ulong right_chunk_counter = chunk_counter + (ulong)(left_input_len / CHUNK_SIZE);

	char[2 * 2 * OUT_SIZE] cv_array;
	usz degree = 1;
	if (left_input_len > CHUNK_SIZE && degree == 1) degree = 2;
	char* right_cvs = &cv_array[degree * OUT_SIZE];

	usz left_n = compress_subtree_wide(input[:left_input_len], key, chunk_counter, flags, &cv_array, use_tbb);
	usz right_n = compress_subtree_wide(right_input[:right_input_len], key, right_chunk_counter, flags, right_cvs, use_tbb);

	if (left_n == 1)
	{
		out[:2 * OUT_SIZE] = cv_array[:2 * OUT_SIZE];
		return 2;
	}

	return compress_parents_parallel(cv_array[..], left_n + right_n, key, flags, out);
}

fn usz compress_parents_parallel(char[] child_chaining_values, usz num_chaining_values, uint[] key, char flags, char* out) @local @noinline
{
	char*[2] parents_array;
	usz parents_array_len = 0;

	while (num_chaining_values - (2 * parents_array_len) >= 2)
	{
		parents_array[parents_array_len++] = &child_chaining_values[2 * parents_array_len * OUT_SIZE];
	}

	hash_many(parents_array[:parents_array_len], parents_array_len, 1, key, 0, false, flags | Blake3Flags.PARENT, 0, 0, out);

	if (num_chaining_values > 2 * parents_array_len)
	{
		out[parents_array_len * OUT_SIZE : OUT_SIZE] = child_chaining_values[2 * parents_array_len * OUT_SIZE : OUT_SIZE];
		return parents_array_len + 1;
	}

	return parents_array_len;
}

fn usz compress_chunks_parallel(char[] input, uint[] key, ulong chunk_counter, char flags, char* out) @local @noinline
{
	char*[1] chunks_array;
	usz input_position = 0;
	usz chunks_array_len = 0;

	for (; input.len - input_position >= CHUNK_SIZE; input_position += CHUNK_SIZE)
	{
		chunks_array[chunks_array_len++] = &input[input_position];
	}

	hash_many(chunks_array[:chunks_array_len], chunks_array_len, CHUNK_SIZE / BLOCK_SIZE, key, chunk_counter, true, flags, Blake3Flags.CHUNK_START, Blake3Flags.CHUNK_END, out);

	if (input.len <= input_position) return chunks_array_len;

	ulong counter = chunk_counter + (ulong)chunks_array_len;
	Blake3ChunkState chunk_state;
	chunk_state.init(key, flags);
	chunk_state.chunk_counter = counter;
	chunk_state.update(input[input_position : input.len - input_position]);
	Blake3Output o = chunk_state.output();
	o.chaining_value(&out[chunks_array_len * OUT_SIZE]);

	return chunks_array_len + 1;
}
