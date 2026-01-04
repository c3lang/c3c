// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::crypto::parallel_hash;

import std::crypto::keccak;
import std::crypto::cshake;
import std::crypto::keccak::nist @public;

import std::math, std::thread;

const MAX_BLOCK_SIZE = 128 * 1024 * 1024;   // 128 MiB
const MAX_THREADS = 64;

macro clamp_threads(amount) => math::clamp(amount, 1, MAX_THREADS);
macro clamp_block_size(size) => math::clamp(size, 1, MAX_BLOCK_SIZE);

macro char[*] apply_simple_cshake($security_level, char[] data) => cshake::xof($security_level, $security_level >> 2, data);

macro void @free(#allocator, #ptr)
{
	if (&#allocator.release) allocator::free(#allocator, #ptr);
}

struct ParallelHash @generic(SECURITY_LEVEL)
{
	inline CShake{SECURITY_LEVEL} c;
	Allocator* allocator;
	bool is_xof_mode;
	bool is_padded;
	usz block_size;
	usz thread_count;
	usz intermediate_xof_size;
	char[] internal;   // handle leftovers; only ever updated by a single thread (ID 0)
	usz offset;
	uint128 total_len;
}

struct WorkerThreadContext @local
{
	usz thread_id;
	void* context;
	Allocator* allocator;
	usz consume_len;
	usz blocks;
	usz xof_count;
	char[] data;
	char[] out;
}

// Set to local scope because only 128 and 256 should ever be exposed.
macro char[*] xof($security_level, $outlen_bytes, Allocator allocator, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool $is_xof = true, bool sequential = false)
{
	char [$outlen_bytes] result;
	xof_into($security_level, allocator, result[..], data, block_size, thread_count, optional_customization, $is_xof, sequential);
	return result;
}

macro char[*] txof($security_level, $outlen_bytes, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool $is_xof = true, bool sequential = false)
	=> xof($security_level, $outlen_bytes, tmem, data, block_size, thread_count, optional_customization, $is_xof, sequential);

macro hash($security_level, $outlen_bytes, Allocator allocator, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool sequential = false)
	=> xof($security_level, $outlen_bytes, allocator, data, block_size, thread_count, optional_customization, false, sequential);

macro thash($security_level, $outlen_bytes, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool sequential = false)
	=> hash($security_level, $outlen_bytes, tmem, data, block_size, thread_count, optional_customization, sequential);

macro void xof_into($security_level, Allocator allocator, char[] into, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool $is_xof = true, bool sequential = false)
{
	ParallelHash{$security_level} c @noinit;
	defer c.wipe();
	c.init(allocator, block_size, optional_customization, $is_xof, thread_count);
	c.update(data, force_sequential: sequential);
	c.final(into);
}

macro void txof_into($security_level, char[] into, char[] data, usz block_size,
	usz thread_count, String optional_customization = "", bool $is_xof = true, bool sequential = false)
	=> xof_into($security_level, tmem, into, data, block_size, thread_count, optional_customization, $ix_xof, sequential);

macro hash_into($security_level, Allocator allocator, char[] into, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool sequential = false)
	=> xof_into($security_level, allocator, into, data, block_size, thread_count, optional_customization, false, sequential);

macro thash_into($security_level, char[] into, char[] data, usz block_size,
	usz thread_count = 0, String optional_customization = "", bool sequential = false)
	=> hash_into($security_level, tmem, into, data, block_size, thread_count, optional_customization, sequential);

// The XOF variant of ParallelHash uses a `right_encode(0)` at the end of the input stream.
//   This causes the output value to be completely different and not dependent on the length of the output buffer.
//   To better understand this, or to see the reference, search for NIST SP 800-185.
struct ParallelHashXOF @generic(SECURITY_LEVEL)
{
	inline ParallelHash{SECURITY_LEVEL} ph;
}

macro void ParallelHashXOF.init(&self, Allocator allocator, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", usz thread_count = 0)
	=> self.ph.init(allocator, block_size, optional_customization, true, thread_count);

macro void ParallelHashXOF.tinit(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", usz thread_count = 0)
	=> self.init(tmem, block_size, optional_customization, thread_count);

<*
 @require block_size > 0 : "The requested block size cannot be zero"
*>
fn void ParallelHash.init(&self, Allocator allocator, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", bool is_xof_mode = false, usz thread_count = 0)
{
	if (!thread_count)   // thread count not explicitly specified
	{
		thread_count = (usz)cpu::native_cpu();
	}

	*self = {
		.allocator = &allocator,
		.is_xof_mode = is_xof_mode,
		.block_size = clamp_block_size(block_size),
		.thread_count = clamp_threads(thread_count),
		.intermediate_xof_size = SECURITY_LEVEL >> 2,
	};
	self.internal = allocator::new_array(allocator, char, self.block_size);

	self.c.init(PARALLEL_HASH, optional_customization);

	self.c.update(nist::encode_length(NistEncodingType.LEFT, self.block_size, true));
}

macro void ParallelHash.tinit(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", bool is_xof_mode = false, usz thread_count = 0)
	=> self.init(tmem, block_size, optional_customization, is_xof_mode, thread_count);

fn void ParallelHash.update(&self, char[] data, bool force_sequential = false)
{
	// note : TODO : ILLEGAL to call `update` after a first squeeze or `final`
	if (!data.len) return;
	self.total_len += data.len;

	if (self.offset > 0)
	{
		usz left = min(self.block_size - self.offset, data.len);

		if (left > 0)
		{
			self.internal[self.offset:left] = data[:left];
			self.offset += left;
			data = data[left..];

			if (self.offset < self.block_size) return;   // waiting for more or 'final'

			self.c.update(apply_simple_cshake(SECURITY_LEVEL, self.internal[..])[..]);
			self.offset = 0;

			if (!data.len) return;
		}
	}

	if (force_sequential || data.len < self.block_size * 2 || !env::NATIVE_THREADING)   // less than two blocks was received? don't thread at all
	{
		for (; data.len >= self.block_size; data = data[self.block_size..])
		{
			self.c.update(apply_simple_cshake(SECURITY_LEVEL, data[:self.block_size])[..]);
		}
	}
	else
	{
$if env::NATIVE_THREADING:
		// start by allocating the necessary threading resources, even if they won't all be used
		Thread[] workers = allocator::new_array(*self.allocator, Thread, self.thread_count);
		defer @free(*self.allocator, workers.ptr);

		WorkerThreadContext[] contexts = allocator::new_array(*self.allocator, WorkerThreadContext, self.thread_count);
		defer
		{
			foreach (ctx : contexts) if (ctx.out.ptr) @free(*self.allocator, ctx.out.ptr);   // free the returned byte array
			@free(*self.allocator, contexts.ptr);
		}

		// round-robin distribute each thread block off the data length
		//   if at any point the length is below the block size, punt it to thread 0
		usz remainder = data.len;
		for (usz i = 0; remainder >= self.block_size; i++, remainder -= self.block_size)
		{
			contexts[i % self.thread_count].consume_len += self.block_size;
		}
		// contexts[0].consume_len += remainder;

		// start each worker. threads work TOP-DOWN, meaning the workers[self.thread_count - 1] thread is handling the
		//   START of the incoming data and thread 0 is handling the tail end of it (as well as handling uneven inputs)
		for (usz i = self.thread_count; i > 0; i--)
		{
			usz to_consume = contexts[i - 1].consume_len;
			usz blocks = to_consume / self.block_size;

			contexts[i - 1] = (WorkerThreadContext){
				.thread_id = i - 1,
				.context = self,
				.allocator = self.allocator,
				.blocks = blocks,
				.xof_count = 0,
				.data = data[:to_consume],
				.out = blocks > 0 ? allocator::new_array(*self.allocator, char, self.intermediate_xof_size * blocks) : {},
			};
			workers[i - 1].create(&threaded_update{SECURITY_LEVEL}, &contexts[i - 1])!!;   // go!
			data = data[to_consume..];
		}
		foreach_r (w : workers) w.join()!!;   // await all workers
		foreach_r (&t : contexts[:self.thread_count])
		{
			// fold all produced XOF chains into the main context's XOF chain
			for (usz i = 0; i < t.blocks; i++) self.c.update(t.out[i * self.intermediate_xof_size  : self.intermediate_xof_size]);
		}

		assert(data.len == remainder, "Not all input data was accounted for; cannot proceed");
$else
		unreachable("NATIVE_THREADING not set, should not have entered this branch");
$endif
	}

	if (data.len > 0)
	{
		// there's some leftover data; hold it for the next call to 'update' or 'final'
		self.internal[:data.len] = data[..];
		self.offset = data.len;
	}
}

<*
 Process the incoming data for each thread.

 @param [&inout] arg : "The thread context to operate with"
*>
fn int threaded_update(void* arg) @local @generic(SECURITY_LEVEL) @if (env::NATIVE_THREADING)
{
	WorkerThreadContext* ctx = (WorkerThreadContext*)arg;
	ParallelHash{SECURITY_LEVEL}* hash_ctx = ctx.context;
	char[] data = ctx.data;

	if (!data.len) return 0;

	for (; data.len >= hash_ctx.block_size; data = data[hash_ctx.block_size..])
	{
		ctx.out[(ctx.xof_count++ * hash_ctx.intermediate_xof_size):hash_ctx.intermediate_xof_size]
			= apply_simple_cshake(SECURITY_LEVEL, data[:hash_ctx.block_size])[..];
	}

	return 0;
}

<*
 @require out.len > 0
*>
macro void ParallelHash.final(&self, char[] out)
{
	defer
	{
		@free(*self.allocator, self.internal.ptr);
		self.wipe();
	}
	self.squeeze(out);
}

<*
 @require out.len > 0
*>
macro void ParallelHash.squeeze(&self, char[] out)
{
	if (!self.is_padded)
	{
		if (self.offset > 0)
		{
			// process remaining buffer from Thread ID 0 first
			self.c.update(apply_simple_cshake(SECURITY_LEVEL, self.internal[:self.offset])[..]);
			self.offset = 0;
		}

		// first, pad with 'n' ceil(total_len / block_size)
		self.c.update(nist::encode_length(NistEncodingType.RIGHT, (int)math::ceil(self.total_len / (float)self.block_size), true));

		// then, pad based on XOF (0) or HASH (out.len)
		if (self.is_xof_mode)
		{
			self.c.update(nist::ZERO_RIGHT);
		}
		else
		{
			self.c.update(nist::encode_length(NistEncodingType.RIGHT, out.len));
		}

		self.is_padded = true;
	}

	self.c.final(out);
}
