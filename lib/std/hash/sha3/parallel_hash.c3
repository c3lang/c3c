// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
<*
 @require types::is_int($typeof(SECURITY_LEVEL)) &&& (SECURITY_LEVEL == 128 ||| SECURITY_LEVEL == 256) : "SECURITY_LEVEL must be a positive integer value of 128 or 256."
*>
module std::hash::sha3::parallel_hash_internal { SECURITY_LEVEL } @private @if(env::NATIVE_THREADING);

import std::hash::sha3::keccak;
import std::hash::sha3::cshake;
import std::hash::sha3::nist @public;

import std::io;   // REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME
import std::math;
import std::thread;

const RATE = keccak::rate(SECURITY_LEVEL);
const INTERMEDIATE_XOF_SIZE = SECURITY_LEVEL >> 2;   // 128 - 32 / 256 - 64

const MAX_THREADS = 64;
macro clamp_threads(amount) => math::clamp(amount, 1, MAX_THREADS);

const MAX_BLOCK_SIZE = 128 * 1024 * 1024;   // 128 MiB
macro clamp_block_size(size) => math::clamp(size, 1, MAX_BLOCK_SIZE);

macro char[*] apply_simple_cshake(char[] data) => cshake::xof(SECURITY_LEVEL, INTERMEDIATE_XOF_SIZE, data);

struct ParallelHashContext
{
	inline CShake{SECURITY_LEVEL} c;
	bool is_xof_mode;
	bool is_padded;
	usz block_size;
	usz thread_count;
	char[] internal;   // handle leftovers; only ever updated by a single thread (ID 0)
	usz offset;
	uint128 total_len;
}

struct WorkerThreadContext @local
{
	usz thread_id;
	ParallelHashContext* context;
	Allocator* allocator;
	usz consume_len;
    usz blocks;
	usz xof_count;
	char[] data;
	char[] out;
}

<*
 @require block_size > 0 : "The requested block size cannot be zero"
*>
fn void ParallelHashContext.init(&self, Allocator allocator, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", bool is_xof_mode = false, usz thread_count = 0)
{
	if (!thread_count)   // thread count not explicitly specified
	{
		thread_count = (usz)cpu::native_cpu();
	}

	*self = {
		.is_xof_mode = is_xof_mode,
		.block_size = clamp_block_size(block_size),
		.thread_count = clamp_threads(thread_count),
	};
	self.internal = allocator::new_array(allocator, char, self.block_size);

	self.c.init(PARALLEL_HASH, optional_customization);

	self.c.update(nist::encode_length(NistEncodingType.LEFT, self.block_size, true));
}

// This is the more distinct feature of ParallelHash: inputs are domain-separated.
//
// So, calling `th.update("abc")` then `th.update("def")` produces a ***DIFFERENT HASH***
//   than calling `th.update("ab")` then `th.update("cdef")`.
//
fn void ParallelHashContext.update(&self, Allocator allocator, char[] data, bool force_sequential = false)
{
	// note : TODO : ILLEGAL to call `update` after a first squeeze or `final`

	// split incoming data into `thread_count` chunks based on the current block size.
	//   Core/Thread 0 ALWAYS receives the uneven-sized block. This is because the first call
	//   to `squeeze` or `final` will round off the `internal` buffer set by Thread 0 EXCLUSIVELY.
	//
	// The allocator is only ever used locally, and no memory will ever leak from this, since all XOF results
	//   from each thread are collapsed and free for each call to `update`.

	if (!data.len) return;

	if (@unlikely(self.offset > 0))
	{
		usz left = min(self.block_size - self.offset, data.len);

		if (left > 0)
		{
			self.offset += left;
			self.internal[self.offset:left] = data[:left];
			data = data[left..];

			if (self.offset < self.block_size) return;   // waiting for more or 'final'

			self.c.update(apply_simple_cshake(self.internal[..])[..]);
			self.offset = 0;

			if (!data.len) return;
		}
	}

	if (force_sequential || data.len < self.block_size)   // less than a block was received? don't thread at all
	{
		for (; data.len >= self.block_size; data = data[self.block_size..])
		{
			self.c.update(apply_simple_cshake(data[:self.block_size])[..]);
		}

		if (data.len > 0) self.c.update(apply_simple_cshake(data[..])[..]);

		self.total_len += data.len;
		return;
	}

	// start by allocating the necessary threading resources, even if they won't all be used
	Thread[] workers = allocator::new_array(allocator, Thread, self.thread_count);
	defer allocator::free(allocator, workers.ptr);

	WorkerThreadContext[] contexts = allocator::new_array(allocator, WorkerThreadContext, self.thread_count);
	defer
	{
		foreach (ctx : contexts) if (ctx.out.ptr) allocator::free(allocator, ctx.out.ptr);   // free the returned byte array
		allocator::free(allocator, contexts.ptr);
	}

	// round-robin distribute each thread block off the data length
	//   if at any point the length is below the block size, punt it to thread 0
	usz remainder = data.len;
	for (usz i = 0; remainder >= self.block_size; i++, remainder -= self.block_size)
	{
io::printfn("Add block to context #%d", i % self.thread_count);
		contexts[i % self.thread_count].consume_len += self.block_size;
	}
io::printfn("REM %d", remainder);
	contexts[0].consume_len += remainder;

	// start each worker. threads work TOP-DOWN, meaning the workers[self.thread_count - 1] thread is handling the
	//   START of the incoming data and thread 0 is handling the tail end of it (as well as handling uneven inputs)
	usz data_offset = 0;
	for (usz i = self.thread_count; i > 0; --i)
	{
		usz to_consume = contexts[i - 1].consume_len;
io::printfn("DATA_OFFSET %d // TO_CONSUME %d", data_offset, to_consume);
		char[] passed_data = data[data_offset:to_consume];
// foreach (c : passed_data) io::eprintf("%02x", c); io::eprintn("");

		usz blocks = (i - 1) == 0
			? ((to_consume + self.offset) / self.block_size)
			: (to_consume / self.block_size);
io::eprintfn("BLOCKS %d", blocks);

		contexts[i - 1] = (WorkerThreadContext){
			.thread_id = i - 1,
			.context = self,
			.allocator = &allocator,
			// .consume_len = 0,   DO NOT SET THIS HERE, IT WAS SET ABOVE
            .blocks = blocks,
			.xof_count = 0,
			.data = passed_data,
			.out = allocator::new_array(allocator, char, INTERMEDIATE_XOF_SIZE * blocks),
		};
io::eprintfn("NEW CONTEXT: id %d, %d bytes, %d clen, %d blocks", contexts[i-1].thread_id, contexts[i-1].data.len, to_consume, blocks);

		data_offset += to_consume;

		workers[i - 1].create(&threaded_update, &contexts[i - 1])!!;   // go!
		self.total_len += to_consume;
	}
	assert(data.len == data_offset, "Not all data was collected into the thread pool; cannot proceed");

	foreach_r (w : workers) w.join()!!;   // await all workers

	foreach_r (&t : contexts[:self.thread_count])
	{
// foreach (c : t.out) io::eprintf("%02x", c); io::eprintn("");
		// fold all produced XOF chains into the main context's XOF chain
        for (usz i = 0; i < t.blocks; i++) self.c.update(t.out[i * INTERMEDIATE_XOF_SIZE  : INTERMEDIATE_XOF_SIZE]);
	}

	if (@unlikely(remainder > 0))
	{
		// assert(ctx.thread_id == 0, "Thread IDs above 0 should NEVER have leftover/uneven data blocks to digest");

		// there's some leftover data; hold it for the next call to 'update' or 'final'
		self.internal[:remainder] = data[^remainder..];
		self.offset = remainder;
	}
}

<*
 Process the incoming data for each thread.

 @param [&inout] arg : "The thread context to operate with"
*>
fn int threaded_update(void* arg) @local
{
	WorkerThreadContext* ctx = (WorkerThreadContext*)arg;
	ParallelHashContext* hash_ctx = ctx.context;
	char[] data = ctx.data;

	if (!data.len) return 0;

	for (; data.len >= hash_ctx.block_size; data = data[hash_ctx.block_size..])
	{
		ctx.out[(ctx.xof_count++ * INTERMEDIATE_XOF_SIZE):INTERMEDIATE_XOF_SIZE] = apply_simple_cshake(data[:hash_ctx.block_size])[..];
	}

	return 0;
}

<*
 @require out.len > 0
*>
macro void ParallelHashContext.final(&self, Allocator allocator, char[] out)
{
	defer self.wipe();
	defer allocator::free(allocator, self.internal.ptr);
	self.squeeze(out);
}

<*
 @require out.len > 0
*>
macro void ParallelHashContext.squeeze(&self, char[] out)
{
	if (!self.is_padded)
	{
		if (self.offset > 0)
		{
			// process remaining buffer from Thread ID 0 first
			self.c.update(apply_simple_cshake(self.internal[:self.offset])[..]);
			self.offset = 0;
		}

		// first, pad with 'n' ceil(total_len / block_size)
		self.c.update(nist::encode_length(NistEncodingType.RIGHT, (int)math::ceil(self.total_len / (float)self.block_size), true));

		// then, pad based on XOF (0) or HASH (out.len)
		if (self.is_xof_mode)
		{
			self.c.update(nist::@ct_encode_length(NistEncodingType.RIGHT, 0));
		}
		else
		{
			self.c.update(nist::encode_length(NistEncodingType.RIGHT, out.len));
		}

		self.is_padded = true;
	}

	self.c.final(out);
}


// Names for the KMAC structures are a bit
module std::hash::sha3::parallel_hash @if(env::NATIVE_THREADING);

import std::io;   // REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME REMOVE ME

import std::hash::sha3::parallel_hash_internal @public;

const MAX_BLOCK_SIZE = 128 * 1024 * 1024;   // 128 MiB

macro typeid parallel_hash($security_level) @local @const
{
	return (ParallelHashContext { $security_level }).typeid;
}

// Set to local scope because only 128 and 256 should ever be exposed.
macro char[*] xof($security_level, $outlen_bytes, char[] data, usz block_size, usz thread_count, String optional_customization, bool $is_xof = false, bool sequential = false) @local
{
	char [$outlen_bytes] result;

	$typefrom(parallel_hash($security_level)) c;
	defer c.wipe();

	c.init(mem, block_size, optional_customization, $is_xof, thread_count);
	c.update(mem, data, force_sequential: sequential);
	c.final(mem, result[..]);

	return result;
}

alias ParallelHash128 = $typefrom(parallel_hash(128));
alias ParallelHash256 = $typefrom(parallel_hash(256));

// The XOF variant of ParallelHash uses a `right_encode(0)` at the end of the input stream.
//   This causes the output value to be completely different and not dependent on the length of the output buffer.
//   To better understand this, or to see the reference, search for NIST SP 800-185.
struct ParallelHashXOF128
{
	inline $typefrom(parallel_hash(128)) ph;
}
fn void ParallelHashXOF128.init(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", bool is_xof_mode = false, usz thread_count = 0)
	=> self.ph.init(mem, block_size, optional_customization, is_xof_mode, thread_count) @inline;

struct ParallelHashXOF256
{
	inline $typefrom(parallel_hash(256)) ph;
}
fn void ParallelHashXOF256.init(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String optional_customization = "", bool is_xof_mode = false, usz thread_count = 0)
	=> self.ph.init(mem, block_size, optional_customization, is_xof_mode, thread_count) @inline;

macro char[*] hash_128($outlen_bytes, char[] data, usz block_size = MAX_BLOCK_SIZE >> 3, usz thread_count = 0, String c = "", bool sequential = false)
	=> xof(128, $outlen_bytes, data, block_size, thread_count, c, sequential: sequential) @inline;
macro char[*] hash_256($outlen_bytes, char[] data, usz block_size = MAX_BLOCK_SIZE >> 3, usz thread_count = 0, String c = "", bool sequential = false)
	=> xof(256, $outlen_bytes, data, block_size, thread_count, c, sequential: sequential) @inline;

macro char[*] xof_128($outlen_bytes, char[] data, usz block_size = MAX_BLOCK_SIZE >> 3, usz thread_count = 0, String c = "", bool sequential = false)
	=> xof(128, $outlen_bytes, data, block_size, thread_count, c, true, sequential: sequential) @inline;
macro char[*] xof_256($outlen_bytes, char[] data, usz block_size = MAX_BLOCK_SIZE >> 3, usz thread_count = 0, String c = "", bool sequential = false)
	=> xof(256, $outlen_bytes, data, block_size, thread_count, c, true, sequential: sequential) @inline;
