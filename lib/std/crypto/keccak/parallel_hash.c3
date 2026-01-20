// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//
module std::crypto::parallel_hash;

import std::math, std::thread;
import std::crypto @public;

<* The maximum allowable block size for this implementation. *>
const MAX_BLOCK_SIZE = 128 * 1024 * 1024;   // 128 MiB

<* The maximum amount of usable processing threads for this implementation. *>
const MAX_THREADS = 64;

<*
 Generate and return a very simple csHAKE computation for the input at the given security level.

 @param $security_level : "The security level to use when hashing."
 @param [in] data : "The data to hash."
*>
macro char[*] apply_simple_cshake($security_level, char[] data) @local
	=> cshake::xof($security_level, $security_level >> 2, data);

<*
 Define all context items necessary to do a ParallelHash operation, wrapping the cSHAKE construct.

 @require @in(SECURITY_LEVEL, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
*>
struct ParallelHash <SECURITY_LEVEL>
{
	inline CShake{SECURITY_LEVEL} c;
	Allocator allocator;
	bool is_xof_mode;
	bool is_padded;
	usz block_size;
	usz threads;
	usz intermediate_xof_size;
	char[] internal;   // handle leftovers; only ever updated by a single thread (ID 0)
	usz offset;
	uint128 total_len;
}

<* Define a worker context, passed to each independent processing thread for hashing operations. *>
struct WorkerThreadContext @local
{
	usz thread_id;
	void* context;
	usz consume_len;
	usz blocks;
	usz xof_count;
	char[] data;
	char[] out;
}

<*
 Generate and return an XOF ParallelHash hash of the specified `$outlen_bytes` length.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [&inout] allocator : "The allocator to use for internal memory operations."
 @param [in] data : "The data to hash."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param sequential : "If `true`, force the sequential processing of all input blocks and ignore threading entirely."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro char[*] xof($security_level, $outlen_bytes, Allocator allocator, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
{
	char [$outlen_bytes] result @noinit;
	xof_into($security_level, allocator, result[..], data, block_size, threads, customizer, sequential);
	return result;
}

<* An alias for ParallelHash's `xof` that implicitly uses the temp allocator for internal memory management. *>
macro char[*] txof($security_level, $outlen_bytes, char[] data, usz block_size,
		usz threads = 0, String customizer = "", bool sequential = false)
	=> xof($security_level, $outlen_bytes, tmem, data, block_size, threads, customizer, sequential);

<*
 Generate an XOF ParallelHash hash of `into.len` bytes and store the result in the `into` slice.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [&inout] allocator : "The allocator to use for internal memory operations."
 @param [out] into : "The storage destination for the hash result. Its length determines the amount of hash bytes yielded."
 @param [in] data : "The data to hash."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param sequential : "If `true`, force the sequential processing of all input blocks and ignore threading entirely."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro void xof_into($security_level, Allocator allocator, char[] into, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
{
	ParallelHashXOF{$security_level} c;
	c.init(allocator, block_size: block_size, threads: threads, customizer: customizer);
	c.update(data, sequential: sequential);
	c.final(into);
}

<* An alias for ParallelHash's `xof_into` that implicitly uses the temp allocator for internal memory management. *>
macro void txof_into($security_level, char[] into, char[] data, usz block_size,
	usz threads, String customizer = "", bool sequential = false)
	=> xof_into($security_level, tmem, into, data, block_size, threads, customizer, sequential);

<*
 Generate and return an non-XOF-variant ParallelHash hash of the specified `$outlen_bytes` length.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param $outlen_bytes : "The length of the XOF hash to return,"
 @param [&inout] allocator : "The allocator to use for internal memory operations."
 @param [in] data : "The data to hash."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param sequential : "If `true`, force the sequential processing of all input blocks and ignore threading entirely."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require $outlen_bytes > 0 : "You must use an output length over zero bytes."

 @return "An array of `$outlen_bytes` bytes containing the hash."
*>
macro hash($security_level, $outlen_bytes, Allocator allocator, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
{
	char [$outlen_bytes] result @noinit;
	hash_into($security_level, allocator, result[..], data, block_size, threads, customizer, sequential);
	return result;
}

<* An alias for ParallelHash's `hash` that implicitly uses the temp allocator for internal memory management. *>
macro thash($security_level, $outlen_bytes, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
	=> hash($security_level, $outlen_bytes, tmem, data, block_size, threads, customizer, sequential);

<*
 Generate an non-XOF-variant ParallelHash hash of `into.len` bytes and store the result in the `into` slice.

 @param $security_level : "The security level of the XOF to use when hashing."
 @param [&inout] allocator : "The allocator to use for internal memory operations."
 @param [out] into : "The storage destination for the hash result. Its length determines the amount of hash bytes yielded."
 @param [in] data : "The data to hash."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
 @param sequential : "If `true`, force the sequential processing of all input blocks and ignore threading entirely."

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
 @require into.len > 0 : "You must use an output length over zero bytes."
*>
macro hash_into($security_level, Allocator allocator, char[] into, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
{
	ParallelHash{$security_level} c;
	c.init(allocator, block_size: block_size, threads: threads, customizer: customizer);
	c.update(data, sequential: sequential);
	c.final(into);
}

<* An alias for ParallelHash's `hash_into` that implicitly uses the temp allocator for internal memory management. *>
macro thash_into($security_level, char[] into, char[] data, usz block_size,
	usz threads = 0, String customizer = "", bool sequential = false)
	=> hash_into($security_level, tmem, into, data, block_size, threads, customizer, sequential);

<*
 The XOF variant of ParallelHash uses a `right_encode(0)` at the end of the input stream.
 This causes the output value to be completely different and not dependent on the length of the output buffer.
 To better understand this, or to see the reference, search for NIST SP 800-185.

 @require @in($security_level, ...cshake::USABLE_LEVELS) : "Shake and Shake-derived structures must use a valid security level."
*>
struct ParallelHashXOF <SECURITY_LEVEL>
{
	inline ParallelHash{SECURITY_LEVEL} ph;
}

<*
 A wrapper initialization for the ParallelHash XOF construct which transparently calls the ParallelHash initializer.

 @param [&inout] allocator : "The allocator to use for internal hash memory management."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
*>
macro void ParallelHashXOF.init(&self, Allocator allocator, usz block_size = MAX_BLOCK_SIZE >> 3, usz threads = 0, String customizer = "")
{
	self.ph.init(allocator, block_size, threads, customizer);
	self.is_xof_mode = true;
}

<* An alias to initialize an XOF-variant hash context with the temp allocator. *>
macro void ParallelHashXOF.tinit(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String customizer = "", usz threads = 0)
	=> self.init(tmem, block_size, threads, customizer);

<*
 Initialize the primary ParallelHash context object.

 @param [&inout] allocator : "The allocator to use for internal hash memory management."
 @param block_size : "The block size in bytes to use for each processing thread."
 @param threads : "How many lanes should be used for parallel processing. If set to 0, defaults to the amount of CPU cores available."
 @param customizer : "An optional, variable-length customization string used for further domain separation."
*>
fn void ParallelHash.init(&self, Allocator allocator, usz block_size = MAX_BLOCK_SIZE >> 3, usz threads = 0, String customizer = "")
{
	threads = threads ?: (usz)cpu::native_cpu();
	bool use_xof = self.is_xof_mode;

	*self = {
		.allocator = allocator,
		.is_xof_mode = use_xof,
		.block_size = math::clamp(block_size, 1, MAX_BLOCK_SIZE),
		.threads = math::clamp(threads, 1, MAX_THREADS),
		.intermediate_xof_size = SECURITY_LEVEL >> 2,
	};
	self.internal = allocator::new_array(allocator, char, self.block_size);

	keccak::@init_runtime_size(self, $sizeof(*self));
	self.c.init(PARALLEL_HASH, customizer);

	self.c.update(nist::encode_length(LEFT, self.block_size, true));
}

<* An alias to initialize a non-XOF-variant hash context with the temp allocator. *>
macro void ParallelHash.tinit(&self, usz block_size = MAX_BLOCK_SIZE >> 3, String customizer = "", usz threads = 0)
	=> self.init(tmem, block_size, threads, customizer);

<*
 Update the ParallelHash hash context with the given data.

 @param [in] data : "The data to digest."
 @param sequential : "Whether the data should be processed sequentially; i.e., when `true`, don't use threads."
*>
fn void ParallelHash.update(&self, char[] data, bool sequential = false)
{
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

	// If fewer than two blocks were received, then don't thread at all. Same for disabled threading of course.
	//   This is a very linear process: apply cSHAKE to each block and immediately push its result into the main cSHAKE context.
	if (sequential || data.len < self.block_size * 2 || !env::NATIVE_THREADING)
	{
		for (; data.len >= self.block_size; data = data[self.block_size..])
		{
			self.c.update(apply_simple_cshake(SECURITY_LEVEL, data[:self.block_size])[..]);
		}
	}
	else
	{
		$if !env::NATIVE_THREADING:
			unreachable("NATIVE_THREADING not set, should not have entered this branch");
		$endif

		// Start by allocating the necessary threading resources, even if they won't all be used
		Thread[] workers = allocator::new_array(self.allocator, Thread, self.threads);
		defer allocator::free(self.allocator, workers);

		WorkerThreadContext[] contexts = allocator::new_array(self.allocator, WorkerThreadContext, self.threads);
		defer
		{
			foreach (ctx : contexts) if (ctx.out.ptr) allocator::free(self.allocator, ctx.out);   // free each returned byte array
			allocator::free(self.allocator, contexts);
		}

		// Round-robin distribute sizes between all thread indices.
		//   This is a bit of flair. There's probably an easier way, but luckily this isn't the performance-critical section.
		usz remainder = data.len;
		for (usz i = 0; remainder >= self.block_size; i++, remainder -= self.block_size)
		{
			contexts[i % self.threads].consume_len += self.block_size;
		}

		// Now distribute the actual work with a context for each thread.
		//   Each thread has a pool of memory `.out`, where it can store an array of its sequential XOF results
		//   This array is later folded into the main context's cSHAKE context. This needs to be done IN ORDER, which is
		//   why these are preserved during parallel processing..
		for (usz i = self.threads; i > 0; i--)
		{
			usz to_consume = contexts[i - 1].consume_len;
			usz blocks = to_consume / self.block_size;

			contexts[i - 1] = (WorkerThreadContext){
				.thread_id = i - 1,
				.context = self,
				.blocks = blocks,
				.xof_count = 0,
				.data = data[:to_consume],
				.out = blocks > 0 ? allocator::new_array(self.allocator, char, self.intermediate_xof_size * blocks) : {},
			};
			workers[i - 1].create(&threaded_update{SECURITY_LEVEL}, &contexts[i - 1])!!;   // go!
			data = data[to_consume..];
		}
		foreach_r (w : workers) w.join()!!;   // await all workers
		foreach_r (&t : contexts[:self.threads])
		{
			// Gorge `self` with all produced XOF chains, absorbing them into the main context's cSHAKE context as described.
			for (usz i = 0; i < t.blocks; i++)
			{
				self.c.update(t.out[i * self.intermediate_xof_size  : self.intermediate_xof_size]);
			}
		}

		assert(data.len == remainder, "Not all input data was accounted for; cannot proceed.");
	}

		// If there's some leftover data, then hold it for either the next call to 'update' or 'final'.
	if (data.len > 0)
	{
		self.internal[:data.len] = data[..];
		self.offset = data.len;
	}
}

<*
 Process the incoming data for each thread.

 @param [&inout] arg : "The thread context to operate with."
*>
fn int threaded_update(void* arg) <SECURITY_LEVEL> @local @if (env::NATIVE_THREADING)
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
 Override the default finalization process by squeezing the underlying sponge primitive into
 the output buffer, then freeing the internal buffer and finalizing per normal.

 @param [out] out : "The destination slice for the hash result. The slice length determines the amount of bytes yielded."

 @require out.len > 0 : "You must use an output length over zero bytes."
*>
fn void ParallelHash.final(&self, char[] out)
{
	defer
	{
		allocator::free(self.allocator, self.internal);
		self.finalize();
	}
	self.squeeze(out);
}

<*
 Override the default squeeze process for this particular hash function.
 This ensures ParallelHash can pad itself appropriately.

 @param [out] out : "The destination slice for the hash result. The slice length determines the amount of bytes yielded."

 @require out.len > 0 : "You must use an output length over zero bytes."
*>
fn void ParallelHash.squeeze(&self, char[] out)
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
		self.c.update(nist::encode_length(RIGHT, (int)math::ceil(self.total_len / (float)self.block_size), true));
		// then, pad based on XOF (0) or HASH (out.len)
		self.c.update(self.is_xof_mode ? nist::ZERO_RIGHT : nist::encode_length(RIGHT, out.len));
		self.is_padded = true;
	}
	self.c.squeeze(out);
}
