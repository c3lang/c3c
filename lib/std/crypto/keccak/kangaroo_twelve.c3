// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// For KT and TurboSHAKE hashes, see: https://www.rfc-editor.org/rfc/rfc9861.html
//
//   KT is based on a parallelized TurboSHAKE variant, with its own customization string
//   domain separation OUTSIDE of dependence on cSHAKE.
//
module std::crypto::kangaroo_twelve;

import std::crypto @public;

const CHAINING_DELIMITER			= 0x0b;
const FINAL_NODE_SHORT_DELIMITER	= 0x07;
const FINAL_NODE_LONG_DELIMITER		= 0x06;

<* RFC 9861 specifically uses this value as the chunk size of all inputs to create the hash tree. *>
const BLOCK_SIZE = 8192;

alias hash = xof;
alias hash_into = xof_into;

macro char[*] xof($security_level, $outlen_bytes, char[] data, String optional_customization = "")
{
	char[$outlen_bytes] result @noinit;
	xof_into($security_level, result[..], data, optional_customization);
	return result;
}

macro void xof_into($security_level, char[] into, char[] data, String optional_customization = "")
{
	KTwelve{$security_level} k;
	k.init(optional_customization);
	k.update(data, force_sequential: true);   // TODO: disable forcing option
	k.final(into);
}

struct KTwelve <SECURITY_LEVEL>
{
	inline TurboShake{SECURITY_LEVEL} t;
	char[BLOCK_SIZE] buffer;   // TODO: get this off the stack
	usz total_bytes;
	usz offset;
	bool consumed_block_s0;
	String c;
	bool is_padded;
	usz lower_bound;
}

macro void KTwelve.consume_s0_padding(&self)
{
	self.t.init(delimiter_override: FINAL_NODE_LONG_DELIMITER);
	self.t.update(self.buffer[..]);
	self.t.update(x'03 00 00 00 00 00 00 00'[..]);
	self.consumed_block_s0 = true;
}

fn void KTwelve.init(&self, String optional_customization = "")
{
	*self = { .c = optional_customization, .lower_bound = BLOCK_SIZE - optional_customization.len - 3 };
	keccak::@init_runtime_size(self, $sizeof(*self));
}

fn void KTwelve.update(&self, char[] data, bool force_sequential = false, usz thread_count = 0)
{
	if (!data.len) return;
	assert(self.total_bytes + data.len > self.total_bytes, "Counter overflow; aborting.");

	self.total_bytes += data.len;

	// EDGE CASE: If the first call to `update` is exactly BLOCK_SIZE, we cannot let that break the
	//   values consumed by the finalizer for KT. If ANY more data is incoming at this exact tipping point,
	//   then the padding string for S_0 must be consumed now.
	if (
		(!self.consumed_block_s0 && self.total_bytes > self.lower_bound)
		|| (self.lower_bound == (self.total_bytes - data.len) && self.consumed_block_s0)
	) self.consume_s0_padding();

	if (self.offset > 0)
	{
		usz left = min(BLOCK_SIZE - self.offset, data.len);
		if (left > 0)
		{
			self.buffer[self.offset:left] = data[:left];
			self.offset += left;
			data = data[left..];

			if (self.offset < BLOCK_SIZE) return; // buffer's not full yet

			if (!self.consumed_block_s0)
			{
				// Consume block S_0 without hashing it, then consume the padding string right away if more data remains.
				self.consumed_block_s0 = true;
				self.consume_s0_padding();
			}
			else
			{
				// Otherwise, we can form the chaining value and consume it with the finalizer context.
				self.t.update(turbo_shake::xof(SECURITY_LEVEL, SECURITY_LEVEL >> 2, self.buffer[..], delimiter_override: CHAINING_DELIMITER)[..]);
			}

			if (!data.len) return;
		}
	}

	if (!self.consumed_block_s0 && self.total_bytes > BLOCK_SIZE) self.consume_s0_padding();

	if (force_sequential || data.len < BLOCK_SIZE * 2 || !env::NATIVE_THREADING)
	{
		for (; data.len >= BLOCK_SIZE; data = data[BLOCK_SIZE..])
		{
			self.t.update(turbo_shake::xof(SECURITY_LEVEL, SECURITY_LEVEL >> 2, data[:BLOCK_SIZE], delimiter_override: CHAINING_DELIMITER)[..]);
		}
	}
	else
	{
$if env::NATIVE_THREADING:
		// threaded work
$else
		unreachable("NATIVE_THREADING not set, should not have entered this branch");
$endif
	}

	if (data.len > 0)
	{
		self.buffer[:data.len] = data[..];
		self.offset = data.len;
	}
}

fn void KTwelve.final(&self, char[] into)
{
	if (!self.is_padded)
	{
		// Not only do we need to confirm consumption of S_0 by itself, but we need to confirm with the total_bytes value.
		//   When more than (NOT ==) a block has been consumed, we consume the length_encode(n-1) and x'ff ff' values with the finalizer context.
		//   When <= a BLOCK_SIZE has been consumed, we just consume S_0 directly as FinalNode.
		if (self.total_bytes <= self.lower_bound)
		{
			self.t.init(delimiter_override: FINAL_NODE_SHORT_DELIMITER);
			self.t.update(self.buffer[:self.total_bytes]);
			self.t.update(self.c);
			self.t.update(!self.c.len ? x'00' : nist::encode_length(RIGHT, self.c.len, true));
		}
		else
		{
			if (self.offset > 0)
			{
				self.t.update(turbo_shake::xof(SECURITY_LEVEL, SECURITY_LEVEL >> 2, self.buffer[:self.offset], delimiter_override: CHAINING_DELIMITER)[..]);
			}
			self.t.update(nist::encode_length(RIGHT, self.total_bytes / BLOCK_SIZE, true));
			self.t.update(x'ff ff');
		}

		self.is_padded = true;
	}

	self.t.final(into);
}
