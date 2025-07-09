// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Dedicated from repo: https://github.com/NotsoanoNimus/whirlpool.c3l
module std::hash::whirlpool;

import std::hash::hmac;


const BLOCK_SIZE = 64;
const HASH_SIZE = 64;
const BLOCK_128 = 64 / int128.sizeof;

struct Whirlpool
{
	ulong[8]                hash;
	union
	{
		char[BLOCK_SIZE]    block;
		int128[BLOCK_128]   block_128;
	}

	// Using these two integers with a funnel shift permits the original WHIRLPOOL 2^256 length limit.
	uint128                 counter_high;
	uint128                 counter_low;
}

alias HmacWhirlpool = Hmac { Whirlpool, HASH_SIZE, BLOCK_SIZE };
alias hmac = hmac::hash { Whirlpool, HASH_SIZE, BLOCK_SIZE };
alias pbkdf2 = hmac::pbkdf2 { Whirlpool, HASH_SIZE, BLOCK_SIZE };

fn char[HASH_SIZE] hash(char[] data)
{
	Whirlpool w;
	w.update(data);
	return w.final();
}

// Added to satisfy HMAC
macro void Whirlpool.init(&self) => *self = { };

<*
 @require data.len <= isz.max : "Update with smaller slices"
*>
fn void Whirlpool.update(&self, char[] data)
{
	char remainder = (char)self.counter_low & 0x3F;   // if not at an even BLOCK_SIZE, how many bytes are over it
	uint to_pad = BLOCK_SIZE - remainder;   // how many bytes must be filled to get the BLOCK_SIZE

	uint128 prev_ctr = self.counter_low;
	self.counter_low += data.len;

	// Handle counter rollover by just adding 1 onto 'high'. The overflow amount in 'low' remains valid.
	//   Since 'data' is limited to INT64_MAX, there are no edge cases where 'high' must be incremented by more than 1.
	//   This is so much data (> 2^128 bytes) that this will likely NEVER happen in practice...
	if (@unlikely(self.counter_low < prev_ctr)) ++self.counter_high;

	// Pad partial blocks of input data.
	if (remainder > 0)
	{
		usz span = to_pad < data.len ? to_pad : data.len;
		self.block[remainder:span] = data[:span];

		// When there wasn't enough data ingested to fill a block, just return to allow more incoming data to fill in.
		//   If the algorithm is finalized during a 'partial' block, it has its own padding to fill the remaining gap.
		if (data.len < to_pad) return;

		self.process_block(&self.block);
		data = data[to_pad..];
	}

	// Digest blocks wholesale.
	while (data.len >= BLOCK_SIZE)
	{
		self.process_block(data);

		data = (data.len > BLOCK_SIZE) ? data[BLOCK_SIZE..] : {};
	}

	// Any leftovers should be swept into the 'block' buffer in the context for the next update or for 'final'.
	if (data.len)
	{
		usz leftover_span = (BLOCK_SIZE < data.len ? BLOCK_SIZE : data.len);

		self.block[:leftover_span] = data[:leftover_span];
	}
}


fn char[HASH_SIZE] Whirlpool.final(&self)
{
	char remainder = (char)self.counter_low & 0x3F;   // if not at an even BLOCK_SIZE, how many bytes are over it

	// Terminating byte gets added to the block.
	self.block[remainder++] = 0x80;

	// When fewer than 256 bits are available to store the counter into, pad the block with zeroes and process it.
	if (remainder > 32)
	{
		if (remainder < 64) self.block[remainder..63] = 0x00;

		self.process_block(&self.block);
		remainder = 0;
	}

	// Zero out the rest of the chunk (or a new chunk if the above 'if' was true), up to byte 32.
	if (remainder < 32) self.block[remainder..31] = 0x00;

	// Insert the big-endian values of the counter as a suffix of the block.
	//   This 'counter' value is actually the number of BITS processed, hence the << 3 (or x8).
	self.block_128[2] = $$bswap(self.counter_high << 3 | (self.counter_low >> 125 & 0x07));
	self.block_128[3] = $$bswap(self.counter_low << 3);

	// Process the final block.
	self.process_block(&self.block);

	// Each ulong in the resultant hash should be bit-swapped before the final return.
	char[HASH_SIZE] hash @align(ulong.alignof);
	ulong* hash_ref = (ulong*)&hash;

	$for var $i = 0; $i < 8; ++$i :
		hash_ref[$i] = $$bswap(self.hash[$i]);
	$endfor

	// All done!
	return hash;
}


macro ulong @w_op(#src, $shift) @private
	=>    S_BOX[(0 * 256) + (int)(#src[($shift + 0) & 7] >> 56)       ]
		^ S_BOX[(1 * 256) + (int)(#src[($shift + 7) & 7] >> 48) & 0xFF]
		^ S_BOX[(2 * 256) + (int)(#src[($shift + 6) & 7] >> 40) & 0xFF]
		^ S_BOX[(3 * 256) + (int)(#src[($shift + 5) & 7] >> 32) & 0xFF]
		^ S_BOX[(4 * 256) + (int)(#src[($shift + 4) & 7] >> 24) & 0xFF]
		^ S_BOX[(5 * 256) + (int)(#src[($shift + 3) & 7] >> 16) & 0xFF]
		^ S_BOX[(6 * 256) + (int)(#src[($shift + 2) & 7] >>  8) & 0xFF]
		^ S_BOX[(7 * 256) + (int)(#src[($shift + 1) & 7] >>  0) & 0xFF];


const ulong[10] RC @private = {
	0x1823c6e887b8014f,
	0x36a6d2f5796f9152,
	0x60bc9b8ea30c7b35,
	0x1de0d7c22e4bfe57,
	0x157737e59ff04ada,
	0x58c9290ab1a06b85,
	0xbd5d10f4cb3e0567,
	0xe427418ba77d95d8,
	0xfbee7c66dd17479e,
	0xca2dbf07ad5a8333
};
const ROUNDS = 10;

fn void Whirlpool.process_block(&self, char* block) @local
{
	ulong[2 * 8] k;   // key
	ulong[2 * 8] state;   // state

	// NOTE: These loops are unrolled with C3's Chad-tier compile-time evaluation.
	$for var $round = 0; $round < 8; $round++:
		k[$round] = self.hash[$round];
		state[$round] = $$bswap(@unaligned_load(((ulong*)block)[$round], 1)) ^ self.hash[$round];
		self.hash[$round] = state[$round];
	$endfor

	$for var $round = 0; $round < ROUNDS; ++$round :
		var $m = $round % 2;

		k[(($m ^ 1) * 8) + 0] = @w_op((&k[$m * 8]), 0) ^ RC[$round];

		$for var $i = 1; $i < 8; $i++ :
			k[(($m ^ 1) * 8) + $i] = @w_op((&k[$m * 8]), $i);
		$endfor

		$for var $i = 0; $i < 8; $i++ :
			state[(($m ^ 1) * 8) + $i] = @w_op(&(state[$m * 8]), $i) ^ k[(($m ^ 1) * 8) + $i];
		$endfor
	$endfor

	$for var $x = 0; $x < 8; $x++:
		self.hash[$x] ^= state[$x];
	$endfor
}
