// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
<*
@require values::@is_int(F) : `F must be an integer type.`
@require values::@is_int(OUTPUT_BITS) : `OUTPUT_BITS must be an integer type.`

@require F >= 200 &&& F <= 1600 : `Invalid Keccak F; out of range.`
@require F % 200 == 0 : `Keccak F must be a multiple of 200.`
@require OUTPUT_BITS > 0 &&& OUTPUT_BITS * 2 < F &&& OUTPUT_BITS % 8 == 0 : `Invalid output bit length. Must be a multiple of 8, less than (F/2), and non-zero.`
*>
module std::crypto::keccak_internal { F, OUTPUT_BITS } @private;

import std::math;
import std::crypto::keccak @public;

const CAPACITY = OUTPUT_BITS * 2;
const BLOCK_BYTES = F / 8;
const MAX_ROUNDS = 12 + 2 * math::@intlog2(F / 25);
const RATE = BLOCK_BYTES - CAPACITY / 8;
const SPONGE_WORDS = F / 8 / ulong.sizeof;

struct KeccakContext
{
	union   // keccak sponge state
	{
		ulong[SPONGE_WORDS] sponge;
		char[SPONGE_WORDS * ulong.sizeof] sponge_bytes;
	}
	usz offset;   // tracking leftover bytes from absorption
	char[RATE] buf;   // absorption buffer
	char runtime_delimiter;
	char runtime_rounds;
	bool is_padded;
	KeccakStateMachine state;
}

<*
 @require runtime_rounds <= MAX_ROUNDS : "Amount of rounds cannot exceed MAX_ROUNDS."
*>
fn void KeccakContext.init(&self, char runtime_rounds = keccak::DEFAULT_ROUNDS, char runtime_delimiter = keccak::DEFAULT_DELIMITER) @inline
{
	*self = {
		.runtime_rounds = runtime_rounds,
		.runtime_delimiter = runtime_delimiter
	};
	self.state.transition(INITIALIZED);
}

fn void KeccakContext.update(&self, char[] data) @inline => self.absorb(data);

fn void KeccakContext.final(&self, char[] into)
{
	defer
	{
		self.wipe();
		self.state.transition(FINALIZED);
	}
	if (!self.is_padded) self.pad();
	self.squeeze(into);
}

fn void KeccakContext.wipe(&self) @inline => mem::zero_volatile(@as_char_view(*self));
macro void KeccakContext.reset(&self) => self.wipe();

fn void KeccakContext.fill_block(&self) @inline
{
	foreach(j, c : self.buf[:self.offset]) self.sponge_bytes[j] ^= c;

	self.permute_r();
	self.offset = 0;
}

fn void KeccakContext.absorb(&self, char[] input)
{
	self.state.transition(ABSORBING);
	usz i;

	if (self.offset > 0)   // handle any leftovers
	{
		usz left = min(RATE - self.offset, input.len);

		self.buf[self.offset:left] = input[:left];
		self.offset += left;

		if (left == input.len) return;   // awaiting more input before running rounds

		if (self.offset == RATE)
		{
			foreach (j, c : self.buf) self.sponge_bytes[j] ^= c;

			self.permute_r();
			self.offset = 0;
		}

		i = left;
	}

	for (; i + RATE < input.len; i += RATE)
	{
		foreach (j, c : input[i:RATE]) self.sponge_bytes[j] ^= c;

		self.permute_r();
	}

	usz left = input.len - i;
	if (left > 0) self.buf[:left] = input[i:left];

	self.offset = left;
}

fn void KeccakContext.pad(&self) @private
{
	foreach (j, c : self.buf[:self.offset]) self.sponge_bytes[j] ^= c;

	if (self.offset == RATE)
	{
		self.permute_r();
		self.offset = 0;
	}

	self.sponge_bytes[self.offset] ^= self.runtime_delimiter;
	self.sponge_bytes[RATE - 1] ^= 0x80;

	self.permute_r();
	self.offset = 0;

	self.is_padded = true;
}


fn void KeccakContext.squeeze(&self, char[] out)
{
	self.state.transition(SQUEEZING);
	usz i;

	if (self.offset == RATE)
	{
		self.permute_r();
	}
	else if (@unlikely(self.offset > 0))
	{
		char[RATE] localbuf;
		localbuf[..] = self.buf[..];

		usz left = min(RATE - self.offset, out.len);

		out[:left] = localbuf[self.offset:left];
		self.offset += left;

		if (left == out.len) return;   // awaiting more data

		if (self.offset == RATE)
		{
			self.offset = 0;
			self.permute_r();
		}

		i = left;
	}

	for (; i + RATE < out.len; i += RATE)
	{
		out[i:RATE] = self.sponge_bytes[:RATE];
		self.permute_r();
	}

	usz left = out.len - i;
	if (left > 0) out[i:left] = self.sponge_bytes[:left];

	self.offset = left;
}

macro void KeccakContext.permute_r(&self) @local
{
	usz i = keccak::RC.len - self.runtime_rounds;

	for (; i < keccak::RC.len - (keccak::RC.len % 3); i += 3)
	{
		self.round(keccak::RC[i + 0]);
		self.round(keccak::RC[i + 1]);
		self.round(keccak::RC[i + 2]);
	}

	for (; i < keccak::RC.len; ++i) self.round(keccak::RC[i]);
}

fn void KeccakContext.round(&self, ulong rc) @local
{
	ulong[5] b;
	ulong t;

	// theta
	$for var $x = 0; $x < 5; ++$x:
		b[$x] = 0;
		$for var $y = 0; $y < 25; $y += 5:
			b[$x] ^= self.sponge[$x + $y];
		$endfor
	$endfor
	$for var $x = 0; $x < 5; ++$x:
		$for var $y = 0; $y < 25; $y += 5:
			self.sponge[$x + $y] ^= b[($x + 4) % 5] ^ b[($x + 1) % 5].rotl(1);
		$endfor
	$endfor

	// rho and pi
	t = self.sponge[1];
	$for var $x = 0; $x < 24; ++$x:
		b[0] = self.sponge[keccak::PI[$x]];
		self.sponge[keccak::PI[$x]] = t.rotl(keccak::RHO[$x]);
		t = b[0];
	$endfor

	// chi
	$for var $y = 0; $y < 25; $y += 5:
		$for var $x = 0; $x < 5; ++$x:
			b[$x] = self.sponge[$y + $x];
		$endfor
		$for var $x = 0; $x < 5; ++$x:
			self.sponge[$y + $x] ^= (~b[($x + 1) % 5]) & b[($x + 2) % 5];
		$endfor
	$endfor

	// iota
	self.sponge[0] ^= rc;
}


// Declare Keccak-specific hashing contexts and methods.
module std::crypto::keccak @private;

enum KeccakState : const inline int
{
	UNINITIALIZED = 0,
	INITIALIZED,
	ABSORBING,
	SQUEEZING,
	FINALIZED,
}

typedef KeccakStateMachine @structlike = inline KeccakState;

fn void KeccakStateMachine.transition(&self, KeccakState to_state)
{
	KeccakState from_state = *self;
	if (from_state == FINALIZED) abort("Cannot change from a finalized Keccak state - use `reset` or create a new context instead.");
	switch (to_state)
	{
		case UNINITIALIZED: abort("Cannot change to an uninitialized Keccak state - use `reset` or create a new context instead.");
		case INITIALIZED:
			if (from_state != UNINITIALIZED) abort("Can only change to an initialized state from an uninitialized Keccak state.");
		case ABSORBING: // TODO: separate absorbing and updating with `pad`
			switch (from_state)
			{
				case UNINITIALIZED: abort("You must initialize a Keccak context before updating it.");
			}
		case SQUEEZING: // TODO
			switch (from_state)
			{
				case UNINITIALIZED: abort("You must initialize and update a Keccak context before squeezing it.");
				case INITIALIZED: abort("You must update a Keccak context before squeezing it.");
			}
		case FINALIZED: break;
		default: unreachable("Not a valid KeccakState");
	}
	*self = (KeccakStateMachine)to_state;
}

const ulong[24] RC = {   // round constants
	0x0000_0000_0000_0001,  0x0000_0000_0000_8082,  0x8000_0000_0000_808a,  0x8000_0000_8000_8000,
	0x0000_0000_0000_808b,  0x0000_0000_8000_0001,  0x8000_0000_8000_8081,  0x8000_0000_0000_8009,
	0x0000_0000_0000_008a,  0x0000_0000_0000_0088,  0x0000_0000_8000_8009,  0x0000_0000_8000_000a,
	0x0000_0000_8000_808b,  0x8000_0000_0000_008b,  0x8000_0000_0000_8089,  0x8000_0000_0000_8003,
	0x8000_0000_0000_8002,  0x8000_0000_0000_0080,  0x0000_0000_0000_800a,  0x8000_0000_8000_000a,
	0x8000_0000_8000_8081,  0x8000_0000_0000_8080,  0x0000_0000_8000_0001,  0x8000_0000_8000_8008,
};

const char[24] RHO = {
	1,  3,  6,  10, 15, 21,
	28, 36, 45, 55, 2,  14,
	27, 41, 56, 8,  25, 43,
	62, 18, 39, 61, 20, 44
};

const char[24] PI = {
	10, 7,  11, 17, 18, 3,
	5,  16, 8,  21, 24, 4,
	15, 23, 19, 13, 12, 2,
	20, 14, 22, 9,  6,  1
};


module std::crypto::keccak;

import std::crypto::keccak_internal @public;

const DEFAULT_F = 1600;
const DEFAULT_ROUNDS = 24;
const DEFAULT_DELIMITER = 0x01;

const usz[] SECURITY_LEVELS = { 224, 256, 384, 512 };

macro rate($security_level, $f = DEFAULT_F) @const => ($f - ($security_level * 2)) / 8;

alias hash = xof;
alias hash_into = xof_into;

macro char[*] xof($security_level, $outlen_bytes, char[] data, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER, $f = DEFAULT_F)
{
	char[$outlen_bytes] result;
	xof_into($security_level, result[..], data, $rounds, $delimiter, $f);
	return result;
}

macro void xof_into($security_level, char[] into, char[] data, $rounds = DEFAULT_ROUNDS, $delimiter = DEFAULT_DELIMITER, $f = DEFAULT_F)
{
	KeccakContext{$f, $security_level} k @noinit;
	defer k.wipe();
	k.init($rounds, $delimiter);
	k.update(data);
	k.final(into);
}

alias Keccak @generic(SECURITY_LEVEL) = KeccakContext { DEFAULT_F, SECURITY_LEVEL };
