// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// For more information about the various Blackman-Vigna Xorshift PRNGs, see: https://prng.di.unimi.it/
//   Also: https://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
//
module std::math::random;

import std::math::random::xoshiro @public;
import std::math::random::xoroshiro @public;

enum XorshiftType @private
{
	PLUS,
	PLUS_PLUS,
	STAR,
	STAR_STAR,
}

macro void @xorshift_seed_from_input(#xorshift_instance, sz $bits, seed)
{
	$switch $typeof(seed):
		$case char[]:
			if (!seed.len) return #xorshift_instance.set_seed_long(0);
			#xorshift_instance.state = random::make_seed($typeof(#xorshift_instance.state), seed);
		$default:
			$switch:
				$case $bits == 64:
					#xorshift_instance.long_state = seed ^ SILVER_RATIO_64;
				$case $bits == 128 &&& $defined(#xorshift_instance.long_state):
					#xorshift_instance.long_state[0] = (seed ^= SILVER_RATIO_64);
					#xorshift_instance.long_state[1] = seed + GOLDEN_RATIO_64;
				$default:
					#xorshift_instance.state[0] = (seed ^= SILVER_RATIO_64);
					foreach (&s : #xorshift_instance.state[1..]) *s = (seed += GOLDEN_RATIO_64);
			$endswitch
	$endswitch
}

alias Xoshiro128p = Xoshiro{128, XorshiftType.PLUS};
alias Xoshiro128pp = Xoshiro{128, XorshiftType.PLUS_PLUS};
alias Xoshiro128ss = Xoshiro{128, XorshiftType.STAR_STAR};

alias Xoshiro256p = Xoshiro{256, XorshiftType.PLUS};
alias Xoshiro256pp = Xoshiro{256, XorshiftType.PLUS_PLUS};
alias Xoshiro256ss = Xoshiro{256, XorshiftType.STAR_STAR};

alias Xoshiro512p = Xoshiro{512, XorshiftType.PLUS};
alias Xoshiro512pp = Xoshiro{512, XorshiftType.PLUS_PLUS};
alias Xoshiro512ss = Xoshiro{512, XorshiftType.STAR_STAR};

alias Xoroshiro64s = Xoroshiro{64, XorshiftType.STAR};
alias Xoroshiro64ss = Xoroshiro{64, XorshiftType.STAR_STAR};

alias Xoroshiro128p = Xoroshiro{128, XorshiftType.PLUS};
alias Xoroshiro128pp = Xoroshiro{128, XorshiftType.PLUS_PLUS};
alias Xoroshiro128ss = Xoroshiro{128, XorshiftType.STAR_STAR};

alias Xoroshiro1024s = Xoroshiro{1024, XorshiftType.STAR};
alias Xoroshiro1024ss = Xoroshiro{1024, XorshiftType.STAR_STAR};
alias Xoroshiro1024pp = Xoroshiro{1024, XorshiftType.PLUS_PLUS};

// =================================================================
<*
 @require @in(BIT_SIZE, 128, 256, 512) : "You must use a supported state size for Xoshiro permutations."
 @require $typeof(OPERATION) == XorshiftType && @in(OPERATION, PLUS, PLUS_PLUS, STAR_STAR) : "You must use a supported operation type for Xoshiro permutations."
*>
module std::math::random::xoshiro <BIT_SIZE, OPERATION> @private;

import std::math::random @public;

struct Xoshiro (Random)
{
	ulong[<BIT_SIZE >> 6>] state @if (BIT_SIZE != 128);
	union @if (BIT_SIZE == 128)
	{
		uint[<4>]  state;
		ulong[<2>] long_state;
	}
}

fn void Xoshiro.set_seed(&self, char[] seed) @dynamic => random::@xorshift_seed_from_input(self, BIT_SIZE, seed);
fn void Xoshiro.set_seed_long(&self, ulong seed) => random::@xorshift_seed_from_input(self, BIT_SIZE, seed);

<*
 @require bytes.len > 0
*>
fn void Xoshiro.next_bytes(&self, char[] bytes) @dynamic => random::@random_value_to_bytes(self.next, bytes);
fn uint128 Xoshiro.next_int128(&self) @dynamic => random::@long_to_int128(self.next_long());
fn ulong Xoshiro.next_long(&self) @dynamic @if (BIT_SIZE != 128) => self.next();
fn uint Xoshiro.next_int(&self) @dynamic @if (BIT_SIZE != 128) => (uint)self.next_long();
fn ulong Xoshiro.next_long(&self) @dynamic @if (BIT_SIZE == 128) => random::@int_to_long(self.next());
fn uint Xoshiro.next_int(&self) @dynamic @if (BIT_SIZE == 128) => self.next();
fn ushort Xoshiro.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Xoshiro.next_byte(&self) @dynamic => (char)self.next_long();

// NOTE: These are defined as functions in order to keep macro expansions contained.
fn ulong Xoshiro.next(&self) @if (BIT_SIZE != 128) => xoshiro(self);
fn uint Xoshiro.next(&self) @if (BIT_SIZE == 128) => xoshiro(self);

macro xoshiro(Xoshiro* self) @local
{
	$typeof(self.state[0]) result;
	$if OPERATION == STAR_STAR:   // same for all types of xoshiro
		result = (self.state[1] * 5u).rotl(7) * 9u;
	$else
		$switch BIT_SIZE:
			$case 128:
			$case 256:
				$switch OPERATION:
					$case PLUS: result = self.state[0] + self.state[3];
					$case PLUS_PLUS: result = (self.state[0] + self.state[3]).rotl(BIT_SIZE == 128 ??? 7 : 23) + self.state[0];
				$endswitch
			$case 512:
				$switch OPERATION:
					$case PLUS: result = self.state[0] + self.state[2];
					$case PLUS_PLUS: result = (self.state[0] + self.state[2]).rotl(17) + self.state[2];
				$endswitch
		$endswitch
	$endif
	$switch BIT_SIZE:
		$case 128:
		$case 256:
			sz $a = BIT_SIZE == 128 ??? 9 : 17;
			sz $b = BIT_SIZE == 128 ??? 11 : 45;
			var t = self.state[1] << $a;
			self.state[2] ^= self.state[0];
			self.state[3] ^= self.state[1];
			self.state[1] ^= self.state[2];
			self.state[0] ^= self.state[3];
			self.state[2] ^= t;
			self.state[3] = self.state[3].rotl($b);
		$case 512:
			ulong t = self.state[1] << 11;
			self.state[2] ^= self.state[0];
			self.state[5] ^= self.state[1];
			self.state[1] ^= self.state[2];
			self.state[7] ^= self.state[3];
			self.state[3] ^= self.state[4];
			self.state[4] ^= self.state[5];
			self.state[0] ^= self.state[6];
			self.state[6] ^= self.state[7];
			self.state[6] ^= t;
			self.state[7] = self.state[7].rotl(21);
	$endswitch
	return result;
}


// =================================================================
<*
 @require @in(BIT_SIZE, 64, 128, 1024) : "You must use a supported state size for Xoroshiro permutations."
 @require $typeof(OPERATION) == XorshiftType && @in(OPERATION, ...XorshiftType::values) : "OPERATION must be a value of the XorshiftType enum."
*>
module std::math::random::xoroshiro <BIT_SIZE, OPERATION> @private;

import std::math::random @public;

struct Xoroshiro (Random)
{
	ulong[<BIT_SIZE >> 6>] state @if (BIT_SIZE != 64);
	union @if (BIT_SIZE == 64)
	{
		uint[<2>]  state;
		ulong      long_state;
	}
	int p @if (BIT_SIZE == 1024);
}

fn void Xoroshiro.set_seed(&self, char[] seed) @dynamic => random::@xorshift_seed_from_input(self, BIT_SIZE, seed);
fn void Xoroshiro.set_seed_long(&self, ulong seed) => random::@xorshift_seed_from_input(self, BIT_SIZE, seed);

<*
 @require bytes.len > 0
*>
fn void Xoroshiro.next_bytes(&self, char[] bytes) @dynamic => random::@random_value_to_bytes(self.next, bytes);

fn uint128 Xoroshiro.next_int128(&self) @dynamic => random::@long_to_int128(self.next_long());
fn ulong Xoroshiro.next_long(&self) @dynamic => BIT_SIZE != 64 ??? self.next() : random::@int_to_long(self.next());
fn uint Xoroshiro.next_int(&self) @dynamic => BIT_SIZE != 64 ??? (uint)self.next_long() : self.next();
fn ushort Xoroshiro.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Xoroshiro.next_byte(&self) @dynamic => (char)self.next_long();

// NOTE: These are defined as functions in order to keep macro expansions contained.
fn ulong Xoroshiro.next(&self) @if (BIT_SIZE != 64) => xoroshiro(self);
fn uint Xoroshiro.next(&self) @if (BIT_SIZE == 64) => xoroshiro(self);

macro xoroshiro(Xoroshiro* self) @local
{
	uint a, b, c;
	$switch BIT_SIZE:
		$case 64:
			uint result;
			$switch OPERATION:
				$case STAR: result = self.state[0] * 0x9e3779bb;
				$case STAR_STAR: result = (self.state[0] * 0x9e3779bb).rotl(5) * 5u;
				$default: $error "Invalid bit-size and XorshiftType combination.";
			$endswitch
			uint s1 = self.state[1] ^ self.state[0];
			self.state[0] = self.state[0].rotl(26) ^ s1 ^ (s1 << 9);
			self.state[1] = s1.rotl(13);
			return result;
		$case 128:
			ulong result;
			ulong s1 = self.state[1] ^ self.state[0];
			$switch OPERATION:
				$case PLUS:
					result = self.state[0] + self.state[1];
					a = 24; b = 16; c = 37;
				$case PLUS_PLUS:
					result = (self.state[0] + self.state[1]).rotl(17) + self.state[0];
					a = 49; b = 21; c = 28;
				$case STAR_STAR:
					result = (self.state[0] * 5u).rotl(7u) * 9u;
					a = 24; b = 16; c = 37;
				$default: $error "Invalid bit-size and XorshiftType combination.";
			$endswitch
			self.state[0] = self.state[0].rotl(a) ^ s1 ^ (s1 << b);
			self.state[1] = s1.rotl(c);
			return result;
		$case 1024:
			ulong result;
			int q = self.p;
			self.p = (self.p + 1) & 0xf;
			ulong s0 = self.state[self.p];
			ulong s15 = self.state[q];
			$switch OPERATION:
				$case PLUS_PLUS:
					result = (s0 + s15).rotl(23) + s15;
				$case STAR:
					result = s0 * 0x9e3779b97f4a7c13;
				$case STAR_STAR:
					result = (s0 * 5u).rotl(7u) * 9u;
				$default: $error "Invalid bit-size and XorshiftType combination.";
			$endswitch
			s15 ^= s0;
			self.state[q] = s0.rotl(25u) ^ s15 ^ (s15 << 27);
			self.state[self.p] = s15.rotl(36u);
			return result;
	$endswitch
	unreachable();
}
