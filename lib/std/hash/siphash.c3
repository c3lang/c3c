// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

<*
 Best for performance-critical applications.
 See std::hash::siphash for more information.
*>
module std::hash::siphash24;
import std::hash::siphash;
alias SipHash24 = SipHash { ulong, 2, 4 };
alias hash = siphash::hash { ulong, 2, 4 };

<*
 Best for security-focused applications.
 See std::hash::siphash for more information.
*>
module std::hash::siphash48;
import std::hash::siphash;
alias SipHash48 = SipHash { ulong, 4, 8 };
alias hash = siphash::hash { ulong, 4, 8 };


<* Exact same as siphash24, but for 128-bit outputs. Algorithm internally changes slightly. *>
module std::hash::siphash24_128;
import std::hash::siphash;
alias SipHash24_128 = SipHash { uint128, 2, 4 };
alias hash = siphash::hash { uint128, 2, 4 };

<* Exact same as siphash48, but for 128-bit outputs. Algorithm internally changes slightly. *>
module std::hash::siphash48_128;
import std::hash::siphash;
alias SipHash48_128 = SipHash { uint128, 4, 8 };
alias hash = siphash::hash { uint128, 4, 8 };

<*
 SipHash is a secure pseudorandom function (PRF) which digests a 128-bit key
  and a variable-length message to produce a 64- or 128-bit hash value.

 SipHash can be employed in numerous useful ways and structures, e.g.:
  - Hash Tables
  - Message Authentication Codes
  - Denial of Service (hash flooding) resistance
  - Bloom filters
  - Keyed runtime identifier derivation

  Read more: https://en.wikipedia.org/wiki/SipHash

 @require OutType.typeid == uint128.typeid || OutType.typeid == ulong.typeid : "Module OutType must be either uint128 or ulong."
*>
module std::hash::siphash <OutType, BLOCK_ROUNDS, FINALIZE_ROUNDS>;


struct SipHash
{
	ulong[4] v;
	long m;
	int m_idx;
	usz len;
}

fn OutType hash(char[] data, uint128 key)
{
	SipHash s;
	s.init(key);
	s.update(data);
	return s.final();
}

fn void SipHash.init(&self, uint128 key)
{
	ulong[2] key_64 = bitcast(key, ulong[2]);

	self.v = {
		0x736f_6d65_7073_6575 ^ key_64[0],
		0x646f_7261_6e64_6f6d ^ key_64[1],
		0x6c79_6765_6e65_7261 ^ key_64[0],
		0x7465_6462_7974_6573 ^ key_64[1],
	};

	$if OutType.typeid == uint128.typeid :
		self.v[1] ^= 0xEE;
	$endif
}

<*
 @param [in] data : "Bytes to hash"
*>
fn void SipHash.update(&self, char[] data)
{
	self.len += data.len;

	foreach (byte : data)
	{
		self.m |= (long)byte << (self.m_idx++ << 3);

		if (self.m_idx < 8) continue;

		// This runs every time the m_idx is 8, then it resets to 0.
		self.v[3] ^= self.m;

		$for var $i = 0; $i < BLOCK_ROUNDS; ++$i :	// unrolled loop
			self.round();
		$endfor

		self.v[0] ^= self.m;

		self.m_idx = 0;
		self.m = 0;
	}
}

fn OutType SipHash.final(&self)
{
	char[8] last = { [7] = (char)self.len };

	self.update(last[(self.m_idx < 7 ? self.m_idx : 7)..]);

	$if OutType.typeid == uint128.typeid :
		self.v[2] ^= 0xEE;
	$else
		self.v[2] ^= 0xFF;
	$endif

	$for var $i = 0; $i < FINALIZE_ROUNDS; ++$i :	// unrolled loop
		self.round();
	$endfor

	$if OutType.typeid == ulong.typeid :
		return self.v[0] ^ self.v[1] ^ self.v[2] ^ self.v[3];
	$else
		ulong lo = self.v[0] ^ self.v[1] ^ self.v[2] ^ self.v[3];

		self.v[1] ^= 0xDD;

		$for var $i = 0; $i < FINALIZE_ROUNDS; ++$i :	// unrolled loop
			self.round();
		$endfor

		return lo | ((uint128)(self.v[0] ^ self.v[1] ^ self.v[2] ^ self.v[3]) << 64);
	$endif
}


fn void SipHash.round(&self) @local
{
	self.v[0] += self.v[1];
	self.v[1] = self.v[1].rotl(13);
	self.v[1] ^= self.v[0];
	self.v[0] = self.v[0].rotl(32);
	self.v[2] += self.v[3];
	self.v[3] = self.v[3].rotl(16);
	self.v[3] ^= self.v[2];
	self.v[0] += self.v[3];
	self.v[3] = self.v[3].rotl(21);
	self.v[3] ^= self.v[0];
	self.v[2] += self.v[1];
	self.v[1] = self.v[1].rotl(17);
	self.v[1] ^= self.v[2];
	self.v[2] = self.v[2].rotl(32);
}



