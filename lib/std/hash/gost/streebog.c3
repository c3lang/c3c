// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// An implementation of Streebog-256 and Streebog-512, defined in the
//   Russian standard GOST R 34.11-2012. Also known as "GOST-12".
//
module std::hash::streebog;


constdef StreebogLength : inline uint
{
	SIZE_256 = 32,
	SIZE_512 = 64,
}

struct Streebog
{
	ulong[8]    h;
	ulong[8]    n;
	ulong[8]    s;
	ulong[8]    message;
	usz 	    index;
	usz			hash_size;
}


<*
 @require $defined(data[0]) &&& $typeof(data[0]) == char : "Input data must be a char slice, char array, or string value."
*>
macro char[*] hash(StreebogLength $hash_size, data)
{
	// TODO: The speed of this hash hinges on use of >>> SIMD instructions <<<. It would be nice to have some added.
	Streebog s @noinit;
	s.init($hash_size);
	s.update(data);
	return s.final($hash_size);
}

macro char[*] hash_256(char[] data) => hash(SIZE_256, data);
macro char[*] hash_512(char[] data) => hash(SIZE_512, data);


macro @xor_512(#x, #y, #z) @local
{
	$for var $i = 0; $i < 8; $i++:
		#z[$i] = #x[$i] ^ #y[$i];
	$endfor
}

macro @add_512(#sum, #x) @local
{
	ulong carry = 0;
	$for usz $i = 0; $i < 8; $i++:
		#sum[$i] += #x[$i] + carry;
		carry = #sum[$i] < #x[$i] ? 1 : (#sum[$i] == #x[$i] ? carry : 0);
	$endfor
}

macro @lpsx(#a, #b, #result) @local
{
	ulong[8] z @noinit;
	@xor_512(#a, #b, z);
	// Do not unroll these loops - produces far too much code at compile-time.
	for (usz i = 0; i < 8; i++)
	{
		#result[i] = TR[0][(z[0] >> (i << 3)) & 0xff];
		for (usz j = 1; j < 8; j++) #result[i] ^= TR[j][(z[j] >> (i << 3)) & 0xff];
	}
}

macro @g_n(#n, #h, #m) @local
{
	ulong[8] k_i;
	ulong[8] state;

	@lpsx(#h, #n, k_i);
	@lpsx(k_i, #m, state);
	// Do not unroll this loop - produces far too much code at compile-time.
	for (usz i = 0; i < 11; i++)
	{
		@lpsx(k_i, ITERATION_CONSTANTS[i], k_i);
		@lpsx(k_i, state, state);
	}
	@lpsx(k_i, ITERATION_CONSTANTS[11], k_i);
	@xor_512(k_i, state, state);
	@xor_512(state, #h, state);
	@xor_512(state, #m, #h);
}


macro Streebog.@stage2(&self, #m) @local
{
	@g_n(self.n, self.h, #m);
	@add_512(self.n, STAGE2_512);
	@add_512(self.s, #m);
}

macro void Streebog.init(&self, StreebogLength $hash_size)
{
	mem::zero_volatile(@as_char_view(*self)); // explicitly initialize the entire state to 0
	self.hash_size = $hash_size;
	$if $hash_size != SIZE_512:
		@as_char_view(self.h)[..] = (char[*]){ [0..63] = 0x01 }[..];
	$endif
}

fn void Streebog.update(&self, char[] data)
{
	if (self.index)
	{
		usz rest = BLOCK_SIZE - self.index;
		usz size = data.len;
		usz len = size < rest ? size : rest;

		@as_char_view(self.message)[self.index:len] = data[:len];
		self.index += size;
		if (size < rest) return;

		self.@stage2(self.message);
		data = data[rest..];
		self.index = 0;
	}
	bool aligned = 0 == (usz)data.ptr % ulong.sizeof;
	for (; data.len >= BLOCK_SIZE; data = data[BLOCK_SIZE..])
	{
		if (aligned)
		{
			self.@stage2((ulong*)data.ptr);
		}
		else
		{
			@as_char_view(self.message)[:BLOCK_SIZE] = data[:BLOCK_SIZE];
			self.@stage2(self.message);
		}
	}
	if (data.len)
	{
		self.index = data.len;
		@as_char_view(self.message)[:data.len] = data[..];
	}
}

<*
 @require $hash_size == self.hash_size : "You must use the same output hash size as was initialized with the context."
*>
macro char[*] Streebog.final(&self, StreebogLength $hash_size)
{
	char[$hash_size] result;
	self._final_private();
	defer mem::zero_volatile(@as_char_view(*self)); // implicitly clear the structure when finalized
	result[..] = @as_char_view(self.h)[(BLOCK_SIZE - $hash_size)..];
	return result;
}

fn void Streebog._final_private(&self) @local
{
	ulong[8] unprocessed_bits_count;
	usz index = self.index >> 3;
	usz shift = (self.index & 0b111) * 8;

	unprocessed_bits_count[0] = self.index * 8ul;
	self.message[index]   &= ~(ulong.max << shift);
	self.message[index++] ^= 1ul << shift;
	if (index < 8) self.message[index..] = {};

	@g_n(self.n, self.h, self.message);
	@add_512(self.n, unprocessed_bits_count);
	@add_512(self.s, self.message);
	@g_n(ZERO_512, self.h, self.n);
	@g_n(ZERO_512, self.h, self.s);
}