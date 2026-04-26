// Copyright (c) 2023-2025 C3 team. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require SIZE > 0 : "The size of the bitset in bits must be at least 1"
 @require SIZE / (uint::size * 8) < uint::max
*>
module std::collections::bitset <SIZE>;

const BITS = uint::size * 8;
const SZ = (SIZE + BITS - 1) / BITS;

struct BitSet
{
	uint[SZ] data;
}

<*
 @return "The number of bits set"
*>
fn sz BitSet.cardinality(&self)
{
	sz n;
	foreach (x : self.data)
	{
		n += x.popcount();
	}
	return n;
}

<*
 Set a bit in the bitset.

 @param i : "The index to set"

 @require i < SIZE && i >= 0 : "Index was out of range"
*>
fn void BitSet.set(&self, sz i)
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	self.data[q] |= 1u << r;
}

<*
 Perform xor over all bits, mutating itself

 @param set : "The bit set to xor with"
 @return "The resulting bit set"
*>
macro BitSet BitSet.xor_self(&self, BitSet set) @operator(^=)
{
	foreach (i, &x : self.data) *x ^= set.data[i];
	return *self;
}

<*
 Perform xor over all bits, returning a new bit set.

 @param set : "The bit set to xor with"
 @return "The resulting bit set"
*>
fn BitSet BitSet.xor(&self, BitSet set) @operator(^)
{
	BitSet new_set @noinit;
	foreach (i, x : self.data) new_set.data[i] = x ^ set.data[i];
	return new_set;
}

<*
 Perform or over all bits, returning a new bit set.

 @param set : "The bit set to or with"
 @return "The resulting bit set"
*>
fn BitSet BitSet.or(&self, BitSet set) @operator(|)
{
	BitSet new_set @noinit;
	foreach (i, x : self.data) new_set.data[i] = x | set.data[i];
	return new_set;
}

<*
 Perform or over all bits, mutating itself

 @param set : "The bit set to or with"
 @return "The resulting bit set"
*>
macro BitSet BitSet.or_self(&self, BitSet set) @operator(|=)
{
	foreach (i, &x : self.data) *x |= set.data[i];
	return *self;
}

<*
 Perform & over all bits, returning a new bit set.

 @param set : "The bit set to & with"
 @return "The resulting bit set"
*>
fn BitSet BitSet.and(&self, BitSet set) @operator(&)
{
	BitSet new_set @noinit;
	foreach (i, x : self.data) new_set.data[i] = x & set.data[i];
	return new_set;
}

<*
 Perform & over all bits, mutating itself.

 @param set : "The bit set to & with"
 @return "The resulting bit set"
*>
macro BitSet BitSet.and_self(&self, BitSet set) @operator(&=)
{
	foreach (i, &x : self.data) *x &= set.data[i];
	return *self;
}

<*
 Unset (clear) a bit in the bitset.

 @param i : "The index to set"

 @require i < SIZE && i >= 0 : "Index was out of range"
*>
fn void BitSet.unset(&self, sz i)
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	self.data[q] &= ~(1u << r);
}

<*
 Get a particular bit in the bitset

 @param i : "The index of the bit"

 @require i < SIZE && i >= 0 : "Index was out of range"
 @pure
*>
fn bool BitSet.get(&self, sz i) @operator([]) @inline
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	return self.data[q] & (1u << r) != 0;
}

<*
 Return the number of bits.

 @pure
*>
fn sz BitSet.len(&self) @operator(len) @inline
{
	return SZ * BITS;
}

<*
 Change a particular bit in the bitset

 @param i : "The index of the bit"
 @param value : "The value to set the bit to"

 @require i < SIZE && i >= 0 : "Index was out of range"
*>
fn void BitSet.set_bool(&self, sz i, bool value) @operator([]=) @inline
{
	if (value) return self.set(i);
	self.unset(i);
}

<*
 @require Type::kind == UNSIGNED_INT
*>
module std::collections::growablebitset <Type>;
import std::collections::list;

const BITS = Type::size * 8;

alias GrowableBitSetList = List{Type};

struct GrowableBitSet
{
	GrowableBitSetList data;
}

<*
 @param initial_capacity : "The initial capcity of the underlying list"
 @param [&inout] allocator : "The allocator to use"

 @require initial_capacity > 0 : "The initial capacity must be at least 1"
*>
fn GrowableBitSet* GrowableBitSet.init(&self, Allocator allocator, sz initial_capacity = 1)
{
	self.data.init(allocator, initial_capacity);
	return self;
}

<*
 @param initial_capacity : "The initial capcity of the underlying list"

 @require initial_capacity > 0 : "The initial capacity must be at least 1"
*>
fn GrowableBitSet* GrowableBitSet.tinit(&self, sz initial_capacity = 1)
{
	return self.init(tmem, initial_capacity) @inline;
}

<*
 Free all memory associated with the set.
*>
fn void GrowableBitSet.free(&self)
{
	self.data.free();
}

fn sz GrowableBitSet.cardinality(&self)
{
	sz n;
	foreach (x : self.data)
	{
		n += x.popcount();
	}
	return n;
}

<*
 Set a bit in the bitset.

 @param i : "The index to set"

 @require i >= 0 : "Index was out of range"
*>
fn void GrowableBitSet.set(&self, sz i)
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	sz current_len = self.data.len();
	while (q >= current_len)
	{
		self.data.push(0);
		current_len++;
	}
	self.data.set(q, self.data[q] | (1u << r));
}

<*
 Clear a bit in the bitset.

 @param i : "The index to clear"

 @require i >= 0 : "Index was out of range"
*>
fn void GrowableBitSet.unset(&self, sz i)
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	if (q >= self.data.len()) return;
	self.data.set(q, self.data[q] &~ (1u << r));
}

fn bool GrowableBitSet.get(&self, sz i) @operator([]) @inline
{
	sz q = i / BITS;
	uint r = (uint)(i % BITS);
	if (q >= self.data.len()) return false;
	return self.data[q] & (1u << r) != 0;
}

fn sz GrowableBitSet.len(&self) @operator(len)
{
	sz n = self.data.len() * BITS;
	if (n > 0) n -= (sz)self.data[^1].clz();
	return n;
}

fn void GrowableBitSet.set_bool(&self, sz i, bool value) @operator([]=) @inline
{
	if (value) return self.set(i);
	self.unset(i);
}