// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require $defined(Key{}.hash()) "No .hash function found on the key"
*>
module std::collections::map(<Key, Value>);
import std::math;

const uint DEFAULT_INITIAL_CAPACITY = 16;
const uint MAXIMUM_CAPACITY = 1u << 31;
const float DEFAULT_LOAD_FACTOR = 0.75;
const bool VALUE_IS_EQUATABLE = Value.is_eq;
const bool COPY_KEYS = types::implements_copy(Key);

struct Map
{
	Entry*[] table;
	Allocator allocator;
	uint count; // Number of elements
	uint threshold; // Resize limit
	float load_factor;
}

<*
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
fn Map* Map.new_init(&self, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.load_factor = load_factor;
	self.threshold = (uint)(capacity * load_factor);
	self.table = allocator::new_array(allocator, Entry*, capacity);
	return self;
}

<*
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
fn Map* Map.temp_init(&self, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.new_init(capacity, load_factor, allocator::temp()) @inline;
}

<*
 @param [&inout] allocator "The allocator to use"
 @require $vacount % 2 == 0 "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
macro Map* Map.new_init_from_va_kv(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	self.new_init(capacity, load_factor, allocator);
	$for (var $i = 0; $i < $vacount; $i += 2)
		self.set($vaarg[$i], $vaarg[$i+1]);
	$endfor
	return self;
}

<*
 @param [in] keys "Array of keys for the Map entries"
 @param [in] values "Array of values for the Map entries"
 @param [&inout] allocator "The allocator to use"
 @require keys.len == values.len "Both keys and values arrays must be the same length"
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
fn Map* Map.new_init_from_kv(&self, Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	self.new_init(capacity, load_factor, allocator);
	for (usz i = 0; i < keys.len; i++)
	{
		self.set(keys[i], values[i]);
	}
	return self;
}

<*
 @require $vacount % 2 == 0 "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
macro Map* Map.temp_init_from_va_kv(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.temp_init(capacity, load_factor);
	$for (var $i = 0; $i < $vacount; $i += 2)
		self.set($vaarg[$i], $vaarg[$i+1]);
	$endfor
	return self;
}

<*
 @param [in] keys "The keys for the HashMap entries"
 @param [in] values "The values for the HashMap entries"
 @param [&inout] allocator "The allocator to use"
 @require keys.len == values.len "Both keys and values arrays must be the same length"
 @require capacity > 0 "The capacity must be 1 or higher"
 @require load_factor > 0.0 "The load factor must be higher than 0"
 @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
*>
fn Map* Map.temp_init_from_kv(&self, Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	self.temp_init(capacity, load_factor);
	for (usz i = 0; i < keys.len; i++)
	{
		self.set(keys[i], values[i]);
	}
	return self;
}

<*
 @param [&in] other_map "The self to copy from."
*>
fn Map* Map.new_init_from_map(&self, Map* other_map, Allocator allocator = allocator::heap())
{
	self.new_init(other_map.table.len, other_map.load_factor, allocator);
	if (!other_map.count) return self;
	foreach (Entry* e : other_map.table)
	{
		while (e)
		{
			self._put_for_create(e.key, e.value);
			e = e.next;
		}
	}
	return self;
}

<*
 @param [&in] other_map "The self to copy from."
*>
fn Map* Map.temp_init_from_map(&self, Map* other_map)
{
	return self.new_init_from_map(other_map, allocator::temp()) @inline;
}

fn bool Map.is_empty(&self) @inline
{
	return !self.count;
}

fn usz Map.len(&self) @inline
{
	return self.count;
}

fn Value*! Map.get_ref(&self, Key key)
{
	if (!self.count) return SearchResult.MISSING?;
	uint hash = _rehash(key.hash());
	for (Entry *e = self.table[_index_for(hash, self.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return &e.value;
	}
	return SearchResult.MISSING?;
}

fn Entry*! Map.get_entry(&self, Key key)
{
	if (!self.count) return SearchResult.MISSING?;
	uint hash = _rehash(key.hash());
	for (Entry *e = self.table[_index_for(hash, self.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e;
	}
	return SearchResult.MISSING?;
}

<*
 Get the value or update and
 @require $assignable(#expr, Value)
*>
macro Value Map.@get_or_set(&self, Key key, Value #expr)
{
	if (!self.count)
	{
		Value val = #expr;
		self.set(key, val);
		return val;
	}
	uint hash = _rehash(key.hash());
	uint index = _index_for(hash, self.table.len);
	for (Entry *e = self.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e.value;
	}
	Value val = #expr;
	self.add_entry(hash, key, val, index);
	return val;
}

fn Value! Map.get(&self, Key key) @operator([])
{
	return *self.get_ref(key) @inline;
}

fn bool Map.has_key(&self, Key key)
{
	return @ok(self.get_ref(key));
}

fn Value Map.set_value_return(&self, Key key, Value value) @operator([]=)
{
	self.set(key, value);
	return value;
}

fn bool Map.set(&self, Key key, Value value)
{
	uint hash = _rehash(key.hash());
	uint index = _index_for(hash, self.table.len);
	for (Entry *e = self.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return true;
		}
	}
	self._add_entry(hash, key, value, index);
	return false;
}

fn void! Map.remove(&self, Key key) @maydiscard
{
	if (!self._remove_entry_for_key(key)) return SearchResult.MISSING?;
}

fn void Map.clear(&self)
{
	if (!self.count) return;
	foreach (Entry** &entry_ref : self.table)
	{
		Entry* entry = *entry_ref;
		if (!entry) continue;
		Entry *next = entry.next;
		while (next)
		{
			Entry *to_delete = next;
			next = next.next;
			self._free_entry(to_delete);
		}
		self._free_entry(entry);
		*entry_ref = null;
	}
	self.count = 0;
}

fn void Map.free(&self)
{
	self.clear();
	self._free_internal(self.table.ptr);
	self.table = {};
}

fn Key[] Map.new_keys_array(&self, Allocator allocator = allocator::heap())
{
	Key[] list = allocator::alloc_array(allocator, Key, self.count);
	usz index = 0;
	foreach (Entry* entry : self.table)
	{
		while (entry)
		{
			list[index++] = entry.key;
			entry = entry.next;
		}
	}
	return list;
}

fn Key[] Map.temp_keys_array(&self)
{
	return self.new_keys_array(allocator::temp()) @inline;
}

fn Value[] Map.new_vals_array(&self, Allocator allocator = allocator::heap())
{
	Value[] list = allocator::alloc_array(allocator, Value, self.count);
	usz index = 0;
	foreach (Entry* entry : self.table)
	{
		while (entry)
		{
			list[index++] = entry.value;
			entry = entry.next;
		}
	}
	return list;
}

fn Value[] Map.temp_vals_array(&self)
{
	return self.new_vals_array(allocator::temp()) @inline;
}

macro Map.@each(&self; @body(key, value))
{
	self.@each_entry(; Entry* entry) {
		@body(entry.key, entry.value);
	};
}

macro Map.@each_entry(&self; @body(entry))
{
	foreach (Entry* entry : self.table)
	{
		while (entry)
		{
			@body(entry);
			entry = entry.next;
		}
	}
}

fn bool Map.has_value(&self, Value v) @if(VALUE_IS_EQUATABLE)
{
	foreach (Entry* entry : self.table)
	{
		while (entry)
		{
			if (equals(v, entry.value)) return true;
			entry = entry.next;
		}
	}
	return false;
}

// --- private methods

fn void Map._add_entry(&self, uint hash, Key key, Value value, uint bucket_index) @private
{
	$if COPY_KEYS:
	key = key.copy(self.allocator);
	$endif
	Entry* entry = allocator::new(self.allocator, Entry, { .hash = hash, .key = key, .value = value, .next = self.table[bucket_index] });
	self.table[bucket_index] = entry;
	if (self.count++ >= self.threshold)
	{
		self._resize(self.table.len * 2);
	}
}

fn void Map._resize(&self, uint new_capacity) @private
{
	Entry*[] old_table = self.table;
	uint old_capacity = old_table.len;
	if (old_capacity == MAXIMUM_CAPACITY)
	{
		self.threshold = uint.max;
		return;
	}
	Entry*[] new_table = allocator::new_array(self.allocator, Entry*, new_capacity);
	self._transfer(new_table);
	self.table = new_table;
	self._free_internal(old_table.ptr);
	self.threshold = (uint)(new_capacity * self.load_factor);
}

fn uint _rehash(uint hash) @inline @private
{
	hash ^= (hash >> 20) ^ (hash >> 12);
	return hash ^ ((hash >> 7) ^ (hash >> 4));
}

fn uint _index_for(uint hash, uint capacity) @inline @private
{
	return hash & (capacity - 1);
}

fn void Map._transfer(&self, Entry*[] new_table) @private
{
	Entry*[] src = self.table;
	uint new_capacity = new_table.len;
	foreach (uint j, Entry *e : src)
	{
		if (!e) continue;
		do
		{
			Entry* next = e.next;
			uint i = _index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void Map._put_for_create(&self, Key key, Value value) @private
{
	uint hash = _rehash(key.hash());
	uint i = _index_for(hash, self.table.len);
	for (Entry *e = self.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return;
		}
	}
	self._create_entry(hash, key, value, i);
}

fn void Map._free_internal(&self, void* ptr) @inline @private
{
	allocator::free(self.allocator, ptr);
}

fn bool Map._remove_entry_for_key(&self, Key key) @private
{
	if (!self.count) return false;
	uint hash = _rehash(key.hash());
	uint i = _index_for(hash, self.table.len);
	Entry* prev = self.table[i];
	Entry* e = prev;
	while (e)
	{
		Entry *next = e.next;
		if (e.hash == hash && equals(key, e.key))
		{
			self.count--;
			if (prev == e)
			{
				self.table[i] = next;
			}
			else
			{
				prev.next = next;
			}
			self._free_entry(e);
			return true;
		}
		prev = e;
		e = next;
	}
	return false;
}

fn void Map._create_entry(&self, uint hash, Key key, Value value, int bucket_index) @private
{
	Entry *e = self.table[bucket_index];
	$if COPY_KEYS:
		key = key.copy(self.allocator);
	$endif
	Entry* entry = allocator::new(self.allocator, Entry, { .hash = hash, .key = key, .value = value, .next = self.table[bucket_index] });
	self.table[bucket_index] = entry;
	self.count++;
}

fn void Map._free_entry(&self, Entry* entry) @private
{
	$if COPY_KEYS:
		allocator::free(self.allocator, entry.key);
	$endif
	self._free_internal(entry);
}

struct Entry
{
	uint hash;
	Key key;
	Value value;
	Entry* next;
}
