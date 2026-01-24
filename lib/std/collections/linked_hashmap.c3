// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require $defined((Key){}.hash()) : `No .hash function found on the key`
*>
module std::collections::map <Key, Value>;
import std::math;
import std::io @norecurse;

const LinkedHashMap LINKEDONHEAP = { .allocator = MAP_HEAP_ALLOCATOR };

struct LinkedEntry
{
	uint hash;
	Key key;
	Value value;
	<* For bucket chain *>
	LinkedEntry* next;
	<* Previous in insertion order *>
	LinkedEntry* before;
	<* Next in insertion order *>
	LinkedEntry* after;
}

struct LinkedHashMap (Printable)
{
	LinkedEntry*[] table;
	Allocator allocator;
	usz count;
	usz threshold;
	float load_factor;
	<* First inserted LinkedEntry *>
	LinkedEntry* head;
	<* Last inserted LinkedEntry *>
	LinkedEntry* tail;
}


<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.init(&self, Allocator allocator, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.load_factor = load_factor;
	self.threshold = (usz)(capacity * load_factor);
	self.table = allocator::new_array(allocator, LinkedEntry*, capacity);
	self.head = null;
	self.tail = null;
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.tinit(&self, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require $vacount % 2 == 0 : "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashMap* LinkedHashMap.init_with_key_values(&self, Allocator allocator, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vacount; $i += 2:
		self.set($vaarg[$i], $vaarg[$i + 1]);
	$endfor
	return self;
}

<*
 @require $vacount % 2 == 0 : "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashMap* LinkedHashMap.tinit_with_key_values(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_key_values(tmem, $vasplat, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] keys : "The keys for the LinkedHashMap entries"
 @param [in] values : "The values for the LinkedHashMap entries"
 @param [&inout] allocator : "The allocator to use"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.init_from_keys_and_values(&self, Allocator allocator, Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	assert(keys.len == values.len);
	self.init(allocator, capacity, load_factor);
	for (usz i = 0; i < keys.len; i++)
	{
		self.set(keys[i], values[i]);
	}
	return self;
}

<*
 @param [in] keys : "The keys for the LinkedHashMap entries"
 @param [in] values : "The values for the LinkedHashMap entries"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.tinit_from_keys_and_values(&self, Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_from_keys_and_values(tmem, keys, values, capacity, load_factor);
}

<*
 Has this hash map been initialized yet?

 @param [&in] map : "The hash map we are testing"
 @return "Returns true if it has been initialized, false otherwise"
*>
fn bool LinkedHashMap.is_initialized(&map)
{
	return map.allocator && map.allocator.ptr != &dummy;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_map : "The map to copy from."
 @require !self.is_initialized() : "Map was already initialized"
*>
fn LinkedHashMap* LinkedHashMap.init_from_map(&self, Allocator allocator, LinkedHashMap* other_map)
{
	self.init(allocator, other_map.table.len, other_map.load_factor);
	self.put_all_for_create(other_map);
	return self;
}

<*
 @param [&in] other_map : "The map to copy from."
 @require !map.is_initialized() : "Map was already initialized"
*>
fn LinkedHashMap* LinkedHashMap.tinit_from_map(&map, LinkedHashMap* other_map)
{
	return map.init_from_map(tmem, other_map) @inline;
}

fn bool LinkedHashMap.is_empty(&map) @inline
{
	return !map.count;
}

fn usz LinkedHashMap.len(&map) @inline => map.count;

fn Value*? LinkedHashMap.get_ref(&map, Key key)
{
	if (!map.count) return NOT_FOUND~;
	uint hash = rehash(key.hash());
	for (LinkedEntry *e = map.table[index_for(hash, map.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return &e.value;
	}
	return NOT_FOUND~;
}

fn LinkedEntry*? LinkedHashMap.get_entry(&map, Key key)
{
	if (!map.count) return NOT_FOUND~;
	uint hash = rehash(key.hash());
	for (LinkedEntry *e = map.table[index_for(hash, map.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e;
	}
	return NOT_FOUND~;
}

<*
 Get the value or set it to the value

 @require $defined(Value val = #expr)
*>
macro Value LinkedHashMap.@get_or_set(&map, Key key, Value #expr)
{
	if (!map.count)
	{
		Value val = #expr;
		map.set(key, val);
		return val;
	}
	uint hash = rehash(key.hash());
	uint index = index_for(hash, map.table.len);
	for (LinkedEntry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e.value;
	}
	Value val = #expr;
	map.add_entry(hash, key, val, index);
	return val;
}

fn Value? LinkedHashMap.get(&map, Key key) @operator([]) => *map.get_ref(key) @inline;

fn bool LinkedHashMap.has_key(&map, Key key) => @ok(map.get_ref(key));

fn bool LinkedHashMap.set(&map, Key key, Value value) @operator([]=)
{
	// If the map isn't initialized, use the defaults to initialize it.
	switch (map.allocator.ptr)
	{
		case &dummy:
			map.init(mem);
		case null:
			map.tinit();
		default:
			break;
	}
	uint hash = rehash(key.hash());
	uint index = index_for(hash, map.table.len);
	for (LinkedEntry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return true;
		}
	}
	map.add_entry(hash, key, value, index);
	return false;
}

fn void? LinkedHashMap.remove(&map, Key key) @maydiscard
{
	if (!map.remove_entry_for_key(key)) return NOT_FOUND~;
}

fn void LinkedHashMap.clear(&map)
{
	if (!map.count) return;

	LinkedEntry* entry = map.head;
	while (entry)
	{
		LinkedEntry* next = entry.after;
		map.free_entry(entry);
		entry = next;
	}

	foreach (LinkedEntry** &bucket : map.table)
	{
		*bucket = null;
	}

	map.count = 0;
	map.head = null;
	map.tail = null;
}

fn void LinkedHashMap.free(&map)
{
	if (!map.is_initialized()) return;
	map.clear();
	map.free_internal(map.table.ptr);
	map.table = {};
}

fn Key[] LinkedHashMap.tkeys(&self)
{
	return self.keys(tmem) @inline;
}

fn Key[] LinkedHashMap.keys(&self, Allocator allocator)
{
	if (!self.count) return {};

	Key[] list = allocator::alloc_array(allocator, Key, self.count);
	usz index = 0;

	LinkedEntry* entry = self.head;
	while (entry)
	{
		$if COPY_KEYS:
			list[index++] = entry.key.copy(allocator);
		$else
			list[index++] = entry.key;
		$endif
		entry = entry.after;
	}
	return list;
}

macro LinkedHashMap.@each(map; @body(key, value))
{
	map.@each_entry(; LinkedEntry* entry)
	{
		@body(entry.key, entry.value);
	};
}

macro LinkedHashMap.@each_entry(map; @body(entry))
{
	LinkedEntry* entry = map.head;
	while (entry)
	{
		@body(entry);
		entry = entry.after;
	}
}

fn Value[] LinkedHashMap.tvalues(&map) => map.values(tmem) @inline;

fn Value[] LinkedHashMap.values(&self, Allocator allocator)
{
	if (!self.count) return {};
	Value[] list = allocator::alloc_array(allocator, Value, self.count);
	usz index = 0;
	LinkedEntry* entry = self.head;
	while (entry)
	{
		list[index++] = entry.value;
		entry = entry.after;
	}
	return list;
}

fn bool LinkedHashMap.has_value(&map, Value v) @if(VALUE_IS_EQUATABLE)
{
	if (!map.count) return false;

	LinkedEntry* entry = map.head;
	while (entry)
	{
		if (equals(v, entry.value)) return true;
		entry = entry.after;
	}
	return false;
}

fn LinkedHashMapIterator LinkedHashMap.iter(&self) => { .map = self, .current = self.head, .started = false };

fn LinkedHashMapValueIterator LinkedHashMap.value_iter(&self) => { .map = self, .current = self.head, .started = false };

fn LinkedHashMapKeyIterator LinkedHashMap.key_iter(&self) => { .map = self, .current = self.head, .started = false };

fn bool LinkedHashMapIterator.next(&self)
{
	if (!self.started)
	{
		self.current = self.map.head;
		self.started = true;
	}
	else if (self.current)
	{
		self.current = self.current.after;
	}
	return self.current != null;
}

fn LinkedEntry*? LinkedHashMapIterator.get(&self)
{
	return self.current ? self.current : NOT_FOUND~;
}

fn Value*? LinkedHashMapValueIterator.get(&self)
{
	return self.current ? &self.current.value : NOT_FOUND~;
}

fn Key*? LinkedHashMapKeyIterator.get(&self)
{
	return self.current ? &self.current.key : NOT_FOUND~;
}

fn bool LinkedHashMapIterator.has_next(&self)
{
	if (!self.started) return self.map.head != null;
	return self.current && self.current.after != null;
}

// --- private methods

fn void LinkedHashMap.add_entry(&map, uint hash, Key key, Value value, uint bucket_index) @private
{
	$if COPY_KEYS:
	key = key.copy(map.allocator);
	$endif

	LinkedEntry* entry = allocator::new(map.allocator, LinkedEntry, {
		.hash = hash,
		.key = key,
		.value = value,
		.next = map.table[bucket_index],
		.before = map.tail,
		.after = null
	});

	// Update bucket chain
	map.table[bucket_index] = entry;

	// Update linked list
	if (map.tail)
	{
		map.tail.after = entry;
		entry.before = map.tail;
	}
	else
	{
		map.head = entry;
	}
	map.tail = entry;

	if (map.count++ >= map.threshold)
	{
		map.resize(map.table.len * 2);
	}
}

fn void LinkedHashMap.resize(&map, uint new_capacity) @private
{
	LinkedEntry*[] old_table = map.table;
	uint old_capacity = old_table.len;

	if (old_capacity == MAXIMUM_CAPACITY)
	{
		map.threshold = uint.max;
		return;
	}

	LinkedEntry*[] new_table = allocator::new_array(map.allocator, LinkedEntry*, new_capacity);
	map.table = new_table;
	map.threshold = (uint)(new_capacity * map.load_factor);

	// Rehash all entries - linked list order remains unchanged
	foreach (uint i, LinkedEntry *e : old_table)
	{
		if (!e) continue;

		// Split the bucket chain into two chains based on new bit
		LinkedEntry* lo_head = null;
		LinkedEntry* lo_tail = null;
		LinkedEntry* hi_head = null;
		LinkedEntry* hi_tail = null;

		do
		{
			LinkedEntry* next = e.next;
			if ((e.hash & old_capacity) == 0)
			{
				if (!lo_tail)
				{
					lo_head = e;
				}
				else
				{
					lo_tail.next = e;
				}
				lo_tail = e;
			}
			else
			{
				if (!hi_tail)
				{
					hi_head = e;
				}
				else
				{
					hi_tail.next = e;
				}
				hi_tail = e;
			}
			e.next = null;
			e = next;
		}
		while (e);

		if (lo_tail)
		{
			lo_tail.next = null;
			new_table[i] = lo_head;
		}
		if (hi_tail)
		{
			hi_tail.next = null;
			new_table[i + old_capacity] = hi_head;
		}
	}

	map.free_internal(old_table.ptr);
}

fn usz? LinkedHashMap.to_format(&self, Formatter* f) @dynamic
{
	usz len;
	len += f.print("{ ")!;
	self.@each_entry(; LinkedEntry* entry)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s: %s", entry.key, entry.value)!;
	};
	return len + f.print(" }");
}

fn void LinkedHashMap.transfer(&map, LinkedEntry*[] new_table) @private
{
	LinkedEntry*[] src = map.table;
	uint new_capacity = new_table.len;
	foreach (uint j, LinkedEntry *e : src)
	{
		if (!e) continue;
		do
		{
			LinkedEntry* next = e.next;
			uint i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void LinkedHashMap.put_all_for_create(&map, LinkedHashMap* other_map) @private
{
	if (!other_map.count) return;
	other_map.@each(; Key key, Value value) {
		map.set(key, value);
	};
}

fn void LinkedHashMap.put_for_create(&map, Key key, Value value) @private
{
	uint hash = rehash(key.hash());
	uint i = index_for(hash, map.table.len);
	for (LinkedEntry *e = map.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return;
		}
	}
	map.create_entry(hash, key, value, i);
}

fn void LinkedHashMap.free_internal(&map, void* ptr) @inline @private
{
	allocator::free(map.allocator, ptr);
}

fn bool LinkedHashMap.remove_entry_for_key(&map, Key key) @private
{
	if (!map.count) return false;

	uint hash = rehash(key.hash());
	uint i = index_for(hash, map.table.len);
	LinkedEntry* prev = null;
	LinkedEntry* e = map.table[i];

	while (e)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			if (prev)
			{
				prev.next = e.next;
			}
			else
			{
				map.table[i] = e.next;
			}

			if (e.before)
			{
				e.before.after = e.after;
			}
			else
			{
				map.head = e.after;
			}

			if (e.after)
			{
				e.after.before = e.before;
			}
			else
			{
				map.tail = e.before;
			}

			map.count--;
			map.free_entry(e);
			return true;
		}
		prev = e;
		e = e.next;
	}
	return false;
}

fn void LinkedHashMap.create_entry(&map, uint hash, Key key, Value value, int bucket_index) @private
{
	LinkedEntry *e = map.table[bucket_index];
	$if COPY_KEYS:
	key = key.copy(map.allocator);
	$endif
	LinkedEntry* entry = allocator::new(map.allocator, LinkedEntry, { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] });
	map.table[bucket_index] = entry;
	map.count++;
}

fn void LinkedHashMap.free_entry(&self, LinkedEntry *entry) @local
{
	$if COPY_KEYS:
	allocator::free(self.allocator, entry.key);
	$endif
	self.free_internal(entry);
}


struct LinkedHashMapIterator
{
	LinkedHashMap* map;
	LinkedEntry* current;
	bool started;
}

typedef LinkedHashMapValueIterator = inline LinkedHashMapIterator;
typedef LinkedHashMapKeyIterator = inline LinkedHashMapIterator;

fn usz LinkedHashMapValueIterator.len(self) @operator(len) => self.map.count;
fn usz LinkedHashMapKeyIterator.len(self) @operator(len) => self.map.count;
fn usz LinkedHashMapIterator.len(self) @operator(len) => self.map.count;

int dummy @local;
