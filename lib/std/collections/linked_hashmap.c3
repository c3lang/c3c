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
	int count;
	int threshold;
	float load_factor;
	<* First inserted LinkedEntry *>
	LinkedEntry* head;
	<* Last inserted LinkedEntry *>
	LinkedEntry* tail;
}


<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.init(&self, Allocator allocator, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.load_factor = load_factor;
	self.threshold = (int)(capacity * load_factor);
	self.table = alloc::new_array(allocator, LinkedEntry*, capacity);
	self.head = null;
	self.tail = null;
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.tinit(&self, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require $vaarg.len % 2 == 0 : "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashMap* LinkedHashMap.init_with_key_values(&self, Allocator allocator, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vaarg.len; $i += 2:
		self.set($vaarg[$i], $vaarg[$i + 1]);
	$endfor
	return self;
}

<*
 @require $vaarg.len % 2 == 0 : "There must be an even number of arguments provided for keys and values"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashMap* LinkedHashMap.tinit_with_key_values(&self, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_key_values(tmem, ...$vaarg, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] keys : "The keys for the LinkedHashMap entries"
 @param [in] values : "The values for the LinkedHashMap entries"
 @param [&inout] allocator : "The allocator to use"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.init_from_keys_and_values(&self, Allocator allocator, Key[] keys, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	foreach (i, key : keys)
	{
		self.set(key, values[i]);
	}
	return self;
}

<*
 @param [in] keys : "The keys for the LinkedHashMap entries"
 @param [in] values : "The values for the LinkedHashMap entries"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashMap* LinkedHashMap.tinit_from_keys_and_values(&self, Key[] keys, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
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
	linkedhashmap_put_all_for_create(self, other_map);
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

fn sz LinkedHashMap.len(&map) @inline => map.count;

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
	int index = index_for(hash, map.table.len);
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
	int index = index_for(hash, map.table.len);
	for (LinkedEntry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return true;
		}
	}
	linkedhashmap_add_entry(map, hash, key, value, index);
	return false;
}

fn void? LinkedHashMap.remove(&map, Key key) @maydiscard
{
	if (!linkedhashmap_remove_entry_for_key(map, key)) return NOT_FOUND~;
}

fn void LinkedHashMap.clear(&map)
{
	if (!map.count) return;

	LinkedEntry* entry = map.head;
	while (entry)
	{
		LinkedEntry* next = entry.after;
		linkedhashmap_free_entry(map, entry);
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
	linkedhashmap_free_internal(map, map.table.ptr);
	map.table = {};
}

fn Key[] LinkedHashMap.tkeys(&self)
{
	return self.keys(tmem) @inline;
}

fn Key[] LinkedHashMap.keys(&self, Allocator allocator)
{
	if (!self.count) return {};

	Key[] list = alloc::alloc_array(allocator, Key, self.count);
	int index = 0;

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
	Value[] list = alloc::alloc_array(allocator, Value, self.count);
	int index = 0;
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

fn void linkedhashmap_add_entry(LinkedHashMap* map, uint hash, Key key, Value value, int bucket_index) @private
{
	$if COPY_KEYS:
	key = key.copy(map.allocator);
	$endif

	LinkedEntry* entry = alloc::new(map.allocator, LinkedEntry, {
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
		linkedhashmap_resize(map, map.table.len * 2);
	}
}

fn void linkedhashmap_resize(LinkedHashMap* map, int new_capacity) @private
{
	LinkedEntry*[] old_table = map.table;
	int old_capacity = old_table.len;

	if (old_capacity == MAXIMUM_CAPACITY)
	{
		map.threshold = int::max;
		return;
	}

	LinkedEntry*[] new_table = alloc::new_array(map.allocator, LinkedEntry*, new_capacity);
	map.table = new_table;
	map.threshold = (int)(new_capacity * map.load_factor);

	// Rehash all entries - linked list order remains unchanged
	foreach (i, LinkedEntry *e : old_table)
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
			if ((e.hash & (uint)old_capacity) == 0)
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

	linkedhashmap_free_internal(map, old_table.ptr);
}

fn sz? LinkedHashMap.to_format(&self, Formatter* f) @dynamic
{
	sz len;
	len += f.print("{ ")!;
	self.@each_entry(; LinkedEntry* entry)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s: %s", entry.key, entry.value)!;
	};
	return len + f.print(" }");
}

fn void linkedhashmap_transfer(LinkedHashMap* map, LinkedEntry*[] new_table) @private
{
	LinkedEntry*[] src = map.table;
	int new_capacity = (int)new_table.len;
	foreach (LinkedEntry *e : src)
	{
		if (!e) continue;
		do
		{
			LinkedEntry* next = e.next;
			int i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void linkedhashmap_put_all_for_create(LinkedHashMap* map, LinkedHashMap* other_map) @private
{
	if (!other_map.count) return;
	other_map.@each(; Key key, Value value) {
		map.set(key, value);
	};
}

fn void linkedhashmap_put_for_create(LinkedHashMap* map, Key key, Value value) @private
{
	uint hash = rehash(key.hash());
	int i = index_for(hash, map.table.len);
	for (LinkedEntry *e = map.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return;
		}
	}
	linkedhashmap_create_entry(map, hash, key, value, i);
}

fn void linkedhashmap_free_internal(LinkedHashMap* map, void* ptr) @inline @private
{
	alloc::free(map.allocator, ptr);
}

fn bool linkedhashmap_remove_entry_for_key(LinkedHashMap* map, Key key) @private
{
	if (!map.count) return false;

	uint hash = rehash(key.hash());
	int i = index_for(hash, map.table.len);
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
			linkedhashmap_free_entry(map, e);
			return true;
		}
		prev = e;
		e = e.next;
	}
	return false;
}

fn void linkedhashmap_create_entry(LinkedHashMap* map, uint hash, Key key, Value value, int bucket_index) @private
{
	LinkedEntry *e = map.table[bucket_index];
	$if COPY_KEYS:
		key = key.copy(map.allocator);
	$endif
	LinkedEntry* entry = alloc::new(map.allocator, LinkedEntry, { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] });
	map.table[bucket_index] = entry;
	map.count++;
}

fn void linkedhashmap_free_entry(LinkedHashMap* self, LinkedEntry *entry) @local
{
	$if COPY_KEYS:
		alloc::free(self.allocator, entry.key);
	$endif
	linkedhashmap_free_internal(self, entry);
}


struct LinkedHashMapIterator
{
	LinkedHashMap* map;
	LinkedEntry* current;
	bool started;
}

typedef LinkedHashMapValueIterator = inline LinkedHashMapIterator;
typedef LinkedHashMapKeyIterator = inline LinkedHashMapIterator;

fn sz LinkedHashMapValueIterator.len(self) @operator(len) => self.map.count;
fn sz LinkedHashMapKeyIterator.len(self) @operator(len) => self.map.count;
fn sz LinkedHashMapIterator.len(self) @operator(len) => self.map.count;

int dummy @local;
