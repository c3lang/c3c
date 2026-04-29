<*
 @require $defined((Value){}.hash()) : `No .hash function found on the value`
*>
module std::collections::set <Value>;
import std::math;
import std::io @norecurse;

const int DEFAULT_INITIAL_CAPACITY = 16;
const int MAXIMUM_CAPACITY = 1 << 30;
const float DEFAULT_LOAD_FACTOR = 0.75;

const Allocator SET_HEAP_ALLOCATOR = (Allocator)&dummy;

<* Copy the ONHEAP allocator to initialize to a set that is heap allocated *>
const HashSet ONHEAP = { .allocator = SET_HEAP_ALLOCATOR };

struct Entry
{
	uint hash;
	Value value;
	Entry* next;
}

struct HashSet (Printable)
{
	Entry*[] table;
	Allocator allocator;
	<* Number of elements *>
	int count;
	<* Resize limit *>
	int threshold;
	float load_factor;
}

fn sz HashSet.len(&self) @operator(len) => self.count;

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.init(&self, Allocator allocator, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.threshold = (int)(capacity * load_factor);
	self.load_factor = load_factor;
	self.table = alloc::new_array(allocator, Entry*, capacity);
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.tinit(&self, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro HashSet* HashSet.init_with_values(&self, Allocator allocator, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vaarg.len; $i++:
		self.add($vaarg[$i]);
	$endfor
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro HashSet* HashSet.tinit_with_values(&self, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_values(tmem, ...$vaarg, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] values : "The values for the HashSet"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.init_from_values(&self, Allocator allocator, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	foreach (v : values) self.add(v);
	return self;
}

<*
 @param [in] values : "The values for the HashSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.tinit_from_values(&self, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_from_values(tmem, values, capacity, load_factor);
}

<*
 Has this hash set been initialized yet?

 @param [&in] set : "The hash set we are testing"
 @return "Returns true if it has been initialized, false otherwise"
*>
fn bool HashSet.is_initialized(&set)
{
	return set.allocator && set.allocator.ptr != &dummy;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_set : "The set to copy from."
 @require !self.is_initialized() : "Set was already initialized"
*>
fn HashSet* HashSet.init_from_set(&self, Allocator allocator, HashSet* other_set)
{
	self.init(allocator, other_set.table.len, other_set.load_factor);
	hashset_put_all_for_create(self, other_set);
	return self;
}

<*
 @param [&in] other_set : "The set to copy from."
 @require !set.is_initialized() : "Set was already initialized"
*>
fn HashSet* HashSet.tinit_from_set(&set, HashSet* other_set)
{
	return set.init_from_set(tmem, other_set) @inline;
}

<*
 Check if the set is empty

 @return "true if it is empty"
 @pure
*>
fn bool HashSet.is_empty(&set) @inline
{
	return !set.count;
}

<*
 Add all elements in the slice to the set.

 @param [in] list
 @return "The number of new elements added"
 @ensure total <= list.len
*>
fn sz HashSet.add_all(&set, Value[] list)
{
	sz total;
	foreach (v : list)
	{
		if (set.add(v)) total++;
	}
	return total;
}

<*
 @param [&in] other
 @return "The number of new elements added"
 @ensure return <= other.count
*>
fn sz HashSet.add_all_from(&set, HashSet* other)
{
	sz total;
	other.@each(;Value value)
	{
		if (set.add(value)) total++;
	};
	return total;
}

<*
 @param value : "The value to add"
 @return "true if the value didn't exist in the set"
*>
fn bool HashSet.add(&set, Value value)
{
	// If the set isn't initialized, use the defaults to initialize it.
	switch (set.allocator.ptr)
	{
		case &dummy:
			set.init(mem);
		case null:
			set.tinit();
		default:
			break;
	}
	uint hash = rehash(value.hash());
	int index = index_for(hash, set.table.len);
	for (Entry *e = set.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value)) return false;
	}
	hashset_add_entry(set, hash, value, index);
	return true;
}

<*
 Iterate over all the values in the set
*>
macro HashSet.@each(set; @body(value))
{
	if (!set.count) return;
	foreach (Entry* entry : set.table)
	{
		while (entry)
		{
			@body(entry.value);
			entry = entry.next;
		}
	}
}

<*
 Check if the set contains the given value.

 @param value : "The value to check"
 @return "true if it exists in the set"
*>
fn bool HashSet.contains(&set, Value value)
{
	if (!set.count) return false;
	uint hash = rehash(value.hash());
	for (Entry *e = set.table[index_for(hash, set.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value)) return true;
	}
	return false;
}

<*
 Remove a single value from the set.

 @param value : "The value to remove"
 @return? NOT_FOUND : "If the entry is not found"
*>
fn void? HashSet.remove(&set, Value value) @maydiscard
{
	if (!hashset_remove_entry_for_value(set, value)) return NOT_FOUND~;
}

fn sz HashSet.remove_all(&set, Value[] values)
{
	sz total;
	foreach (v : values)
	{
		if (hashset_remove_entry_for_value(set, v)) total++;
	}
	return total;
}

<*
 @param [&in] other : "Other set"
*>
fn sz HashSet.remove_all_from(&set, HashSet* other)
{
	sz total;
	other.@each(;Value val)
    {
		if (hashset_remove_entry_for_value(set, val)) total++;
    };
    return total;
}

<*
 Free all memory allocated by the hash set.
*>
fn void HashSet.free(&set)
{
	if (!set.is_initialized()) return;
	set.clear();
	hashset_free_internal(set, set.table.ptr);
	*set = {};
}

<*
 Clear all elements from the set while keeping the underlying storage

 @ensure set.count == 0
*>
fn void HashSet.clear(&set)
{
	if (!set.count) return;

	foreach (Entry** &entry_ref : set.table)
	{
		Entry* entry = *entry_ref;
		if (!entry) continue;

		Entry *next = entry.next;
		while (next)
		{
			Entry *to_delete = next;
			next = next.next;
			hashset_free_entry(set, to_delete);
		}

		hashset_free_entry(set, entry);
		*entry_ref = null;
	}
	set.count = 0;
}

<*
 @require capacity > 0 : "Expected positive capacity"
*>
fn void HashSet.reserve(&set, int capacity)
{
	if (capacity > set.threshold)
	{
		hashset_resize(set, math::next_power_of_2(capacity));
	}
}

fn Value[] HashSet.tvalues(&self) => self.values(tmem) @inline;

fn Value[] HashSet.values(&self, Allocator allocator)
{
	if (!self.count) return {};
	Value[] list = alloc::alloc_array(allocator, Value, self.count);
	sz index;
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

// --- Set Operations ---

<*
 Returns the union of two sets (A | B)

 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn HashSet HashSet.set_union(&self, Allocator allocator, HashSet* other)
{
	int new_capacity = math::next_power_of_2(self.count + other.count);
	HashSet result;
	result.init(allocator, new_capacity, self.load_factor);
	result.add_all_from(self);
	result.add_all_from(other);
	return result;
}

fn HashSet HashSet.tset_union(&self, HashSet* other) => self.set_union(tmem, other);

<*
 Returns the intersection of the two sets (A & B)

 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn HashSet HashSet.intersection(&self, Allocator allocator, HashSet* other)
{
	HashSet result;
	result.init(allocator, (int)math::min(self.table.len, other.table.len), self.load_factor);

	// Iterate through the smaller set for efficiency
	HashSet* smaller = self.count <= other.count ? self : other;
	HashSet* larger = self.count > other.count ? self : other;

	smaller.@each(;Value value)
	{
		if (larger.contains(value)) result.add(value);
	};

	return result;
}

fn HashSet HashSet.tintersection(&self, HashSet* other) => self.intersection(tmem, other);

<*
 Return this set - other, so (A & ~B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn HashSet HashSet.difference(&self, Allocator allocator, HashSet* other)
{
	HashSet result;
	result.init(allocator, self.table.len, self.load_factor);
	self.@each(;Value value)
	{
		if (!other.contains(value))
		{
			result.add(value);
		}
	};
	return result;
}

fn HashSet HashSet.tdifference(&self, HashSet* other) => self.difference(tmem, other) @inline;

<*
 Return (A ^ B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set or the other, but not both"
*>
fn HashSet HashSet.symmetric_difference(&self, Allocator allocator, HashSet* other)
{
	HashSet result;
	result.init(allocator, self.table.len, self.load_factor);
	result.add_all_from(self);
	other.@each(;Value value)
	{
		if (!result.add(value))
		{
			result.remove(value);
		}
	};
	return result;
}

fn HashSet HashSet.tsymmetric_difference(&self, HashSet* other) => self.symmetric_difference(tmem, other) @inline;

<*
 Check if this hash set is a subset of another set.

 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool HashSet.is_subset(&self, HashSet* other)
{
	if (self.count == 0) return true;
	if (self.count > other.count) return false;

	self.@each(;Value value)
	{
		if (!other.contains(value)) return false;
	};
	return true;
}


// --- private methods

fn void hashset_add_entry(HashSet* set, uint hash, Value value, int bucket_index) @private
{
	Entry* entry = alloc::new(set.allocator, Entry, { .hash = hash, .value = value, .next = set.table[bucket_index] });
	set.table[bucket_index] = entry;
	if (set.count++ >= set.threshold)
	{
		hashset_resize(set, set.table.len * 2);
	}
}

<*
 @require new_capacity > 0 : "Expected positive capacity"
*>
fn void hashset_resize(HashSet* self, int new_capacity) @private
{
	Entry*[] old_table = self.table;
	int old_capacity = (int)old_table.len;
	if (old_capacity == MAXIMUM_CAPACITY)
	{
		self.threshold = int::max;
		return;
	}
	Entry*[] new_table = alloc::new_array(self.allocator, Entry*, new_capacity);
	hashset_transfer(self, new_table);
	self.table = new_table;
	hashset_free_internal(self, old_table.ptr);
	self.threshold = (int)(new_capacity * self.load_factor);
}

fn sz? HashSet.to_format(&self, Formatter* f) @dynamic
{
	sz len;
	len += f.print("{ ")!;
	self.@each(; Value value)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s", value)!;
	};
	return len + f.print(" }");
}

fn void hashset_transfer(HashSet* self, Entry*[] new_table) @private
{
	Entry*[] src = self.table;
	int new_capacity = (int)new_table.len;
	foreach (Entry *e : src)
	{
		if (!e) continue;
		do
		{
			Entry* next = e.next;
			int i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void hashset_put_all_for_create(HashSet* set, HashSet* other_set) @private
{
	if (!other_set.count) return;
	foreach (Entry *e : other_set.table)
	{
		while (e)
		{
			hashset_put_for_create(set, e.value);
			e = e.next;
		}
	}
}

fn void hashset_put_for_create(HashSet* set, Value value) @private
{
	uint hash = rehash(value.hash());
	uint i = index_for(hash, set.table.len);
	for (Entry *e = set.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value))
		{
			// Value already exists, no need to do anything
			return;
		}
	}
	hashset_create_entry(set, hash, value, i);
}

fn void hashset_free_internal(HashSet* self, void* ptr) @inline @private
{
	alloc::free(self.allocator, ptr);
}

fn void hashset_create_entry(HashSet* set, uint hash, Value value, uint bucket_index) @private
{
	Entry* entry = alloc::new(set.allocator, Entry, {
		.hash = hash,
		.value = value,
		.next = set.table[bucket_index]
	});
	set.table[bucket_index] = entry;
	set.count++;
}

<*
 Removes the entry for the specified value if present
 @return "true if found and removed, false otherwise"
*>
fn bool hashset_remove_entry_for_value(HashSet* set, Value value) @private
{
	if (!set.count) return false;
	uint hash = rehash(value.hash());
	uint i = index_for(hash, set.table.len);
	Entry* prev = set.table[i];
	Entry* e = prev;
	while (e)
	{
		Entry *next = e.next;
		if (e.hash == hash && equals(value, e.value))
		{
			set.count--;
			if (prev == e)
			{
				set.table[i] = next;
			}
			else
			{
				prev.next = next;
			}
			hashset_free_entry(set, e);
			return true;
		}
		prev = e;
		e = next;
	}

	return false;
}

fn void hashset_free_entry(HashSet* set, Entry *entry) @private
{
	alloc::free(set.allocator, entry);
}

struct HashSetIterator
{
	HashSet* set;
	int bucket_index;
	Entry* current;
}

fn HashSetIterator HashSet.iter(&set) => { .set = set, .bucket_index = 0, .current = null };

fn Value? HashSetIterator.next(&self)
{
	if (self.current)
	{
		Value value = self.current.value;
		self.current = self.current.next;
		return value;
	}

	while (self.bucket_index < self.set.table.len)
	{
		self.current = self.set.table[self.bucket_index++];
		if (self.current)
		{
			Value value = self.current.value;
			self.current = self.current.next;
			return value;
		}
	}

	return NOT_FOUND~;
}

fn sz HashSetIterator.len(&self) @operator(len)
{
	return self.set.count;
}

<* @pure *>
fn uint rehash(uint hash) @inline @private
{
	hash ^= (hash >> 20) ^ (hash >> 12);
	return hash ^ ((hash >> 7) ^ (hash >> 4));
}

macro int index_for(uint hash, sz capacity) @private => (int)(hash & ((uint)capacity - 1u));

int dummy @local;
