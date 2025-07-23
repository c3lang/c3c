<*
 @require $defined((Value){}.hash()) : `No .hash function found on the value`
*>
module std::collections::set {Value};
import std::math;
import std::io @norecurse;

const uint DEFAULT_INITIAL_CAPACITY = 16;
const uint MAXIMUM_CAPACITY = 1u << 31;
const float DEFAULT_LOAD_FACTOR = 0.75;

const Allocator SET_HEAP_ALLOCATOR = (Allocator)&dummy;

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
	usz count; 		// Number of elements
	usz threshold; 	// Resize limit
	float load_factor;
}

fn int HashSet.len(&self) @operator(len) => (int) self.count;

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.init(&self, Allocator allocator, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR) 
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.threshold = (usz) (capacity * load_factor);
    self.load_factor = load_factor;
	self.table = allocator::new_array(allocator, Entry*, capacity);
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.tinit(&self, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro HashSet* HashSet.init_with_values(&self, Allocator allocator, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vacount; $i++:
		self.add($vaarg[$i]);
	$endfor
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro HashSet* HashSet.tinit_with_key_values(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_key_values(tmem, $vasplat, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] values : "The values for the HashSet"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Map was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.init_from_values(&self, Allocator allocator, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	for (usz i = 0; i < values.len; i++)
	{
		self.add(values[i]);
	}
	return self;
}

<*
 @param [in] values : "The values for the HashSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn HashSet* HashSet.tinit_from_values(&self, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
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
fn HashSet* HashSet.init_from_map(&self, Allocator allocator, HashSet* other_set)
{
	self.init(allocator, other_set.table.len, other_set.load_factor);
	self.put_all_for_create(other_set);
	return self;
}

<*
 @param [&in] other_set : "The set to copy from."
 @require !set.is_initialized() : "Set was already initialized"
*>
fn HashSet* HashSet.tinit_from_map(&set, HashSet* other_set)
{
	return set.init_from_map(tmem, other_set) @inline;
}

fn bool HashSet.is_empty(&set) @inline
{
	return !set.count;
}

<*
 Get the value or add and
 @require @assignable_to(#expr, Value)
*>
macro Value HashSet.@add_or_get(&set, Value #expr)
{
    if (!set.count)
    {
        Value val = #expr;
        set.add(val);
        return val;
    }
    uint hash = rehash(value.hash());
    uint index = index_for(hash, set.table.len);
    for (Entry *e = set.table[index]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value)) return e.value;
    }
    Value val = #expr;
    set.add_entry(hash, val, index);
    return val;
}

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
    uint index = index_for(hash, set.table.len);
    for (Entry *e = set.table[index]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value)) return false;
    }
    set.add_entry(hash, value, index);
    return true;
}

macro HashSet.@each(set; @body(value))
{
    set.@each_entry(; Entry* entry)
	{
		@body(entry.value);
	};
}

macro HashSet.@each_entry(set; @body(entry))
{
	if (!set.count) return;
	foreach (Entry* entry : set.table)
	{
		while (entry)
		{
			@body(entry);
			entry = entry.next;
		}
	}
}

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

fn void? HashSet.remove(&set, Value value) @maydiscard
{
    if (!set.remove_entry_for_value(value)) return NOT_FOUND?;
}

fn void HashSet.free(&set) 
{
	if (!set.is_initialized()) return;
	set.clear();
	set.free_internal(set.table.ptr);
	set.table = {};
}

<*
 Clear all elements from the set while keeping the underlying storage
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
            set.free_entry(to_delete);
        }
        
        set.free_entry(entry);
        *entry_ref = null;
    }
    set.count = 0;
}

fn void HashSet.reserve(&set, usz capacity)
{
    if (capacity > set.threshold)
    {
        set.resize(math::next_power_of_2(capacity));
    }
}

// --- Set Operations ---

<*
 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn HashSet* HashSet.merge(&self, Allocator allocator, HashSet* other)
{
    usz new_capacity = math::next_power_of_2(self.count + other.count);
    HashSet* result = allocator::new(allocator, HashSet);
    result.init(allocator, new_capacity, self.load_factor);
    
    self.@each_entry(; Entry* entry) {
        result.add(entry.value);
    };
    
    other.@each_entry(; Entry* entry) {
        result.add(entry.value);
    };
    
    return result;
}

fn HashSet* HashSet.tmerge(&self, HashSet* other) => self.merge(tmem, other);

<*
 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn HashSet* HashSet.intersection(&self, Allocator allocator, HashSet* other)
{
    HashSet* result = allocator::new(allocator, HashSet);
    result.init(allocator, math::min(self.table.len, other.table.len), self.load_factor);
    
    // Iterate through the smaller set for efficiency
    HashSet* smaller = self.count <= other.count ? self : other;
    HashSet* larger = self.count > other.count ? self : other;
    
    smaller.@each_entry(; Entry* entry) {
        if (larger.contains(entry.value)) {
            result.add(entry.value);
        }
    };
    
    return result;
}

fn HashSet* HashSet.tintersection(&self, HashSet* other) => self.intersection(tmem, other);

<*
 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn HashSet* HashSet.difference(&self, Allocator allocator, HashSet* other)
{
    HashSet* result = allocator::new(allocator, HashSet);
    result.init(allocator, self.table.len, self.load_factor);
    
    self.@each_entry(; Entry* entry) {
        if (!other.contains(entry.value)) {
            result.add(entry.value);
        }
    };
    
    return result;
}

fn HashSet* HashSet.tdifference(&self, HashSet* other) => self.difference(tmem, other);

<*
 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool HashSet.is_subset(&self, HashSet* other)
{
    if (self.count == 0) return true;
    if (self.count > other.count) return false;
    
    self.@each_entry(; Entry* entry) {
        if (!other.contains(entry.value)) return false;
    };
    return true;
}


// --- private methods

fn void HashSet.add_entry(&set, uint hash, Value value, uint bucket_index) @private
{
	Entry* entry = allocator::new(set.allocator, Entry, { .hash = hash, .value = value, .next = set.table[bucket_index] });
	set.table[bucket_index] = entry;
	if (set.count++ >= set.threshold)
	{
		set.resize(set.table.len * 2);
	}
}

fn void HashSet.resize(&self, usz new_capacity) @private {
	Entry*[] old_table = self.table;
	usz old_capacity = old_table.len;
	if (old_capacity == MAXIMUM_CAPACITY)
	{
		self.threshold = uint.max;
		return;
	}
	Entry*[] new_table = allocator::new_array(self.allocator, Entry*, new_capacity);
	self.transfer(new_table);
	self.table = new_table;
	self.free_internal(old_table.ptr);
	self.threshold = (uint)(new_capacity * self.load_factor);
}

fn usz? HashSet.to_format(&self, Formatter* f) @dynamic
{
	usz len;
	len += f.print("{ ")!;
	self.@each_entry(; Entry* entry)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s", entry.value)!;
    };
	return len + f.print(" }");
}

fn void HashSet.transfer(&self, Entry*[] new_table) @private
{
	Entry*[] src = self.table;
	uint new_capacity = new_table.len;
	foreach (uint j, Entry *e : src)
	{
		if (!e) continue;
		do
		{
			Entry* next = e.next;
			uint i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void HashSet.put_all_for_create(&set, HashSet* other_set) @private
{
	if (!other_set.count) return;
	foreach (Entry *e : other_set.table)
	{
		while (e)
		{
			set.put_for_create(e.value);
			e = e.next;
		}
	}
}

fn void HashSet.put_for_create(&set, Value value) @private
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
    set.create_entry(hash, value, i);
}

fn void HashSet.free_internal(&self, void* ptr) @inline @private
{
	allocator::free(self.allocator, ptr);
}

fn void HashSet.create_entry(&set, uint hash, Value value, int bucket_index) @private
{
    Entry* entry = allocator::new(set.allocator, Entry, { 
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
fn bool HashSet.remove_entry_for_value(&set, Value value) @private
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
            set.free_entry(e);
            return true;
        }
        prev = e;
        e = next;
    }
    
    return false;
}

fn void HashSet.free_entry(&set, Entry *entry) @private
{
	// TODO: Maybe some checking for entry cleanup? Like
    // $if $defined((Value){}.free()):
    //     entry.value.free(); ?
    // or  allocator::free(set.allocator, entry.value); ?
    // $endif
    
    allocator::free(set.allocator, entry);
}

struct HashSetIterator
{
    HashSet* set;
    usz bucket_index;
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

    return NOT_FOUND?;
}

fn usz HashSetIterator.len(&self) @operator(len)
{
    return self.set.count;
}

fn uint rehash(uint hash) @inline @private
{
	hash ^= (hash >> 20) ^ (hash >> 12);
	return hash ^ ((hash >> 7) ^ (hash >> 4));
}

macro uint index_for(uint hash, uint capacity) @private => hash & (capacity - 1);

int dummy @local;
