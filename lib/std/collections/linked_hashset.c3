<*
 @require $defined((Value){}.hash()) : `No .hash function found on the value`
*>
module std::collections::set {Value};
import std::math;
import std::io @norecurse;

const LinkedHashSet LINKEDONHEAP = { .allocator = SET_HEAP_ALLOCATOR };

struct LinkedEntry
{
    uint hash;
    Value value;
    LinkedEntry* next;      // For bucket chain
    LinkedEntry* before;    // Previous in insertion order
    LinkedEntry* after;     // Next in insertion order
}

struct LinkedHashSet (Printable)
{
    LinkedEntry*[] table;
    Allocator allocator;
    usz count;
    usz threshold;
    float load_factor;
    LinkedEntry* head;      // First inserted LinkedEntry
    LinkedEntry* tail;      // Last inserted LinkedEntry
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.init(&self, Allocator allocator, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
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
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.tinit(&self, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
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
macro LinkedHashSet* LinkedHashSet.init_with_values(&self, Allocator allocator, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
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
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashSet* LinkedHashSet.tinit_with_values(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
    return self.init_with_values(tmem, $vasplat, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] values : "The values for the LinkedHashSet"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.init_from_values(&self, Allocator allocator, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
    self.init(allocator, capacity, load_factor);
    for (usz i = 0; i < values.len; i++)
    {
        self.add(values[i]);
    }
    return self;
}

<*
 @param [in] values : "The values for the LinkedHashSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.tinit_from_values(&self, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
    return self.init_from_values(tmem, values, capacity, load_factor);
}

<*
 Has this linked hash set been initialized yet?

 @param [&in] set : "The linked hash set we are testing"
 @return "Returns true if it has been initialized, false otherwise"
*>
fn bool LinkedHashSet.is_initialized(&set)
{
    return set.allocator && set.allocator.ptr != &dummy;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_set : "The set to copy from."
 @require !self.is_initialized() : "Set was already initialized"
*>
fn LinkedHashSet* LinkedHashSet.init_from_set(&self, Allocator allocator, LinkedHashSet* other_set)
{
    self.init(allocator, other_set.table.len, other_set.load_factor);
    self.put_all_for_create(other_set);
    return self;
}

<*
 @param [&in] other_set : "The set to copy from."
 @require !set.is_initialized() : "Set was already initialized"
*>
fn LinkedHashSet* LinkedHashSet.tinit_from_set(&set, LinkedHashSet* other_set)
{
    return set.init_from_set(tmem, other_set) @inline;
}

fn bool LinkedHashSet.is_empty(&set) @inline => !set.count;

fn usz LinkedHashSet.len(&set) @inline => set.count;

<*
 Get the value or add and
 @require @assignable_to(#expr, Value)
*>
macro Value LinkedHashSet.@add_or_get(&set, Value #expr)
{
    if (!set.count)
    {
        Value val = #expr;
        set.add(val);
        return val;
    }
    uint hash = rehash(value.hash());
    uint index = index_for(hash, set.table.len);
    for (LinkedEntry *e = set.table[index]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value)) return e.value;
    }
    Value val = #expr;
    set.add_entry(hash, val, index);
    return val;
}

fn bool LinkedHashSet.add(&set, Value value)
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
    for (LinkedEntry *e = set.table[index]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value)) return false;
    }
    set.add_entry(hash, value, index);
    return true;
}

macro LinkedHashSet.@each(set; @body(value))
{
    set.@each_entry(; LinkedEntry* entry)
    {
        @body(entry.value);
    };
}

macro LinkedHashSet.@each_entry(set; @body(entry))
{
    LinkedEntry* entry = set.head;
    while (entry)
    {
        @body(entry);
        entry = entry.after;
    }
}

fn bool LinkedHashSet.contains(&set, Value value)
{
    if (!set.count) return false;
    uint hash = rehash(value.hash());
    for (LinkedEntry *e = set.table[index_for(hash, set.table.len)]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value)) return true;
    }
    return false;
}

fn void? LinkedHashSet.remove(&set, Value value) @maydiscard
{
    if (!set.remove_entry_for_value(value)) return NOT_FOUND?;
}

fn void LinkedHashSet.clear(&set)
{
    if (!set.count) return;
    
    LinkedEntry* entry = set.head;
    while (entry)
    {
        LinkedEntry* next = entry.after;
        set.free_entry(entry);
        entry = next;
    }
    
    foreach (LinkedEntry** &bucket : set.table)
    {
        *bucket = null;
    }
    
    set.count = 0;
    set.head = null;
    set.tail = null;
}

fn void LinkedHashSet.reserve(&set, usz capacity)
{
    if (capacity > set.threshold)
    {
        set.resize(math::next_power_of_2(capacity));
    }
}

fn void LinkedHashSet.free(&set)
{
    if (!set.is_initialized()) return;
    set.clear();
    set.free_internal(set.table.ptr);
    set.table = {};
}

fn Value[] LinkedHashSet.tvalues(&set) => set.values(tmem) @inline;

fn Value[] LinkedHashSet.values(&set, Allocator allocator)
{
    if (!set.count) return {};
    Value[] list = allocator::alloc_array(allocator, Value, set.count);
    usz index = 0;
    LinkedEntry* entry = set.head;
    while (entry)
    {
        list[index++] = entry.value;
        entry = entry.after;
    }
    return list;
}

fn LinkedHashSetIterator LinkedHashSet.iter(&set) => { .set = set, .current = set.head, .started = false };

fn bool LinkedHashSetIterator.next(&self)
{
    if (!self.started)
    {
        self.current = self.set.head;
        self.started = true;
    }
    else if (self.current)
    {
        self.current = self.current.after;
    }
    return self.current != null;
}

fn Value*? LinkedHashSetIterator.get(&self)
{
    return self.current ? &self.current.value : NOT_FOUND?;
}

fn bool LinkedHashSetIterator.has_next(&self)
{
    if (!self.started) return self.set.head != null;
    return self.current && self.current.after != null;
}

// --- set operations

<*
 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn LinkedHashSet* LinkedHashSet.merge(&self, Allocator allocator, LinkedHashSet* other)
{
    usz new_capacity = math::next_power_of_2(self.count + other.count);
    LinkedHashSet* result = allocator::new(allocator, LinkedHashSet);
    result.init(allocator, new_capacity, self.load_factor);
    
    self.@each_entry(; LinkedEntry* entry) {
        result.add(entry.value);
    };
    
    other.@each_entry(; LinkedEntry* entry) {
        result.add(entry.value);
    };
    
    return result;
}

fn LinkedHashSet* LinkedHashSet.tmerge(&self, LinkedHashSet* other) => self.merge(tmem, other);

<*
 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn LinkedHashSet* LinkedHashSet.intersection(&self, Allocator allocator, LinkedHashSet* other)
{
    LinkedHashSet* result = allocator::new(allocator, LinkedHashSet);
    result.init(allocator, math::min(self.table.len, other.table.len), self.load_factor);
    
    // Iterate through the smaller set for efficiency
    LinkedHashSet* smaller = self.count <= other.count ? self : other;
    LinkedHashSet* larger = self.count > other.count ? self : other;
    
    smaller.@each_entry(; LinkedEntry* entry) {
        if (larger.contains(entry.value)) {
            result.add(entry.value);
        }
    };
    
    return result;
}

fn LinkedHashSet* LinkedHashSet.tintersection(&self, LinkedHashSet* other) => self.intersection(tmem, other);

<*
 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn LinkedHashSet* LinkedHashSet.difference(&self, Allocator allocator, LinkedHashSet* other)
{
    LinkedHashSet* result = allocator::new(allocator, LinkedHashSet);
    result.init(allocator, self.table.len, self.load_factor);
    
    self.@each_entry(; LinkedEntry* entry) {
        if (!other.contains(entry.value)) {
            result.add(entry.value);
        }
    };
    
    return result;
}

fn LinkedHashSet* LinkedHashSet.tdifference(&self, LinkedHashSet* other) => self.difference(tmem, other);

<*
 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool LinkedHashSet.is_subset(&self, LinkedHashSet* other)
{
    if (self.count == 0) return true;
    if (self.count > other.count) return false;
    
    self.@each_entry(; LinkedEntry* entry) {
        if (!other.contains(entry.value)) return false;
    };
    return true;
}

// --- private methods

fn void LinkedHashSet.add_entry(&set, uint hash, Value value, uint bucket_index) @private
{
    LinkedEntry* entry = allocator::new(set.allocator, LinkedEntry, {
        .hash = hash,
        .value = value,
        .next = set.table[bucket_index],
        .before = set.tail,
        .after = null
    });
    
    // Update bucket chain
    set.table[bucket_index] = entry;
    
    // Update linked list
    if (set.tail)
    {
        set.tail.after = entry;
        entry.before = set.tail;
    }
    else
    {
        set.head = entry;
    }
    set.tail = entry;
    
    if (set.count++ >= set.threshold)
    {
        set.resize(set.table.len * 2);
    }
}

fn void LinkedHashSet.resize(&set, usz new_capacity) @private
{
    LinkedEntry*[] old_table = set.table;
    usz old_capacity = old_table.len;
    
    if (old_capacity == MAXIMUM_CAPACITY)
    {
        set.threshold = uint.max;
        return;
    }
    
    LinkedEntry*[] new_table = allocator::new_array(set.allocator, LinkedEntry*, new_capacity);
    set.table = new_table;
    set.threshold = (uint)(new_capacity * set.load_factor);
    
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
    
    set.free_internal(old_table.ptr);
}

fn void LinkedHashSet.transfer(&set, LinkedEntry*[] new_table) @private
{
    LinkedEntry*[] src = set.table;
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

fn usz? LinkedHashSet.to_format(&self, Formatter* f) @dynamic
{
    usz len;
    len += f.print("{ ")!;
    self.@each_entry(; LinkedEntry* entry)
    {
        if (len > 2) len += f.print(", ")!;
        len += f.printf("%s", entry.value)!;
    };
    return len + f.print(" }");
}

fn void LinkedHashSet.put_all_for_create(&set, LinkedHashSet* other_set) @private
{
    if (!other_set.count) return;
    foreach (LinkedEntry *e : other_set.table)
    {
        while (e)
        {
            set.put_for_create(e.value);
            e = e.next;
        }
    }
}

fn void LinkedHashSet.put_for_create(&set, Value value) @private
{
    uint hash = rehash(value.hash());
    uint i = index_for(hash, set.table.len);
    for (LinkedEntry *e = set.table[i]; e != null; e = e.next)
    {
        if (e.hash == hash && equals(value, e.value))
        {
            // Value already exists, no need to do anything
            return;
        }
    }
    set.create_entry(hash, value, i);
}

fn void LinkedHashSet.free_internal(&set, void* ptr) @inline @private
{
    allocator::free(set.allocator, ptr);
}

fn bool LinkedHashSet.remove_entry_for_value(&set, Value value) @private
{
    if (!set.count) return false;
    
    uint hash = rehash(value.hash());
    uint i = index_for(hash, set.table.len);
    LinkedEntry* prev = null;
    LinkedEntry* e = set.table[i];
    
    while (e)
    {
        if (e.hash == hash && equals(value, e.value))
        {
            if (prev)
            {
                prev.next = e.next;
            }
            else
            {
                set.table[i] = e.next;
            }
            
            if (e.before)
            {
                e.before.after = e.after;
            }
            else
            {
                set.head = e.after;
            }
            
            if (e.after)
            {
                e.after.before = e.before;
            }
            else
            {
                set.tail = e.before;
            }
            
            set.count--;
            set.free_entry(e);
            return true;
        }
        prev = e;
        e = e.next;
    }
    return false;
}

fn void LinkedHashSet.create_entry(&set, uint hash, Value value, int bucket_index) @private
{
    LinkedEntry* entry = allocator::new(set.allocator, LinkedEntry, { 
        .hash = hash, 
        .value = value, 
        .next = set.table[bucket_index],
        .before = set.tail,
        .after = null
    });
    
    set.table[bucket_index] = entry;
    
    // Update linked list
    if (set.tail)
    {
        set.tail.after = entry;
        entry.before = set.tail;
    }
    else
    {
        set.head = entry;
    }
    set.tail = entry;
    
    set.count++;
}

fn void LinkedHashSet.free_entry(&set, LinkedEntry *entry) @private
{
    allocator::free(set.allocator, entry);
}

struct LinkedHashSetIterator
{
    LinkedHashSet* set;
    LinkedEntry* current;
    bool started;
}

fn usz LinkedHashSetIterator.len(&self) @operator(len) => self.set.count;

int dummy @local;