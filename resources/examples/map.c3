module std::container::faults;

faultdef KEY_NOT_FOUND;

module std::container::map <Key, Type>;
import std::core::builtin;
import std::io;

struct Entry
{
    Key key;
    Type value;
    usz hash;
    Entry* next;
    bool used;
}

struct Map
{
    usz size;
    Entry* map;
    uint mod;
}

/**
 * @require map != null
 **/
fn void Map.init(Map *map, uint capacity = 128)
{
	if (capacity < 16) capacity = 4;
	capacity = math::next_power_of_2(capacity);
	map.map = calloc(Entry.sizeof * capacity);
    map.mod = capacity - 1;
}

fn Type? Map.valueForKey(Map *map, Key key)
{
    if (!map.map) return faults::KEY_NOT_FOUND~;
    uint hash = key.hash();
    usz pos = hash & map.mod;
    Entry* entry = &map.map[pos];
    if (!entry) return faults::KEY_NOT_FOUND~;
    while (entry)
    {
        if (entry.hash == hash && entry.key == key) return entry.value;
        entry = entry.next;
    }
    return faults::KEY_NOT_FOUND~;
}

fn Type? Map.set(Map *map, Key key, Type value) @maydiscard
{
	if (!map.map)
    {
    	map.map = calloc(Entry.sizeof * 16);
        map.mod = 0x0F;
    }

    uint hash = key.hash();
    uint pos = hash & map.mod;
    Entry *entry = &map.map[pos];
    while (1)
    {
        if (!entry.used)
        {
            entry.used = true;
            entry.value = value;
            entry.hash = hash;
            entry.key = key;
            return faults::KEY_NOT_FOUND~;
        }
        if (entry.hash == hash && entry.key == key)
        {
            Type old = entry.value;
            entry.value = value;
            return old;
        }
        if (entry.next)
        {
            entry = entry.next;
            continue;
        }
        Entry* new = mem::calloc(Entry.sizeof);
        new.hash = hash;
        new.key = key;
        new.value = value;
        new.next = null;
        new.used = true;
        entry.next = new;
        return faults::KEY_NOT_FOUND~;
    }
}

fn usz Map.size(Map* map)
{
    return map.size;
}
