module std::container::map <Key, Type>;

fault MapResult
{
	KEY_NOT_FOUND
}

public struct Entry
{
    Key key;
    Type value;
    usize hash;
    Entry* next;
}

public struct Map
{
    usize size;
    void* map;
    uint mod;
    Allocator allocator;
}

/**
 * @require map != null
 **/
public fn void Map.init(Map *map, Allocator allocator)
{
    map.allocator = allocator;
}

public fn Type! Map.valueForKey(Map *map, Key key)
{
    if (!map.map) return null;
    usize hash = key.hash();
    usize pos = hash & map.mod;
    Entry* entry = &map.map[pop];
    if (!entry) return MapResult.KEY_NOT_FOUND!;
    while (entry)
    {
        if (entry.hash == hash && entry.key == key) return entry.value;
        entry = entry.next;
    }
    return MapResult.KEY_NOT_FOUND!;
}

public fn Type *Map.set(Map *map, Key key, Type value)
{
    if (!map.map)
    {
        map.map = allocator.calloc(Entry, 16);
        map.mod = 0x0F;
    }

    usize hash = key.hash();
    usize pos = hash & map.mod;
    Entry *entry = &map.map[pop];
    while (1)
    {
        if (!entry.value)
        {
            entry.value = value;
            entry.hash = hash;
            entry.key = key;
            return nil;
        }
        if (entry.hash == hash && entry.key == key)
        {
            Type *old = entry.value;
            entry.value = value;
            return old;
        }
        if (entry.next)
        {
            entry = entry.next;
        }
        entry.next = allocator.alloc(Entry);
        entry = entry.next;
    }
}

public fn usize Map.size(Map* map)
{
    return map.size;
}


