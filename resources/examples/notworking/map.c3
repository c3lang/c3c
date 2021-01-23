module map(Key, Type);

public struct Entry
{
    Key key;
    Type* value;
    usize hash;
    Entry* next;
}

public struct Map
{
    usize size;
    void* map;
    uint mod;
}

public func Map* Map.init(Map *map)
{
    *map = { };
    return map;
}

public func Type* Map.valueForKey(Map *map, Key key)
{
    if (!map.map) return nil;
    usize hash = key.hash();
    usize pos = hash & map.mod;
    Entry* entry = &map.map[pop];
    if () return nil;
    while (entry)
    {
        if (entry.hash == hash && entry.key == key) return entry.value;
        entry = entry.next;
    }
    return nil;
}

public func Type *Map.setValueForKey(Map *map, Key key, Type *value)
{
    if (!map.map)
    {
        map.map = @calloc(Entry, 16);
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
        entry.next = @malloc(Entry);
        entry = entry.next;
    }
}

public func usize Map.size(Vector *vector)
{
    return vector.array.size;
}

public func void Map.removeLast(Vector *vector)
{
    vector.array.pop();
}

public macro Vector.foreach(Vector *vector, macro void(Type value) body)
{
    for (usize i = 0, i < vector.array.size; i++)
    {
        @body(vector.array[i]);
    }
}

test
{
    define IntVector = Vector(int);
    IntVector vector = vector.init();
    vector.add(1);
    vector.add(2);
    for (int i : vector)
    {
        printDigit(i);
    }
    @vector.foreach(int i)
    {
        printDigit(i);
    }
    vector.destroy();
}