module std::array;
import std::mem;

public macro make($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::alloc($Type.sizeof, elements);
    return ptr[0..(elements - 1)];
}

public macro make_zero($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::calloc($Type.sizeof, elements);
    return ptr[0..(elements - 1)];
}

public template vararray <Type>
{
    public struct VarArray
    {
        usize size;
        usize capacity;
        Type *entries;
    }

    public func void VarArray.append(VarArray *array, Type element)
    {
        if (array.capacity == array.size)
        {
            array.capacity = array.capacity ? 2 * array.capacity : 16;
            array.entries = mem::realloc(array.entries, Type.sizeof * array.capacity);
        }
        array.entries[array.size++] = element;
    }

    public func usize VarArray.len(VarArray *array)
    {
        return array.size;
    }

    public func Type VarArray.get(VarArray *array, usize index)
    {
        return array.entries[index];
    }

    public func void VarArray.free(VarArray *array)
    {
        mem::free(array.entries);
        array.capacity = 0;
        array.size = 0;
    }
}

