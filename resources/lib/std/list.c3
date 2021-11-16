module std::array::list<Type>;
import std::mem;

struct List
{
    usize size;
    usize capacity;
    Type *entries;
}

private fn void List.ensureCapacity(List *list) @inline
{
    if (list.capacity == list.size)
    {
        list.capacity = list.capacity ? 2 * list.capacity : 16;
        list.entries = mem::realloc(list.entries, $sizeof(Type) * list.capacity);
    }
}

fn void List.push(List *list, Type element) @inline
{
    list.append(element);
}

fn void List.append(List *list, Type element)
{
    list.ensureCapacity();
    list.entries[list.size++] = element;
}

/**
 * @require list.size > 0
 */
fn Type List.pop(List *list)
{
    return list.entries[--list.size];
}

/**
 * @require list.size > 0
 */
fn Type List.popFirst(List *list)
{
    Type value = list.entries[0];
    list.removeAt(0);
    return value;
}

fn void List.removeAt(List *list, usize index)
{
    for (usize i = index + 1; i < list.size; i++)
    {
        list.entries[i - 1] = list.entries[i];
    }
    list.size--;
}

fn void List.pushFront(List *list, Type type) @inline
{
    list.insertAt(0, type);
}

fn void List.insertAt(List *list, usize index, Type type)
{
    list.ensureCapacity();
    for (usize i = list.size; i > index; i--)
    {
        list.entries[i] = list.entries[i - 1];
    }
    list.size++;
    list.entries[index] = type;
}

fn void List.removeLast(List *list)
{
    list.size--;
}

fn void List.removeFirst(List *list)
{
    list.removeAt(0);
}

fn Type* List.first(List *list)
{
    return list.size ? &list.entries[0] : null;
}

fn Type* List.last(List *list)
{
    return list.size ? &list.entries[list.size - 1] : null;
}

fn bool List.isEmpty(List *list)
{
    return list.size;
}

fn usize List.len(List *list)
{
    return list.size;
}

fn Type List.get(List *list, usize index)
{
    return list.entries[index];
}

fn void List.free(List *list)
{
    mem::free(list.entries);
    list.capacity = 0;
    list.size = 0;
}

/*

operator for(List *list; index, Type type)
{
    $IndexType = typeof(index);
    $IndexType last_index = ($IndexType)(list.size);
    for ($IndexType i = 0; i < last_index; i++)
    {
        yield(i, list.entries[index]);
    }
}

operator for(List *list; index, Type *type)
{
    $IndexType = typeof(index);
    $IndexType last_index = ($IndexType)(list.size);
    for ($IndexType i = 0; i < last_index; i++)
    {
        yield(i, &list.entries[index]);
    }
}

operator for(List *list; Type *type)
{
    usize size = list.size;
    for (usize i = 0; i < last_index; i++)
    {
        yield(i, &list.entries[index]);
    }
}
*/
/*
operator for(List *list; Type type)
{
    usize size = list.size;
    for (usize i = 0; i < last_index; i++)
    {
        yield(i, list.entries[index]);
    }
}*/