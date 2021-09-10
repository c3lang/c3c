module std::array;
import std::mem;

macro make($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::alloc($sizeof($Type), elements);
    return ptr[0..(elements - 1)];
}

macro make_zero($Type, usize elements)
{
    assert(elements > 0);
    $Type* ptr = mem::calloc($sizeof($Type), elements);
    return ptr[0..(elements - 1)];
}
