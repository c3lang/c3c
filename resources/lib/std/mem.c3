module std::mem;

extern func void* _malloc(usize bytes) @cname("malloc");
extern func void* _calloc(usize bytes, usize elements) @cname("calloc");
extern func void _free(void* ptr) @cname("free");

public macro malloc($Type)
{
    // TODO: return cast(_malloc($Type.sizeof) as $Type*);
    return cast(mem::alloc($Type.sizeof) as $Type*);
}
public func void* alloc(usize size, usize elements = 1) @inline
{
    return _malloc(size * elements);
}

public func void* calloc(usize size, usize elements = 1) @inline
{
    return _calloc(size, elements);
}
public func void free(void* ptr) @inline
{
    _free(ptr);
}