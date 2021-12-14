module std::runtime;

struct VirtualAny
{
    void* ptr;
    typeid type_id;
}

struct VirtualContainer
{
    void* ptr;
    void* impl_ptr;
}

struct SubArrayContainer
{
    void* ptr;
    usize len;
}

struct VarArrayHeader
{
    usize size;
    usize capacity;
    void *allocator;

}

