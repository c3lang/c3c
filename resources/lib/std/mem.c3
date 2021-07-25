module std::mem;

extern func void* _malloc(usize bytes) @extname("malloc");
extern func void* _realloc(void* ptr, usize bytes) @extname("realloc");
extern func void* _calloc(usize bytes, usize elements) @extname("calloc");
extern func void _free(void* ptr) @extname("free");

enum AllocationKind
{
    ALLOC,
    REALLOC,
    FREE,
}
enum AllocationFailureKind
{
    OUT_OF_MEMORY
}

errtype AllocationFailure
{
    AllocationFailureKind failureKind;
}

define AllocatorFunction = func void!(void *data, void** pointer, usize bytes, usize alignment, AllocationKind kind);

struct Allocator
{
    AllocatorFunction allocation_function;
    void *data;
}

func void! system_malloc_function(void *unused, void** pointer, usize bytes, usize alignment, AllocationKind kind) @inline
{
    switch (kind)
    {
        case ALLOC:
            void* data = _malloc(bytes);
            if (!data) return AllocationFailure({ OUT_OF_MEMORY })!;
            *pointer = data;
            return;
        case REALLOC:
            void* data = _realloc(*pointer, bytes);
            if (!data) return AllocationFailure({ OUT_OF_MEMORY })!;
            *pointer = data;
            return;
        case FREE:
            _free(*pointer);
            *pointer = null;
            return;
    }
    $unreachable;
}

struct RingAllocator
{
    char *data;
    usize size;
    usize offset;
}


func void* RingAllocator.alloc(RingAllocator *allocator, usize size)
{
    if (size > allocator.size) return null;
    // Wraparound? If so, start at the beginning.
    if (allocator.offset + size > allocator.size)
    {
        allocator.offset = size;
        return allocator.data;
    }
    void* data = allocator.offset + allocator.data;
    allocator.offset = (allocator.offset + size) & allocator.size;
    return data;
}

func void* RingAllocator.realloc(RingAllocator *allocator, void* ptr, usize size)
{
    if (size > allocator.size) return null;
    assert(allocator.data >= ptr && ptr < allocator.data + size, "Realloc on other allocator.");
    // 1. The pointer is before the allocator
    if (allocator.data + allocator.offset > ptr)
    {
        if (allocator.data + allocator.size < ptr + size)
        {
            // 1a. There is not enough space, we need to copy to the start.
            usize pointer_offset = ptr - allocator.data;
            usize copy_len = pointer_offset + size > allocator.offset ? allocator.offset - pointer_offset : size;
            //memcpy(allocator.data, ptr, copy_len);
            allocator.offset = size;
            return allocator.data;
        }
        // 1b. There is enough space, so we just change the offset:
        allocator.offset = ptr - allocator.data + size;
        return ptr;
    }
    // 2. The pointer is after the allocator
    // 2a. Is there sufficient space?
    if (ptr + size <= allocator.data + allocator.size)
    {
        // Good, if so we simply change the offset and return the pointer.
        allocator.offset = ptr - allocator.data + size;
        return ptr;
    }
    // 2b. Not sufficient space, we copy to the beginning.
    usize pointer_offset = ptr - allocator.data;
    usize copy_len = allocator.size - (ptr - allocator.data);
    if (copy_len > size) copy_len = size;
    //memcpy(allocator.data, ptr, copy_len);
    allocator.offset = size;
    return allocator.data;
}

Allocator main_allocator = { &system_malloc_function, null };

macro malloc($Type)
{
    return ($Type*)(mem::alloc($sizeof($Type)));
}

func void* alloc(usize size, usize elements = 1) @inline
{
    return _malloc(size * elements);
}

func void* calloc(usize size, usize elements = 1) @inline
{
    return _calloc(size, elements);
}

func void* realloc(void *ptr, usize size) @inline
{
    return _realloc(ptr, size);
}

func void free(void* ptr) @inline
{
    _free(ptr);
}