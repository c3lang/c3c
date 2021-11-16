module std::mem;

extern fn void* _malloc(usize bytes) @extname("malloc");
extern fn void* _realloc(void* ptr, usize bytes) @extname("realloc");
extern fn void* _calloc(usize bytes, usize elements) @extname("calloc");
extern fn void _free(void* ptr) @extname("free");

macro volatile_load(&x)
{
    return $$volatile_load(&x);
}

macro volatile_store(&x, y)
{
    return $$volatile_store(&x, y);
}

enum AllocationKind
{
    ALLOC,
    REALLOC,
    FREE,
}

errtype AllocationFailure
{
    OUT_OF_MEMORY
}

define AllocatorFunction = fn void!(void *data, void** pointer, usize bytes, usize alignment, AllocationKind kind);

struct Allocator
{
    AllocatorFunction allocation_function;
    void *data;
}

fn void copy(char* dst, char* src, usize size)
{
    for (usize i = 0; i < size; i++) dst[i] = src[i];
}

fn void! system_malloc_function(void *unused, void** pointer, usize bytes, usize alignment, AllocationKind kind) @inline
{
    switch (kind)
    {
        case ALLOC:
            void* data = _malloc(bytes);
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            *pointer = data;
            return;
        case REALLOC:
            void* data = _realloc(*pointer, bytes);
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            *pointer = data;
            return;
        case FREE:
            _free(*pointer);
            *pointer = null;
            return;
    }
    $unreachable;
}


struct SlotAllocator
{
    void* pages;
    usize page_size;
    usize page_count;
    usize bitmask;
    usize current_page;
}

fn void*! SlotAllocator.alloc(SlotAllocator *allocator, usize size)
{
    void* active_page = (char*)(allocator.pages) + allocator.current_page * allocator.page_size;
    void** page_pointer = (void**)(active_page);
    if (*page_pointer)
    {
        mem::free(*page_pointer);
        *page_pointer = null;
    }
    if (size > allocator.page_size - $sizeof(page_pointer))
    {
        void* mem = mem::_malloc(size);
        if (!mem) return AllocationFailure.OUT_OF_MEMORY!;
        *page_pointer = mem;
        allocator.current_page = (allocator.current_page + 1) & (allocator.bitmask);
        return mem;
    }
    allocator.current_page = (allocator.current_page + 1) & (allocator.bitmask);
    return &page_pointer[1];
}

struct RingAllocator
{
    char *data;
    usize size;
    usize offset;
}


fn void* RingAllocator.alloc(RingAllocator *allocator, usize size)
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

fn void* RingAllocator.realloc(RingAllocator *allocator, void* ptr, usize size)
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

fn void* alloc(usize size, usize count = 1) @inline
{
    return _malloc(size * count);
}

fn void* calloc(usize size, usize elements = 1) @inline
{
    return _calloc(size, elements);
}

fn void* realloc(void *ptr, usize size) @inline
{
    return _realloc(ptr, size);
}

fn void free(void* ptr) @inline
{
    _free(ptr);
}


const TEMP_BLOCK_SIZE = 1024;
const TEMP_PAGES = 64;

private char[TEMP_BLOCK_SIZE * TEMP_PAGES] allocator_static_storage;
private void*[TEMP_PAGES] allocator_static_page_storage;

SlotAllocator default_allocator = {
    .pages = &allocator_static_storage,
    .page_size = TEMP_BLOCK_SIZE,
    .page_count = TEMP_PAGES,
    .bitmask = TEMP_PAGES - 1,
    .current_page = 0,
};

fn void*! talloc(usize size)
{
    return default_allocator.alloc(size);
}