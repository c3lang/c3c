module std::os::win32 @if(env::WIN32);

extern fn void* _aligned_malloc(usz size, usz alignment);
extern fn void* _aligned_realloc(void* memblock, usz size, usz alignment);
extern fn void* _aligned_recalloc(void* memblock, usz num, usz size, usz alignment);
extern fn void _aligned_free(void* memblock);
extern fn void _aligned_msize(void* memblock, usz alignment, usz offset);
extern fn void* _aligned_offset_malloc(usz size, usz alignment, usz offset);
extern fn void* _aligned_offset_realloc(void* memblock, usz size, usz alignment, usz offset);
extern fn void* _aligned_offset_recalloc(void* memblock, usz num, usz size, usz alignment, usz offset);
extern fn usz _msize(void* memblock);