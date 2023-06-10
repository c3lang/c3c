module std::os::macos::cf @if(DARWIN_LIBC);

def CFAllocatorRef = distinct void*;
def CFAllocatorContextRef = distinct void*;
def CFOptionFlags = usz;

macro CFAllocatorRef default_allocator() => _macos_CFAllocatorGetDefault();
macro void CFAllocatorRef.dealloc(CFAllocatorRef allocator, void* ptr) => _macos_CFAllocatorDeallocate(allocator, ptr);
macro void* CFAllocatorRef.alloc(CFAllocatorRef allocator, usz size) => _macos_CFAllocatorAllocate(allocator, size, 0);
macro usz CFAllocatorRef.get_preferred_size(CFAllocatorRef allocator, usz req_size) => _macos_CFAllocatorGetPreferredSizeForSize(allocator, req_size, 0);
macro void CFAllocatorRef.set_default(CFAllocatorRef allocator) => _macos_CFAllocatorSetDefault(allocator);

extern fn CFAllocatorRef _macos_CFAllocatorCreate(CFAllocatorRef allocator, CFAllocatorContextRef context) @extern("CFAllocatorCreate");
extern fn void _macos_CFAllocatorDeallocate(CFAllocatorRef allocator, void* ptr) @extern("CFAllocatorDeallocate");
extern fn CFAllocatorRef _macos_CFAllocatorGetDefault() @extern("CFAllocatorGetDefault");
extern fn void _macos_CFAllocatorSetDefault(CFAllocatorRef allocator) @extern("CFAllocatorSetDefault");
extern fn void* _macos_CFAllocatorAllocate(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extern("CFAllocatorAllocate");
extern fn CFIndex _macos_CFAllocatorGetPreferredSizeForSize(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extern("CFAllocatorGetPreferredSizeForSize");
