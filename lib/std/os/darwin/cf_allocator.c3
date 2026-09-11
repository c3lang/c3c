module std::os::darwin::cf @feat(DARWIN) @link("CoreFoundation.framework");

typedef CFAllocatorRef = void*;
typedef CFAllocatorContextRef = void*;
alias CFOptionFlags = usz;

macro CFAllocatorRef default_allocator() => macos_CFAllocatorGetDefault();
macro void CFAllocatorRef.dealloc(CFAllocatorRef allocator, void* ptr) => macos_CFAllocatorDeallocate(allocator, ptr);
macro void* CFAllocatorRef.alloc(CFAllocatorRef allocator, usz size) => macos_CFAllocatorAllocate(allocator, size, 0);
macro usz CFAllocatorRef.get_preferred_size(CFAllocatorRef allocator, usz req_size) => macos_CFAllocatorGetPreferredSizeForSize(allocator, req_size, 0);
macro void CFAllocatorRef.set_default(CFAllocatorRef allocator) => macos_CFAllocatorSetDefault(allocator);

extern fn CFAllocatorRef allocatorCreate(CFAllocatorRef allocator, CFAllocatorContextRef context) @cname("CFAllocatorCreate");
extern fn void allocatorDeallocate(CFAllocatorRef allocator, void* ptr) @cname("CFAllocatorDeallocate");
extern fn CFAllocatorRef allocatorGetDefault() @cname("CFAllocatorGetDefault");
extern fn void allocatorSetDefault(CFAllocatorRef allocator) @cname("CFAllocatorSetDefault");
extern fn void* allocatorAllocate(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @cname("CFAllocatorAllocate");
extern fn CFIndex allocatorGetPreferredSizeForSize(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @cname("CFAllocatorGetPreferredSizeForSize");
