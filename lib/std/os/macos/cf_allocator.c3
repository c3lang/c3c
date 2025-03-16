module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFAllocatorRef = void*;
typedef CFAllocatorContextRef = void*;
alias CFOptionFlags = usz;

macro CFAllocatorRef default_allocator() => macos_CFAllocatorGetDefault();
macro void CFAllocatorRef.dealloc(CFAllocatorRef allocator, void* ptr) => macos_CFAllocatorDeallocate(allocator, ptr);
macro void* CFAllocatorRef.alloc(CFAllocatorRef allocator, usz size) => macos_CFAllocatorAllocate(allocator, size, 0);
macro usz CFAllocatorRef.get_preferred_size(CFAllocatorRef allocator, usz req_size) => macos_CFAllocatorGetPreferredSizeForSize(allocator, req_size, 0);
macro void CFAllocatorRef.set_default(CFAllocatorRef allocator) => macos_CFAllocatorSetDefault(allocator);

extern fn CFAllocatorRef macos_CFAllocatorCreate(CFAllocatorRef allocator, CFAllocatorContextRef context) @extern("CFAllocatorCreate") @builtin;
extern fn void macos_CFAllocatorDeallocate(CFAllocatorRef allocator, void* ptr) @extern("CFAllocatorDeallocate") @builtin;
extern fn CFAllocatorRef macos_CFAllocatorGetDefault() @extern("CFAllocatorGetDefault") @builtin;
extern fn void macos_CFAllocatorSetDefault(CFAllocatorRef allocator) @extern("CFAllocatorSetDefault") @builtin;
extern fn void* macos_CFAllocatorAllocate(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extern("CFAllocatorAllocate") @builtin;
extern fn CFIndex macos_CFAllocatorGetPreferredSizeForSize(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extern("CFAllocatorGetPreferredSizeForSize") @builtin;
