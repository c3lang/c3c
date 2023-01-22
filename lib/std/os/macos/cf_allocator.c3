module std::os::macos::cf;

$if (env::OS_TYPE == OsType.MACOSX):

define CFAllocatorRef = distinct void*;
define CFAllocatorContextRef = distinct void*;
define CFOptionFlags = usz;

macro CFAllocatorRef default_allocator() => _macos_CFAllocatorGetDefault();
macro void CFAllocatorRef.dealloc(CFAllocatorRef allocator, void* ptr) => _macos_CFAllocatorDeallocate(allocator, ptr);
macro void* CFAllocatorRef.alloc(CFAllocatorRef allocator, usz size) => _macos_CFAllocatorAllocate(allocator, size, 0);
macro usz CFAllocatorRef.get_preferred_size(CFAllocatorRef allocator, usz req_size) => _macos_CFAllocatorGetPreferredSizeForSize(allocator, req_size, 0);
macro void CFAllocatorRef.set_default(CFAllocatorRef allocator) => _macos_CFAllocatorSetDefault(allocator);

extern fn CFAllocatorRef _macos_CFAllocatorCreate(CFAllocatorRef allocator, CFAllocatorContextRef context) @extname("CFAllocatorCreate");
extern fn void _macos_CFAllocatorDeallocate(CFAllocatorRef allocator, void* ptr) @extname("CFAllocatorDeallocate");
extern fn CFAllocatorRef _macos_CFAllocatorGetDefault() @extname("CFAllocatorGetDefault");
extern fn void _macos_CFAllocatorSetDefault(CFAllocatorRef allocator) @extname("CFAllocatorSetDefault");
extern fn void* _macos_CFAllocatorAllocate(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extname("CFAllocatorAllocate");
extern fn CFIndex _macos_CFAllocatorGetPreferredSizeForSize(CFAllocatorRef allocator, CFIndex size, CFOptionFlags hint) @extname("CFAllocatorGetPreferredSizeForSize");
$endif;