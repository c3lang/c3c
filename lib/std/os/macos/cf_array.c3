module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFArrayRef = void*;
typedef CFArrayCallBacksRef = void*;
typedef CFMutableArrayRef = void*;
extern fn CFArrayRef macos_CFArrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @extern("CFArrayCreate") @builtin;
extern fn CFArrayRef macos_CFArrayCopy(CFAllocatorRef allocator, CFArrayRef array) @extern("CFArrayCopy") @builtin;
extern fn CFIndex macos_CFArrayGetCount(CFArrayRef array) @extern("CFArrayGetCount") @builtin;
extern fn void macos_CFArrayAppendArray(CFMutableArrayRef theArray, CFArrayRef otherArray, CFRange otherRange) @extern("CFArrayAppendArray") @builtin;
extern fn CFMutableArrayRef macos_CFArrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @extern("CFArrayCreateMutable") @builtin;
extern fn void macos_CFArrayAppendValue(CFMutableArrayRef theArray, void *value) @extern("CFArrayAppendValue") @builtin;
