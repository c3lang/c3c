module std::os::macos::cf @if(DARWIN_LIBC);

def CFArrayRef = distinct void*;
def CFArrayCallBacksRef = distinct void*;
def CFMutableArrayRef = distinct void*;
extern fn CFArrayRef _macos_CFArrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @extern("CFArrayCreate");
extern fn CFArrayRef _macos_CFArrayCopy(CFAllocatorRef allocator, CFArrayRef array) @extern("CFArrayCopy");
extern fn CFIndex _macos_CFArrayGetCount(CFArrayRef array) @extern("CFArrayGetCount");
extern fn void _macos_CFArrayAppendArray(CFMutableArrayRef theArray, CFArrayRef otherArray, CFRange otherRange) @extern("CFArrayAppendArray");
extern fn CFMutableArrayRef _macos_CFArrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @extern("CFArrayCreateMutable");
extern fn void _macos_CFArrayAppendValue(CFMutableArrayRef theArray, void *value) @extern("CFArrayAppendValue");
