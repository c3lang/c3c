module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFArray = inline CFType;
alias CFArrayRef = CFArray*;
typedef CFArrayCallBacksRef = void*;
typedef CFMutableArray = inline CFArray;
typedef CFMutableArrayRef = CFMutableArray*;

extern fn CFIndex CFArray.getCount(&self) @extern("CFArrayGetCount");
extern fn void* CFArray.getValueAtIndex(&self, CFIndex i) @extern("CFArrayGetValueAtIndex");

extern fn CFArrayRef macos_CFArrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @extern("CFArrayCreate") @builtin;
extern fn CFArrayRef macos_CFArrayCopy(CFAllocatorRef allocator, CFArrayRef array) @extern("CFArrayCopy") @builtin;
extern fn void CFMutableArray.appendArray(&self, CFArrayRef otherArray, CFRange otherRange) @extern("CFArrayAppendArray");
extern fn void CFMutableArray.appendValue(&self, void *value) @extern("CFArrayAppendValue");

extern fn CFMutableArrayRef macos_CFArrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @extern("CFArrayCreateMutable") @builtin;

