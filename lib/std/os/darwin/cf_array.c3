module std::os::darwin::cf @feat(DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFArray = inline CFType;
typedef CFArrayCallBacksRef = void*;
typedef CFMutableArray = inline CFArray;

alias CFArrayRef = CFArray*;
alias CFMutableArrayRef = CFMutableArray*;

extern fn CFArrayRef arrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @cname("CFArrayCreate");
extern fn CFArrayRef arrayCopy(CFAllocatorRef allocator, CFArrayRef array) @cname("CFArrayCopy");
extern fn CFMutableArrayRef arrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @cname("CFArrayCreateMutable");

extern fn CFIndex CFArray.getCount(&self) @operator(len) @cname("CFArrayGetCount");
extern fn void* CFArray.getValueAtIndex(&self, CFIndex i) @operator([]) @cname("CFArrayGetValueAtIndex");
extern fn void CFMutableArray.appendArray(&self, CFArrayRef otherArray, CFRange otherRange) @cname("CFArrayAppendArray");
extern fn void CFMutableArray.appendValue(&self, void *value) @cname("CFArrayAppendValue");


