module std::os::macos::cf;

$if (env::OS_TYPE == OsType.MACOSX):

define CFArrayRef = distinct void*;
define CFArrayCallBacksRef = distinct void*;
define CFMutableArrayRef = distinct void*;
extern fn CFArrayRef _macos_CFArrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @extname("CFArrayCreate");
extern fn CFArrayRef _macos_CFArrayCopy(CFAllocatorRef allocator, CFArrayRef array) @extname("CFArrayCopy");
extern fn CFIndex _macos_CFArrayGetCount(CFArrayRef array) @extname("CFArrayGetCount");
extern fn void _macos_CFArrayAppendArray(CFMutableArrayRef theArray, CFArrayRef otherArray, CFRange otherRange) @extname("CFArrayAppendArray");
extern fn CFMutableArrayRef _macos_CFArrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @extname("CFArrayCreateMutable");
extern fn void _macos_CFArrayAppendValue(CFMutableArrayRef theArray, void *value) @extname("CFArrayAppendValue");
$endif;