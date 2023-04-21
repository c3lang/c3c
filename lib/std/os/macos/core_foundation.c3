module std::os::macos::cf;

$if (env::os_is_darwin())

typedef CFTypeRef = distinct void*;
typedef CFIndex = isz;
struct CFRange
{
	CFIndex location;
	CFIndex length;
}

extern fn CFTypeRef _macos_CFRetain(CFTypeRef cf) @extern("CFRetain");
extern fn void _macos_CFRelease(CFTypeRef cf) @extern("CFRelease");

$endif