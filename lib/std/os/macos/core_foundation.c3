module std::os::macos::cf;

$if (env::OS_TYPE == OsType.MACOSX):

define CFTypeRef = distinct void*;
define CFIndex = isz;
struct CFRange
{
	CFIndex location;
	CFIndex length;
}

extern fn CFTypeRef _macos_CFRetain(CFTypeRef cf) @extname("CFRetain");
extern fn void _macos_CFRelease(CFTypeRef cf) @extname("CFRelease");

$endif;