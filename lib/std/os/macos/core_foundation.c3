module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

distinct CFTypeRef = void*;
def CFIndex = isz;
struct CFRange
{
	CFIndex location;
	CFIndex length;
}

extern fn CFTypeRef macos_CFRetain(CFTypeRef cf) @extern("CFRetain") @builtin;
extern fn void macos_CFRelease(CFTypeRef cf) @extern("CFRelease") @builtin;