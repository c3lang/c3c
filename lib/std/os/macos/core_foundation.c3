module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFType = void;
typedef CFTypeRef = CFType*;
alias CFIndex = isz;

typedef CFString = inline CFType;
alias CFStringRef = CFString*;

struct CFRange
{
	CFIndex location;
	CFIndex length;
}

alias CGFloat = $typefrom(env::ARCH_64_BIT ??? double : float);

struct CGPoint
{
	CGFloat x;
	CGFloat y;
}

struct CGSize
{
	CGFloat width;
	CGFloat height;
}

struct CGRect
{
	CGPoint origin;
	CGSize size;
}

extern fn ZString CFString.getCStringPtr(&self, CFStringEncoding encoding) @cname("CFStringGetCStringPtr");
extern fn ZString CFString.getCString(&self, char* buffer, usz len, CFStringEncoding encoding) @cname("CFStringGetCString");

extern fn CFTypeRef CFType.retain(&self) @cname("CFRetain");
extern fn void CFType.release(&self) @cname("CFRelease");
extern fn CFIndex CFType.getRetainCount(&self) @cname("CFGetRetainCount");

constdef CFStringEncoding : uint
{
	INVALID_ID        = 0xffffffffU,
    MAC_ROMAN         = 0,
    WINDOWS_LATIN_1   = 0x0500,
    ISO_LATIM_1       = 0x0201,
    NEXT_STEP_LATIN   = 0x0B01,
    ASCII             = 0x0600,
    UNICODE           = 0x0100,
    UTF8              = 0x08000100,
    NON_LOSSY_ASCII   = 0x0BFF,

    UTF16             = 0x0100,
    UTF16BE           = 0x10000100,
    UTF16LE           = 0x14000100,

    UTF32             = 0x0c000100,
    UTF32BE           = 0x18000100,
    UTF32LE           = 0x1c000100
}
