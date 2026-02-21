module std::os::darwin @if(env::DARWIN) @link("Foundation.framework");
import std::os::macos::cf, std::os::macos::objc, std::io;

constdef NSSearchPathDomainMask : NSUInteger
{
	USER = 1,
	LOCAL = 2,
	NETWORK = 4,
	SYSTEM = 8,
	ALL = 0x0ffff
}

constdef NSSearchPathDirectory : NSUInteger
{
	APPLICATION = 1,
	DEMO_APPLICATION,
	DEVELOPER_APPLICATION,
	ADMIN_APPLICATION,
	LIBRARY,
	DEVELOPER,
	USER,
	DOCUMENTATION,
	DOCUMENT,
	CORE_SERVICE,
	AUTOSAVED_INFORMATION,
	DESKTOP = 12,
	CACHES = 13,
	APPLICATION_SUPPORT = 14,
	DOWNLOADS = 15,
	INPUT_METHODS = 16,
	MOVIES = 17,
	MUSIC = 18,
	PICTURES = 19,
	PRINTER_DESCRIPTION = 20,
	SHARED_PUBLIC = 21,
	PREFERENCE_PANES = 22,
	APPLICATION_SCRIPTS = 23,
	ITEM_REPLACEMENT = 99,
	ALL_APPLICATIONS = 100,
	ALL_LIBRARIES = 101,
	TRASH = 102,
}

// real signature in Foundation
extern fn CFArrayRef nsSearchPathForDirectoriesInDomains(NSSearchPathDirectory directory, NSSearchPathDomainMask domainMask, bool expandTilde) @cname("NSSearchPathForDirectoriesInDomains");

fn String? find_first_directory_temp(NSSearchPathDirectory directory, NSSearchPathDomainMask domainMask)
{
	objc::@autoreleasepool()
	{
		CFArrayRef arr = nsSearchPathForDirectoriesInDomains(directory, domainMask, true);
		if (!arr.getCount()) return io::PATH_COULD_NOT_BE_FOUND~;
        CFStringRef str = (CFStringRef)arr.getValueAtIndex(0);
		char* buffer = tmalloc(2048);
        if (!str.getCString(buffer, 2048, UTF8)) return io::PATH_COULD_NOT_BE_FOUND~;
        return ((ZString)buffer).str_view();
	};
}
