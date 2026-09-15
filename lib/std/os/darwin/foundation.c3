module std::os::darwin @feat(DARWIN) @link("Foundation.framework");
import std::io, std::os::darwin::objc;

alias NSUInteger = env::ARCH_64_BIT ??? ulong : uint;
alias NSInteger = env::ARCH_64_BIT ??? long : int;


fn ObjId string(ZString str)
{
	return objc::send(objc::@class("NSString"), ObjId, "stringWithUTF8String:", str);
}

fn ZString string_to_zstr(ObjId str)
{
	return str.send(ZString, "UTF8String");
}

fn ObjId String.to_nsstring(self) => @pool()
{
	return string(self.zstr_copy(tmem));
}

constset NSSearchPathDomainMask : NSUInteger
{
	USER = 1,
	LOCAL = 2,
	NETWORK = 4,
	SYSTEM = 8,
	ALL = 0x0ffff
}


constset NSSearchPathDirectory : NSUInteger
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
