module std::os::macos::objc @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef ObjcClass = void*;
typedef ObjcMethod = void*;
typedef ObjcIvar = void*;
typedef ObjcSelector = void*;
alias ObjcId = void*;
alias SendVoid = fn void*(void*, ObjcSelector);

faultdef CLASS_NOT_FOUND, UNKNOWN_EVENT;

macro ZString ObjcClass.name(ObjcClass cls) => class_getName(cls);
macro ObjcClass ObjcClass.superclass(ObjcClass cls) => class_getSuperclass(cls);
macro bool ObjcClass.responds_to(ObjcClass cls, ObjcSelector sel) => class_respondsToSelector(cls, sel);
macro ObjcMethod ObjcClass.method(ObjcClass cls, ObjcSelector name) => class_getClassMethod(cls, name);

macro bool ObjcSelector.equals(ObjcSelector a, ObjcSelector b) => a == b;
macro bool ObjcClass.equals(ObjcClass a, ObjcClass b) => a == b;

fn ObjcId alloc(ObjcClass cls) => objc::msg_send(cls, SendVoid, "alloc");
fn void release(ObjcId id) => objc::msg_send(id, SendVoid, "release"); 

alias NSUInteger = $typefrom(env::ARCH_64_BIT ??? ulong : uint);
alias NSInteger = $typefrom(env::ARCH_64_BIT ??? long : int);

macro ObjcClass? class_by_name(ZString c)
{
	ObjcClass cls = objc::lookUpClass(c);
	return cls ?: CLASS_NOT_FOUND?;
}

macro ObjcClass[] class_get_list(Allocator allocator)
{
	int num_classes = objc::getClassList(null, 0);
	if (!num_classes) return {};
	ObjcClass[] entries = allocator.new_array(ObjcClass, num_classes);
	objc::getClassList(entries.ptr, entries.len);
	return entries;
}

extern fn void msgSend(...) @cname("objc_msgSend") @builtin;
extern fn ObjcSelector sel_getUid(ZString);

macro msg_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&msgSend)((ObjcId)id, sel_getUid($selector), $vasplat);
}

macro void @autoreleasepool(;@body())
{
	void* ctx = objc_autoreleasePoolPush();
	defer objc_autoreleasePoolPop(ctx);
	@body();
}

extern fn void* objc_autoreleasePoolPush();
extern fn void objc_autoreleasePoolPop(void* context);
extern fn ObjcClass getClass(ZString name) @cname("objc_getClass");
extern fn int getClassList(ObjcClass* buffer, int buffer_count) @cname("objc_getClassList");
extern fn ObjcClass lookUpClass(ZString name) @cname("objc_lookUpClass") @builtin;

extern fn ZString class_getName(ObjcClass cls);
extern fn ObjcClass class_getSuperclass(ObjcClass cls);
extern fn ObjcMethod class_getClassMethod(ObjcClass cls, ObjcSelector name);
extern fn bool class_respondsToSelector(ObjcClass cls, ObjcSelector name);
extern fn ObjcSelector sel_registerName(ZString str);
extern fn bool class_addIvar(ObjcClass cls, ZString name, int size, double alignment, ZString types);
extern fn bool class_addMethod(ObjcClass cls, ObjcSelector name, void* imp, ZString types);

extern fn ObjcIvar getInstanceVariable(ObjcId id, ZString name, void* outValue) @cname("object_getInstanceVariable");
extern fn ObjcIvar setInstanceVariable(ObjcId id, ZString name, void* value) @cname("object_setInstanceVariable");
extern fn ObjcClass allocateClassPair(ObjcClass cls, ZString name, uint extraBytes) @cname("objc_allocateClassPair");

enum StatusItemLength : const inline double
{
    VARIABLE = -1.0,
    SQUARE   = -2.0,
}

enum ApplicationActivationPolicy : const inline NSInteger
{
	REGULAR    = 0,
	ACCESSORY  = 1,
	PROHIBITED = 2,
}

enum BackingStore : const inline NSUInteger
{
	RETAINED    = 0,
	NONRETAINED = 1,
	BUFFERED    = 2
}

enum EventType : const inline NSUInteger
{
	LEFT_MOUSE_DOWN     = 1,
	LEFT_MOUSE_UP       = 2,
	RIGHT_MOUSE_DOWN    = 3,
	RIGHT_MOUSE_UP      = 4,
	MOUSE_MOVED         = 5,
	LEFT_MOUSE_DRAGGED  = 6,
	RIGHT_MOUSE_DRAGGED = 7,
	MOUSE_ENTERED       = 8,
	MOUSE_EXITED        = 9,
	KEY_DOWN            = 10,
	KEY_UP              = 11,
	FLAGS_CHANGED       = 12,
	APPKIT_DEFINED      = 13,
	SYSTEM_DEFINED      = 14,
	APPLICATION_DEFINED = 15,
	PERIODIC            = 16,
	CURSOR_UPDATE       = 17,
	SCROLL_WHEEL        = 22,
	TABLET_POINT        = 23,
	TABLET_PROXIMITY    = 24,
	OTHER_MOUSE_DOWN    = 25,
	OTHER_MOUSE_UP      = 26,
	OTHER_MOUSE_DRAGGED = 27,
	GESTURE             = 29,
	MAGNIFY             = 30,
	SWIPE               = 31,
	ROTATE              = 18,
	BEGIN_GESTURE       = 19,
	END_GESTURE         = 20,
	SMART_MAGNIFY       = 32,
	QUICK_LOOK          = 33,
	PRESSURE            = 34,
	DIRECT_TOUCH        = 37,
	CHANGE_MODE         = 38,
}

enum EventMask : const inline ulong
{
	LEFT_MOUSE_DOWN     = 1ul << EventType.LEFT_MOUSE_DOWN,
	LEFT_MOUSE_UP       = 1ul << EventType.LEFT_MOUSE_UP,
	RIGHT_MOUSE_DOWN    = 1ul << EventType.RIGHT_MOUSE_DOWN,
	RIGHT_MOUSE_UP      = 1ul << EventType.RIGHT_MOUSE_UP,
	MOUSE_MOVED         = 1ul << EventType.MOUSE_MOVED,
	LEFT_MOUSE_DRAGGED  = 1ul << EventType.LEFT_MOUSE_DRAGGED,
	RIGHT_MOUSE_DRAGGED = 1ul << EventType.RIGHT_MOUSE_DRAGGED,
	MOUSE_ENTERED       = 1ul << EventType.MOUSE_ENTERED,
	MOUSE_EXITED        = 1ul << EventType.MOUSE_EXITED,
	KEY_DOWN            = 1ul << EventType.KEY_DOWN,
	KEY_UP              = 1ul << EventType.KEY_UP,
	FLAGS_CHANGED       = 1ul << EventType.FLAGS_CHANGED,
	APPKIT_DEFINED      = 1ul << EventType.APPKIT_DEFINED,
	SYSTEM_DEFINED      = 1ul << EventType.SYSTEM_DEFINED,
	APPLICATION_DEFINED = 1ul << EventType.APPLICATION_DEFINED,
	PERIODIC            = 1ul << EventType.PERIODIC,
	CURSOR_UPDATE       = 1ul << EventType.CURSOR_UPDATE,
	ROTATE              = 1ul << EventType.ROTATE,
	BEGIN_GESTURE       = 1ul << EventType.BEGIN_GESTURE,
	END_GESTURE         = 1ul << EventType.END_GESTURE,
	SCROLL_WHEEL        = 1ul << EventType.SCROLL_WHEEL,
	TABLET_POINT        = 1ul << EventType.TABLET_POINT,
	TABLET_PROXIMITY    = 1ul << EventType.TABLET_PROXIMITY,
	OTHER_MOUSE_DOWN    = 1ul << EventType.OTHER_MOUSE_DOWN,
	OTHER_MOUSE_UP      = 1ul << EventType.OTHER_MOUSE_UP,
	OTHER_MOUSE_DRAGGED = 1ul << EventType.OTHER_MOUSE_DRAGGED,
	GESTURE             = 1ul << EventType.GESTURE,
	MAGNIFY             = 1ul << EventType.MAGNIFY,
	SWIPE               = 1ul << EventType.SWIPE,
	SMART_MAGNIFY       = 1ul << EventType.SMART_MAGNIFY,
	QUICK_LOOK          = 1ul << EventType.QUICK_LOOK,
	PRESSURE            = 1ul << EventType.PRESSURE,
	DIRECT_TOUCH        = 1ul << EventType.DIRECT_TOUCH,
	CHANGE_MODE         = 1ul << EventType.CHANGE_MODE,
	ANY                 = ulong.max,
}


fn EventMask event_mask_from_type(EventType type) => (EventMask)1ul << type;

enum EventModifierFlag : const inline NSUInteger
{
	CAPS_LOCK   = 1 << 16,
	SHIFT       = 1 << 17,
	CONTROL     = 1 << 18,
	OPTION      = 1 << 19,
	COMMAND     = 1 << 20,
	NUMERIC_PAD = 1 << 21,
    FUNCTION    = 1 << 23,
	HELP        = 1 << 22,
}

enum WindowCollectionBehavior : const inline NSUInteger
{
	DEFAULT                       = 0,
	CAN_JOIN_ALL_SPACES           = 1 << 0,
	MOVE_TO_ACTIVE_SPACE          = 1 << 1,
	MANAGED                       = 1 << 2,
	TRANSIENT                     = 1 << 3,
	STATIONARY                    = 1 << 4,
	PARTICIPATES_IN_CYCLE         = 1 << 5,
	IGNORES_CYCLE                 = 1 << 6,
	FULL_SCREEN_PRIMARY           = 1 << 7,
	FULL_SCREEN_AUXILIARY         = 1 << 8,
	FULL_SCREEN_NONE              = 1 << 9,
	FULL_SCREEN_ALLOWS_TILING     = 1 << 11,
	FULL_SCREEN_DISALLOWS_TILING  = 1 << 12,
	PRIMARY                       = 1 << 16,
	AUXILIARY                     = 1 << 17,
	CAN_JOIN_ALL_APPLICATIONS     = 1 << 18,
}

enum WindowLevel : const inline NSInteger
{
	NORMAL          = 0,
	FLOATING        = 3,
	SUBMENU         = 3,
	TORN_OFF_MENU   = 3,
	MODAL_PANEL     = 8,
	MAIN_MENU       = 24,
	STATUS          = 25,
	POP_UP_MENU     = 101,
	SCREEN_SAVER    = 1000,
}

enum WindowStyleMask : const inline NSUInteger
{
	BORDERLESS                = 0,
	TITLED                    = 1 << 0,
	CLOSABLE                  = 1 << 1,
	MINIATURIZABLE            = 1 << 2,
	RESIZABLE                 = 1 << 3,
	TEXTURED_BACKGROUND       = 1 << 8,
	UNIFIED_TITLE_AND_TOOLBAR = 1 << 12,
	FULL_SCREEN               = 1 << 14,
	FULL_SIZE_CONTENT_VIEW    = 1 << 15,
	UTILITY_WINDOW            = 1 << 4,
	DOC_MODAL_WINDOW          = 1 << 6,
	NONACTIVATING_PANEL       = 1 << 7,
	HUD_WINDOW                = 1 << 13
}

enum WindowTabbingMode : const inline NSInteger
{
	AUTOMATIC   = 0,
	DISALLOWED  = 2,
	PREFERRED   = 1,
}

