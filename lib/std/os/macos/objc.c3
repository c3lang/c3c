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
	return cls ?: CLASS_NOT_FOUND~;
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
extern fn void registerClassPair(ObjcClass cls) @cname("objc_registerClassPair");

module std::os::macos::objc::ns @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");
import std::os::macos::cf;

enum StatusItemLength : (double val) @deprecated("Use NSStatusItemLength.")
{
    VARIABLE { -1.0 },
    SQUARE   { -2.0 },
}

enum ApplicationActivationPolicy : (int val) @deprecated("Use NSApplicationActivationPolicy.")
{
	REGULAR    { 0 },
	ACCESSORY  { 1 },
	PROHIBITED { 2 },
}

constdef NSApplicationActivationPolicy : inline NSInteger
{
	REGULAR    = 0,
	ACCESSORY  = 1,
	PROHIBITED = 2,
}

enum WindowStyleMask : (int val) @deprecated("Use NSWindowStyleMask.")
{
    BORDERLESS                { 0 },
    TITLED                    { 1 << 0 },
    CLOSABLE                  { 1 << 1 },
	MINIATURIZABLE            { 1 << 2 },
	RESIZABLE                 { 1 << 3 },
	TEXTURED_BACKGROUND       { 1 << 8 },
	UNIFIED_TITLE_AND_TOOLBAR { 1 << 12 },
	FULL_SCREEN               { 1 << 14 },
	FULL_SIZE_CONTENT_VIEW    { 1 << 15 },
	UTILITY_WINDOW            { 1 << 4 },
	DOC_MODAL_WINDOW          { 1 << 6 },
	NONACTIVATING_PANEL       { 1 << 7 },
	HUD_WINDOW                { 1 << 13 }
}

enum BackingStore : (int val) @deprecated("Use NSBackingStoreType.")
{
	RETAINED    { 0 },
	NONRETAINED { 1 },
	BUFFERED    { 2 }
}

constdef NSBackingStoreType : inline NSUInteger
{
	RETAINED    = 0,
	NONRETAINED = 1,
	BUFFERED    = 2
}

enum EventType : (long val) @deprecated("Use NSEventType.")
{
	LEFT_MOUSE_DOWN     { 1 },
	LEFT_MOUSE_UP       { 2 },
	RIGHT_MOUSE_DOWN    { 3 },
	RIGHT_MOUSE_UP      { 4 },
	MOUSE_MOVED         { 5 },
	LEFT_MOUSE_DRAGGED  { 6 },
	RIGHT_MOUSE_DRAGGED { 7 },
	MOUSE_ENTERED       { 8 },
	MOUSE_EXITED        { 9 },
	KEY_DOWN            { 10 },
	KEY_UP              { 11 },
	FLAGS_CHANGED       { 12 },
	APPKIT_DEFINED      { 13 },
	SYSTEM_DEFINED      { 14 },
	APPLICATION_DEFINED { 15 },
	PERIODIC            { 16 },
	CURSOR_UPDATE       { 17 },
	SCROLL_WHEEL        { 22 },
	TABLET_POINT        { 23 },
	TABLET_PROXIMITY    { 24 },
	OTHER_MOUSE_DOWN    { 25 },
	OTHER_MOUSE_UP      { 26 },
	OTHER_MOUSE_DRAGGED { 27 },
	GESTURE             { 29 },
	MAGNIFY             { 30 },
	SWIPE               { 31 },
	ROTATE              { 18 },
	BEGIN_GESTURE       { 19 },
	END_GESTURE         { 20 },
	SMART_MAGNIFY       { 32 },
	QUICK_LOOK          { 33 },
	PRESSURE            { 34 },
	DIRECT_TOUCH        { 37 },
	CHANGE_MODE         { 38 },
}

constdef NSEventType : inline NSUInteger
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

fn EventType? event_type_from(int val) @deprecated("Use NSEventType directly.")
{
    switch(val)
    {
        case EventType.LEFT_MOUSE_DOWN.val:     return LEFT_MOUSE_DOWN;
        case EventType.LEFT_MOUSE_UP.val:       return LEFT_MOUSE_UP;
        case EventType.RIGHT_MOUSE_DOWN.val:    return RIGHT_MOUSE_DOWN;
        case EventType.RIGHT_MOUSE_UP.val:      return RIGHT_MOUSE_UP;
        case EventType.MOUSE_MOVED.val:         return MOUSE_MOVED;
        case EventType.LEFT_MOUSE_DRAGGED.val:  return LEFT_MOUSE_DRAGGED;
        case EventType.RIGHT_MOUSE_DRAGGED.val: return RIGHT_MOUSE_DRAGGED;
        case EventType.MOUSE_ENTERED.val:       return MOUSE_ENTERED;
        case EventType.MOUSE_EXITED.val:        return MOUSE_EXITED;
        case EventType.KEY_DOWN.val:            return KEY_DOWN;
        case EventType.KEY_UP.val:              return KEY_UP;
        case EventType.FLAGS_CHANGED.val:       return FLAGS_CHANGED;
        case EventType.APPKIT_DEFINED.val:      return APPKIT_DEFINED;
        case EventType.SYSTEM_DEFINED.val:      return SYSTEM_DEFINED;
        case EventType.APPLICATION_DEFINED.val: return APPLICATION_DEFINED;
        case EventType.PERIODIC.val:            return PERIODIC;
        case EventType.CURSOR_UPDATE.val:       return CURSOR_UPDATE;
        case EventType.SCROLL_WHEEL.val:        return SCROLL_WHEEL;
        case EventType.TABLET_POINT.val:        return TABLET_POINT;
        case EventType.TABLET_PROXIMITY.val:    return TABLET_PROXIMITY;
        case EventType.OTHER_MOUSE_DOWN.val:    return OTHER_MOUSE_DOWN;
        case EventType.OTHER_MOUSE_UP.val:      return OTHER_MOUSE_UP;
        case EventType.OTHER_MOUSE_DRAGGED.val: return OTHER_MOUSE_DRAGGED;
        case EventType.GESTURE.val:             return GESTURE;
        case EventType.MAGNIFY.val:             return MAGNIFY;
        case EventType.SWIPE.val:               return SWIPE;
        case EventType.ROTATE.val:              return ROTATE;
        case EventType.BEGIN_GESTURE.val:       return BEGIN_GESTURE;
        case EventType.END_GESTURE.val:         return END_GESTURE;
        case EventType.SMART_MAGNIFY.val:       return SMART_MAGNIFY;
        case EventType.QUICK_LOOK.val:          return QUICK_LOOK;
        case EventType.PRESSURE.val:            return PRESSURE;
        case EventType.DIRECT_TOUCH.val:        return DIRECT_TOUCH;
        case EventType.CHANGE_MODE.val:         return CHANGE_MODE;
        default: return objc::UNKNOWN_EVENT~;
    }
}

enum EventMask : (long val) @deprecated("Use NSEventMask.")
{
	LEFT_MOUSE_DOWN     { 1 << EventType.LEFT_MOUSE_DOWN.val     },
	LEFT_MOUSE_UP       { 1 << EventType.LEFT_MOUSE_UP.val       },
	RIGHT_MOUSE_DOWN    { 1 << EventType.RIGHT_MOUSE_DOWN.val    },
    RIGHT_MOUSE_UP      { 1 << EventType.RIGHT_MOUSE_UP.val      },
    MOUSE_MOVED         { 1 << EventType.MOUSE_MOVED.val         },
	LEFT_MOUSE_DRAGGED  { 1 << EventType.LEFT_MOUSE_DRAGGED.val  },
	RIGHT_MOUSE_DRAGGED { 1 << EventType.RIGHT_MOUSE_DRAGGED.val },
	MOUSE_ENTERED       { 1 << EventType.MOUSE_ENTERED.val       },
	MOUSE_EXITED        { 1 << EventType.MOUSE_EXITED.val        },
	KEY_DOWN            { 1 << EventType.KEY_DOWN.val            },
	KEY_UP              { 1 << EventType.KEY_UP.val              },
	FLAGS_CHANGED       { 1 << EventType.FLAGS_CHANGED.val       },
	APPKIT_DEFINED      { 1 << EventType.APPKIT_DEFINED.val      },
	SYSTEM_DEFINED      { 1 << EventType.SYSTEM_DEFINED.val      },
	APPLICATION_DEFINED { 1 << EventType.APPLICATION_DEFINED.val },
	PERIODIC            { 1 << EventType.PERIODIC.val            },
	CURSOR_UPDATE       { 1 << EventType.CURSOR_UPDATE.val       },
	SCROLL_WHEEL        { 1 << EventType.SCROLL_WHEEL.val        },
	TABLET_POINT        { 1 << EventType.TABLET_POINT.val        },
	TABLET_PROXIMITY    { 1 << EventType.TABLET_PROXIMITY.val    },
	OTHER_MOUSE_DOWN    { 1 << EventType.OTHER_MOUSE_DOWN.val    },
	OTHER_MOUSE_UP      { 1 << EventType.OTHER_MOUSE_UP.val      },
	OTHER_MOUSE_DRAGGED { 1 << EventType.OTHER_MOUSE_DRAGGED.val },
    GESTURE             { 1 << EventType.GESTURE.val             },
    MAGNIFY             { 1 << EventType.MAGNIFY.val             },
    SWIPE               { 1 << EventType.SWIPE.val               },
    ROTATE              { 1 << EventType.ROTATE.val              },
    BEGIN_GESTURE       { 1 << EventType.BEGIN_GESTURE.val       },
    END_GESTURE         { 1 << EventType.END_GESTURE.val         },
    SMART_MAGNIFY       { 1L << EventType.SMART_MAGNIFY.val      },
    DIRECT_TOUCH        { 1L << EventType.DIRECT_TOUCH.val       },
    ANY                 { long.max                               },
}

constdef NSEventMask : inline ulong
{
	LEFT_MOUSE_DOWN     = 1ul << NSEventType.LEFT_MOUSE_DOWN,
	LEFT_MOUSE_UP       = 1ul << NSEventType.LEFT_MOUSE_UP,
	RIGHT_MOUSE_DOWN    = 1ul << NSEventType.RIGHT_MOUSE_DOWN,
	RIGHT_MOUSE_UP      = 1ul << NSEventType.RIGHT_MOUSE_UP,
	MOUSE_MOVED         = 1ul << NSEventType.MOUSE_MOVED,
	LEFT_MOUSE_DRAGGED  = 1ul << NSEventType.LEFT_MOUSE_DRAGGED,
	RIGHT_MOUSE_DRAGGED = 1ul << NSEventType.RIGHT_MOUSE_DRAGGED,
	MOUSE_ENTERED       = 1ul << NSEventType.MOUSE_ENTERED,
	MOUSE_EXITED        = 1ul << NSEventType.MOUSE_EXITED,
	KEY_DOWN            = 1ul << NSEventType.KEY_DOWN,
	KEY_UP              = 1ul << NSEventType.KEY_UP,
	FLAGS_CHANGED       = 1ul << NSEventType.FLAGS_CHANGED,
	APPKIT_DEFINED      = 1ul << NSEventType.APPKIT_DEFINED,
	SYSTEM_DEFINED      = 1ul << NSEventType.SYSTEM_DEFINED,
	APPLICATION_DEFINED = 1ul << NSEventType.APPLICATION_DEFINED,
	PERIODIC            = 1ul << NSEventType.PERIODIC,
	CURSOR_UPDATE       = 1ul << NSEventType.CURSOR_UPDATE,
	ROTATE              = 1ul << NSEventType.ROTATE,
	BEGIN_GESTURE       = 1ul << NSEventType.BEGIN_GESTURE,
	END_GESTURE         = 1ul << NSEventType.END_GESTURE,
	SCROLL_WHEEL        = 1ul << NSEventType.SCROLL_WHEEL,
	TABLET_POINT        = 1ul << NSEventType.TABLET_POINT,
	TABLET_PROXIMITY    = 1ul << NSEventType.TABLET_PROXIMITY,
	OTHER_MOUSE_DOWN    = 1ul << NSEventType.OTHER_MOUSE_DOWN,
	OTHER_MOUSE_UP      = 1ul << NSEventType.OTHER_MOUSE_UP,
	OTHER_MOUSE_DRAGGED = 1ul << NSEventType.OTHER_MOUSE_DRAGGED,
	GESTURE             = 1ul << NSEventType.GESTURE,
	MAGNIFY             = 1ul << NSEventType.MAGNIFY,
	SWIPE               = 1ul << NSEventType.SWIPE,
	SMART_MAGNIFY       = 1ul << NSEventType.SMART_MAGNIFY,
	QUICK_LOOK          = 1ul << NSEventType.QUICK_LOOK,
	PRESSURE            = 1ul << NSEventType.PRESSURE,
	DIRECT_TOUCH        = 1ul << NSEventType.DIRECT_TOUCH,
	CHANGE_MODE         = 1ul << NSEventType.CHANGE_MODE,
	ANY                 = ulong.max,
}

fn NSEventMask event_mask_from_type(NSEventType type) => (NSEventMask)1ul << type;

enum EventModifierFlag : (int val) @deprecated("Use NSEventModifierFlags.")
{
	CAPS_LOCK   { 1 << 16 },
	SHIFT       { 1 << 17 },
	CONTROL     { 1 << 18 },
	OPTION      { 1 << 19 },
	COMMAND     { 1 << 20 },
	NUMERIC_PAD { 1 << 21 },
    FUNCTION    { 1 << 23 },
	HELP        { 1 << 22 },
}

constdef NSEventModifierFlags : inline NSUInteger
{
	CAPS_LOCK                     = 1 << 16,
	SHIFT                         = 1 << 17,
	CONTROL                       = 1 << 18,
	OPTION                        = 1 << 19,
	COMMAND                       = 1 << 20,
	NUMERIC_PAD                   = 1 << 21,
	HELP                          = 1 << 22,
	FUNCTION                      = 1 << 23,
	DEVICE_INDEPENDENT_FLAGS_MASK = 0xffff0000UL,
}

constdef NSWindowCollectionBehavior : inline NSUInteger
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

constdef NSWindowLevel : inline NSInteger
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

constdef NSWindowStyleMask : inline NSUInteger
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

constdef NSWindowTabbingMode : inline NSInteger
{
	AUTOMATIC   = 0,
	DISALLOWED  = 2,
	PREFERRED   = 1,
}

constdef NSStatusItemLength : inline CGFloat
{
	VARIABLE = -1.0,
	SQUARE   = -2.0
}

constdef NSApplicationTerminateReply : inline NSUInteger
{
	CANCEL = 0,
	NOW    = 1,
	LATER  = 2,
}
