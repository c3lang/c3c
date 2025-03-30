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

extern fn void msgSend(...) @extern("objc_msgSend") @builtin;
extern fn ObjcSelector sel_getUid(ZString);

macro msg_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&msgSend)((ObjcId)id, sel_getUid($selector), $vasplat);
}

extern fn ObjcClass getClass(ZString name) @extern("objc_getClass");
extern fn int getClassList(ObjcClass* buffer, int buffer_count) @extern("objc_getClassList");
extern fn ObjcClass lookUpClass(ZString name) @extern("objc_lookUpClass") @builtin;

extern fn ZString class_getName(ObjcClass cls);
extern fn ObjcClass class_getSuperclass(ObjcClass cls);
extern fn ObjcMethod class_getClassMethod(ObjcClass cls, ObjcSelector name);
extern fn bool class_respondsToSelector(ObjcClass cls, ObjcSelector name);
extern fn ObjcSelector sel_registerName(ZString str);
extern fn bool class_addIvar(ObjcClass cls, ZString name, int size, double alignment, ZString types);
extern fn bool class_addMethod(ObjcClass cls, ObjcSelector name, void* imp, ZString types);

extern fn ObjcIvar getInstanceVariable(ObjcId id, ZString name, void* outValue) @extern("object_getInstanceVariable");
extern fn ObjcIvar setInstanceVariable(ObjcId id, ZString name, void* value) @extern("object_setInstanceVariable");
extern fn ObjcClass allocateClassPair(ObjcClass cls, ZString name, uint extraBytes) @extern("objc_allocateClassPair");

enum StatusItemLength : (double val)
{
    VARIABLE = -1.0,
    SQUARE   = -2.0,
}

enum ApplicationActivationPolicy : (int val)
{
	REGULAR    = 0,
	ACCESSORY  = 1,
	PROHIBITED = 2,
}

enum WindowStyleMask : (int val)
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

enum BackingStore : (int val)
{
	RETAINED    = 0,
	NONRETAINED = 1,
	BUFFERED    = 2
}

enum EventType : (long val)
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

fn EventType? event_type_from(int val)
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
        default: return UNKNOWN_EVENT?;
    }
}

enum EventMask : (long val)
{
	LEFT_MOUSE_DOWN     = 1 << EventType.LEFT_MOUSE_DOWN.val,
	LEFT_MOUSE_UP       = 1 << EventType.LEFT_MOUSE_UP.val,
	RIGHT_MOUSE_DOWN    = 1 << EventType.RIGHT_MOUSE_DOWN.val,
    RIGHT_MOUSE_UP      = 1 << EventType.RIGHT_MOUSE_UP.val,
    MOUSE_MOVED         = 1 << EventType.MOUSE_MOVED.val,
	LEFT_MOUSE_DRAGGED  = 1 << EventType.LEFT_MOUSE_DRAGGED.val,
	RIGHT_MOUSE_DRAGGED = 1 << EventType.RIGHT_MOUSE_DRAGGED.val,
	MOUSE_ENTERED       = 1 << EventType.MOUSE_ENTERED.val,
	MOUSE_EXITED        = 1 << EventType.MOUSE_EXITED.val,
	KEY_DOWN            = 1 << EventType.KEY_DOWN.val,
	KEY_UP              = 1 << EventType.KEY_UP.val,
	FLAGS_CHANGED       = 1 << EventType.FLAGS_CHANGED.val,
	APPKIT_DEFINED      = 1 << EventType.APPKIT_DEFINED.val,
	SYSTEM_DEFINED      = 1 << EventType.SYSTEM_DEFINED.val,
	APPLICATION_DEFINED = 1 << EventType.APPLICATION_DEFINED.val,
	PERIODIC            = 1 << EventType.PERIODIC.val,
	CURSOR_UPDATE       = 1 << EventType.CURSOR_UPDATE.val,
	SCROLL_WHEEL        = 1 << EventType.SCROLL_WHEEL.val,
	TABLET_POINT        = 1 << EventType.TABLET_POINT.val,
	TABLET_PROXIMITY    = 1 << EventType.TABLET_PROXIMITY.val,
	OTHER_MOUSE_DOWN    = 1 << EventType.OTHER_MOUSE_DOWN.val,
	OTHER_MOUSE_UP      = 1 << EventType.OTHER_MOUSE_UP.val,
	OTHER_MOUSE_DRAGGED = 1 << EventType.OTHER_MOUSE_DRAGGED.val,
    GESTURE             = 1 << EventType.GESTURE.val,
    MAGNIFY             = 1 << EventType.MAGNIFY.val,
    SWIPE               = 1 << EventType.SWIPE.val,
    ROTATE              = 1 << EventType.ROTATE.val,
    BEGIN_GESTURE       = 1 << EventType.BEGIN_GESTURE.val,
    END_GESTURE         = 1 << EventType.END_GESTURE.val,
    SMART_MAGNIFY       = 1L << EventType.SMART_MAGNIFY.val,
    DIRECT_TOUCH        = 1L << EventType.DIRECT_TOUCH.val,
    ANY                 = long.max,
}

enum EventModifierFlag : (int val)
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

