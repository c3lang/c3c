module std::os::darwin::cocoa @feat(MACOS) @link("Cocoa.framework");
import std::os::macos;

extern fn int nsApplicationMain(int argc, char **argv) @cname("NSApplicationMain");

constset NSApplicationActivationPolicy : inline NSInteger
{
	REGULAR    = 0,
	ACCESSORY  = 1,
	PROHIBITED = 2,
}

constset NSEventType : inline NSUInteger
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

constset NSWindowButton : inline NSUInteger
{
	CLOSE             = 0,
	MINIATURIZE       = 1,
	ZOOM              = 2,
	TOOLBAR           = 3,
	DOCUMENT_ICON     = 4,
	DOCUMENT_VERSIONS = 6,
}

constset NSEventMask : inline ulong
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
	ANY                 = ulong::max,
}

fn NSEventMask event_mask_from_type(NSEventType type) => (NSEventMask)1ul << type;


constset NSEventModifierFlags : inline NSUInteger
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

constset NSWindowCollectionBehavior : inline NSUInteger
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

constset NSWindowLevel : inline NSInteger
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

constset NSBackingStoreType : inline NSUInteger
{
	RETAINED     = 0,
	NON_RETAINED = 1,
	BUFFERED     = 2,
}


constset NSTrackingAreaOptions : inline NSUInteger
{
	MOUSE_ENTERED_AND_EXITED      = 0x01,
	MOUSE_MOVED                   = 0x02,
	CURSOR_UPDATE                 = 0x04,
	ACTIVE_WHEN_FIRST_RESPONDER   = 0x10,
	ACTIVE_IN_KEY_WINDOW          = 0x20,
	ACTIVE_IN_ACTIVE_APP          = 0x40,
	ACTIVE_ALWAYS                 = 0x80,
	ASSUME_INSIDE                 = 0x100,
	IN_VISIBLE_RECT               = 0x200,
	ENABLED_DURING_MOUSE_DRAG     = 0x400,
}

constset NSWindowStyleMask : inline NSUInteger
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

constset NSWindowTabbingMode : inline NSInteger
{
	AUTOMATIC   = 0,
	DISALLOWED  = 2,
	PREFERRED   = 1,
}

constset NSStatusItemLength : inline CGFloat
{
	VARIABLE = -1.0,
	SQUARE   = -2.0
}

constset NSApplicationTerminateReply : inline NSUInteger
{
	CANCEL = 0,
	NOW    = 1,
	LATER  = 2,
}
