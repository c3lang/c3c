module std::os::macos::objc @test @if(env::DARWIN);

fn void test_application_activation_policy()
{
	NSApplicationActivationPolicy regular = NSApplicationActivationPolicy.REGULAR;
	NSApplicationActivationPolicy accessory = NSApplicationActivationPolicy.ACCESSORY;
	NSApplicationActivationPolicy prohibited = NSApplicationActivationPolicy.PROHIBITED;

	assert(regular == 0);
	assert(accessory == 1);
	assert(prohibited == 2);
}

fn void test_backing_store_type()
{
	NSBackingStoreType retained = NSBackingStoreType.RETAINED;
	NSBackingStoreType nonretained = NSBackingStoreType.NONRETAINED;
	NSBackingStoreType buffered = NSBackingStoreType.BUFFERED;

	assert(retained == 0);
	assert(nonretained == 1);
	assert(buffered == 2);
}

fn void test_event_type()
{
	NSEventType keyDown = NSEventType.KEY_DOWN;
	NSEventType keyUp = NSEventType.KEY_UP;
	NSEventType mouseDown = NSEventType.LEFT_MOUSE_DOWN;
	NSEventType smartMagnify = NSEventType.SMART_MAGNIFY;
	NSEventType directTouch = NSEventType.DIRECT_TOUCH;

	assert(keyDown == 10);
	assert(keyUp == 11);
	assert(mouseDown == 1);
	assert(smartMagnify == 32);
	assert(directTouch == 37);

	NSUInteger raw = 10;
	NSEventType converted = (NSEventType)raw;
	assert(converted == NSEventType.KEY_DOWN);
}

fn void test_event_mask_from_type()
{
	NSEventMask maskMouseDown = ns::event_mask_from_type(NSEventType.LEFT_MOUSE_DOWN);
	NSEventMask maskKeyDown = ns::event_mask_from_type(NSEventType.KEY_DOWN);
	NSEventMask maskKeyUp = ns::event_mask_from_type(NSEventType.KEY_UP);
	NSEventMask maskSmartMagnify = ns::event_mask_from_type(NSEventType.SMART_MAGNIFY);
	NSEventMask maskDirectTouch = ns::event_mask_from_type(NSEventType.DIRECT_TOUCH);

	assert(maskMouseDown == NSEventMask.LEFT_MOUSE_DOWN);
	assert(maskKeyDown == NSEventMask.KEY_DOWN);
	assert(maskKeyUp == NSEventMask.KEY_UP);
	assert(maskSmartMagnify == NSEventMask.SMART_MAGNIFY);
	assert(maskDirectTouch == NSEventMask.DIRECT_TOUCH);

	assert(maskMouseDown == (1ul << 1));
	assert(maskKeyDown == (1ul << 10));
	assert(maskKeyUp == (1ul << 11));
	assert(maskSmartMagnify == (1ul << 32));
	assert(maskDirectTouch == (1ul << 37));
}

fn void test_event_mask()
{
	NSEventMask keyDownMask = NSEventMask.KEY_DOWN;
	NSEventMask keyUpMask = NSEventMask.KEY_UP;
	NSEventMask smartMagnifyMask = NSEventMask.SMART_MAGNIFY;
	NSEventMask directTouchMask = NSEventMask.DIRECT_TOUCH;
	NSEventMask anyMask = NSEventMask.ANY;

	assert(keyDownMask == (1ul << 10));
	assert(keyUpMask == (1ul << 11));
	assert(smartMagnifyMask == (1ul << 32));
	assert(directTouchMask == (1ul << 37));
	assert(anyMask == ulong.max);

	NSEventMask combined = NSEventMask.KEY_DOWN | NSEventMask.FLAGS_CHANGED;
	assert(combined == ((1ul << 10) | (1ul << 12)));
}


fn void test_status_item_length()
{
	assert(NSStatusItemLength.VARIABLE == -1.0);
	assert(NSStatusItemLength.SQUARE == -2.0);
}

fn void test_event_modifier_flags()
{
	NSEventModifierFlags shift = NSEventModifierFlags.SHIFT;
	NSEventModifierFlags command = NSEventModifierFlags.COMMAND;
	NSEventModifierFlags option = NSEventModifierFlags.OPTION;
	NSEventModifierFlags control = NSEventModifierFlags.CONTROL;

	assert(shift == (1 << 17));
	assert(command == (1 << 20));
	assert(option == (1 << 19));
	assert(control == (1 << 18));

	NSUInteger cmdShift = (NSUInteger)command | (NSUInteger)shift;
	assert(cmdShift == ((1 << 20) | (1 << 17)));
}

fn void test_window_style_mask()
{
	NSWindowStyleMask titled = NSWindowStyleMask.TITLED;
	NSWindowStyleMask closable = NSWindowStyleMask.CLOSABLE;
	NSWindowStyleMask resizable = NSWindowStyleMask.RESIZABLE;
	NSWindowStyleMask borderless = NSWindowStyleMask.BORDERLESS;

	assert(titled == (1 << 0));
	assert(closable == (1 << 1));
	assert(resizable == (1 << 3));
	assert(borderless == 0);

	NSWindowStyleMask style = titled | closable | resizable;
	assert(style == ((1 << 0) | (1 << 1) | (1 << 3)));
}

fn void test_window_collection_behavior()
{
	NSWindowCollectionBehavior def = NSWindowCollectionBehavior.DEFAULT;
	NSWindowCollectionBehavior canJoin = NSWindowCollectionBehavior.CAN_JOIN_ALL_SPACES;
	NSWindowCollectionBehavior managed = NSWindowCollectionBehavior.MANAGED;
	NSWindowCollectionBehavior fullscreen = NSWindowCollectionBehavior.FULL_SCREEN_PRIMARY;
	NSWindowCollectionBehavior primary = NSWindowCollectionBehavior.PRIMARY;

	assert(def == 0);
	assert(canJoin == (1 << 0));
	assert(managed == (1 << 2));
	assert(fullscreen == (1 << 7));
	assert(primary == (1 << 16));

	NSWindowCollectionBehavior combined = managed | fullscreen;
	assert(combined == ((1 << 2) | (1 << 7)));
}

fn void test_window_level()
{
	NSWindowLevel normal = NSWindowLevel.NORMAL;
	NSWindowLevel floating = NSWindowLevel.FLOATING;
	NSWindowLevel modalPanel = NSWindowLevel.MODAL_PANEL;
	NSWindowLevel screenSaver = NSWindowLevel.SCREEN_SAVER;

	assert(normal == 0);
	assert(floating == 3);
	assert(modalPanel == 8);
	assert(screenSaver == 1000);
}

fn void test_window_tabbing_mode()
{
	NSWindowTabbingMode automatic = NSWindowTabbingMode.AUTOMATIC;
	NSWindowTabbingMode preferred = NSWindowTabbingMode.PREFERRED;
	NSWindowTabbingMode disallowed = NSWindowTabbingMode.DISALLOWED;

	assert(automatic == 0);
	assert(preferred == 1);
	assert(disallowed == 2);
}

fn void test_nsuinteger_nsinteger_architecture()
{
	if (env::ARCH_64_BIT)
	{
		assert(NSUInteger.sizeof == ulong.sizeof);
		assert(NSInteger.sizeof == long.sizeof);
	}
	else
	{
		assert(NSUInteger.sizeof == uint.sizeof);
		assert(NSInteger.sizeof == int.sizeof);
	}
}

// EventMask requires 64-bit values even on 32-bit systems
fn void test_event_mask_64bit_values()
{
	NSEventMask smartMagnify = NSEventMask.SMART_MAGNIFY;
	NSEventMask directTouch = NSEventMask.DIRECT_TOUCH;

	assert(smartMagnify == (1ul << 32));
	assert(directTouch == (1ul << 37));

	assert(NSEventMask.sizeof == ulong.sizeof);
}

fn void test_application_terminate_reply()
{
	NSApplicationTerminateReply cancel = NSApplicationTerminateReply.CANCEL;
	NSApplicationTerminateReply now = NSApplicationTerminateReply.NOW;
	NSApplicationTerminateReply later = NSApplicationTerminateReply.LATER;

	assert(cancel == 0);
	assert(now == 1);
	assert(later == 2);
}
