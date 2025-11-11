module std::os::macos::objc @test @if(env::DARWIN);

fn void test_application_activation_policy()
{
	ApplicationActivationPolicy regular = ApplicationActivationPolicy.REGULAR;
	ApplicationActivationPolicy accessory = ApplicationActivationPolicy.ACCESSORY;
	ApplicationActivationPolicy prohibited = ApplicationActivationPolicy.PROHIBITED;
	
	assert(regular == 0);
	assert(accessory == 1);
	assert(prohibited == 2);
}

fn void test_backing_store()
{
	BackingStore retained = BackingStore.RETAINED;
	BackingStore nonretained = BackingStore.NONRETAINED;
	BackingStore buffered = BackingStore.BUFFERED;
	
	assert(retained == 0);
	assert(nonretained == 1);
	assert(buffered == 2);
}

fn void test_event_type()
{
	EventType keyDown = EventType.KEY_DOWN;
	EventType keyUp = EventType.KEY_UP;
	EventType mouseDown = EventType.LEFT_MOUSE_DOWN;
	EventType smartMagnify = EventType.SMART_MAGNIFY;
	EventType directTouch = EventType.DIRECT_TOUCH;
	
	assert(keyDown == 10);
	assert(keyUp == 11);
	assert(mouseDown == 1);
	assert(smartMagnify == 32);
	assert(directTouch == 37);
	
	NSUInteger raw = 10;
	EventType converted = (EventType)raw;
	assert(converted == EventType.KEY_DOWN);
}

fn void test_event_mask_from_type()
{
	EventMask maskMouseDown = event_mask_from_type(EventType.LEFT_MOUSE_DOWN);
	EventMask maskKeyDown = event_mask_from_type(EventType.KEY_DOWN);
	EventMask maskKeyUp = event_mask_from_type(EventType.KEY_UP);
	EventMask maskSmartMagnify = event_mask_from_type(EventType.SMART_MAGNIFY);
	EventMask maskDirectTouch = event_mask_from_type(EventType.DIRECT_TOUCH);
	
	assert(maskMouseDown == EventMask.LEFT_MOUSE_DOWN);
	assert(maskKeyDown == EventMask.KEY_DOWN);
	assert(maskKeyUp == EventMask.KEY_UP);
	assert(maskSmartMagnify == EventMask.SMART_MAGNIFY);
	assert(maskDirectTouch == EventMask.DIRECT_TOUCH);
	
	assert(maskMouseDown == (1ul << 1));
	assert(maskKeyDown == (1ul << 10));
	assert(maskKeyUp == (1ul << 11));
	assert(maskSmartMagnify == (1ul << 32));
	assert(maskDirectTouch == (1ul << 37));
}

fn void test_event_mask()
{
	EventMask keyDownMask = EventMask.KEY_DOWN;
	EventMask keyUpMask = EventMask.KEY_UP;
	EventMask smartMagnifyMask = EventMask.SMART_MAGNIFY;
	EventMask directTouchMask = EventMask.DIRECT_TOUCH;
	EventMask anyMask = EventMask.ANY;
	
	assert(keyDownMask == (1ul << 10));
	assert(keyUpMask == (1ul << 11));
	assert(smartMagnifyMask == (1ul << 32));
	assert(directTouchMask == (1ul << 37));
	assert(anyMask == ulong.max);
	
	EventMask combined = EventMask.KEY_DOWN | EventMask.FLAGS_CHANGED;
	assert(combined == ((1ul << 10) | (1ul << 12)));
}

fn void test_event_modifier_flag()
{
	EventModifierFlag shift = EventModifierFlag.SHIFT;
	EventModifierFlag command = EventModifierFlag.COMMAND;
	EventModifierFlag option = EventModifierFlag.OPTION;
	EventModifierFlag control = EventModifierFlag.CONTROL;
	
	assert(shift == (1 << 17));
	assert(command == (1 << 20));
	assert(option == (1 << 19));
	assert(control == (1 << 18));
	
	NSUInteger cmdShift = (NSUInteger)command | (NSUInteger)shift;
	assert(cmdShift == ((1 << 20) | (1 << 17)));
}

fn void test_window_style_mask()
{
	WindowStyleMask titled = WindowStyleMask.TITLED;
	WindowStyleMask closable = WindowStyleMask.CLOSABLE;
	WindowStyleMask resizable = WindowStyleMask.RESIZABLE;
	WindowStyleMask borderless = WindowStyleMask.BORDERLESS;
	
	assert(titled == (1 << 0));
	assert(closable == (1 << 1));
	assert(resizable == (1 << 3));
	assert(borderless == 0);
	
	WindowStyleMask style = titled | closable | resizable;
	assert(style == ((1 << 0) | (1 << 1) | (1 << 3)));
}

fn void test_window_collection_behavior()
{
	WindowCollectionBehavior def = WindowCollectionBehavior.DEFAULT;
	WindowCollectionBehavior canJoin = WindowCollectionBehavior.CAN_JOIN_ALL_SPACES;
	WindowCollectionBehavior managed = WindowCollectionBehavior.MANAGED;
	WindowCollectionBehavior fullscreen = WindowCollectionBehavior.FULL_SCREEN_PRIMARY;
	WindowCollectionBehavior primary = WindowCollectionBehavior.PRIMARY;
	
	assert(def == 0);
	assert(canJoin == (1 << 0));
	assert(managed == (1 << 2));
	assert(fullscreen == (1 << 7));
	assert(primary == (1 << 16));
	
	WindowCollectionBehavior combined = managed | fullscreen;
	assert(combined == ((1 << 2) | (1 << 7)));
}

fn void test_window_level()
{
	WindowLevel normal = WindowLevel.NORMAL;
	WindowLevel floating = WindowLevel.FLOATING;
	WindowLevel modalPanel = WindowLevel.MODAL_PANEL;
	WindowLevel screenSaver = WindowLevel.SCREEN_SAVER;
	
	assert(normal == 0);
	assert(floating == 3);
	assert(modalPanel == 8);
	assert(screenSaver == 1000);
}

fn void test_window_tabbing_mode()
{
	WindowTabbingMode automatic = WindowTabbingMode.AUTOMATIC;
	WindowTabbingMode preferred = WindowTabbingMode.PREFERRED;
	WindowTabbingMode disallowed = WindowTabbingMode.DISALLOWED;
	
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
	EventMask smartMagnify = EventMask.SMART_MAGNIFY;
	EventMask directTouch = EventMask.DIRECT_TOUCH;
	
	assert(smartMagnify == (1ul << 32));
	assert(directTouch == (1ul << 37));

	assert(EventMask.sizeof == ulong.sizeof);
}
