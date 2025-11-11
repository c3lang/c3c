module std::os::macos::objc @test;

fn void test_application_activation_policy()
{
	ApplicationActivationPolicy regular = ApplicationActivationPolicy.REGULAR;
	ApplicationActivationPolicy accessory = ApplicationActivationPolicy.ACCESSORY;
	ApplicationActivationPolicy prohibited = ApplicationActivationPolicy.PROHIBITED;
	
	assert(regular.val == 0);
	assert(accessory.val == 1);
	assert(prohibited.val == 2);
	
	assert(@typeid(regular.val) == NSInteger.typeid);
}

fn void test_backing_store()
{
	BackingStore retained = BackingStore.RETAINED;
	BackingStore nonretained = BackingStore.NONRETAINED;
	BackingStore buffered = BackingStore.BUFFERED;
	
	assert(retained.val == 0);
	assert(nonretained.val == 1);
	assert(buffered.val == 2);
	
	assert(@typeid(retained.val) == NSUInteger.typeid);
}

fn void test_event_type()
{
	EventType keyDown = EventType.KEY_DOWN;
	EventType keyUp = EventType.KEY_UP;
	EventType mouseDown = EventType.LEFT_MOUSE_DOWN;
	EventType smartMagnify = EventType.SMART_MAGNIFY;
	EventType directTouch = EventType.DIRECT_TOUCH;
	
	assert(keyDown.val == 10);
	assert(keyUp.val == 11);
	assert(mouseDown.val == 1);
	assert(smartMagnify.val == 32);
	assert(directTouch.val == 37);
	
	// Verify backing type is NSUInteger (unsigned)
	assert(@typeid(keyDown.val) == NSUInteger.typeid);
	
	// Test helper function found case
	if (try found = event_type_from(10))
	{
		assert(found.val == EventType.KEY_DOWN.val);
	}
	else
	{
		unreachable("Should have found KEY_DOWN event type");
	}
	
	// Test helper function not found case
	if (catch event_type_from(9999))
	{
		// Expected - invalid event type
	}
	else
	{
		unreachable("Should not have found event type 9999");
	}
}

fn void test_event_mask()
{
	EventMask keyDownMask = EventMask.KEY_DOWN;
	EventMask keyUpMask = EventMask.KEY_UP;
	EventMask smartMagnifyMask = EventMask.SMART_MAGNIFY;
	EventMask directTouchMask = EventMask.DIRECT_TOUCH;
	EventMask anyMask = EventMask.ANY;
	
	assert(keyDownMask.val == (1ul << 10));
	assert(keyUpMask.val == (1ul << 11));
	assert(smartMagnifyMask.val == (1ul << 32));
	assert(directTouchMask.val == (1ul << 37));
	assert(anyMask.val == ulong.max);
	
	assert(@typeid(keyDownMask.val) == ulong.typeid);
	
	ulong combined = keyDownMask.val | keyUpMask.val;
	assert(combined == ((1ul << 10) | (1ul << 11)));
}

fn void test_event_modifier_flag()
{
	EventModifierFlag shift = EventModifierFlag.SHIFT;
	EventModifierFlag command = EventModifierFlag.COMMAND;
	EventModifierFlag option = EventModifierFlag.OPTION;
	EventModifierFlag control = EventModifierFlag.CONTROL;
	
	assert(shift.val == (1 << 17));
	assert(command.val == (1 << 20));
	assert(option.val == (1 << 19));
	assert(control.val == (1 << 18));
	
	assert(@typeid(shift.val) == NSUInteger.typeid);
	
	NSUInteger cmdShift = command.val | shift.val;
	assert(cmdShift == ((1 << 20) | (1 << 17)));
}

fn void test_window_style_mask()
{
	WindowStyleMask titled = WindowStyleMask.TITLED;
	WindowStyleMask closable = WindowStyleMask.CLOSABLE;
	WindowStyleMask resizable = WindowStyleMask.RESIZABLE;
	WindowStyleMask borderless = WindowStyleMask.BORDERLESS;
	
	assert(titled.val == (1 << 0));
	assert(closable.val == (1 << 1));
	assert(resizable.val == (1 << 3));
	assert(borderless.val == 0);
	
	assert(@typeid(titled.val) == NSUInteger.typeid);
	
	NSUInteger standard = titled.val | closable.val | resizable.val;
	assert(standard == ((1 << 0) | (1 << 1) | (1 << 3)));
}

fn void test_window_collection_behavior()
{
	WindowCollectionBehavior def = WindowCollectionBehavior.DEFAULT;
	WindowCollectionBehavior canJoin = WindowCollectionBehavior.CAN_JOIN_ALL_SPACES;
	WindowCollectionBehavior managed = WindowCollectionBehavior.MANAGED;
	WindowCollectionBehavior fullscreen = WindowCollectionBehavior.FULL_SCREEN_PRIMARY;
	WindowCollectionBehavior primary = WindowCollectionBehavior.PRIMARY;
	
	assert(def.val == 0);
	assert(canJoin.val == (1 << 0));
	assert(managed.val == (1 << 2));
	assert(fullscreen.val == (1 << 7));
	assert(primary.val == (1 << 16));
	
	assert(@typeid(canJoin.val) == NSUInteger.typeid);
	
	NSUInteger combo = managed.val | fullscreen.val;
	assert(combo == ((1 << 2) | (1 << 7)));
}

fn void test_window_level()
{
	WindowLevel normal = WindowLevel.NORMAL;
	WindowLevel floating = WindowLevel.FLOATING;
	WindowLevel modalPanel = WindowLevel.MODAL_PANEL;
	WindowLevel screenSaver = WindowLevel.SCREEN_SAVER;
	
	assert(normal.val == 0);
	assert(floating.val == 3);
	assert(modalPanel.val == 8);
	assert(screenSaver.val == 1000);
	
	assert(@typeid(normal.val) == NSInteger.typeid);
}

fn void test_window_tabbing_mode()
{
	WindowTabbingMode automatic = WindowTabbingMode.AUTOMATIC;
	WindowTabbingMode preferred = WindowTabbingMode.PREFERRED;
	WindowTabbingMode disallowed = WindowTabbingMode.DISALLOWED;
	
	assert(automatic.val == 0);
	assert(preferred.val == 1);
	assert(disallowed.val == 2);
	
	assert(@typeid(automatic.val) == NSInteger.typeid);
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

fn void test_event_mask_64bit_values()
{
	EventMask smartMagnify = EventMask.SMART_MAGNIFY;
	EventMask directTouch = EventMask.DIRECT_TOUCH;
	
	assert(smartMagnify.val == (1ul << 32));
	assert(directTouch.val == (1ul << 37));

	assert(@typeid(smartMagnify.val) == ulong.typeid);
}
