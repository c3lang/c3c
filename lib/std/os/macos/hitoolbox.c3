module std::os::macos @feat(DARWIN);

typedef UCKeyboardLayout = void;

typedef TISInputSource = inline CFType;
alias TISInputSourceRef = TISInputSource*;

extern fn TISInputSourceRef tisCopyCurrentKeyboardLayoutInputSource() @cname("TISCopyCurrentKeyboardLayoutInputSource");
extern fn TISInputSourceRef tisCopyCurrentASCIICapableKeyboardInputSource() @cname("TISCopyCurrentASCIICapableKeyboardInputSource");
extern fn TISInputSourceRef tisCopyInputSourceForLanguage(CFStringRef language) @cname("TISCopyInputSourceForLanguage");
extern fn TISInputSourceRef tisCopyInputMethodKeyboardLayoutOverride() @cname("TISCopyInputMethodKeyboardLayoutOverride");

// extern fn CFTypeID tisInputSourceGetTypeID() @cname("TISInputSourceGetTypeID");

extern fn OSStatus tisRegisterInputSource(CFURLRef location) @cname("TISRegisterInputSource");
extern fn CFArrayRef tisCreateASCIICapableInputSourceList() @cname("TISCreateASCIICapableInputSourceList");
extern fn CFArrayRef tisCreateInputSourceList(CFDictionaryRef properties, bool includeAllInstalled) @cname("TISCreateInputSourceList");

extern fn void* TISInputSource.getProperty(&self, CFStringRef propertyKey) @cname("TISGetInputSourceProperty");
extern fn OSStatus TISInputSource.selectInputSource(&self) @cname("TISSelectInputSource");
extern fn OSStatus TISInputSource.deselectInputSource(&self) @cname("TISDeselectInputSource");
extern fn OSStatus TISInputSource.enableInputSource(&self) @cname("TISEnableInputSource");
extern fn OSStatus TISInputSource.disableInputSource(&self) @cname("TISDisableInputSource");
extern fn OSStatus TISInputSource.setInputMethodKeyboardLayoutOverride(&self) @cname("TISSetInputMethodKeyboardLayoutOverride");

extern CFStringRef kTISNotifySelectedKeyboardInputSourceChanged;
extern CFStringRef kTISNotifyEnabledKeyboardInputSourcesChanged;
extern CFStringRef kTISCategoryKeyboardInputSource;
extern CFStringRef kTISCategoryPaletteInputSource;
extern CFStringRef kTISCategoryInkInputSource;
extern CFStringRef kTISTypeKeyboardLayout;
extern CFStringRef kTISTypeKeyboardInputMethodWithoutModes;
extern CFStringRef kTISTypeKeyboardInputMethodModeEnabled;
extern CFStringRef kTISTypeKeyboardInputMode;
extern CFStringRef kTISTypeCharacterPalette;
extern CFStringRef kTISTypeKeyboardViewer;
extern CFStringRef kTISTypeInk;
extern CFStringRef kTISPropertyBundleID;
extern CFStringRef kTISPropertyInputModeID;
extern CFStringRef kTISPropertyIconImageURL;
extern CFStringRef kTISPropertyLocalizedName;
extern CFStringRef kTISPropertyUnicodeKeyLayoutData;
extern CFStringRef kTISPropertyInputSourceID;
extern CFStringRef kTISPropertyIconRef;
extern CFStringRef kTISPropertyInputSourceLanguages;
extern CFStringRef kTISPropertyInputSourceCategory;
extern CFStringRef kTISPropertyInputSourceType;
extern CFStringRef kTISPropertyInputSourceIsASCIICapable;
extern CFStringRef kTISPropertyInputSourceIsEnableCapable;
extern CFStringRef kTISPropertyInputSourceIsSelectCapable;
extern CFStringRef kTISPropertyInputSourceIsEnabled;
extern CFStringRef kTISPropertyInputSourceIsSelected;