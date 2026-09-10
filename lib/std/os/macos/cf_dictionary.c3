module std::os::macos::cf @feat(DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

typedef CFDictionary = inline CFType;
alias CFDictionaryRef = CFDictionary*;
typedef CFMutableDictionary = inline CFDictionary;
typedef CFMutableDictionaryRef = CFMutableDictionary*;