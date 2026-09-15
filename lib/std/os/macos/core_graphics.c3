module std::os::macos::cg @feat(DARWIN);
import std::os::darwin::cf;

typedef CGDirectDisplayID = uint;
typedef CGDisplayMode = inline CFType;
typedef CGEvent = inline CFType;

alias CGDisplayModeRef = CGDisplayMode*;
alias CGEventRef = CGEvent*;

alias CGFloat = env::ARCH_64_BIT ??? double : float;

struct CGPoint
{
	CGFloat x;
	CGFloat y;
}

struct CGSize
{
	CGFloat width;
	CGFloat height;
}

struct CGRect
{
	CGPoint origin;
	CGSize size;
}

extern fn CGDirectDisplayID mainDisplayID() @cname("CGMainDisplayID");
extern fn int getActiveDisplayList(uint maxDisplays, CGDirectDisplayID* activeDisplays, uint* displayCount) @cname("CGGetActiveDisplayList");
extern fn CGEventRef eventCreate(void* source) @cname("CGEventCreate");

extern fn CGRect CGDirectDisplayID.bounds(self) @cname("CGDisplayBounds");
extern fn CGDisplayModeRef CGDirectDisplayID.copyDisplayMode(self) @cname("CGDisplayCopyDisplayMode");
extern fn CFArrayRef CGDirectDisplayID.copyAllDisplayModes(self, void* options) @cname("CGDisplayCopyAllDisplayModes");
extern fn CGSize CGDirectDisplayID.screenSize(&self) @cname("CGDisplayScreenSize");
extern fn uint CGDirectDisplayID.gammaTableCapacity(self) @cname("CGDisplayGammaTableCapacity");
extern fn int CGDirectDisplayID.transferByTable(self, uint capacity, float* red, float* green, float* blue, uint* sampleCount) @cname("CGGetDisplayTransferByTable");
extern fn int CGDirectDisplayID.setTransferByTable(self, uint count, float* red, float* green, float* blue) @cname("CGSetDisplayTransferByTable");
extern fn int CGDirectDisplayID.setDisplayMode(self, CGDisplayModeRef mode, void* options) @cname("CGDisplaySetDisplayMode");
extern fn int CGDirectDisplayID.showCursor(self) @cname("CGDisplayShowCursor");
extern fn int CGDirectDisplayID.hideCursor(self) @cname("CGDisplayHideCursor");

extern fn int associateMouseAndMouseCursorPosition(bool connected) @cname("CGAssociateMouseAndMouseCursorPosition");
extern fn int warpMouseCursorPosition(CGPoint p) @cname("CGWarpMouseCursorPosition");

extern fn sz CGDisplayMode.getWidth(&self) @cname("CGDisplayModeGetWidth");
extern fn sz CGDisplayMode.getHeight(&self) @cname("CGDisplayModeGetHeight");
extern fn double CGDisplayMode.getRefreshRate(&self) @cname("CGDisplayModeGetRefreshRate");


extern fn CGPoint CGEvent.getLocation(&self) @cname("CGEventGetLocation");
