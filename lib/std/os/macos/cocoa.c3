module std::os::darwin::cocoa @feat(MACOS) @link("Cocoa.framework");
import std::os::macos;

extern fn int nsApplicationMain(int argc, char **argv) @cname("NSApplicationMain");
