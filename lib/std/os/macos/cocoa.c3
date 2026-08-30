module std::os::darwin::cocoa @feat(MACOS) @link("Cocoa.framework");

extern fn int nsApplicationMain(int argc, char **argv) @cname("NSApplicationMain");
