module std::os::darwin::cocoa @feat(MACROS) @link("Cocoa.framework");

extern fn int nsApplicationMain(int argc, char **argv) @cname("NSApplicationMain");
