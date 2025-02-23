module std::os::darwin::cocoa @if(env::OS_TYPE == MACOS) @link("Cocoa.framework");

extern fn int nsApplicationMain(int argc, char **argv) @extern("NSApplicationMain");
