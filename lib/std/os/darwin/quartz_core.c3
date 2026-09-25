module std::os::darwin @feat(DARWIN) @link("QuartzCore.framework");

fn ObjClass cametal_layer() @link("Metal.framework") => objc::@class("CAMetalLayer");
