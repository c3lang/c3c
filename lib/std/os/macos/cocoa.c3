module std::os::darwin::cocoa @feat(MACOS) @link("Cocoa.framework");
import std::os::macos;

extern fn int nsApplicationMain(int argc, char **argv) @cname("NSApplicationMain");


const NSUInteger NS_WINDOW_STYLE_BORDERLESS               = 0;
const NSUInteger NS_WINDOW_STYLE_TITLED                   = 1;
const NSUInteger NS_WINDOW_STYLE_CLOSABLE                 = 2;
const NSUInteger NS_WINDOW_STYLE_MINIATURIZABLE           = 4;
const NSUInteger NS_WINDOW_STYLE_RESIZABLE                = 8;
const NSUInteger NS_WINDOW_STYLE_UTILITY_WINDOW           = 1 << 4;
const NSUInteger NS_WINDOW_STYLE_DOC_MODAL_WINDOW         = 1 << 6;
const NSUInteger NS_WINDOW_STYLE_NONACTIVATING_PANEL      = 1 << 7;
const NSUInteger NS_WINDOW_STYLE_TEXTURED_BACKGROUND      = 1 << 8; // deprecated
const NSUInteger NS_WINDOW_STYLE_UNIFIED_TITLE_AND_TOOLBAR = 1 << 12;
const NSUInteger NS_WINDOW_STYLE_HUD_WINDOW               = 1 << 13;
const NSUInteger NS_WINDOW_STYLE_FULL_SCREEN              = 1 << 14;
const NSUInteger NS_WINDOW_STYLE_FULL_SIZE_CONTENT_VIEW   = 1 << 15;

const NSUInteger NS_WINDOW_CLOSE_BUTTON             = 0;
const NSUInteger NS_WINDOW_MINIATURIZE_BUTTON       = 1;
const NSUInteger NS_WINDOW_ZOOM_BUTTON              = 2;
const NSUInteger NS_WINDOW_TOOLBAR_BUTTON           = 3;
const NSUInteger NS_WINDOW_DOCUMENT_ICON_BUTTON     = 4;
const NSUInteger NS_WINDOW_DOCUMENT_VERSIONS_BUTTON = 6;

const NSInteger NS_WINDOW_LEVEL_NORMAL       = 0;
const NSInteger NS_WINDOW_LEVEL_FLOATING     = 3;
const NSInteger NS_WINDOW_LEVEL_SUBMENU      = 3;
const NSInteger NS_WINDOW_LEVEL_TORN_OFF_MENU = 3;
const NSInteger NS_WINDOW_LEVEL_MODAL_PANEL  = 8;
const NSInteger NS_WINDOW_LEVEL_MAIN_MENU    = 24;
const NSInteger NS_WINDOW_LEVEL_STATUS       = 25;
const NSInteger NS_WINDOW_LEVEL_POP_UP_MENU  = 101;
const NSInteger NS_WINDOW_LEVEL_SCREEN_SAVER = 1000;