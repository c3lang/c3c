module sdl;

const uint INIT_TIMER = 0x00000001;
const uint INIT_AUDIO = 0x00000010;
const uint INIT_VIDEO = 0x00000020;  /**< INIT_VIDEO implies INIT_EVENTS */
const uint INIT_JOYSTICK = 0x00000200;  /**< INIT_JOYSTICK implies INIT_EVENTS */
const uint INIT_HAPTIC = 0x00001000;
const uint INIT_GAMECONTROLLER = 0x00002000;  /**< SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK */
const uint INIT_EVENTS = 0x00004000;
const uint INIT_SENSOR = 0x00008000;
const uint INIT_NOPARACHUTE = 0x00100000;  /**< compatibility; this flag is ignored. */
//const uint INIT_EVERYTHING = INIT_TIMER | INIT_AUDIO | INIT_VIDEO | INIT_EVENTS |
//                INIT_JOYSTICK | INIT_HAPTIC | INIT_GAMECONTROLLER | INIT_SENSOR;



//Screen dimension constants
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;


//The window we'll be rendering to
void* gWindow = null;

//The surface contained by the window
void* gScreenSurface = null;


extern func int init(uint flags) @cname("SDL_Init");
extern func int initSubSystem(uint flags) @cname("SDL_InitSubSystem");
extern func void quitSubSystem(uint flags) @cname("SDL_QuitSubSystem");
extern func uint wasInit(uint flags) @cname("SDL_WasInit");
extern func void quit() @cname("SDL_Quit");

enum SDLWindowFlags : c_int
{
    FULLSCREEN = 0x00000001,         /**< fullscreen window */
    OPENGL = 0x00000002,             /**< window usable with OpenGL context */
    SHOWN = 0x00000004,              /**< window is visible */
    HIDDEN = 0x00000008,             /**< window is not visible */
    BORDERLESS = 0x00000010,         /**< no window decoration */
    RESIZABLE = 0x00000020,          /**< window can be resized */
    MINIMIZED = 0x00000040,          /**< window is minimized */
    MAXIMIZED = 0x00000080,          /**< window is maximized */
    INPUT_GRABBED = 0x00000100,      /**< window has grabbed input focus */
    INPUT_FOCUS = 0x00000200,        /**< window has input focus */
    MOUSE_FOCUS = 0x00000400,        /**< window has mouse focus */
    FULLSCREEN_DESKTOP = 0x00001001,
    FOREIGN = 0x00000800,            /**< window not created by SDL */
    ALLOW_HIGHDPI = 0x00002000,      /**< window should be created in high-DPI mode if supported.
                                                     On macOS NSHighResolutionCapable must be set true in the
                                                     application's Info.plist for this to have any effect. */
    MOUSE_CAPTURE = 0x00004000,      /**< window has mouse captured (unrelated to INPUT_GRABBED) */
    ALWAYS_ON_TOP = 0x00008000,      /**< window should always be above others */
    SKIP_TASKBAR  = 0x00010000,      /**< window should not be added to the taskbar */
    UTILITY       = 0x00020000,      /**< window should be treated as a utility window */
    TOOLTIP       = 0x00040000,      /**< window should be treated as a tooltip */
    POPUP_MENU    = 0x00080000,      /**< window should be treated as a popup menu */
    VULKAN        = 0x10000000       /**< window usable for Vulkan surface */
}

extern func void* createWindow(char *title, uint x, uint y, int w, int h, uint flags) @cname("SDL_CreateWindow");
extern func void* getWindowSurface(void *window) @cname("SDL_GetWindowSurface");
const uint WINDOWPOS_UNDEFINED_MASK = 0x1FFF0000;
extern func int exit(int code);
extern func int printf(char *mess, ...);

func bool initX()
{
	//Initialization flag
	bool success = true;

	//Initialize SDL
	if (init(INIT_VIDEO) < 0)
	{
		printf( "SDL could not initialize!");
		exit(-1);
	}

	//Create window
	gWindow = createWindow( "SDL from C3", WINDOWPOS_UNDEFINED_MASK, WINDOWPOS_UNDEFINED_MASK, SCREEN_WIDTH, SCREEN_HEIGHT, (uint)(SDLWindowFlags.SHOWN));
	if (!gWindow) exit(-1);
	//Get window surface
	gScreenSurface = getWindowSurface(gWindow);
	return true;
}


func void close()
{
	//Destroy window
//	SDL_DestroyWindow( gWindow );
//	gWindow = NULL;

	//Quit SDL subsystems
	quit();
}

extern func int pollEvent(SDL_Event *event) @cname("SDL_PollEvent");
extern func int updateWindowSurface(void *window) @cname("SDL_UpdateWindowSurface");

enum SDLEventType : uint
{
    QUIT = 0x100, /**< User-requested quit */

    /* These application events have special meaning on iOS, see README-ios.md for details */
    APP_TERMINATING,        /**< The application is being terminated by the OS
                                     Called on iOS in applicationWillTerminate()
                                     Called on Android in onDestroy()
                                */
    APP_LOWMEMORY,          /**< The application is low on memory, free memory if possible.
                                     Called on iOS in applicationDidReceiveMemoryWarning()
                                     Called on Android in onLowMemory()
                                */
    APP_WILLENTERBACKGROUND, /**< The application is about to enter the background
                                     Called on iOS in applicationWillResignActive()
                                     Called on Android in onPause()
                                */
    APP_DIDENTERBACKGROUND, /**< The application did enter the background and may not get CPU for some time
                                     Called on iOS in applicationDidEnterBackground()
                                     Called on Android in onPause()
                                */
    APP_WILLENTERFOREGROUND, /**< The application is about to enter the foreground
                                     Called on iOS in applicationWillEnterForeground()
                                     Called on Android in onResume()
                                */
    APP_DIDENTERFOREGROUND, /**< The application is now interactive
                                     Called on iOS in applicationDidBecomeActive()
                                     Called on Android in onResume()
                                */

    /* Display events */
    DISPLAYEVENT   = 0x150,  /**< Display state change */

    /* Window events */
    WINDOWEVENT    = 0x200, /**< Window state change */
    SYSWMEVENT,             /**< System specific event */

    /* Keyboard events */
    KEYDOWN        = 0x300, /**< Key pressed */
    KEYUP,                  /**< Key released */
    TEXTEDITING,            /**< Keyboard text editing (composition) */
    TEXTINPUT,              /**< Keyboard text input */
    KEYMAPCHANGED,          /**< Keymap changed due to a system event such as an
                                     input language or keyboard layout change.
                                */

    /* Mouse events */
    MOUSEMOTION    = 0x400, /**< Mouse moved */
    MOUSEBUTTONDOWN,        /**< Mouse button pressed */
    MOUSEBUTTONUP,          /**< Mouse button released */
    MOUSEWHEEL,             /**< Mouse wheel motion */

    /* Joystick events */
    JOYAXISMOTION  = 0x600, /**< Joystick axis motion */
    JOYBALLMOTION,          /**< Joystick trackball motion */
    JOYHATMOTION,           /**< Joystick hat position change */
    JOYBUTTONDOWN,          /**< Joystick button pressed */
    JOYBUTTONUP,            /**< Joystick button released */
    JOYDEVICEADDED,         /**< A new joystick has been inserted into the system */
    JOYDEVICEREMOVED,       /**< An opened joystick has been removed */

    /* Game controller events */
    CONTROLLERAXISMOTION  = 0x650, /**< Game controller axis motion */
    CONTROLLERBUTTONDOWN,          /**< Game controller button pressed */
    CONTROLLERBUTTONUP,            /**< Game controller button released */
    CONTROLLERDEVICEADDED,         /**< A new Game controller has been inserted into the system */
    CONTROLLERDEVICEREMOVED,       /**< An opened Game controller has been removed */
    CONTROLLERDEVICEREMAPPED,      /**< The controller mapping was updated */

    /* Touch events */
    FINGERDOWN      = 0x700,
    FINGERUP,
    FINGERMOTION,

    /* Gesture events */
    DOLLARGESTURE   = 0x800,
    DOLLARRECORD,
    MULTIGESTURE,

    /* Clipboard events */
    CLIPBOARDUPDATE = 0x900, /**< The clipboard changed */

    /* Drag and drop events */
    DROPFILE        = 0x1000, /**< The system requests a file open */
    DROPTEXT,                 /**< text/plain drag-and-drop event */
    DROPBEGIN,                /**< A new set of drops is beginning (NULL filename) */
    DROPCOMPLETE,             /**< Current set of drops is now complete (NULL filename) */

    /* Audio hotplug events */
    AUDIODEVICEADDED = 0x1100, /**< A new audio device is available */
    AUDIODEVICEREMOVED,        /**< An audio device has been removed. */

    /* Sensor events */
    SENSORUPDATE = 0x1200,     /**< A sensor was updated */

    /* Render events */
    RENDER_TARGETS_RESET = 0x2000, /**< The render targets have been reset and their contents need to be updated */
    RENDER_DEVICE_RESET, /**< The device has been reset and all textures need to be recreated */

    /** Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
     *  and should be allocated with SDL_RegisterEvents()
     */
    USEREVENT    = 0x8000,

    /**
     *  This last event is only for bounding internal arrays
     */
    LASTEVENT    = 0xFFFF
}
struct CommonEvent
{
    SDLEventType type;
    uint timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
}

union SDL_Event
{
    SDLEventType type;                    /**< Event type, shared with all events */
    CommonEvent common;         /**< Common event data */
    byte[56] padding;
}

const uint SDL_QUIT = 0x100;

func int main(int argc, char** args)
{
	initX();
	//Main loop flag
	bool quitd = false;

	//Event handler
	SDL_Event e;

	//While application is running
	while (!quitd)
	{
	    //Handle events on queue
		while (pollEvent(&e) != 0)
		{
					//User requests quit
					if (e.type == SDLEventType.QUIT)
					{
					    printf("Quit!!\n");
						quitd = true;
					}
				}

				//Apply the image
				//SDL_BlitSurface( gXOut, NULL, gScreenSurface, NULL );

				//Update the surface
		updateWindowSurface(gWindow);
	}

	close();
	return 0;
}

