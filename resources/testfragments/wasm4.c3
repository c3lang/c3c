// WASM-4: https://wasm4.org/docs
module w4;
// ┌───────────────────────────────────────────────────────────────────────────┐
// │                                                                           │
// │ Platform Constants                                                        │
// │                                                                           │
// └───────────────────────────────────────────────────────────────────────────┘

const SCREEN_SIZE = 160;

// ┌───────────────────────────────────────────────────────────────────────────┐
// │                                                                           │
// │ Memory Addresses                                                          │
// │                                                                           │
// └───────────────────────────────────────────────────────────────────────────┘

const uint* PALETTE = (void*)0x04;
const ushort* DRAW_COLORS = (void*)0x14;
const char* GAMEPAD1 = (void*)0x16;
const char* GAMEPAD2 = (void*)0x17;
const char* GAMEPAD3 = (void*)0x18;
const char* GAMEPAD4 = (void*)0x19;
const short* MOUSE_X = (void*)0x1a;
const short* MOUSE_Y = (void*)0x1c;
const char* MOUSE_BUTTONS = (void*)0x1e;
const char* SYSTEM_FLAGS = (void*)0x1f;
const char* NETPLAY = (void*)0x20;
const char* FRAMEBUFFER = (void*)0xa0;

const BUTTON_1 = 1;
const BUTTON_2 = 2;
const BUTTON_LEFT = 16;
const BUTTON_RIGHT = 32;
const BUTTON_UP = 64;
const BUTTON_DOWN = 128;

const MOUSE_LEFT = 1;
const MOUSE_RIGHT = 2;
const MOUSE_MIDDLE = 4;

const SYSTEM_PRESERVE_FRAMEBUFFER = 1;
const SYSTEM_HIDE_GAMEPAD_OVERLAY = 2;

// ┌───────────────────────────────────────────────────────────────────────────┐
// │                                                                           │
// │ Drawing Functions                                                         │
// │                                                                           │
// └───────────────────────────────────────────────────────────────────────────┘

/* Copies pixels to the framebuffer. */
extern fn void blit(char* data, int x, int y, uint width, uint height, uint flags) @cname("blit");

/* Copies a subregion within a larger sprite atlas to the framebuffer. */
extern fn void blit_sub(char* data, int x, int y, uint width, uint height,
                     uint src_x, uint src_y, uint stride, uint flags) @cname("blitSub");

const BLIT_2BPP = 1;
const BLIT_1BPP = 0;
const BLIT_FLIP_X = 2;
const BLIT_FLIP_Y = 4;
const BLIT_ROTATE = 8;

/* Draws a line between two points. */
extern fn void line(int x1, int y1, int x2, int y2);

/* Draws a horizontal line. */
extern fn void hline(int x, int y, uint len);

/* Draws a vertical line. */
extern fn void vline(int x, int y, uint len);

/* Draws an oval (or circle). */
extern fn void oval(int x, int y, uint width, uint height);

/* Draws a rectangle. */
extern fn void rect(int x, int y, uint width, uint height);

/* Draws text using the built-in system font. */
extern fn void text(char* text, int x, int y) @cname("text");

// ┌───────────────────────────────────────────────────────────────────────────┐
// │                                                                           │
// │ Sound Functions                                                           │
// │                                                                           │
// └───────────────────────────────────────────────────────────────────────────┘

/* Plays a sound tone. */
extern fn void tone(uint frequency, uint duration, uint volume, uint flags);

const TONE_PULSE1 = 0;
const TONE_PULSE2 = 1;
const TONE_TRIANGLE = 2;
const TONE_NOISE = 3;
const TONE_MODE1 = 0;
const TONE_MODE2 = 4;
const TONE_MODE3 = 8;
const TONE_MODE4 = 12;
const TONE_PAN_LEFT = 16;
const TONE_PAN_RIGHT = 32;

// ┌───────────────────────────────────────────────────────────────────────────┐
// │                                                                           │
// │ Storage Functions                                                         │
// │                                                                           │
// └───────────────────────────────────────────────────────────────────────────┘

/* Reads up to `size` bytes from persistent storage into the pointer `dest`. */
extern fn uint diskr(void* dest, uint size);

/* Writes up to `size` bytes from the pointer `src` into persistent storage. */
extern fn uint diskw (void* src, uint size);

/* Prints a message to the debug console. */
extern fn void trace(char* str);

/* Prints a message to the debug console. */
extern fn void tracef(char* fmt, ...);

char[8] smiley = {
	0b11000011,
	0b10000001,
	0b00100100,
	0b00100100,
	0b00000000,
	0b00100100,
	0b10011001,
	0b11000011,
};

fn void start() @cname("start")
{}

fn void update() @cname("update")
{
	DRAW_COLORS[0] = 2;
	text("Hello from C3", 10, 10);
	if (GAMEPAD1[0] & BUTTON_1)
	{
		DRAW_COLORS[0] = 4;
	}
	blit(&smiley, 76, 76, 8, 8, BLIT_1BPP);
    text("Press X to blink", 16, 90);
}