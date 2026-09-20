module std::os::win32 @link("gdi32") @feat(WIN32);


struct Win32_BITMAPV5HEADER
{
	Win32_DWORD        bV5Size;
	Win32_LONG         bV5Width;
	Win32_LONG         bV5Height;
	Win32_WORD         bV5Planes;
	Win32_WORD         bV5BitCount;
	Win32_DWORD        bV5Compression;
	Win32_DWORD        bV5SizeImage;
	Win32_LONG         bV5XPelsPerMeter;
	Win32_LONG         bV5YPelsPerMeter;
	Win32_DWORD        bV5ClrUsed;
	Win32_DWORD        bV5ClrImportant;
	Win32_DWORD        bV5RedMask;
	Win32_DWORD        bV5GreenMask;
	Win32_DWORD        bV5BlueMask;
	Win32_DWORD        bV5AlphaMask;
	Win32_DWORD        bV5CSType;
	Win32_CIEXYZTRIPLE bV5Endpoints;
	Win32_DWORD        bV5GammaRed;
	Win32_DWORD        bV5GammaGreen;
	Win32_DWORD        bV5GammaBlue;
	Win32_DWORD        bV5Intent;
	Win32_DWORD        bV5ProfileData;
	Win32_DWORD        bV5ProfileSize;
	Win32_DWORD        bV5Reserved;
}

struct Win32_CIEXYZ
{
	Win32_FXPT2DOT30 ciexyzX;
	Win32_FXPT2DOT30 ciexyzY;
	Win32_FXPT2DOT30 ciexyzZ;
}

struct Win32_CIEXYZTRIPLE
{
	Win32_CIEXYZ ciexyzRed;
	Win32_CIEXYZ ciexyzGreen;
	Win32_CIEXYZ ciexyzBlue;
}


struct Win32_PIXELFORMATDESCRIPTOR
{
	Win32_WORD  nSize;
	Win32_WORD  nVersion;
	Win32_DWORD dwFlags;
	Win32_BYTE  iPixelType;
	Win32_BYTE  cColorBits;
	Win32_BYTE  cRedBits;
	Win32_BYTE  cRedShift;
	Win32_BYTE  cGreenBits;
	Win32_BYTE  cGreenShift;
	Win32_BYTE  cBlueBits;
	Win32_BYTE  cBlueShift;
	Win32_BYTE  cAlphaBits;
	Win32_BYTE  cAlphaShift;
	Win32_BYTE  cAccumBits;
	Win32_BYTE  cAccumRedBits;
	Win32_BYTE  cAccumGreenBits;
	Win32_BYTE  cAccumBlueBits;
	Win32_BYTE  cAccumAlphaBits;
	Win32_BYTE  cDepthBits;
	Win32_BYTE  cStencilBits;
	Win32_BYTE  cAuxBuffers;
	Win32_BYTE  iLayerType;
	Win32_BYTE  bReserved;
	Win32_DWORD dwLayerMask;
	Win32_DWORD dwVisibleMask;
	Win32_DWORD dwDamageMask;
}

alias Win32_PPIXELFORMATDESCRIPTOR = Win32_PIXELFORMATDESCRIPTOR*;
alias Win32_LPPIXELFORMATDESCRIPTOR = Win32_PIXELFORMATDESCRIPTOR*;
alias Win32_LPBITMAPV5HEADER = Win32_BITMAPV5HEADER*;
alias Win32_PBITMAPV5HEADER = Win32_BITMAPV5HEADER*;
alias Win32_FXPT2DOT30 = CLong;
alias Win32_LPFXPT2DOT30 = Win32_FXPT2DOT30*;

const PFD_DRAW_TO_WINDOW      = 0x00000004;
const PFD_DRAW_TO_BITMAP      = 0x00000008;
const PFD_SUPPORT_GDI         = 0x00000010;
const PFD_SUPPORT_OPENGL      = 0x00000020;
const PFD_GENERIC_ACCELERATED = 0x00001000;
const PFD_GENERIC_FORMAT      = 0x00000040;
const PFD_NEED_PALETTE        = 0x00000080;
const PFD_NEED_SYSTEM_PALETTE = 0x00000100;
const PFD_DOUBLEBUFFER        = 0x00000001;
const PFD_STEREO              = 0x00000002;
const PFD_SWAP_LAYER_BUFFERS  = 0x00000800;

const PFD_TYPE_RGBA       = 0;
const PFD_TYPE_COLORINDEX = 1;

const PFD_MAIN_PLANE      = 0;
const PFD_OVERLAY_PLANE   = 1;
const PFD_UNDERLAY_PLANE  = -1;

// biCompression values (wingdi.h)
const Win32_DWORD BI_RGB       = 0;
const Win32_DWORD BI_RLE8      = 1;
const Win32_DWORD BI_RLE4      = 2;
const Win32_DWORD BI_BITFIELDS = 3;
const Win32_DWORD BI_JPEG      = 4;
const Win32_DWORD BI_PNG       = 5;

const Win32_UINT DIB_RGB_COLORS = 0;   // color table holds RGBs
const Win32_UINT DIB_PAL_COLORS = 1;   // color table holds palette indices

// DEVMODE.dmFields bits — only the display-related ones your code needs
const Win32_DWORD DM_BITSPERPEL       = 0x00040000;
const Win32_DWORD DM_PELSWIDTH        = 0x00080000;
const Win32_DWORD DM_PELSHEIGHT       = 0x00100000;
const Win32_DWORD DM_DISPLAYFLAGS     = 0x00200000;
const Win32_DWORD DM_DISPLAYFREQUENCY = 0x00400000;

// DISPLAY_DEVICE.StateFlags
const Win32_DWORD DISPLAY_DEVICE_ATTACHED_TO_DESKTOP = 0x00000001;
const Win32_DWORD DISPLAY_DEVICE_ACTIVE              = 0x00000001;
const Win32_DWORD DISPLAY_DEVICE_MULTI_DRIVER        = 0x00000002;
const Win32_DWORD DISPLAY_DEVICE_ATTACHED            = 0x00000002;
const Win32_DWORD DISPLAY_DEVICE_PRIMARY_DEVICE      = 0x00000004;
const Win32_DWORD DISPLAY_DEVICE_MIRRORING_DRIVER    = 0x00000008;
const Win32_DWORD DISPLAY_DEVICE_VGA_COMPATIBLE      = 0x00000010;
const Win32_DWORD DISPLAY_DEVICE_REMOVABLE           = 0x00000020;
const Win32_DWORD DISPLAY_DEVICE_DISCONNECT          = 0x02000000;
const Win32_DWORD DISPLAY_DEVICE_REMOTE              = 0x04000000;
const Win32_DWORD DISPLAY_DEVICE_MODESPRUNED         = 0x08000000;

const Win32_DWORD SRCCOPY     = 0x00CC0020;
const Win32_DWORD SRCPAINT    = 0x00EE0086;
const Win32_DWORD SRCAND      = 0x008800C6;
const Win32_DWORD SRCINVERT   = 0x00660046;
const Win32_DWORD SRCERASE    = 0x00440328;
const Win32_DWORD NOTSRCCOPY  = 0x00330008;
const Win32_DWORD NOTSRCERASE = 0x001100A6;
const Win32_DWORD MERGECOPY   = 0x00C000CA;
const Win32_DWORD MERGEPAINT  = 0x00BB0226;
const Win32_DWORD PATCOPY     = 0x00F00021;
const Win32_DWORD PATPAINT    = 0x00FB0A09;
const Win32_DWORD PATINVERT   = 0x005A0049;
const Win32_DWORD DSTINVERT   = 0x00550009;
const Win32_DWORD BLACKNESS   = 0x00000042;
const Win32_DWORD WHITENESS   = 0x00FF0062;
const Win32_DWORD NOOP        = 0x00AA0029;

extern fn CInt choosePixelFormat(Win32_HDC hdc, Win32_PIXELFORMATDESCRIPTOR* ppfd) @cname("ChoosePixelFormat");
extern fn Win32_BOOL setPixelFormat(Win32_HDC hdc, CInt format, Win32_PIXELFORMATDESCRIPTOR* ppfd) @cname("SetPixelFormat");
extern fn CInt describePixelFormat(Win32_HDC hdc, CInt iPixelFormat, Win32_UINT nBytes, Win32_PIXELFORMATDESCRIPTOR* ppfd) @cname("DescribePixelFormat");
extern fn Win32_BOOL swapBuffers(Win32_HDC hdc) @cname("SwapBuffers");
extern fn Win32_HBITMAP createDIBSection(Win32_HDC hdc, Win32_LPVOID pbmi, Win32_UINT usage, void** ppvBits, Win32_HANDLE hSection, Win32_DWORD offset) @cname("CreateDIBSection");
extern fn Win32_HDC createCompatibleDC(Win32_HDC hdc) @cname("CreateCompatibleDC");
extern fn Win32_HDC createDCW(Win32_LPCWSTR lpszDriver, Win32_LPCWSTR lpszDevice, Win32_LPCWSTR lpszOutput, Win32_DEVMODEW* lpInitData) @cname("CreateDCW");
extern fn Win32_HGDIOBJ selectObject(Win32_HDC hdc, Win32_HGDIOBJ h) @cname("SelectObject");
extern fn Win32_BOOL deleteObject(Win32_HGDIOBJ ho) @cname("DeleteObject");
extern fn Win32_BOOL bitBlt(Win32_HDC hdc, CInt x, CInt y, CInt cx, CInt cy, Win32_HDC hdcSrc, CInt x1, CInt y1, Win32_DWORD rop) @cname("BitBlt");
extern fn CInt getDeviceCaps(Win32_HDC hdc, CInt index) @cname("GetDeviceCaps");
extern fn Win32_BOOL getDeviceGammaRamp(Win32_HDC hdc, Win32_LPVOID lpRamp) @cname("GetDeviceGammaRamp");
extern fn Win32_BOOL setDeviceGammaRamp(Win32_HDC hdc, Win32_LPVOID lpRamp) @cname("SetDeviceGammaRamp");
extern fn Win32_HBITMAP createBitmap(CInt nWidth, CInt nHeight, Win32_UINT nPlanes, Win32_UINT nBitCount, Win32_LPCVOID lpBits) @cname("CreateBitmap");
