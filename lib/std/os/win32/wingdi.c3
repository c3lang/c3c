module std::os::win32 @link("gdi32") @feat(WIN32);


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
