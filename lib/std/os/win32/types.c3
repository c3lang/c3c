module std::os::win32;

def Win32_BOOL = int;
def Win32_BOOLEAN = Win32_BYTE;
def Win32_BYTE = char;
def Win32_CCHAR = cinterop::CChar;
def Win32_CHAR = cinterop::CChar;
def Win32_COLORREF = Win32_DWORD;
def Win32_DWORD = uint;
def Win32_DWORDLONG = ulong;
def Win32_DWORD_PTR = Win32_ULONG_PTR;
def Win32_DWORD32 = uint;
def Win32_DWORD64 = ulong;
def Win32_FLOAT = float;
def Win32_HACCEL = Win32_HANDLE;
def Win32_HALF_PTR = int;
def Win32_HANDLE = Win32_PVOID;
def Win32_HBITMAP = Win32_HANDLE;
def Win32_HBRUSH = Win32_HANDLE;
def Win32_HCOLORSPACE = Win32_HANDLE;
def Win32_HCONV = Win32_HANDLE;
def Win32_HCONVLIST = Win32_HANDLE;
def Win32_HCURSOR = Win32_HICON;
def Win32_HDC = Win32_HANDLE;
def Win32_HDDEDATA = Win32_HANDLE;
def Win32_HDESK = Win32_HANDLE;
def Win32_HDROP = Win32_HANDLE;
def Win32_HDWP = Win32_HANDLE;
def Win32_HFILE = int;
def Win32_HFONT = Win32_HANDLE;
def Win32_HGDIOBJ = Win32_HANDLE;
def Win32_HGLOBAL = Win32_HANDLE;
def Win32_HHOOK = Win32_HANDLE;
def Win32_HICON = Win32_HANDLE;
def Win32_HINSTANCE = Win32_HANDLE;
def Win32_HKEY = Win32_HANDLE;
def Win32_HKL = Win32_HANDLE;
def Win32_HLOCAL = Win32_HANDLE;
def Win32_HMENU = Win32_HANDLE;
def Win32_HMETAFILE = Win32_HANDLE;
def Win32_HMODULE = Win32_HANDLE;
def Win32_HMONITOR = Win32_HANDLE;
def Win32_HPALETTE = Win32_HANDLE;
def Win32_HPEN = Win32_HANDLE;
def Win32_HRESULT = Win32_LONG;
def Win32_HRGN = Win32_HANDLE;
def Win32_HRSRC = Win32_HANDLE;
def Win32_HSZ = Win32_HANDLE;
def Win32_HWINSTA = Win32_HANDLE;
def Win32_HWND = Win32_HANDLE;
def Win32_INT = int;
def Win32_INT_PTR = iptr;
def Win32_INT8 = ichar;
def Win32_INT16 = short;
def Win32_INT32 = int;
def Win32_INT64 = long;
def Win32_LANGID = Win32_WORD;
def Win32_LCID = Win32_DWORD;
def Win32_LCTYPE = Win32_DWORD;
def Win32_LGRPID = Win32_DWORD;
def Win32_LONG = int;
def Win32_LONGLONG = long;
def Win32_LONG_PTR = iptr;
def Win32_LONG32 = int;
def Win32_LONG64 = long;
def Win32_LPARAM = Win32_LONG_PTR;
def Win32_LPBOOL = Win32_BOOL*;
def Win32_LPBYTE = Win32_BYTE*;
def Win32_LPCOLORREF = Win32_DWORD*;
def Win32_LPCSTR = Win32_CCHAR*;
def Win32_LPCTSTR = Win32_LPCWSTR;
def Win32_LPCVOID = void*;
def Win32_LPCWSTR = Win32_WCHAR*;
def Win32_LPDWORD = Win32_DWORD*;
def Win32_LPHANDLE = Win32_HANDLE*;
def Win32_LPINT = int*;
def Win32_LPLONG = int*;
def Win32_LPSTR = Win32_CCHAR*;
def Win32_LPTSTR = Win32_LPWSTR;
def Win32_LPVOID = void*;
def Win32_LPWORD = Win32_WORD*;
def Win32_LPWSTR = Win32_WCHAR*;
def Win32_LRESULT = Win32_LONG_PTR;
def Win32_PBOOL = Win32_BOOL*;
def Win32_PBOOLEAN = Win32_BOOLEAN*;
def Win32_PBYTE = Win32_BYTE*;
def Win32_PCHAR = Win32_CHAR*;
def Win32_PCSTR = Win32_CHAR*;
def Win32_PCTSTR = Win32_LPCWSTR;
def Win32_PCUNICODE_STRING = Win32_UNICODE_STRING*;
def Win32_PCWSTR = WString;
def Win32_PDWORD = Win32_DWORD*;
def Win32_PDWORDLONG = Win32_DWORDLONG*;
def Win32_PDWORDPTR = Win32_DWORD_PTR*;
def Win32_PDWORD32 = Win32_DWORD32*;
def Win32_PDWORD64 = Win32_DWORD64*;
def Win32_PFLOAT = Win32_FLOAT*;
def Win32_PHALFPTR = Win32_HALF_PTR*;
def Win32_PHANDLE = Win32_HANDLE*;
def Win32_PHKEY = Win32_HKEY*;
def Win32_PINT = int*;
def Win32_PINTPTR = Win32_INT_PTR*;
def Win32_PINT8 = Win32_INT8*;
def Win32_PINT16 = Win32_INT16*;
def Win32_PINT32 = Win32_INT32*;
def Win32_PINT64 = Win32_INT64*;
def Win32_PLCID = Win32_PDWORD;
def Win32_PLONG = Win32_LONG*;
def Win32_PLONGLONG = Win32_LONGLONG*;
def Win32_PLONG_PTR = Win32_LONG_PTR*;
def Win32_PLONG32 = Win32_LONG32*;
def Win32_PLONG64 = Win32_LONG64*;
def Win32_POINTER_32 = uint;
def Win32_POINTER_64 = uptr;
def Win32_POINTER_SIGNED = iptr;
def Win32_POINTER_UNSIGNED = uptr;
def Win32_PSHORT = Win32_SHORT*;
def Win32_PSIZE_T = usz*;
def Win32_PSSIZE_T = isz*;
def Win32_PSTR = Win32_CHAR*;
def Win32_PTBYTE = Win32_TBYTE*;
def Win32_PTCHAR = Win32_TCHAR*;
def Win32_PTSTR = Win32_LPWSTR;
def Win32_PUCHAR = Win32_UCHAR*;
def Win32_PUHALFPTR = Win32_UHALF_PTR*;
def Win32_PUINT = Win32_UINT*;
def Win32_PUINTPTR = Win32_UINT_PTR*;
def Win32_PUINT8 = Win32_UINT8*;
def Win32_PUINT16 = Win32_UINT16*;
def Win32_PUINT32 = Win32_UINT32*;
def Win32_PUINT64 = Win32_UINT64*;
def Win32_PULONG = Win32_ULONG*;
def Win32_PULONGLONG = Win32_ULONGLONG*;
def Win32_PULONG_PTR = Win32_ULONG_PTR*;
def Win32_PULONG32 = Win32_ULONG32*;
def Win32_PULONG64 = Win32_ULONG64*;
def Win32_PUNICODE_STRING = Win32_UNICODE_STRING*;
def Win32_PUSHORT = Win32_USHORT*;
def Win32_PVOID = void*;
def Win32_PWCHAR = Win32_WCHAR*;
def Win32_PWORD = Win32_WORD*;
def Win32_PWSTR = Win32_WCHAR*;
def Win32_QWORD = ulong;
def Win32_SC_HANDLE = Win32_HANDLE;
def Win32_SC_LOCK = Win32_LPVOID;
def Win32_SERVICE_STATUS_HANDLE = Win32_HANDLE;
def Win32_SHORT = short;
def Win32_SIZE_T = usz;
def Win32_SOCKET = Win32_HANDLE;
def Win32_SSIZE_T = isz;
def Win32_TBYTE = Win32_WCHAR;
def Win32_TCHAR = Win32_WCHAR;
def Win32_UCHAR = char;
def Win32_UHALF_PTR = uint;
def Win32_UINT = uint;
def Win32_UINT_PTR = uptr;
def Win32_UINT8 = char;
def Win32_UINT16 = ushort;
def Win32_UINT32 = uint;
def Win32_UINT64 = ulong;
def Win32_ULONG = uint;
def Win32_ULONGLONG = ulong;
def Win32_ULONG_PTR = ulong;
def Win32_ULONG32 = uint;
def Win32_ULONG64 = ulong;
def Win32_USHORT = ushort;
def Win32_USN = Win32_LONGLONG;
def Win32_WCHAR = Char16;
def Win32_WORD = ushort;
def Win32_WPARAM = Win32_UINT_PTR;

const INVALID_HANDLE_VALUE = (Win32_HANDLE)(uptr)-1;

struct Win32_UNICODE_STRING
{
	Win32_USHORT length;
	Win32_USHORT maximum_length;
	Win32_PWSTR buffer;
}

union Win32_LARGE_INTEGER
{
	struct
	{
		Win32_DWORD lowPart;
		Win32_LONG  highPart;
	}
	ulong quadPart;
}

def Win32_CRITICAL_SECTION = distinct ulong[5];

struct Win32_SECURITY_ATTRIBUTES
{
	Win32_DWORD  nLength;
	Win32_LPVOID lpSecurityDescriptor;
	Win32_BOOL   bInheritHandle;
}



def Win32_LPSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;
def Win32_PSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;

struct Win32_STARTUPINFOW
{
	Win32_DWORD  cb;
	Win32_LPWSTR lpReserved;
	Win32_LPWSTR lpDesktop;
	Win32_LPWSTR lpTitle;
	Win32_DWORD  dwX;
	Win32_DWORD  dwY;
	Win32_DWORD  dwXSize;
	Win32_DWORD  dwYSize;
	Win32_DWORD  dwXCountChars;
	Win32_DWORD  dwYCountChars;
	Win32_DWORD  dwFillAttribute;
	Win32_DWORD  dwFlags;
	Win32_WORD   wShowWindow;
	Win32_WORD   cbReserved2;
	Win32_LPBYTE lpReserved2;
	Win32_HANDLE hStdInput;
	Win32_HANDLE hStdOutput;
	Win32_HANDLE hStdError;
}

struct Win32_OVERLAPPED
{
	Win32_ULONG_PTR internal;
	Win32_ULONG_PTR internalHigh;
	union {
		struct {
			Win32_DWORD offset;
			Win32_DWORD offsetHigh;
		}
		Win32_PVOID pointer;
	}
	Win32_HANDLE hEvent;
}

def Win32_LPOVERLAPPED = Win32_OVERLAPPED*;
def Win32_LPSTARTUPINFOW = Win32_STARTUPINFOW*;

struct Win32_STARTUPINFOEXW
{
	inline Win32_STARTUPINFOW win32_StartupInfo;
	Win32_LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList;
}

def Win32_LPPROC_THREAD_ATTRIBUTE_LIST = void*;
def Win32_LPSTARTUPINFOEXW = Win32_STARTUPINFOEXW*;

struct Win32_FILETIME
{
	Win32_DWORD dwLowDateTime;
	Win32_DWORD dwHighDateTime;
}

struct Win32_PROCESS_INFORMATION
{
	Win32_HANDLE hProcess;
	Win32_HANDLE hThread;
	Win32_DWORD  dwProcessId;
	Win32_DWORD  dwThreadId;
}

def Win32_PPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;
def Win32_LPPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;
