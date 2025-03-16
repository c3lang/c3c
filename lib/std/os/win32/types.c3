module std::os::win32;

alias Win32_BOOL = int;
alias Win32_BOOLEAN = Win32_BYTE;
alias Win32_BYTE = char;
alias Win32_CCHAR = cinterop::CChar;
alias Win32_CHAR = cinterop::CChar;
alias Win32_COLORREF = Win32_DWORD;
alias Win32_DWORD = uint;
alias Win32_DWORDLONG = ulong;
alias Win32_DWORD_PTR = Win32_ULONG_PTR;
alias Win32_DWORD32 = uint;
alias Win32_DWORD64 = ulong;
alias Win32_FLOAT = float;
alias Win32_HACCEL = Win32_HANDLE;
alias Win32_HALF_PTR = int;
alias Win32_HANDLE = Win32_PVOID;
alias Win32_HBITMAP = Win32_HANDLE;
alias Win32_HBRUSH = Win32_HANDLE;
alias Win32_HCOLORSPACE = Win32_HANDLE;
alias Win32_HCONV = Win32_HANDLE;
alias Win32_HCONVLIST = Win32_HANDLE;
alias Win32_HCURSOR = Win32_HICON;
alias Win32_HDC = Win32_HANDLE;
alias Win32_HDDEDATA = Win32_HANDLE;
alias Win32_HDESK = Win32_HANDLE;
alias Win32_HDROP = Win32_HANDLE;
alias Win32_HDWP = Win32_HANDLE;
alias Win32_HFILE = int;
alias Win32_HFONT = Win32_HANDLE;
alias Win32_HGDIOBJ = Win32_HANDLE;
alias Win32_HGLOBAL = Win32_HANDLE;
alias Win32_HHOOK = Win32_HANDLE;
alias Win32_HICON = Win32_HANDLE;
alias Win32_HINSTANCE = Win32_HANDLE;
alias Win32_HKEY = Win32_HANDLE;
alias Win32_HKL = Win32_HANDLE;
alias Win32_HLOCAL = Win32_HANDLE;
alias Win32_HMENU = Win32_HANDLE;
alias Win32_HMETAFILE = Win32_HANDLE;
alias Win32_HMODULE = Win32_HANDLE;
alias Win32_HMONITOR = Win32_HANDLE;
alias Win32_HPALETTE = Win32_HANDLE;
alias Win32_HPEN = Win32_HANDLE;
alias Win32_HRESULT = Win32_LONG;
alias Win32_HRGN = Win32_HANDLE;
alias Win32_HRSRC = Win32_HANDLE;
alias Win32_HSZ = Win32_HANDLE;
alias Win32_HWINSTA = Win32_HANDLE;
alias Win32_HWND = Win32_HANDLE;
alias Win32_INT = int;
alias Win32_INT_PTR = iptr;
alias Win32_INT8 = ichar;
alias Win32_INT16 = short;
alias Win32_INT32 = int;
alias Win32_INT64 = long;
alias Win32_LANGID = Win32_WORD;
alias Win32_LCID = Win32_DWORD;
alias Win32_LCTYPE = Win32_DWORD;
alias Win32_LGRPID = Win32_DWORD;
alias Win32_LONG = int;
alias Win32_LONGLONG = long;
alias Win32_LONG_PTR = iptr;
alias Win32_LONG32 = int;
alias Win32_LONG64 = long;
alias Win32_LPARAM = Win32_LONG_PTR;
alias Win32_LPBOOL = Win32_BOOL*;
alias Win32_LPBYTE = Win32_BYTE*;
alias Win32_LPCOLORREF = Win32_DWORD*;
alias Win32_LPCSTR = Win32_CCHAR*;
alias Win32_LPCTSTR = Win32_LPCWSTR;
alias Win32_LPCVOID = void*;
alias Win32_LPCWSTR = Win32_WCHAR*;
alias Win32_LPDWORD = Win32_DWORD*;
alias Win32_LPHANDLE = Win32_HANDLE*;
alias Win32_LPINT = int*;
alias Win32_LPLONG = int*;
alias Win32_LPSTR = Win32_CCHAR*;
alias Win32_LPTSTR = Win32_LPWSTR;
alias Win32_LPVOID = void*;
alias Win32_LPWORD = Win32_WORD*;
alias Win32_LPWSTR = Win32_WCHAR*;
alias Win32_LRESULT = Win32_LONG_PTR;
alias Win32_NTSTATUS = Win32_LONG;
alias Win32_PBOOL = Win32_BOOL*;
alias Win32_PBOOLEAN = Win32_BOOLEAN*;
alias Win32_PBYTE = Win32_BYTE*;
alias Win32_PCHAR = Win32_CHAR*;
alias Win32_PCSTR = Win32_CHAR*;
alias Win32_PCTSTR = Win32_LPCWSTR;
alias Win32_PCUNICODE_STRING = Win32_UNICODE_STRING*;
alias Win32_PCWSTR = WString;
alias Win32_PDWORD = Win32_DWORD*;
alias Win32_PDWORDLONG = Win32_DWORDLONG*;
alias Win32_PDWORDPTR = Win32_DWORD_PTR*;
alias Win32_PDWORD32 = Win32_DWORD32*;
alias Win32_PDWORD64 = Win32_DWORD64*;
alias Win32_PFLOAT = Win32_FLOAT*;
alias Win32_PHALFPTR = Win32_HALF_PTR*;
alias Win32_PHANDLE = Win32_HANDLE*;
alias Win32_PHKEY = Win32_HKEY*;
alias Win32_PINT = int*;
alias Win32_PINTPTR = Win32_INT_PTR*;
alias Win32_PINT8 = Win32_INT8*;
alias Win32_PINT16 = Win32_INT16*;
alias Win32_PINT32 = Win32_INT32*;
alias Win32_PINT64 = Win32_INT64*;
alias Win32_PLCID = Win32_PDWORD;
alias Win32_PLONG = Win32_LONG*;
alias Win32_PLONGLONG = Win32_LONGLONG*;
alias Win32_PLONG_PTR = Win32_LONG_PTR*;
alias Win32_PLONG32 = Win32_LONG32*;
alias Win32_PLONG64 = Win32_LONG64*;
alias Win32_POINTER_32 = uint;
alias Win32_POINTER_64 = uptr;
alias Win32_POINTER_SIGNED = iptr;
alias Win32_POINTER_UNSIGNED = uptr;
alias Win32_PSHORT = Win32_SHORT*;
alias Win32_PSIZE_T = usz*;
alias Win32_PSSIZE_T = isz*;
alias Win32_PSTR = Win32_CHAR*;
alias Win32_PTBYTE = Win32_TBYTE*;
alias Win32_PTCHAR = Win32_TCHAR*;
alias Win32_PTSTR = Win32_LPWSTR;
alias Win32_PUCHAR = Win32_UCHAR*;
alias Win32_PUHALFPTR = Win32_UHALF_PTR*;
alias Win32_PUINT = Win32_UINT*;
alias Win32_PUINTPTR = Win32_UINT_PTR*;
alias Win32_PUINT8 = Win32_UINT8*;
alias Win32_PUINT16 = Win32_UINT16*;
alias Win32_PUINT32 = Win32_UINT32*;
alias Win32_PUINT64 = Win32_UINT64*;
alias Win32_PULONG = Win32_ULONG*;
alias Win32_PULONGLONG = Win32_ULONGLONG*;
alias Win32_PULONG_PTR = Win32_ULONG_PTR*;
alias Win32_PULONG32 = Win32_ULONG32*;
alias Win32_PULONG64 = Win32_ULONG64*;
alias Win32_PUNICODE_STRING = Win32_UNICODE_STRING*;
alias Win32_PUSHORT = Win32_USHORT*;
alias Win32_PVOID = void*;
alias Win32_PWCHAR = Win32_WCHAR*;
alias Win32_PWORD = Win32_WORD*;
alias Win32_PWSTR = Win32_WCHAR*;
alias Win32_QWORD = ulong;
alias Win32_SC_HANDLE = Win32_HANDLE;
alias Win32_SC_LOCK = Win32_LPVOID;
alias Win32_SERVICE_STATUS_HANDLE = Win32_HANDLE;
alias Win32_SHORT = short;
alias Win32_SIZE_T = usz;
alias Win32_SOCKET = Win32_HANDLE;
alias Win32_SSIZE_T = isz;
alias Win32_TBYTE = Win32_WCHAR;
alias Win32_TCHAR = Win32_WCHAR;
alias Win32_UCHAR = char;
alias Win32_UHALF_PTR = uint;
alias Win32_UINT = uint;
alias Win32_UINT_PTR = uptr;
alias Win32_UINT8 = char;
alias Win32_UINT16 = ushort;
alias Win32_UINT32 = uint;
alias Win32_UINT64 = ulong;
alias Win32_ULONG = uint;
alias Win32_ULONGLONG = ulong;
alias Win32_ULONG_PTR = ulong;
alias Win32_ULONG32 = uint;
alias Win32_ULONG64 = ulong;
alias Win32_USHORT = ushort;
alias Win32_USN = Win32_LONGLONG;
alias Win32_WCHAR = Char16;
alias Win32_WORD = ushort;
alias Win32_WPARAM = Win32_UINT_PTR;

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

typedef Win32_CRITICAL_SECTION = ulong[5];

struct Win32_SECURITY_ATTRIBUTES
{
	Win32_DWORD  nLength;
	Win32_LPVOID lpSecurityDescriptor;
	Win32_BOOL   bInheritHandle;
}



alias Win32_LPSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;
alias Win32_PSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;

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

alias Win32_LPOVERLAPPED = Win32_OVERLAPPED*;
alias Win32_LPSTARTUPINFOW = Win32_STARTUPINFOW*;

struct Win32_STARTUPINFOEXW
{
	inline Win32_STARTUPINFOW win32_StartupInfo;
	Win32_LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList;
}

alias Win32_LPPROC_THREAD_ATTRIBUTE_LIST = void*;
alias Win32_LPSTARTUPINFOEXW = Win32_STARTUPINFOEXW*;

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

alias Win32_PPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;
alias Win32_LPPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;

struct Win32_SYSTEM_INFO
{
	union
	{
		Win32_DWORD dwOemId;
		struct
		{
			Win32_WORD wProcessorArchitecture;
			Win32_WORD wReserved;
		}
	}
	Win32_DWORD         dwPageSize;
	Win32_LPVOID        lpMinimumApplicationAddress;
	Win32_LPVOID        lpMaximumApplicationAddress;
	Win32_DWORD_PTR     dwActiveProcessorMask;
	Win32_DWORD         dwNumberOfProcessors;
	Win32_DWORD         dwProcessorType;
	Win32_DWORD         dwAllocationGranularity;
	Win32_WORD          wProcessorLevel;
	Win32_WORD          wProcessorRevision;
}

alias Win32_LPSYSTEM_INFO = Win32_SYSTEM_INFO*;

struct Win32_MODULEINFO
{
	Win32_LPVOID lpBaseOfDll;
	Win32_DWORD  sizeOfImage;
	Win32_LPVOID entryPoint;
}

struct Win32_IMAGEHLP_LINE64
{
	Win32_DWORD   sizeOfStruct;
	Win32_PVOID   key;
	Win32_DWORD   lineNumber;
	Win32_PCHAR   fileName;
	Win32_DWORD64 address;
}

enum Win32_SYM_TYPE
{
	SYM_NONE,
	SYM_COFF,
	SYM_CV,
	SYM_PDB,
	SYM_EXPORT,
	SYM_DEFERRED,
	SYM_SYM,
	SYM_DIA,
	SYM_VIRTUAL
}

struct Win32_GUID
{
	CULong data1;
	CUShort data2;
	CUShort data3;
	char[8] data4;
}

struct Win32_IMAGEHLP_MODULE64
{
	Win32_DWORD     sizeOfStruct;
	Win32_DWORD64   baseOfImage;
	Win32_DWORD     imageSize;
	Win32_DWORD     timeDateStamp;
	Win32_DWORD     checkSum;
	Win32_DWORD     numSyms;
	Win32_SYM_TYPE  symType;
	Win32_CHAR[32]  moduleName;
	Win32_CHAR[256] imageName;
	Win32_CHAR[256] loadedImageName;
	Win32_CHAR[256] loadedPdbName;
	Win32_DWORD     cVSig;
	Win32_CHAR**    cVData;
	Win32_DWORD     pdbSig;
	Win32_GUID      pdbSig70;
	Win32_DWORD     pdbAge;
	Win32_BOOL      pdbUnmatched;
	Win32_BOOL      dbgUnmatched;
	Win32_BOOL      lineNumbers;
	Win32_BOOL      globalSymbols;
	Win32_BOOL      typeInfo;
	Win32_BOOL      sourceIndexed;
	Win32_BOOL      publics;
	Win32_DWORD     machineType;
	Win32_DWORD     reserved;
}

alias Win32_PIMAGEHLP_MODULE64 = Win32_IMAGEHLP_MODULE64*;

struct Win32_ARM64_NT_CONTEXT @align(16)
{
	Win32_DWORD   contextFlags;
	Win32_DWORD   cpsr;
	union
	{
		struct
		{
			Win32_DWORD64 x0;
			Win32_DWORD64 x1;
			Win32_DWORD64 x2;
			Win32_DWORD64 x3;
			Win32_DWORD64 x4;
			Win32_DWORD64 x5;
			Win32_DWORD64 x6;
			Win32_DWORD64 x7;
			Win32_DWORD64 x8;
			Win32_DWORD64 x9;
			Win32_DWORD64 x10;
			Win32_DWORD64 x11;
			Win32_DWORD64 x12;
			Win32_DWORD64 x13;
			Win32_DWORD64 x14;
			Win32_DWORD64 x15;
			Win32_DWORD64 x16;
			Win32_DWORD64 x17;
			Win32_DWORD64 x18;
			Win32_DWORD64 x19;
			Win32_DWORD64 x20;
			Win32_DWORD64 x21;
			Win32_DWORD64 x22;
			Win32_DWORD64 x23;
			Win32_DWORD64 x24;
			Win32_DWORD64 x25;
			Win32_DWORD64 x26;
			Win32_DWORD64 x27;
			Win32_DWORD64 x28;
			Win32_DWORD64 fp;
			Win32_DWORD64 lr;
		}
		Win32_DWORD64[31] x;
	}
	Win32_DWORD64                           sp;
	Win32_DWORD64                           pc;
	Win32_ARM64_NT_NEON128[32]              v;
	Win32_DWORD                             fpcr;
	Win32_DWORD                             fpsr;
	Win32_DWORD[ARM64_MAX_BREAKPOINTS]      bcr;
	Win32_DWORD64[ARM64_MAX_BREAKPOINTS]    bvr;
	Win32_DWORD[ARM64_MAX_WATCHPOINTS]      wcr;
	Win32_DWORD64[ARM64_MAX_WATCHPOINTS]    wvr;
}
const ARM64_MAX_BREAKPOINTS = 8;
const ARM64_MAX_WATCHPOINTS = 2;

struct Win32_ARM64_NT_NEON128
{
	Win32_ULONGLONG low;
	Win32_LONGLONG  high;
}

struct Win32_XMM_SAVE_AREA32
{
	Win32_USHORT    controlWord;
	Win32_USHORT    statusWord;
	Win32_UCHAR     tagWord;
	Win32_UCHAR     reserved1;
	Win32_USHORT    errorOpcode;
	Win32_ULONG     errrorOffset;
	Win32_USHORT    errorSelector;
	Win32_USHORT    reserved2;
	Win32_ULONG     dataOffset;
	Win32_USHORT    dataSelector;
	Win32_USHORT    reserved3;
	Win32_ULONG     mxCsr;
	Win32_ULONG     mxCsr_Mask;
	Win32_M128A[8]  floatRegisters;
	Win32_M128A[16] xmmRegisters;
	Win32_UCHAR[96] reserved4;
}

struct Win32_AMD64_CONTEXT @align(16)
{
	Win32_DWORD64 p1Home;
	Win32_DWORD64 p2Home;
	Win32_DWORD64 p3Home;
	Win32_DWORD64 p4Home;
	Win32_DWORD64 p5Home;
	Win32_DWORD64 p6Home;
	Win32_DWORD   contextFlags;
	Win32_DWORD   mxCsr;
	Win32_WORD    segCs;
	Win32_WORD    segDs;
	Win32_WORD    segEs;
	Win32_WORD    segFs;
	Win32_WORD    segGs;
	Win32_WORD    segSs;
	Win32_DWORD   eFlags;
	Win32_DWORD64 dr0;
	Win32_DWORD64 dr1;
	Win32_DWORD64 dr2;
	Win32_DWORD64 dr3;
	Win32_DWORD64 dr6;
	Win32_DWORD64 dr7;
	Win32_DWORD64 rax;
	Win32_DWORD64 rcx;
	Win32_DWORD64 rdx;
	Win32_DWORD64 rbx;
	Win32_DWORD64 rsp;
	Win32_DWORD64 rbp;
	Win32_DWORD64 rsi;
	Win32_DWORD64 rdi;
	Win32_DWORD64 r8;
	Win32_DWORD64 r9;
	Win32_DWORD64 r10;
	Win32_DWORD64 r11;
	Win32_DWORD64 r12;
	Win32_DWORD64 r13;
	Win32_DWORD64 r14;
	Win32_DWORD64 r15;
	Win32_DWORD64 rip;
	union
	{
		Win32_XMM_SAVE_AREA32   fltSave;
		//Win32_NEON128[16]       q;
		Win32_ULONGLONG[32]     d;
		struct
		{
			Win32_M128A[2]  header;
			Win32_M128A[8]  legacy;
			Win32_M128A     xmm0;
			Win32_M128A     xmm1;
			Win32_M128A     xmm2;
			Win32_M128A     xmm3;
			Win32_M128A     xmm4;
			Win32_M128A     xmm5;
			Win32_M128A     xmm6;
			Win32_M128A     xmm7;
			Win32_M128A     xmm8;
			Win32_M128A     xmm9;
			Win32_M128A     xmm10;
			Win32_M128A     xmm11;
			Win32_M128A     xmm12;
			Win32_M128A     xmm13;
			Win32_M128A     xmm14;
			Win32_M128A     xmm15;
		}
		Win32_DWORD[32]     s;
	}
	Win32_M128A[26]         vectorRegister;
	Win32_DWORD64           vectorControl;
	Win32_DWORD64           debugControl;
	Win32_DWORD64           lastBranchToRip;
	Win32_DWORD64           lastBranchFromRip;
	Win32_DWORD64           lastExceptionToRip;
	Win32_DWORD64           lastExceptionFromRip;
}

const CONTEXT_AMD64 = 0x00100000;
const CONTEXT_AMD64_CONTROL = CONTEXT_AMD64 | 0x0001;
const CONTEXT_AMD64_INTEGER = CONTEXT_AMD64 | 0x0002;
const CONTEXT_AMD64_SEGMENTS = CONTEXT_AMD64 | 0x0004;
const CONTEXT_AMD64_FLOATING_POINT = CONTEXT_AMD64 | 0x0008;
const CONTEXT_AMD64_DEBUG_REGISTERS = CONTEXT_AMD64 | 0x0010;
const CONTEXT_AMD64_FULL = CONTEXT_AMD64_CONTROL | CONTEXT_AMD64_INTEGER | CONTEXT_AMD64_FLOATING_POINT;
const CONTEXT_AMD64_ALL = CONTEXT_AMD64_CONTROL | CONTEXT_AMD64_INTEGER | CONTEXT_AMD64_SEGMENTS | CONTEXT_AMD64_FLOATING_POINT | CONTEXT_AMD64_DEBUG_REGISTERS;

alias CONTEXT_CONTROL = CONTEXT_AMD64_CONTROL;
alias CONTEXT_FULL = CONTEXT_AMD64_FULL;
alias CONTEXT_ALL = CONTEXT_AMD64_ALL;

alias Win32_CONTEXT = Win32_AMD64_CONTEXT;
alias Win32_PCONTEXT = Win32_CONTEXT*;

struct Win32_M128A @align(16)
{
	Win32_ULONGLONG low;
	Win32_LONGLONG  high;
}

struct Win32_IMAGE_DATA_DIRECTORY
{
	Win32_ULONG   virtualAddress;
	Win32_ULONG   size;
}

struct Win32_IMAGE_OPTIONAL_HEADER64
{
	Win32_WORD                 magic;
	Win32_BYTE                 majorLinkerVersion;
	Win32_BYTE                 minorLinkerVersion;
	Win32_DWORD                sizeOfCode;
	Win32_DWORD                sizeOfInitializedData;
	Win32_DWORD                sizeOfUninitializedData;
	Win32_DWORD                addressOfEntryPoint;
	Win32_DWORD                baseOfCode;
	Win32_ULONGLONG            imageBase;
	Win32_DWORD                sectionAlignment;
	Win32_DWORD                fileAlignment;
	Win32_WORD                 majorOperatingSystemVersion;
	Win32_WORD                 minorOperatingSystemVersion;
	Win32_WORD                 majorImageVersion;
	Win32_WORD                 minorImageVersion;
	Win32_WORD                 majorSubsystemVersion;
	Win32_WORD                 minorSubsystemVersion;
	Win32_DWORD                win32VersionValue;
	Win32_DWORD                sizeOfImage;
	Win32_DWORD                sizeOfHeaders;
	Win32_DWORD                checkSum;
	Win32_WORD                 subsystem;
	Win32_WORD                 dllCharacteristics;
	Win32_ULONGLONG            sizeOfStackReserve;
	Win32_ULONGLONG            sizeOfStackCommit;
	Win32_ULONGLONG            sizeOfHeapReserve;
	Win32_ULONGLONG            sizeOfHeapCommit;
	Win32_DWORD                loaderFlags;
	Win32_DWORD                numberOfRvaAndSizes;
	Win32_IMAGE_DATA_DIRECTORY[16] dataDirectory;
}

alias Win32_PIMAGE_OPTIONAL_HEADER64 = Win32_IMAGE_OPTIONAL_HEADER64*;
struct Win32_IMAGE_FILE_HEADER
{
	Win32_WORD      machine;
	Win32_WORD      numberOfSections;
	Win32_DWORD     timeDateStamp;
	Win32_DWORD     pointerToSymbolTable;
	Win32_DWORD     numberOfSymbols;
	Win32_WORD      sizeOfOptionalHeader;
	Win32_WORD      characteristics;
}

alias Win32_PIMAGE_FILE_HEADER = Win32_IMAGE_FILE_HEADER*;

struct Win32_IMAGE_NT_HEADERS
{
	Win32_DWORD signature;
	Win32_IMAGE_FILE_HEADER fileHeader;
	Win32_IMAGE_OPTIONAL_HEADER64 optionalHeader;
}

alias Win32_PIMAGE_NT_HEADERS = Win32_IMAGE_NT_HEADERS*;

struct Win32_SYMBOL_INFO
{
	Win32_ULONG         sizeOfStruct;
	Win32_ULONG         typeIndex;
	Win32_ULONG64[2]    reserved;
	Win32_ULONG         index;
	Win32_ULONG         size;
	Win32_ULONG64       modBase;
	Win32_ULONG         flags;
	Win32_ULONG64       value;
	Win32_ULONG64       address;
	Win32_ULONG         register;
	Win32_ULONG         scope;
	Win32_ULONG         tag;
	Win32_ULONG         nameLen;
	Win32_ULONG         maxNameLen;
	Win32_CHAR[1]       name;
}

alias Win32_PSYMBOL_INFO = Win32_SYMBOL_INFO*;

struct Win32_MODLOAD_DATA
{
	Win32_DWORD ssize;
	Win32_DWORD ssig;
	Win32_PVOID data;
	Win32_DWORD size;
	Win32_DWORD flags;
}

enum Win32_ADDRESS_MODE
{
	ADDR_MODE_1616,
	ADDR_MODE_1632,
	ADDR_MODE_REAL,
	ADDR_MODE_FLAT,
}

struct Win32_ADDRESS64
{
	Win32_DWORD64       offset;
	Win32_WORD          segment;
	Win32_ADDRESS_MODE  mode;
}

struct Win32_KDHELP64
{
	Win32_DWORD64       thread;
	Win32_DWORD         thCallbackStack;
	Win32_DWORD         thCallbackBStore;
	Win32_DWORD         nextCallback;
	Win32_DWORD         framePointer;
	Win32_DWORD64       kiCallUserMode;
	Win32_DWORD64       keUserCallbackDispatcher;
	Win32_DWORD64       systemRangeStart;
	Win32_DWORD64       kiUserExceptionDispatcher;
	Win32_DWORD64       stackBase;
	Win32_DWORD64       stackLimit;
	Win32_DWORD         buildVersion;
	Win32_DWORD         retpolineStubFunctionTableSize;
	Win32_DWORD64       retpolineStubFunctionTable;
	Win32_DWORD         retpolineStubOffset;
	Win32_DWORD         retpolineStubSize;
	Win32_DWORD64[2]    reserved0;
}

struct Win32_STACKFRAME64
{
	Win32_ADDRESS64     addrPC;
	Win32_ADDRESS64     addrReturn;
	Win32_ADDRESS64     addrFrame;
	Win32_ADDRESS64     addrStack;
	Win32_ADDRESS64     addrBStore;
	Win32_PVOID         funcTableEntry;
	Win32_DWORD64[4]    params;
	Win32_BOOL          far;
	Win32_BOOL          virtual;
	Win32_DWORD64[3]    reserved;
	Win32_KDHELP64      kdHelp;
}

alias Win32_PREAD_PROCESS_MEMORY_ROUTINE64 = fn Win32_BOOL(Win32_HANDLE hProcess, Win32_DWORD64 qwBaseAddress, Win32_PVOID lpBuffer, Win32_DWORD nSize, Win32_LPDWORD lpNumberOfBytesRead);
alias Win32_PFUNCTION_TABLE_ACCESS_ROUTINE64 = fn Win32_PVOID(Win32_HANDLE ahProcess, Win32_DWORD64 addrBase);
alias Win32_PGET_MODULE_BASE_ROUTINE64 = fn Win32_DWORD64(Win32_HANDLE hProcess, Win32_DWORD64 address);
alias Win32_PTRANSLATE_ADDRESS_ROUTINE64 = fn Win32_DWORD64(Win32_HANDLE hProcess, Win32_HANDLE hThread, Win32_LPADDRESS64 lpaddr);
alias Win32_PKDHELP64 = Win32_KDHELP64*;
alias Win32_LPADDRESS64 = Win32_ADDRESS64*;
alias Win32_LPSTACKFRAME64 = Win32_STACKFRAME64*;
alias Win32_PMODLOAD_DATA = Win32_MODLOAD_DATA*;
alias Win32_PIMAGEHLP_LINE64 = Win32_IMAGEHLP_LINE64*;
alias Win32_LPMODULEINFO = Win32_MODULEINFO*;

