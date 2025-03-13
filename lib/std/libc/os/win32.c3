module libc @if(env::WIN32);
import std::os::win32;

alias fdopen = _fdopen;
alias close = _close;
alias fileno = _fileno;
alias isatty = _isatty;
alias difftime = _difftime64;
alias fseek = _fseeki64;
alias ftell = _ftelli64;
alias timegm = _mkgmtime64;
alias mktime = _mktime64;

extern fn CFile __acrt_iob_func(CInt c);
extern fn CInt _close(Fd fd);
extern fn double _difftime64(Time_t time1, Time_t time2);
extern fn CFile _fdopen(Fd fd, ZString mode);
extern fn CInt _fileno(CFile stream);

extern fn CInt _fseeki64(CFile, long, int);
extern fn CLong _ftelli64(CFile);
extern fn Errno _get_timezone(CLong *timezone);
extern fn Tm* _gmtime64_s(Tm* buf, Time_t *timer);
extern fn CInt _isatty(Fd fd);
extern fn Tm* _localtime64_s(Tm* buf, Time_t *timer);
extern fn Time_t _mkgmtime64(Tm* timeptr);
extern fn Time_t _mktime64(Tm *timeptr);
extern fn usz _msize(void* ptr);
extern fn CInt _read(Fd fd, void* buffer, CUInt buffer_size);
extern fn CInt _setjmp(JmpBuf* buffer, void* frameptr);
extern fn CFile _wfopen(WString, WString);
extern fn CFile _wfreopen(WString, WString, CFile);
extern fn CInt _write(Fd fd, void* buffer, CUInt count);
extern fn CInt _wremove(WString);
extern fn int recv(Win32_SOCKET s, void* buf, int len, int flags);
extern fn int send(Win32_SOCKET s, void* buf, int len, int flags);

const CInt SD_RECEIVE = 0;
const CInt SD_SEND = 1;
const CInt SD_BOTH = 2;
extern fn CInt shutdown(Win32_SOCKET s, CInt how);

struct SystemInfo
{
	union
	{
		uint dwOemId;
		struct
		{
			ushort wProcessorArchitecture;
			ushort wReserved;
		}
	}
	uint     dwPageSize;
	void*    lpMinimumApplicationAddress;
	void*    lpMaximumApplicationAddress;
	usz      dwActiveProcessorMask;
	uint     dwNumberOfProcessors;
	uint     dwProcessorType;
	uint     dwAllocationGranularity;
	ushort   wProcessorLevel;
	ushort   wProcessorRevision;
}

extern fn CInt get_system_info(SystemInfo*) @extern("GetSystemInfo");

// Aliases to simplify libc use
macro Tm* localtime_r(Time_t* timer, Tm* buf) => _localtime64_s(buf, timer);
macro CInt setjmp(JmpBuf* buffer) => _setjmp(buffer, null);
macro Tm* gmtime_r(Time_t* timer, Tm* buf) => _gmtime64_s(buf, timer);
macro isz read(Fd fd, void* buffer, usz buffer_size) => _read(fd, buffer, (CUInt)buffer_size);
macro isz write(Fd fd, void* buffer, usz count) => _write(fd, buffer, (CUInt)count);
