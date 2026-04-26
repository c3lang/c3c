// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module libc;

// Constants need to be per os/arch
const int EXIT_FAILURE = 1;
const int EXIT_SUCCESS = 0;
const int RAND_MAX = 0x7fffffff;

alias WChar @if(env::WIN32) = Char16;
alias WChar @if(!env::WIN32) = Char32;

struct DivResult
{
	CInt quot;
	CInt rem;
}

struct LongDivResult
{
	CLong quot;
	CLong rem;
}

struct Fpos_t @if(!env::WIN32)
{
	long __pos;
	Mbstate_t __state;
}

struct Mbstate_t @if(!env::WIN32)
{
	int __count;
	union __value
	{
		uint __wch;
		char[4] __wcb;
	}
}

fn Errno errno()
{
	return (Errno)os::errno();
}

fn void errno_set(Errno e)
{
	os::errno_set((int)e);
}

alias TerminateFunction = fn void();
alias CompareFunction = fn int(void*, void*);
alias JmpBuf = uptr[$$JMP_BUF_SIZE];
alias Fd = CInt;
alias Fpos_t @if(env::WIN32) = long;
alias SignalFunction = fn void(CInt);

// Common signals for all targets
const CInt SIGINT = 2;
const CInt SIGILL = 4;
const CInt SIGFPE = 8;
const CInt SIGSEGV = 11;
const CInt SIGTERM = 15;

alias Time_t = $typefrom(env::WIN32 || env::EMSCRIPTEN ? long::typeid : CLong::typeid);
alias Off_t = $typefrom(env::WIN32 || env::EMSCRIPTEN ? long::typeid : sz::typeid);

module libc @if(env::LIBC);

extern fn void abort();
extern fn CInt abs(CInt n);
extern fn ZString asctime(Tm* timeptr);
extern fn ZString asctime_r(Tm* timeptr, char* buf) @if(!env::WIN32);
extern fn CInt atexit(TerminateFunction func);
extern fn double atof(char* str);
extern fn int atoi(char* str);
extern fn CLongLong atoll(char* str);
extern fn void* bsearch(void* key, void* base, usz items, usz size, CompareFunction compare);
extern fn void* calloc(usz count, usz size);
extern fn void clearerr(CFile stream);
extern fn Clock_t clock();
extern fn CInt close(CInt fd) @if(!env::WIN32);
extern fn double difftime(Time_t time1, Time_t time2) @if(!env::WIN32);
extern fn DivResult div(CInt numer, CInt denom);
extern fn void exit(CInt status);
extern fn void _exit(CInt status) @cname("_Exit");
extern fn CInt fclose(CFile stream);
extern fn CFile fdopen(CInt fd, ZString mode) @if(!env::WIN32);
extern fn CInt feof(CFile stream);
extern fn CInt ferror(CFile stream);
extern fn CInt fflush(CFile stream);
extern fn CInt fgetc(CFile stream);
extern fn ZString fgets(char* string, CInt n, CFile stream);
extern fn CInt fgetpos(CFile stream, Fpos_t* pos) @if(!env::NETBSD);
extern fn Fd fileno(CFile stream) @if(!env::WIN32);
extern fn CFile fopen(ZString filename, ZString mode);
extern fn CInt fprintf(CFile stream, ZString format, ...);
extern fn CInt fputc(CInt c, CFile stream);
extern fn CInt fputs(ZString string, CFile stream);
extern fn usz fread(void* ptr, usz size, usz nmemb, CFile stream);
extern fn void free(void*);
extern fn CFile freopen(ZString filename, ZString mode, CFile stream);
extern fn CInt fscanf(CFile stream, ZString format, ...);
extern fn CInt fseek(CFile stream, SeekIndex offset, CInt whence) @if(!env::WIN32);
extern fn CInt fsetpos(CFile stream, Fpos_t* pos) @if(!env::NETBSD);
extern fn SeekIndex ftell(CFile stream) @if(!env::WIN32);
extern fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream);
extern fn CInt getc(CFile stream);
extern fn CInt getchar();
extern fn ZString getenv(ZString name);
extern fn ZString gets(char* buffer);
extern fn Tm* gmtime(Time_t* timer);
extern fn Tm* gmtime_r(Time_t* timer, Tm* buf) @if(!env::WIN32 && !env::NETBSD);
extern fn CInt ioctl(CInt fd, ulong request, ...);
extern fn CInt isatty(Fd fd) @if(!env::WIN32);
extern fn CLong labs(CLong x);
extern fn LongDivResult ldiv(CLong number, CLong denom);
extern fn Tm* localtime(Time_t* timer);
extern fn Tm* localtime_r(Time_t* timer, Tm* result) @if(!env::WIN32 && !env::NETBSD);
extern fn void longjmp(JmpBuf* buffer, CInt value) @if(!env::NETBSD && !env::OPENBSD && !env::EMSCRIPTEN);
extern fn void* malloc(usz size);
extern fn void* memchr(void* str, CInt c, usz n);
extern fn CInt memcmp(void* buf1, void* buf2, usz count);
extern fn void* memcpy(void* dest, void* src, usz n);
extern fn void* memmove(void* dest, void* src, usz n);
extern fn void* memset(void* dest, CInt value, usz n);
extern fn ZString mktemp(char* template) @cname(env::WIN32 ??? "_mktemp" : "mktemp");
extern fn Time_t* mktime(Tm* time) @if(!env::WIN32);
extern fn void perror(ZString string);
extern fn CInt printf(ZString format, ...);
extern fn CInt putc(CInt c, CFile stream);
extern fn CInt putchar(CInt c);
extern fn CInt puts(ZString str);
extern fn void qsort(void* base, usz items, usz size, CompareFunction compare);
extern fn CInt raise(CInt signal);
extern fn CInt rand();
extern fn sz read(Fd fd, void* buf, usz nbyte) @if(!env::WIN32);
extern fn sz readlink(ZString pathname, char* buf, int bufsize) @if(!env::WIN32);
extern fn void* realloc(void* ptr, usz size);
extern fn CInt remove(ZString filename);
extern fn CInt rename(ZString old_name, ZString new_name);
extern fn void rewind(CFile stream);
extern fn CInt scanf(ZString format, ...);
extern fn void setbuf(CFile stream, char* buffer);
extern fn int setenv(ZString name, ZString value, CInt overwrite);
extern fn CInt setjmp(JmpBuf* buffer) @if(!env::WIN32 && !env::NETBSD && !env::OPENBSD && !env::EMSCRIPTEN);
extern fn void setvbuf(CFile stream, char* buf, CInt type, usz size);
extern fn SignalFunction signal(CInt sig, SignalFunction function);
extern fn CInt snprintf(char* buffer, usz size, ZString format, ...);
extern fn CInt sprintf(char* buffer, ZString format, ...);
extern fn void srand(uint seed);
extern fn CInt sscanf(char* buffer, ZString format, ...);
extern fn ZString strcat(ZString dest, ZString src);
extern fn char* strchr(char* str, CInt c);
extern fn CInt strcmp(ZString str1, ZString str2);
extern fn CInt strcoll(ZString str1, ZString str2);
extern fn usz strcspn(ZString str1, ZString str2);
extern fn ZString strcpy(ZString dst, ZString src);
extern fn ZString strdup(ZString s);
extern fn ZString strerror(CInt errn);
extern fn usz strftime(char* dest, usz maxsize, ZString format, Tm* timeptr);
extern fn usz strlen(ZString str);
extern fn ZString strncat(char* dest, char* src, usz n);
extern fn CInt strncmp(char* str1, char* str2, usz n);
extern fn char* strncpy(char* dst, char* src, usz n);
extern fn CULong stroul(char* str, char** endptr, int base);
extern fn char* strpbrk(ZString str1, ZString str2);
extern fn usz strspn(ZString str1, ZString str2);
extern fn ZString strptime(char* buf, ZString format, Tm* tm);
extern fn char* strrchr(ZString str, CInt c);
extern fn char* strstr(ZString haystack, ZString needle);
extern fn double strtod(char* str, char** endptr);
extern fn float strtof(char* str, char** endptr);
extern fn ZString strtok(ZString str, ZString delim);
extern fn CLong strtol(char* str, char** endptr, CInt base);
extern fn CULong strtoul(char* str, char** endptr, CInt base);
extern fn usz strxfrm(char* dest, ZString src, usz n);
extern fn CInt system(ZString str);
extern fn ZString tempnam(ZString dir, ZString prefix) @cname(env::WIN32 ??? "_tempnam" : "tempnam") @nodiscard;
extern fn Time_t timegm(Tm* timeptr) @if(!env::WIN32 && !env::NETBSD);
extern fn ZString tmpnam(char *buffer);
extern fn CFile tmpfile();
extern fn CInt ungetc(CInt c, CFile stream);
extern fn CInt unsetenv(ZString name) @if(!env::NETBSD);
extern fn sz write(Fd fd, void* buffer, usz count) @if(!env::WIN32);

extern fn CFile fmemopen(void* ptr, usz size, ZString mode);
extern fn sz getline(char** linep, usz* linecapp, CFile stream);
extern fn CInt timespec_get(TimeSpec* ts, CInt base);
extern fn CInt nanosleep(TimeSpec* req, TimeSpec* remaining) @if(!env::NETBSD);
extern fn ZString ctime(Time_t* timer);
extern fn Time_t time(Time_t* timer);

const CInt STDIN_FD = 0;
const CInt STDOUT_FD = 1;
const CInt STDERR_FD = 2;

module libc @if(env::LINUX || env::ANDROID || env::EMSCRIPTEN);
extern CFile __stdin @cname("stdin");
extern CFile __stdout @cname("stdout");
extern CFile __stderr @cname("stderr");
extern fn usz malloc_usable_size(void* ptr);
macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);
extern fn void* aligned_alloc(usz align, usz size);
macro CFile stdin() => __stdin;
macro CFile stdout() => __stdout;
macro CFile stderr() => __stderr;

// WebAssembly doesn't natively support setjmp/longjmp.
// Emscripten emulates them using JS exceptions, but this requires running
// LLVM's `wasm-emscripten-sjlj` lowering pass to instrument caller functions
// so they can actually catch the JS throw.
//
// Since c3c doesn't currently inject this pass into the LLVM pipeline,
// binding directly to __wasm_setjmp or emscripten_longjmp will crash
// with `unreachable` when the JS exception unwinds.
//
// For now, longjmp is stubbed to just exit cleanly.
// See: https://emscripten.org/docs/porting/setjmp-longjmp.html
// See: https://llvm.org/docs/ExceptionHandling.html#setjmp-longjmp-exception-handling-in-webassembly
module libc @if(env::EMSCRIPTEN);
import std::os;
fn int setjmp(JmpBuf* ptr) => 0;
fn void longjmp(JmpBuf* ptr, int i) => os::exit(i);


module libc @if(env::NETBSD || env::OPENBSD);
extern fn int fcntl(CInt socket, int cmd, ...);
extern fn int _setjmp(void*);
macro int setjmp(void* ptr) => _setjmp(ptr);
extern fn void _longjmp(void*, int);
macro void longjmp(void* ptr, CInt i) => _longjmp(ptr, i);
extern fn usz malloc_size(void* ptr);
extern fn void* aligned_alloc(usz align, usz size);
extern fn Tm* gmtime_r(Time_t* timer, Tm* buf) @cname("__gmtime_r50") @if(env::NETBSD);
extern fn Tm* localtime_r(Time_t* timer, Tm* result) @cname("__localtime_r50") @if(env::NETBSD);
extern fn CInt nanosleep(TimeSpec* req, TimeSpec* remaining) @cname("__nanosleep50") @if(env::NETBSD);
extern fn CInt fgetpos(CFile stream, Fpos_t* pos) @cname("__fgetpos50") @if(env::NETBSD);
extern fn CInt fsetpos(CFile stream, Fpos_t* pos) @cname("__fsetpos50") @if(env::NETBSD);
extern fn Time_t timegm(Tm* timeptr) @cname("__timegm50") @if(env::NETBSD);
extern fn CInt unsetenv(ZString name) @cname("__unsetenv13") @if(env::NETBSD);
macro CFile stdin() { return fdopen(0, "r"); }
macro CFile stdout() { return fdopen(1, "w"); }
macro CFile stderr() { return fdopen(2, "w"); }

module libc @if(env::DARWIN || env::FREEBSD);
extern CFile __stdinp;
extern CFile __stdoutp;
extern CFile __stderrp;
extern fn usz malloc_size(void* ptr) @if(!env::FREEBSD);
extern fn void* aligned_alloc(usz align, usz size);
macro CFile stdin() => __stdinp;
macro CFile stdout() => __stdoutp;
macro CFile stderr() => __stderrp;

module libc @if(env::FREEBSD);
extern fn usz malloc_usable_size(void* ptr);
macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);

module libc @if(env::WIN32);
macro usz malloc_size(void* ptr) => _msize(ptr);
macro CFile stdin() => __acrt_iob_func(STDIN_FD);
macro CFile stdout() => __acrt_iob_func(STDOUT_FD);
macro CFile stderr() => __acrt_iob_func(STDERR_FD);

module libc @if(env::LIBC && !env::WIN32 && !env::LINUX && !env::ANDROID && !env::DARWIN && !env::BSD_FAMILY && !env::EMSCRIPTEN);
macro CFile stdin() { return (CFile*)(uptr)STDIN_FD; }
macro CFile stdout() { return (CFile*)(uptr)STDOUT_FD; }
macro CFile stderr() { return (CFile*)(uptr)STDERR_FD; }

module libc @if(env::NO_LIBC);
import std::core::mem;


fn void longjmp(JmpBuf* buffer, CInt value) @weak @cname("longjmp") @nostrip
{
	unreachable("longjmp unavailable");
}

fn CInt setjmp(JmpBuf* buffer) @weak @cname("setjmp") @nostrip
{
	unreachable("setjmp unavailable");
}


fn void* malloc(usz size) @weak @cname("malloc") @nostrip
{
	unreachable("malloc unavailable");
}
fn void* calloc(usz count, usz size) @weak @cname("calloc") @nostrip
{
	unreachable("calloc unavailable");
}
fn void* free(void*) @weak @cname("free")
{
	unreachable("free unavailable");
}

fn void* realloc(void* ptr, usz size) @weak @cname("realloc") @nostrip
{
	unreachable("realloc unavailable");
}

alias memcpy = mem::__memcpy;
alias memmove = mem::__memcpy;
alias memset = mem::__memset;

fn int fseek(CFile stream, SeekIndex offset, int whence) @weak @cname("fseek") @nostrip
{
	unreachable("'fseek' not available.");
}
fn CFile fopen(ZString filename, ZString mode) @weak @cname("fopen") @nostrip
{
	unreachable("'fopen' not available.");
}

fn CFile freopen(ZString filename, ZString mode, CFile stream) @weak @cname("fopen") @nostrip
{
	unreachable("'freopen' not available.");
}

fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream) @weak @cname("fwrite") @nostrip
{
	unreachable("'fwrite' not available.");
}

fn usz fread(void* ptr, usz size, usz nmemb, CFile stream) @weak @cname("fread") @nostrip
{
	unreachable("'fread' not available.");
}

fn CFile fclose(CFile) @weak @cname("fclose") @nostrip
{
	unreachable("'fclose' not available.");
}

fn int fflush(CFile stream) @weak @cname("fflush") @nostrip
{
	unreachable("'fflush' not available.");
}

fn int fputc(int c, CFile stream) @weak @cname("fputc") @nostrip
{
	unreachable("'fputc' not available.");
}

fn char* fgets(ZString str, int n, CFile stream) @weak @cname("fgets") @nostrip
{
	unreachable("'fgets' not available.");
}

fn int fgetc(CFile stream) @weak @cname("fgetc") @nostrip
{
	unreachable("'fgetc' not available.");
}

fn int feof(CFile stream) @weak @cname("feof") @nostrip
{
	unreachable("'feof' not available.");
}

fn int putc(int c, CFile stream) @weak @cname("putc") @nostrip
{
	unreachable("'putc' not available.");
}
fn int putchar(int c) @weak @cname("putchar") @nostrip
{
	unreachable("'putchar' not available.");
}
fn int puts(ZString str) @weak @cname("puts") @nostrip
{
	unreachable("'puts' not available.");
}

module libc;

// stdio

alias CFile = void*;


const HAS_MALLOC_SIZE = env::LINUX || env::ANDROID || env::WIN32 || env::DARWIN;

// The following needs to be set per arch+os
// For now I have simply pulled the defaults from MacOS
const int SEEK_SET = 0;
const int SEEK_CUR = 1;
const int SEEK_END = 2;
const int _IOFBF = 0; // Fully buffered
const int _IOLBF = 1; // Line buffered
const int _IONBF = 2; // Unbuffered
const int BUFSIZ = 1024;
const int EOF = -1;
const int FOPEN_MAX = 20;
const int FILENAME_MAX = 1024;

macro bool libc_S_ISTYPE(value, mask) @builtin => (value & S_IFMT) == mask;
const S_IFMT   = 0o170000; // type of file mask
const S_IFIFO  = 0o010000; // named pipe (fifo)
const S_IFCHR  = 0o020000; // character special
const S_IFDIR  = 0o040000; // directory
const S_IFBLK  = 0o060000; // block special
const S_IFREG  = 0o100000; // regular
const S_IFLNK  = 0o120000; // symbolic link
const S_IFSOCK = 0o140000; // socket
const S_ISUID  = 0o004000; // Set user id on execution
const S_ISGID  = 0o002000; // Set group id on execution
const S_ISVTX  = 0o001000; // Save swapped text even after use
const S_IRUSR  = 0o000400; // Read permission, owner
const S_IWUSR  = 0o000200; // Write permission, owner
const S_IXUSR  = 0o000100; // Execute/search permission, owner

alias SeekIndex = CLong;

// vsprintf vprintf not supported

// time.h

struct Tm
{
	<* seconds after the minute [0-60] *>
	CInt	tm_sec;
	<* minutes after the hour [0-59] *>
	CInt	tm_min;
	<* hours since midnight [0-23] *>
	CInt	tm_hour;
	<* day of the month [1-31] *>
	CInt	tm_mday;
	<* months since January [0-11] *>
	CInt	tm_mon;
	<* years since 1900 *>
	CInt	tm_year;
	<* days since Sunday [0-6] *>
	CInt	tm_wday;
	<* days since January 1 [0-365] *>
	CInt	tm_yday;
	<* Daylight Savings Time flag *>
	CInt	tm_isdst;
	<* offset from UTC in seconds *>
	TimeOffset tm_gmtoff @if(!env::WIN32);
	<* timezone abbreviation *>
	char* tm_zone @if(!env::WIN32);
	CInt tm_nsec @if(env::WASI);
}

struct TimeSpec
{
	Time_t s;
	ulong ns @if(env::WIN32);
	CLong ns @if(!env::WIN32);
}


alias Clock_t @if(env::WIN32) = int;
alias Clock_t @if(!env::WIN32) = CLong;

alias TimeOffset @if(env::WASI) = int;
alias TimeOffset @if(!env::WASI) = CLong;

const int TIME_UTC = 1;


// This is a best-effort approximation, but the C standard does not enforce
// that this is a compile-time standard.
const CLOCKS_PER_SEC @if(env::WIN32) = 1000;
const CLOCKS_PER_SEC @if(!env::WIN32) = 1000000;

constdef Errno : inline CInt @if(env::DARWIN)
{
	OK                       = 0,
	EPERM                    = 1, // Operation not permitted
	ENOENT                   = 2, // No such file or directory
	ESRCH                    = 3, // No such process
	EINTR                    = 4, // Interrupted system call
	EIO                      = 5, // I/O error
	ENXIO                    = 6, // No such device or address
	E2BIG                    = 7, // Argument list too long
	ENOEXEC                  = 8, // Exec format error
	EBADF                    = 9, // Bad file number
	ECHILD                   = 10, // No child processes
	EDEADLK                  = 11, // Resource deadlock would occur
	ENOMEM                   = 12, // Out of memory
	EACCES                   = 13, // Permission denied
	EFAULT                   = 14, // Bad address
	ENOTBLK                  = 15, // Block device required, not on Win32
	EBUSY                    = 16, // Device or resource busy
	EEXIST                   = 17, // File exists
	EXDEV                    = 18, // Cross-device link
	ENODEV                   = 19, // No such device
	ENOTDIR                  = 20, // Not a directory
	EISDIR                   = 21, // Is a directory
	EINVAL                   = 22, // Invalid argument
	ENFILE                   = 23, // File table overflow
	EMFILE                   = 24, // Too many open files
	ENOTTY                   = 25, // Not a typewriter
	ETXTBSY                  = 26, // Text file busy, not on Win32
	EFBIG                    = 27, // File too large
	ENOSPC                   = 28, // No space left on device
	ESPIPE                   = 29, // Illegal seek
	EROFS                    = 30, // Read-only file system
	EMLINK                   = 31, // Too many links
	EPIPE                    = 32, // Broken pipe
	EDOM                     = 33, // Math argument out of domain of func
	ERANGE                   = 34, // Math result not representable
	EAGAIN                   = 35, // Try again
	EWOULDBLOCK              = EAGAIN, // Operation would block
	EINPROGRESS              = 36, // Operation now in progress
	EALREADY                 = 37, // Operation already in progress
	ENOTSOCK                 = 38, // Socket operation on non-socket
	EDESTADDRREQ             = 39, // Destination address required
	EMSGSIZE                 = 40, // Message too long
	EPROTOTYPE               = 41, // Protocol wrong type for socket
	ENOPROTOOPT              = 42, // Protocol not available
	EPROTONOSUPPORT          = 43, // Protocol not supported
	ESOCKTNOSUPPORT          = 44, // Socket type not supported
	ENOTSUP                  = 45, // Not supported
	EPFNOSUPPORT             = 46, // Protocol family not supported
	EAFNOSUPPORT             = 47, // Address family not supported by protocol family
	EADDRINUSE               = 48, // Address already in use
	EADDRNOTAVAIL            = 49, // Cannot assign requested address
	ENETDOWN                 = 50, // Network is down
	ENETUNREACH              = 51, // Network is unreachable
	ENETRESET                = 52, // Network dropped connection on reset
	ECONNABORTED             = 53, // Software caused connection abort
	ECONNRESET               = 54, // Connection reset by peer
	ENOBUFS                  = 55, // No buffer space available
	EISCONN                  = 56, // Socket is already connected
	ENOTCONN                 = 57, // Socket is not connected
	ESHUTDOWN                = 58, // Cannot send after socket shutdown
	ETIMEDOUT                = 60, // Operation timed out
	ECONNREFUSED             = 61, // Connection refused
	ELOOP                    = 62, // Too many levels of symbolic links
	ENAMETOOLONG             = 63, // File name too long
	EHOSTDOWN                = 64, // Host is down
	EHOSTUNREACH             = 65, // No route to host
	ENOTEMPTY                = 66, // Directory not empty
	EPROCLIM                 = 67, // Too many processes
	EUSERS                   = 68, // Too many users
	EDQUOT                   = 69, // Disc quota exceeded
	ESTALE                   = 70, // Stale NFS file handle
	EBADRPC                  = 72, // RPC struct is bad
	ERPCMISMATCH             = 73, // RPC version wrong
	EPROGUNAVAIL             = 74, // RPC prog. not avail
	EPROGMISMATCH            = 75, // Program version wrong
	EPROCUNAVAIL             = 76, // Bad procedure for program
	ENOLCK                   = 77, // No locks available
	ENOSYS                   = 78, // Function not implemented
	EFTYPE                   = 79, // Inappropriate file type or format
	EAUTH                    = 80, // Authentication error
	ENEEDAUTH                = 81, // Need authenticator
	EPWROFF                  = 82, // Device power is off
	EDEVERR                  = 83, // Device error
	EOVERFLOW                = 84, // Value too large to be stored in data type
	EBADEXEC                 = 85, // Bad executable (or shared library)
	EBADARCH                 = 86, // Bad CPU type in executable
	ESHLIBVERS               = 87, // Shared library version mismatch
	EBADMACHO                = 88, // Malformed Mach-o file
	ECANCELED                = 89, // Operation canceled
	EIDRM                    = 90, // Identifier removed
	ENOMSG                   = 91, // No message of desired type
	EILSEQ                   = 92, // Illegal byte sequence
	ENOATTR                  = 93, // Attribute not found
	EBADMSG                  = 94, // Bad message
	EMULTIHOP                = 95, // Reserved
	ENODATA                  = 96, // No message available
	ENOLINK                  = 97, // Reserved
	ENOSR                    = 98, // No STREAM resources
	ENOSTR                   = 99, // Not a STREAM
	EPROTO                   = 100, // Protocol error
	ETIME                    = 101, // STREAM ioctl() timeout
	EOPNOTSUPP               = 102, // Operation not supported on socket
}

constdef Errno : inline CInt @if(env::NETBSD)
{
	OK                       = 0,
	EPERM                    = 1, // Operation not permitted
	ENOENT                   = 2, // No such file or directory
	ESRCH                    = 3, // No such process
	EINTR                    = 4, // Interrupted system call
	EIO                      = 5, // I/O error
	ENXIO                    = 6, // No such device or address
	E2BIG                    = 7, // Argument list too long
	ENOEXEC                  = 8, // Exec format error
	EBADF                    = 9, // Bad file number
	ECHILD                   = 10, // No child processes
	EDEADLK                  = 11, // Resource deadlock avoided
	ENOMEM                   = 12, // Out of memory
	EACCES                   = 13, // Permission denied
	EFAULT                   = 14, // Bad address
	ENOTBLK                  = 15, // Block device required, not on Win32
	EBUSY                    = 16, // Device or resource busy
	EEXIST                   = 17, // File exists
	EXDEV                    = 18, // Cross-device link
	ENODEV                   = 19, // No such device
	ENOTDIR                  = 20, // Not a directory
	EISDIR                   = 21, // Is a directory
	EINVAL                   = 22, // Invalid argument
	ENFILE                   = 23, // File table overflow
	EMFILE                   = 24, // Too many open files
	ENOTTY                   = 25, // Not a typewriter
	ETXTBSY                  = 26, // Text file busy, not on Win32
	EFBIG                    = 27, // File too large
	ENOSPC                   = 28, // No space left on device
	ESPIPE                   = 29, // Illegal seek
	EROFS                    = 30, // Read-only file system
	EMLINK                   = 31, // Too many links
	EPIPE                    = 32, // Broken pipe
	EDOM                     = 33, // Math argument out of domain of func
	ERANGE                   = 34, // Math result not representable
	EAGAIN                   = 35, // Try again
	EWOULDBLOCK              = EAGAIN,    // Operation would block
	EINPROGRESS              = 36, // Operation now in progress
	EALREADY                 = 37, // Operation already in progress
	ENOTSOCK                 = 38, // Socket operation on non-socket
	EDESTADDRREQ             = 39, // Destination address required
	EMSGSIZE                 = 40, // Message too long
	EPROTOTYPE               = 41, // Protocol wrong type for socket
	ENOPROTOOPT              = 42, // Protocol option not available
	EPROTONOSUPPORT          = 43, // Protocol not supported
	ESOCKTNOSUPPORT          = 44, // Socket type not supported
	EOPNOTSUPP               = 45, // Operation not supported
	EPFNOSUPPORT             = 46, // Protocol family not supported
	EAFNOSUPPORT             = 47, // Address family not supported by protocol family
	EADDRINUSE               = 48, // Address already in use
	EADDRNOTAVAIL            = 49, // Can't assign requested address
	ENETDOWN                 = 50, // Network is down
	ENETUNREACH              = 51, // Network is unreachable
	ENETRESET                = 52, // Network dropped connection on reset
	ECONNABORTED             = 53, // Software caused connection abort
	ECONNRESET               = 54, // Connection reset by peer
	ENOBUFS                  = 55, // No buffer space available
	EISCONN                  = 56, // Socket is already connected
	ENOTCONN                 = 57, // Socket is not connected
	ESHUTDOWN                = 58, // Can't send after socket shutdown
	ETOOMANYREFS             = 59, // Too many references: can't splice
	ETIMEDOUT                = 60, // Operation timed out
	ECONNREFUSED             = 61, // Connection refused
	ELOOP                    = 62, // Too many levels of symbolic links
	ENAMETOOLONG             = 63, // File name too long
	EHOSTDOWN                = 64, // Host is down
	EHOSTUNREACH             = 65, // No route to host
	ENOTEMPTY                = 66, // Directory not empty
	EPROCLIM                 = 67, // Too many processes
	EUSERS                   = 68, // Too many users
	EDQUOT                   = 69, // Disc quota exceeded
	ESTALE                   = 70, // Stale NFS file handle
	EREMOTE                  = 71, // Too many levels of remote in path
	EBADRPC                  = 72, // RPC struct is bad
	ERPCMISMATCH             = 73, // RPC version wrong
	EPROGUNAVAIL             = 74, // RPC prog. not avail
	EPROGMISMATCH            = 75, // Program version wrong
	EPROCUNAVAIL             = 76, // Bad procedure for program
	ENOLCK                   = 77, // No locks available
	ENOSYS                   = 78, // Function not implemented
	EFTYPE                   = 79, // Inappropriate file type or format
	EAUTH                    = 80, // Authentication error
	ENEEDAUTH                = 81, // Need authenticator
	EIDRM                    = 82, // Identifier removed
	ENOMSG                   = 83, // No message of desired type
	EOVERFLOW                = 84, // Value too large to be stored in data type
	EILSEQ                   = 85, // Illegal byte sequence
	ENOTSUP                  = 86, // Not supported
	ECANCELED                = 87, // Operation canceled
	EBADMSG                  = 88, // Bad or Corrupt message
	ENODATA                  = 89, // No message available
	ENOSR                    = 90, // No STREAM resources
	ENOSTR                   = 91, // Not a STREAM
	ETIME                    = 92, // STREAM ioctl timeout
	ENOATTR                  = 93, // Attribute not found
	EMULTIHOP                = 94, // Multihop attempted
	ENOLINK                  = 95, // Link has been severed
	EPROTO                   = 96, // Protocol error
	EOWNERDEAD               = 97, // Previous owner died
	ENOTRECOVERABLE          = 98, // State not recoverable

}


constdef Errno : inline CInt @if(env::WIN32)
{
	OK                       = 0,
	EPERM                    = 1, // Operation not permitted
	ENOENT                   = 2, // No such file or directory
	ESRCH                    = 3, // No such process
	EINTR                    = 4, // Interrupted system call
	EIO                      = 5, // I/O error
	ENXIO                    = 6, // No such device or address
	E2BIG                    = 7, // Argument list too long
	ENOEXEC                  = 8, // Exec format error
	EBADF                    = 9, // Bad file number
	ECHILD                   = 10, // No child processes
	ENOMEM                   = 12, // Out of memory
	EAGAIN                   = 11, // Try again
	EACCES                   = 13, // Permission denied
	EFAULT                   = 14, // Bad address
	ENOTBLK                  = 15, // Block device required, not on Win32
	EBUSY                    = 16, // Device or resource busy
	EEXIST                   = 17, // File exists
	EXDEV                    = 18, // Cross-device link
	ENODEV                   = 19, // No such device
	ENOTDIR                  = 20, // Not a directory
	EISDIR                   = 21, // Is a directory
	EINVAL                   = 22, // Invalid argument
	ENFILE                   = 23, // File table overflow
	EMFILE                   = 24, // Too many open files
	ENOTTY                   = 25, // Not a typewriter
	ETXTBSY                  = 26, // Text file busy, not on Win32
	EFBIG                    = 27, // File too large
	ENOSPC                   = 28, // No space left on device
	ESPIPE                   = 29, // Illegal seek
	EROFS                    = 30, // Read-only file system
	EMLINK                   = 31, // Too many links
	EPIPE                    = 32, // Broken pipe
	EDOM                     = 33, // Math argument out of domain of func
	ERANGE                   = 34, // Math result not representable
	EDEADLK                  = 36, // Resource deadlock would occur Win32
	ENAMETOOLONG             = 38, // File name too long Win32
	ENOTEMPTY                = 41, // Directory not empty
	ELOOP                    = 114, // Too many symbolic links encountered
	EOVERFLOW                = 132, // Value too large for defined data type
	ENETDOWN                 = 116, // Network is down
	ECONNRESET               = 108, // Connection reset by peer
	ENETUNREACH              = 118, // Network is unreachable
	ENETRESET                = 117, // Network dropped connection because of reset
	EOPNOTSUPP               = 130, // Operation not supported on transport endpoint
	ETIMEDOUT                = 138, // Connection timed out
	EALREADY                 = 103, // Operation already in progress
	EINPROGRESS              = 112, // Operation now in progress Win32
	EDQUOT                   = -122, // Quota exceeded, not in Win32
	EWOULDBLOCK              = 140, // Operation would block
}


constdef Errno : inline CInt @if(env::EMSCRIPTEN)
{
	OK                       = 0, // No error occurred. System call completed successfully.
	E2BIG                    = 1, // Argument list too long.
	EACCES                   = 2, // Permission denied.
	EADDRINUSE               = 3, // Address in use.
	EADDRNOTAVAIL            = 4, // Address not available.
	EAFNOSUPPORT             = 5, // Address family not supported.
	EAGAIN                   = 6, // Resource unavailable, or operation would block.
	EWOULDBLOCK              = 6, // Operation would block
	EALREADY                 = 7, // Connection already in progress.
	EBADF                    = 8, // Bad file descriptor.
	EBADMSG                  = 9, // Bad message.
	EBUSY                    = 10, // Device or resource busy.
	ECANCELED                = 11, // Operation canceled.
	ECHILD                   = 12, // No child processes.
	ECONNABORTED             = 13, // Connection aborted.
	ECONNREFUSED             = 14, // Connection refused.
	ECONNRESET               = 15, // Connection reset.
	EDEADLK                  = 16, // Resource deadlock would occur.
	EDEADLOCK                = 16, // Resource deadlock would occur
	EDESTADDRREQ             = 17, // Destination address required.
	EDOM                     = 18, // Mathematics argument out of domain of function.
	EDQUOT                   = 19, // Reserved.
	EEXIST                   = 20, // File exists.
	EFAULT                   = 21, // Bad address.
	EFBIG                    = 22, // File too large.
	EHOSTUNREACH             = 23, // Host is unreachable.
	EIDRM                    = 24, // Identifier removed.
	EILSEQ                   = 25, // Illegal byte sequence.
	EINPROGRESS              = 26, // Operation in progress.
	EINTR                    = 27, // Interrupted function.
	EINVAL                   = 28, // Invalid argument.
	EIO                      = 29, // I/O error.
	EISCONN                  = 30, // Socket is connected.
	EISDIR                   = 31, // Is a directory.
	ELOOP                    = 32, // Too many levels of symbolic links.
	EMFILE                   = 33, // File descriptor value too large.
	EMLINK                   = 34, // Too many links.
	EMSGSIZE                 = 35, // Message too long.
	EMULTIHOP                = 36, // Reserved.
	ENAMETOOLONG             = 37, // Filename too long.
	ENETDOWN                 = 38, // Network is down.
	ENETRESET                = 39, // Connection aborted by network.
	ENETUNREACH              = 40, // Network unreachable.
	ENFILE                   = 41, // Too many files open in system.
	ENOBUFS                  = 42, // No buffer space available.
	ENODEV                   = 43, // No such device.
	ENOENT                   = 44, // No such file or directory.
	ENOEXEC                  = 45, // Executable file format error.
	ENOLCK                   = 46, // No locks available.
	ENOLINK                  = 47, // Reserved.
	ENOMEM                   = 48, // Not enough space.
	ENOMSG                   = 49, // No message of the desired type.
	ENOPROTOOPT              = 50, // Protocol not available.
	ENOSPC                   = 51, // No space left on device.
	ENOSYS                   = 52, // Function not supported.
	ENOTCONN                 = 53, // The socket is not connected.
	ENOTDIR                  = 54, // Not a directory or a symbolic link to a directory.
	ENOTEMPTY                = 55, // Directory not empty.
	ENOTRECOVERABLE          = 56, // State not recoverable.
	ENOTSOCK                 = 57, // Not a socket.
	ENOTTY                   = 59, // Inappropriate I/O control operation.
	ENXIO                    = 60, // No such device or address.
	EOVERFLOW                = 61, // Value too large to be stored in data type.
	EOWNERDEAD               = 62, // Previous owner died.
	EPERM                    = 63, // Operation not permitted.
	EPIPE                    = 64, // Broken pipe.
	EPROTO                   = 65, // Protocol error.
	EPROTONOSUPPORT          = 66, // Protocol not supported.
	EPROTOTYPE               = 67, // Protocol wrong type for socket.
	ERANGE                   = 68, // Result too large.
	EROFS                    = 69, // Read-only file system.
	ESPIPE                   = 70, // Invalid seek.
	ESRCH                    = 71, // No such process.
	ESTALE                   = 72, // Reserved.
	ETIMEDOUT                = 73, // Connection timed out.
	ETXTBSY                  = 74, // Text file busy.
	EXDEV                    = 75, // Cross-device link.
	ENOTCAPABLE              = 76, // Extension: Capabilities insufficient.
	ENOSTR                   = 100, // Not a STREAM
	EBFONT                   = 101, // Format of font file is unknown
	EBADSLT                  = 102, // Invalid slot
	EBADRQC                  = 103, // Invalid request code
	ENOANO                   = 104, // No anode
	ENOTBLK                  = 105, // Block device required
	ECHRNG                   = 106, // Channel number out of range
	EL3HLT                   = 107, // Level 3 halted
	EL3RST                   = 108, // Level 3 reset
	ELNRNG                   = 109, // Link number out of range
	EUNATCH                  = 110, // Protocol driver not attached
	ENOCSI                   = 111, // No CSI structure available
	EL2HLT                   = 112, // Level 2 halted
	EBADE                    = 113, // Invalid exchange
	EBADR                    = 114, // Invalid request descriptor
	EXFULL                   = 115, // Exchange full
	ENODATA                  = 116, // No data available
	ETIME                    = 117, // Timer expired
	ENOSR                    = 118, // Out of streams resources
	ENONET                   = 119, // Machine is not on the network
	ENOPKG                   = 120, // Package not installed
	EREMOTE                  = 121, // Object is remote
	EADV                     = 122, // Advertise error
	ESRMNT                   = 123, // Srmount error
	ECOMM                    = 124, // Communication error on send
	EDOTDOT                  = 125, // RFS specific error
	ENOTUNIQ                 = 126, // Name not unique on network
	EBADFD                   = 127, // File descriptor in bad state
	EREMCHG                  = 128, // Remote address changed
	ELIBACC                  = 129, // Can not access a needed shared library
	ELIBBAD                  = 130, // Accessing a corrupted shared library
	ELIBSCN                  = 131, // .lib section in a.out corrupted
	ELIBMAX                  = 132, // Attempting to link in too many shared libraries
	ELIBEXEC                 = 133, // Cannot exec a shared library directly
	ERESTART                 = 134, // Interrupted system call should be restarted
	ESTRPIPE                 = 135, // Streams pipe error
	EUSERS                   = 136, // Too many users
	ESOCKTNOSUPPORT          = 137, // Socket type not supported
	ENOTSUP                  = 138, // Not supported.
	EOPNOTSUPP               = ENOTSUP, // Operation not supported on socket.
	EPFNOSUPPORT             = 139, // Protocol family not supported
	ESHUTDOWN                = 140, // Cannot send after transport endpoint shutdown
	ETOOMANYREFS             = 141, // Too many references: cannot splice
	EHOSTDOWN                = 142, // Host is down
	EUCLEAN                  = 143, // Structure needs cleaning
	ENOTNAM                  = 144, // Not a XENIX named type file
	ENAVAIL                  = 145, // No XENIX semaphores available
	EISNAM                   = 146, // Is a named type file
	EREMOTEIO                = 147, // Remote I/O error
	ENOMEDIUM                = 148, // No medium found
	EMEDIUMTYPE              = 149, // Wrong medium type
	ENOKEY                   = 150, // Required key not available
	EKEYEXPIRED              = 151, // Key has expired
	EKEYREVOKED              = 152, // Key has been revoked
	EKEYREJECTED             = 153, // Key was rejected by service
	ERFKILL                  = 154, // Operation not possible due to RF-kill
	EHWPOISON                = 155, // Memory page has hardware error
	EL2NSYNC                 = 156, // Level 2 not synchronized
}


constdef Errno : inline CInt @if(!env::WIN32 && !env::DARWIN && !env::NETBSD && !env::EMSCRIPTEN)
{
	OK                       = 0,
	EPERM                    = 1, // Operation not permitted
	ENOENT                   = 2, // No such file or directory
	ESRCH                    = 3, // No such process
	EINTR                    = 4, // Interrupted system call
	EIO                      = 5, // I/O error
	ENXIO                    = 6, // No such device or address
	E2BIG                    = 7, // Argument list too long
	ENOEXEC                  = 8, // Exec format error
	EBADF                    = 9, // Bad file number
	ECHILD                   = 10, // No child processes
	ENOMEM                   = 12, // Out of memory
	EAGAIN                   = 11, // Try again
	EACCES                   = 13, // Permission denied
	EFAULT                   = 14, // Bad address
	ENOTBLK                  = 15, // Block device required, not on Win32
	EBUSY                    = 16, // Device or resource busy
	EEXIST                   = 17, // File exists
	EXDEV                    = 18, // Cross-device link
	ENODEV                   = 19, // No such device
	ENOTDIR                  = 20, // Not a directory
	EISDIR                   = 21, // Is a directory
	EINVAL                   = 22, // Invalid argument
	ENFILE                   = 23, // File table overflow
	EMFILE                   = 24, // Too many open files
	ENOTTY                   = 25, // Not a typewriter
	ETXTBSY                  = 26, // Text file busy, not on Win32
	EFBIG                    = 27, // File too large
	ENOSPC                   = 28, // No space left on device
	ESPIPE                   = 29, // Illegal seek
	EROFS                    = 30, // Read-only file system
	EMLINK                   = 31, // Too many links
	EPIPE                    = 32, // Broken pipe
	EDOM                     = 33, // Math argument out of domain of func
	ERANGE                   = 34, // Math result not representable
	EDEADLK                  = 35, // Resource deadlock would occur Linux (others?)
	ENAMETOOLONG             = 36, // File name too long Linux (others?)
	ENOTEMPTY                = 39, // Directory not empty
	ELOOP                    = 40, // Too many symbolic links encountered
	EWOULDBLOCK              = EAGAIN, // Operation would block
	EOVERFLOW                = 75, // Value too large for defined data type
	ENOTSOCK                 = 88, // Socket operation on non-socket
	EOPNOTSUPP               = 95, // Operation not supported on transport endpoint
	EADDRINUSE               = 98, // Address already in use
	EADDRNOTAVAIL            = 99, // Cannot assign requested address
	ENETDOWN                 = 100, // Network is down
	ENETUNREACH              = 101, // Network is unreachable
	ENETRESET                = 102, // Network dropped connection because of reset
	ECONNRESET               = 104, // Connection reset by peer
	EISCONN                  = 106, // Socket is already connected
	ETIMEDOUT                = 110, // Connection timed out
	ECONNREFUSED             = 111, // Connection refused
	EALREADY                 = 114, // Operation already in progress
	EINPROGRESS              = 115, // Operation now in progress
	EDQUOT                   = 122, // Quota exceeded
}
