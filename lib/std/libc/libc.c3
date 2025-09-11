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

typedef Errno = inline CInt;
alias TerminateFunction = fn void();
alias CompareFunction = fn int(void*, void*);
alias JmpBuf = uptr[$$JMP_BUF_SIZE];
alias Fd = CInt;
alias Fpos_t @if(env::WIN32) = long;
alias SignalFunction = fn void(CInt);

const CInt SIGHUP = 1;
const CInt SIGINT = 2;
const CInt SIGQUIT = 3;
const CInt SIGILL = 4;
const CInt SIGTRAP = 5;
const CInt SIGABRT = 6;
const CInt SIGBUS = BSD_FLAVOR_SIG ? 10 : 7; // Or Mips
const CInt SIGFPE = 8;
const CInt SIGKILL = 9;
const CInt SIGSEGV = 11;
const CInt SIGSYS = BSD_FLAVOR_SIG ? 12 : 31;
const CInt SIGPIPE = 13;
const CInt SIGALRM = 14;
const CInt SIGTERM = 15;
const CInt SIGURG = BSD_FLAVOR_SIG ? 16 : 23;
const CInt SIGSTOP = BSD_FLAVOR_SIG ? 17 : 19;
const CInt SIGTSTP = BSD_FLAVOR_SIG ? 18 : 20;
const CInt SIGCONT = BSD_FLAVOR_SIG ? 19 : 18;
const CInt SIGCHLD = BSD_FLAVOR_SIG ? 20 : 17;

const bool BSD_FLAVOR_SIG @local = env::DARWIN || env::BSD_FAMILY;

alias Time_t = $typefrom(env::WIN32 ? long.typeid : CLong.typeid);
alias Off_t = $typefrom(env::WIN32 ? int.typeid : usz.typeid);

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
extern fn void _exit(CInt status) @extern("_Exit");
extern fn CInt fclose(CFile stream);
extern fn CFile fdopen(CInt fd, ZString mode) @if(!env::WIN32);
extern fn CInt feof(CFile stream);
extern fn CInt ferror(CFile stream);
extern fn CInt fflush(CFile stream);
extern fn CInt fgetc(CFile stream);
extern fn ZString fgets(char* string, CInt n, CFile stream);
extern fn CInt fgetpos(CFile stream, Fpos_t* pos);
extern fn Fd fileno(CFile stream) @if(!env::WIN32);
extern fn CFile fopen(ZString filename, ZString mode);
extern fn CInt fprintf(CFile stream, ZString format, ...);
extern fn CInt fputc(CInt c, CFile stream);
extern fn CInt fputs(ZString string, CFile stream);
extern fn usz fread(void* ptr, usz size, usz nmemb, CFile stream);
extern fn void* free(void*);
extern fn CFile freopen(ZString filename, ZString mode, CFile stream);
extern fn CInt fscanf(CFile stream, ZString format, ...);
extern fn CInt fseek(CFile stream, SeekIndex offset, CInt whence) @if(!env::WIN32);
extern fn CInt fsetpos(CFile stream, Fpos_t* pos);
extern fn SeekIndex ftell(CFile stream) @if(!env::WIN32);
extern fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream);
extern fn CInt getc(CFile stream);
extern fn CInt getchar();
extern fn ZString getenv(ZString name);
extern fn ZString gets(char* buffer);
extern fn Tm* gmtime(Time_t* timer);
extern fn Tm* gmtime_r(Time_t* timer, Tm* buf) @if(!env::WIN32);
extern fn CInt ioctl(CInt fd, ulong request, ...);
extern fn CInt isatty(Fd fd) @if(!env::WIN32);
extern fn CLong labs(CLong x);
extern fn LongDivResult ldiv(CLong number, CLong denom);
extern fn Tm* localtime(Time_t* timer);
extern fn Tm* localtime_r(Time_t* timer, Tm* result) @if(!env::WIN32);
extern fn void longjmp(JmpBuf* buffer, CInt value) @if(!env::NETBSD && !env::OPENBSD);
extern fn void* malloc(usz size);
extern fn void* memchr(void* str, CInt c, usz n);
extern fn CInt memcmp(void* buf1, void* buf2, usz count);
extern fn void* memcpy(void* dest, void* src, usz n);
extern fn void* memmove(void* dest, void* src, usz n);
extern fn void* memset(void* dest, CInt value, usz n);
extern fn Time_t* mktime(Tm* time) @if(!env::WIN32);
extern fn void perror(ZString string);
extern fn CInt printf(ZString format, ...);
extern fn CInt putc(CInt c, CFile stream);
extern fn CInt putchar(CInt c);
extern fn CInt puts(ZString str);
extern fn void qsort(void* base, usz items, usz size, CompareFunction compare);
extern fn CInt raise(CInt signal);
extern fn CInt rand();
extern fn isz read(Fd fd, void* buf, usz nbyte) @if(!env::WIN32);
extern fn isz readlink(ZString pathname, char* buf, int bufsize) @if(!env::WIN32);
extern fn void* realloc(void* ptr, usz size);
extern fn CInt remove(ZString filename);
extern fn CInt rename(ZString old_name, ZString new_name);
extern fn void rewind(CFile stream);
extern fn CInt scanf(ZString format, ...);
extern fn void setbuf(CFile stream, char* buffer);
extern fn int setenv(ZString name, ZString value, CInt overwrite);
extern fn CInt setjmp(JmpBuf* buffer) @if(!env::WIN32 && !env::NETBSD && !env::OPENBSD);
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
extern fn Time_t timegm(Tm* timeptr) @if(!env::WIN32);
extern fn ZString tmpnam(ZString str);
extern fn CFile tmpfile();
extern fn CInt ungetc(CInt c, CFile stream);
extern fn CInt unsetenv(ZString name);
extern fn isz write(Fd fd, void* buffer, usz count) @if(!env::WIN32);

extern fn CFile fmemopen(void* ptr, usz size, ZString mode);
extern fn isz getline(char** linep, usz* linecapp, CFile stream);
extern fn CInt timespec_get(TimeSpec* ts, CInt base);
extern fn CInt nanosleep(TimeSpec* req, TimeSpec* remaining);
extern fn ZString ctime(Time_t* timer);
extern fn Time_t time(Time_t* timer);

const CInt STDIN_FD = 0;
const CInt STDOUT_FD = 1;
const CInt STDERR_FD = 2;

module libc @if(env::LINUX || env::ANDROID);
extern CFile __stdin @extern("stdin");
extern CFile __stdout @extern("stdout");
extern CFile __stderr @extern("stderr");
extern fn usz malloc_usable_size(void* ptr);
macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);
extern fn void* aligned_alloc(usz align, usz size);
macro CFile stdin() => __stdin;
macro CFile stdout() => __stdout;
macro CFile stderr() => __stderr;

module libc @if(env::NETBSD || env::OPENBSD);
extern fn int fcntl(CInt socket, int cmd, ...);
extern fn int _setjmp(void*);
macro int setjmp(void* ptr) => _setjmp(ptr);
extern fn int _longjmp(void*, int);
macro usz longjmp(void* ptr, CInt i) => _longjmp(ptr, i);
extern fn usz malloc_size(void* ptr);
extern fn void* aligned_alloc(usz align, usz size);
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

module libc @if(env::LIBC && !env::WIN32 && !env::LINUX && !env::ANDROID && !env::DARWIN && !env::BSD_FAMILY);
macro CFile stdin() { return (CFile*)(uptr)STDIN_FD; }
macro CFile stdout() { return (CFile*)(uptr)STDOUT_FD; }
macro CFile stderr() { return (CFile*)(uptr)STDERR_FD; }

module libc @if(!env::LIBC);
import std::core::mem;


fn void longjmp(JmpBuf* buffer, CInt value) @weak @extern("longjmp") @nostrip
{
	unreachable("longjmp unavailable");
}

fn CInt setjmp(JmpBuf* buffer) @weak @extern("setjmp") @nostrip
{
	unreachable("setjmp unavailable");
}

fn void* malloc(usz size) @weak @extern("malloc") @nostrip
{
	unreachable("malloc unavailable");
}
fn void* calloc(usz count, usz size) @weak @extern("calloc") @nostrip
{
	unreachable("calloc unavailable");
}
fn void* free(void*) @weak @extern("free")
{
	unreachable("free unavailable");
}

fn void* realloc(void* ptr, usz size) @weak @extern("realloc") @nostrip
{
	unreachable("realloc unavailable");
}

alias memcpy = mem::__memcpy;
alias memmove = mem::__memcpy;
alias memset = mem::__memset;

fn int fseek(CFile stream, SeekIndex offset, int whence) @weak @extern("fseek") @nostrip
{
	unreachable("'fseek' not available.");
}
fn CFile fopen(ZString filename, ZString mode) @weak @extern("fopen") @nostrip
{
	unreachable("'fopen' not available.");
}

fn CFile freopen(ZString filename, ZString mode, CFile stream) @weak @extern("fopen") @nostrip
{
	unreachable("'freopen' not available.");
}

fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fwrite") @nostrip
{
	unreachable("'fwrite' not available.");
}

fn usz fread(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fread") @nostrip
{
	unreachable("'fread' not available.");
}

fn CFile fclose(CFile) @weak @extern("fclose") @nostrip
{
	unreachable("'fclose' not available.");
}

fn int fflush(CFile stream) @weak @extern("fflush") @nostrip
{
	unreachable("'fflush' not available.");
}

fn int fputc(int c, CFile stream) @weak @extern("fputc") @nostrip
{
	unreachable("'fputc' not available.");
}

fn char* fgets(ZString str, int n, CFile stream) @weak @extern("fgets") @nostrip
{
	unreachable("'fgets' not available.");
}

fn int fgetc(CFile stream) @weak @extern("fgetc") @nostrip
{
	unreachable("'fgetc' not available.");
}

fn int feof(CFile stream) @weak @extern("feof") @nostrip
{
	unreachable("'feof' not available.");
}

fn int putc(int c, CFile stream) @weak @extern("putc") @nostrip
{
	unreachable("'putc' not available.");
}
fn int putchar(int c) @weak @extern("putchar") @nostrip
{
	unreachable("'putchar' not available.");
}
fn int puts(ZString str) @weak @extern("puts") @nostrip
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
<* Fully buffered *>
const int _IOFBF = 0;
<* Line buffered *>
const int _IOLBF = 1;
<* Unbuffered *>
const int _IONBF = 2;
const int BUFSIZ = 1024;
const int EOF = -1;
const int FOPEN_MAX = 20;
const int FILENAME_MAX = 1024;

macro bool libc_S_ISTYPE(value, mask) @builtin => (value & S_IFMT) == mask;
<* type of file mask *>
const S_IFMT   = 0o170000;
<* named pipe (fifo) *>
const S_IFIFO  = 0o010000;
<* character special *>
const S_IFCHR  = 0o020000;
<* directory *>
const S_IFDIR  = 0o040000;
<* block special *>
const S_IFBLK  = 0o060000;
<* regular *>
const S_IFREG  = 0o100000;
<* regular *>
const S_IFLNK  = 0o120000;
<* symbolic link *>
const S_IFSOCK = 0o140000;
<* Set user id on execution *>
const S_ISUID  = 0o004000;
<* Set group id on execution *>
const S_ISGID  = 0o002000;
<* Set group id on execution *>
const S_ISVTX  = 0o001000;
<* Save swapped text even after use *>
const S_IRUSR  = 0o000400;
<* Write permission, owner *>
const S_IWUSR  = 0o000200;
<* Execute/search permission, owner *>
const S_IXUSR  = 0o000100;

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

module libc::errno;

const Errno OK                       = 0;
<* Operation not permitted *>
const Errno EPERM                    = 1;
<* No such file or directory *>
const Errno ENOENT                   = 2;
<* No such process *>
const Errno ESRCH                    = 3;
<* Interrupted system call *>
const Errno EINTR                    = 4;
<* I/O error *>
const Errno EIO                      = 5;
<* No such device or address *>
const Errno ENXIO                    = 6;
<* Argument list too long *>
const Errno E2BIG                    = 7;
<* Exec format error *>
const Errno ENOEXEC                  = 8;
<* Bad file number *>
const Errno EBADF                    = 9;
<* No child processes *>
const Errno ECHILD                   = 10;


<* Try again Macos *>
const Errno EAGAIN @if(env::DARWIN)  = 35;
<* Try again *>
const Errno EAGAIN @if(!env::DARWIN) = 11;

<* Out of memory *>
const Errno ENOMEM                   = 12;
<* Permission denied *>
const Errno EACCES                   = 13;
<* Bad address *>
const Errno EFAULT                   = 14;
<* Block device required, not on Win32 *>
const Errno ENOTBLK                  = 15;
<* Device or resource busy *>
const Errno EBUSY                    = 16;
<* File exists *>
const Errno EEXIST                   = 17;
<* Cross-device link *>
const Errno EXDEV                    = 18;
<* No such device *>
const Errno ENODEV                   = 19;
<* Not a directory *>
const Errno ENOTDIR                  = 20;
<* Is a directory *>
const Errno EISDIR                   = 21;
<* Invalid argument *>
const Errno EINVAL                   = 22;
<* File table overflow *>
const Errno ENFILE                   = 23;
<* Too many open files *>
const Errno EMFILE                   = 24;
<* Not a typewriter *>
const Errno ENOTTY                   = 25;
<* Text file busy, not on Win32 *>
const Errno ETXTBSY                  = 26;
<* File too large *>
const Errno EFBIG                    = 27;
<* No space left on device *>
const Errno ENOSPC                   = 28;
<* Illegal seek *>
const Errno ESPIPE                   = 29;
<* Read-only file system *>
const Errno EROFS                    = 30;
<* Too many links *>
const Errno EMLINK                   = 31;
<* Broken pipe *>
const Errno EPIPE                    = 32;
<* Math argument out of domain of func *>
const Errno EDOM                     = 33;
<* Math result not representable *>
const Errno ERANGE                   = 34;

// https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/intro.2.html
module libc::errno @if(env::DARWIN);
<* Operation would block *>
const Errno EWOULDBLOCK              = EAGAIN;
<* Resource deadlock would occur *>
const Errno EDEADLK                  = 11;
<* Operation now in progress *>
const Errno EINPROGRESS              = 36;
<* Operation already in progress *>
const Errno EALREADY                 = 37;
<* Socket operation on non-socket *>
const Errno ENOTSOCK                 = 38;
<* Destination address required *>
const Errno EDESTADDRREQ             = 39;
<* Message too long *>
const Errno EMSGSIZE                 = 40;
<* Protocol wrong type for socket *>
const Errno EPROTOTYPE               = 41;
<* Protocol not available *>
const Errno ENOPROTOOPT              = 42;
<* Protocol not supported *>
const Errno EPROTONOSUPPORT          = 43;
<* Socket type not supported *>
const Errno ESOCKTNOSUPPORT          = 44;
<* Not supported *>
const Errno ENOTSUP                  = 45;
<* Protocol family not supported *>
const Errno EPFNOSUPPORT             = 46;
<* Address family not supported by protocol family *>
const Errno EAFNOSUPPORT             = 47;
<* Address already in use *>
const Errno EADDRINUSE               = 48;
<* Cannot assign requested address *>
const Errno EADDRNOTAVAIL            = 49;
<* Network is down *>
const Errno ENETDOWN                 = 50;
<* Network is unreachable *>
const Errno ENETUNREACH              = 51;
<* Network dropped connection on reset *>
const Errno ENETRESET                = 52;
<* Software caused connection abort *>
const Errno ECONNABORTED             = 53;
<* Connection reset by peer *>
const Errno ECONNRESET               = 54;
<* No buffer space available *>
const Errno ENOBUFS                  = 55;
<* Socket is already connected *>
const Errno EISCONN                  = 56;
<* Socket is not connected *>
const Errno ENOTCONN                 = 57;
<* Cannot send after socket shutdown *>
const Errno ESHUTDOWN                = 58;
<* Operation timed out *>
const Errno ETIMEDOUT                = 60;
<* Connection refused *>
const Errno ECONNREFUSED             = 61;
<* Too many levels of symbolic links *>
const Errno ELOOP                    = 62;
<* File name too long *>
const Errno ENAMETOOLONG             = 63;
<* Host is down *>
const Errno EHOSTDOWN                = 64;
<* No route to host *>
const Errno EHOSTUNREACH             = 65;
<* Directory not empty *>
const Errno ENOTEMPTY                = 66;
<* Too many processes *>
const Errno EPROCLIM                 = 67;
<* Too many users *>
const Errno EUSERS                   = 68;
<* Disc quota exceeded *>
const Errno EDQUOT                   = 69;
<* Stale NFS file handle *>
const Errno ESTALE                   = 70;
<* RPC struct is bad *>
const Errno EBADRPC                  = 72;
<* RPC version wrong *>
const Errno ERPCMISMATCH             = 73;
<* RPC prog. not avail *>
const Errno EPROGUNAVAIL             = 74;
<* Program version wrong *>
const Errno EPROGMISMATCH            = 75;
<* Bad procedure for program *>
const Errno EPROCUNAVAIL             = 76;
<* No locks available *>
const Errno ENOLCK                   = 77;
<* Function not implemented *>
const Errno ENOSYS                   = 78;
<* Inappropriate file type or format *>
const Errno EFTYPE                   = 79;
<* Authentication error *>
const Errno EAUTH                    = 80;
<* Need authenticator *>
const Errno ENEEDAUTH                = 81;
<* Device power is off *>
const Errno EPWROFF                  = 82;
<* Device error *>
const Errno EDEVERR                  = 83;
<* Value too large to be stored in data type *>
const Errno EOVERFLOW                = 84;
<* Bad executable (or shared library) *>
const Errno EBADEXEC                 = 85;
<* Bad CPU type in executable *>
const Errno EBADARCH                 = 86;
<* Shared library version mismatch *>
const Errno ESHLIBVERS               = 87;
<* Malformed Mach-o file *>
const Errno EBADMACHO                = 88;
<* Operation canceled *>
const Errno ECANCELED                = 89;
<* Identifier removed *>
const Errno EIDRM                    = 90;
<* No message of desired type *>
const Errno ENOMSG                   = 91;
<* Illegal byte sequence *>
const Errno EILSEQ                   = 92;
<* Attribute not found *>
const Errno ENOATTR                  = 93;
<* Bad message *>
const Errno EBADMSG                  = 94;
<* Reserved *>
const Errno EMULTIHOP                = 95;
<* No message available *>
const Errno ENODATA                  = 96;
<* Reserved *>
const Errno ENOLINK                  = 97;
<* No STREAM resources *>
const Errno ENOSR                    = 98;
<* Not a STREAM *>
const Errno ENOSTR                   = 99;
<* Protocol error *>
const Errno EPROTO                   = 100;
<* STREAM ioctl() timeout *>
const Errno ETIME                    = 101;
<* Operation not supported on socket *>
const Errno EOPNOTSUPP               = 102;

module libc::errno @if(env::WIN32);
<* Resource deadlock would occur Win32 *>
const Errno EDEADLK                  = 36;
<* File name too long Win32 *>
const Errno ENAMETOOLONG             = 38;
<* Directory not empty *>
const Errno ENOTEMPTY                = 41;
<* Too many symbolic links encountered *>
const Errno ELOOP                    = 114;
<* Value too large for defined data type *>
const Errno EOVERFLOW                = 132;
<* Network is down *>
const Errno ENETDOWN                 = 116;
<* Connection reset by peer *>
const Errno ECONNRESET               = 108;
<* Network is unreachable *>
const Errno ENETUNREACH              = 118;
<* Network dropped connection because of reset *>
const Errno ENETRESET                = 117;
<* Operation not supported on transport endpoint *>
const Errno EOPNOTSUPP               = 130;
<* Connection timed out *>
const Errno ETIMEDOUT                = 138;
<* Operation already in progress *>
const Errno EALREADY                 = 103;
<* Operation now in progress Win32 *>
const Errno EINPROGRESS              = 112;
<* Quota exceeded, not in Win32 *>
const Errno EDQUOT                   = -122;
<* Operation would block *>
const Errno EWOULDBLOCK              = 140;

module libc::errno @if(!env::WIN32 && !env::DARWIN);
<* Resource deadlock would occur Linux (others?) *>
const Errno EDEADLK                  = 35;
<* File name too long Linux (others?) *>
const Errno ENAMETOOLONG             = 36;
<* Directory not empty *>
const Errno ENOTEMPTY                = 39;
<* Too many symbolic links encountered *>
const Errno ELOOP                    = 40;
<* Operation would block *>
const Errno EWOULDBLOCK              = EAGAIN;
<* Value too large for defined data type *>
const Errno EOVERFLOW                = 75;
<* Socket operation on non-socket *>
const Errno ENOTSOCK                 = 88;
<* Operation not supported on transport endpoint *>
const Errno EOPNOTSUPP               = 95;
<* Address already in use *>
const Errno EADDRINUSE               = 98;
<* Cannot assign requested address *>
const Errno EADDRNOTAVAIL            = 99;
<* Network is down *>
const Errno ENETDOWN                 = 100;
<* Network is unreachable *>
const Errno ENETUNREACH              = 101;
<* Network dropped connection because of reset *>
const Errno ENETRESET                = 102;
<* Connection reset by peer *>
const Errno ECONNRESET               = 104;
<* Socket is already connected *>
const Errno EISCONN                  = 106;
<* Connection timed out *>
const Errno ETIMEDOUT                = 110;
<* Connection refused *>
const Errno ECONNREFUSED             = 111;
<* Operation already in progress *>
const Errno EALREADY                 = 114;
<* Operation now in progress *>
const Errno EINPROGRESS              = 115;
<* Quota exceeded *>
const Errno EDQUOT                   = 122;


/*
const Errno ENOLCK = 37;  /* No record locks available */
const Errno ENOSYS = 38;  /* Function not implemented */

const Errno ENOMSG = 42;  /* No message of desired type */
const Errno EIDRM = 43;  /* Identifier removed */
const Errno ECHRNG = 44;  /* Channel number out of range */
const Errno EL2NSYNC = 45;  /* Level 2 not synchronized */
const Errno EL3HLT = 46;  /* Level 3 halted */
const Errno EL3RST = 47;  /* Level 3 reset */
const Errno ELNRNG = 48;  /* Link number out of range */
const Errno EUNATCH = 49;  /* Protocol driver not attached */
const Errno ENOCSI = 50;  /* No CSI structure available */
const Errno EL2HLT = 51;  /* Level 2 halted */
const Errno EBADE = 52;  /* Invalid exchange */
const Errno EBADR = 53;  /* Invalid request descriptor */
const Errno EXFULL = 54;  /* Exchange full */
const Errno ENOANO = 55;  /* No anode */
const Errno EBADRQC = 56;  /* Invalid request code */
const Errno EBADSLT = 57;  /* Invalid slot */

const Errno EBFONT = 59;  /* Bad font file format */
const Errno ENOSTR = 60;  /* Device not a stream */
const Errno ENODATA = 61;  /* No data available */
const Errno ETIME = 62;  /* Timer expired */
const Errno ENOSR = 63;  /* Out of streams resources */
const Errno ENONET = 64;  /* Machine is not on the network */
const Errno ENOPKG = 65;  /* Package not installed */
const Errno EREMOTE = 66;  /* Object is remote */
const Errno ENOLINK = 67;  /* Link has been severed */
const Errno EADV = 68;  /* Advertise error */
const Errno ESRMNT = 69;  /* Srmount error */
const Errno ECOMM = 70;  /* Communication error on send */
const Errno EPROTO = 71;  /* Protocol error */
const Errno EMULTIHOP = 72;  /* Multihop attempted */
const Errno EDOTDOT = 73;  /* RFS specific error */
const Errno EBADMSG = 74;  /* Not a data message */
const Errno ENOTUNIQ = 76;  /* Name not unique on network */
const Errno EBADFD = 77;  /* File descriptor in bad state */
const Errno EREMCHG = 78;  /* Remote address changed */
const Errno ELIBACC = 79;  /* Can not access a needed shared library */
const Errno ELIBBAD = 80;  /* Accessing a corrupted shared library */
const Errno ELIBSCN = 81;  /* .lib section in a.out corrupted */
const Errno ELIBMAX = 82;  /* Attempting to link in too many shared libraries */
const Errno ELIBEXEC = 83;  /* Cannot exec a shared library directly */
const Errno EILSEQ = 84;  /* Illegal byte sequence */
const Errno ERESTART = 85;  /* Interrupted system call should be restarted */
const Errno ESTRPIPE = 86;  /* Streams pipe error */
const Errno EUSERS = 87;  /* Too many users */
const Errno ENOTSOCK = 88;  /* Socket operation on non-socket */
const Errno EDESTADDRREQ = 89;  /* Destination address required */
const Errno EMSGSIZE = 90;  /* Message too long */
const Errno EPROTOTYPE = 91;  /* Protocol wrong type for socket */
const Errno ENOPROTOOPT = 92;  /* Protocol not available */
const Errno EPROTONOSUPPORT = 93;  /* Protocol not supported */
const Errno ESOCKTNOSUPPORT = 94;  /* Socket type not supported */
const Errno EPFNOSUPPORT = 96;  /* Protocol family not supported */
const Errno EAFNOSUPPORT = 97;  /* Address family not supported by protocol */
const Errno EADDRINUSE = 98;  /* Address already in use */
const Errno ECONNABORTED = 103; /* Software caused connection abort */
const Errno ENOBUFS = 105; /* No buffer space available */
const Errno EISCONN = 106; /* Transport endpoint is already connected */
const Errno ENOTCONN = 107; /* Transport endpoint is not connected */
const Errno ESHUTDOWN = 108; /* Cannot send after transport endpoint shutdown */
const Errno ETOOMANYREFS = 109; /* Too many references: cannot splice */
const Errno ECONNREFUSED = 111; /* Connection refused */
const Errno EHOSTDOWN = 112; /* Host is down */
const Errno EHOSTUNREACH = 113; /* No route to host */
*/


/*
const Errno ESTALE = 116; /* Stale NFS file handle */
const Errno EUCLEAN = 117; /* Structure needs cleaning */
const Errno ENOTNAM = 118; /* Not a XENIX named type file */
const Errno ENAVAIL = 119; /* No XENIX semaphores available */
const Errno EISNAM = 120; /* Is a named type file */
const Errno EREMOTEIO = 121; /* Remote I/O error */

const Errno ENOMEDIUM = 123; /* No medium found */
const Errno EMEDIUMTYPE = 124; /* Wrong medium type */
const Errno ECANCELED = 125; /* Operation Canceled */
const Errno ENOKEY = 126; /* Required key not available */
const Errno EKEYEXPIRED = 127; /* Key has expired */
const Errno EKEYREVOKED = 128; /* Key has been revoked */
const Errno EKEYREJECTED = 129; /* Key was rejected by service */

const Errno EOWNERDEAD = 130; /* Owner died */
const Errno ENOTRECOVERABLE = 131; /* State not recoverable */
*/

