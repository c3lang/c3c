// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module libc;


// Constants need to be per os/arch
const int EXIT_FAILURE = 1;
const int EXIT_SUCCESS = 0;
const int RAND_MAX = 0x7fffffff;

struct DivResult
{
    int quot;
    int rem;
}

struct LongDivResult
{
    long quot;
    long rem;
}

fn Errno errno()
{
	return (Errno)os::errno();
}

fn void errno_set(Errno e)
{
	os::errno_set((int)e);
}

define TerminateFunction = fn void();
define CompareFunction = fn int(void*, void*);
define JmpBuf = CInt[$$JMP_BUF_SIZE];

$if (env::COMPILER_LIBC_AVAILABLE):

extern fn double atof(char* str);
extern fn int atoi(char* str);
extern fn CLongLong atoll(char* str);
extern fn double strtod(char* str, char** endptr);
extern fn CLong strtol(char* str, char** endptr, int base);
extern fn CULong stroul(char* str, char** endptr, int base);
extern fn void abort();
extern fn void atexit(TerminateFunction f);
extern fn void exit(int status);
extern fn char* getenv(char* name);
extern fn int system(char* str);
extern fn void bsearch(void* key, void *base, usz items, usz size, CompareFunction compare);
extern fn void qsort(void* base, usz items, usz size, CompareFunction compare);
extern fn int abs(int x);
extern fn DivResult div(int numer, int denom);
extern fn long labs(long x);
extern fn LongDivResult ldiv(long number, long denom);
extern fn int rand();
extern fn void srand(uint seed);

extern fn void longjmp(JmpBuf* buffer, CInt value);
$if (env::OS_TYPE == OsType.WIN32):
// TODO win32 aarch64
extern fn CInt _setjmp(void* frameptr, JmpBuf* buffer);
macro CInt setjmp(JmpBuf* buffer) => _setjmp($$frameaddress(), buffer);
$else:
extern fn CInt setjmp(JmpBuf* buffer);
$endif;
// MB functions omitted

// string
extern fn void* memchr(void* str, int c, usz n);
extern fn int memcmp(void* str1, void* str2, usz n);
extern fn void* memcpy(void* dest, void* src, usz n);
extern fn void* memmove(void* dest, void* src, usz n);
extern fn void* memset(void* dest, CInt value, usz n);
extern fn char* strcat(char* dest, char* src);
extern fn char* strncat(char* dest, char* src, usz n);
extern fn char* strchr(char* str, int c);
extern fn int strcmp(char* str1, char* str2);
extern fn int strncmp(char* str1, char* str2, usz n);
extern fn int strcoll(char* str1, char* str2);
extern fn char* strcpy(char* dst, char* src);
extern fn char* strncpy(char* dst, char* src, usz n);
extern fn usz strcspn(char* str1, char* str2);
extern fn char* strerror(int errn);
extern fn usz strlen(char* str);
extern fn char* strpbrk(char* str1, char* str2);
extern fn usz strspn(char* str1, char* str2);
extern fn char* strstr(char* haystack, char* needle);
extern fn char* strtok(char* str, char* delim);
extern fn usz strxfrm(char* dest, char* src, usz n);

// malloc
extern fn void* malloc(usz size);
extern fn void* calloc(usz count, usz size);
extern fn void* free(void*);
extern fn void* realloc(void* ptr, usz size);

$else:

fn void longjmp(JmpBuf* buffer, CInt value) @weak @extname("longjmp")
{
	unreachable("longjmp unavailable");
}

fn CInt setjmp(JmpBuf* buffer) @weak @extname("setjmp")
{
	unreachable("setjmp unavailable");
}

fn void* malloc(usz size) @weak @extname("malloc")
{
	unreachable("malloc unavailable");
}
fn void* calloc(usz count, usz size) @weak @extname("calloc")
{
	unreachable("calloc unavailable");
}
fn void* free(void*) @weak @extname("free")
{
	unreachable("free unavailable");
}

fn void* realloc(void* ptr, usz size) @weak @extname("realloc")
{
	unreachable("realloc unavailable");
}

fn void* memcpy(void* dest, void* src, usz n) @weak @extname("memcpy")
{
	for (usz i = 0; i < n; i++) ((char*)dest)[i] = ((char*)src)[i];
	return dest;
}

fn void* memmove(void* dest, void* src, usz n) @weak @extname("memmove")
{
	return memcpy(dest, src, n) @inline;
}

fn void* memset(void* dest, CInt value, usz n) @weak @extname("memset")
{
	for (usz i = 0; i < n; i++) ((char*)dest)[i] = (char)value;
	return dest;
}

$endif;

// stdio

define Fpos = long;
define CFile = void*;

$if (env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == OsType.LINUX):
    extern CFile __stdin @extname("stdin");
    extern CFile __stdout @extname("stdout");
    extern CFile __stderr @extname("stderr");
    extern fn usz malloc_usable_size(void* ptr);
    macro usz malloc_size(void* ptr) { return malloc_usable_size(ptr); }
	extern fn void* aligned_alloc(usz align, usz size);
    macro CFile stdin() { return __stdin; }
    macro CFile stdout() { return __stdout; }
    macro CFile stderr() { return __stderr; }
$elif (env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == OsType.MACOSX):
    extern CFile __stdinp;
    extern CFile __stdoutp;
    extern CFile __stderrp;
    extern fn usz malloc_size(void* ptr);
    extern fn void* aligned_alloc(usz align, usz size);
	macro CFile stdin() { return __stdinp; }
    macro CFile stdout() { return __stdoutp; }
    macro CFile stderr() { return __stderrp; }
$elif (env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == OsType.WIN32):
    extern fn CFile __acrt_iob_func(CInt c);
    extern fn usz _msize(void* ptr);
    macro usz malloc_size(void* ptr) { return _msize(ptr); }
    macro CFile stdin() { return __acrt_iob_func(0); }
    macro CFile stdout() { return __acrt_iob_func(1); }
    macro CFile stderr() { return __acrt_iob_func(2); }
$else:
    macro CFile stdin() { return (CFile*)(uptr)0; }
    macro CFile stdout() { return (CFile*)(uptr)1; }
    macro CFile stderr() { return (CFile*)(uptr)2; }
$endif;

const HAS_MALLOC_SIZE =
	env::OS_TYPE == OsType.LINUX
	|| env::OS_TYPE == OsType.WIN32
	|| env::OS_TYPE == OsType.MACOSX;

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

define Errno = distinct CInt;
define SeekIndex = CLong;

$if (env::COMPILER_LIBC_AVAILABLE):

extern fn int fclose(CFile stream);
extern fn void clearerr(CFile stream);
extern fn int feof(CFile stream);
extern fn int ferror(CFile stream);
extern fn int fflush(CFile stream);
extern fn int fgetpos(CFile stream, Fpos* pos);
extern fn CFile fopen(char* filename, char* mode);
extern fn usz fread(void* ptr, usz size, usz nmemb, CFile stream);
extern fn CFile freopen(char* filename, char* mode, CFile stream);
extern fn int fseek(CFile stream, SeekIndex offset, int whence);
extern fn int fsetpos(CFile stream, Fpos* pos);
extern fn SeekIndex ftell(CFile stream);
extern fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream);
extern fn int remove(char* filename);
extern fn int rename(char* old_name, char* new_name);
extern fn void rewind(CFile stream);
extern fn void setbuf(CFile stream, char* buffer);
extern fn void setvbuf(CFile stream, char* buffer, int mode, usz size);
extern fn CFile tmpnam(char* str);
extern fn int fprintf(CFile stream, char* format, ...);
extern fn int printf(char* format, ...);
extern fn int sprintf(char* str, char* format, ...);
extern fn int snprintf(char* str, usz size, char* format, ...);
extern fn int fscanf(CFile stream, char* format, ...);
extern fn int scanf(char* format, ...);
extern fn int sscanf(char* str, char* format, ...);
extern fn int fgetc(CFile stream);
extern fn char* fgets(char* str, int n, CFile stream);
extern fn int fputc(int c, CFile stream);
extern fn int getc(CFile stream);
extern fn int getchar();
extern fn int putc(int c, CFile stream);
extern fn int putchar(int c);
extern fn int puts(char* str);
extern fn int ungetc(int c, CFile stream);
extern fn void perror(char* str);
extern fn isz getline(char** linep, usz* linecapp, CFile stream);

$else:

fn int fseek(CFile stream, SeekIndex offset, int whence) @weak @extname("fseek")
{
	unreachable("'fseek' not available.");
}
fn CFile fopen(char* filename, char* mode) @weak @extname("fopen")
{
	unreachable("'fopen' not available.");
}

fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream) @weak @extname("fwrite")
{
	unreachable("'fwrite' not available.");
}

fn usz fread(void* ptr, usz size, usz nmemb, CFile stream) @weak @extname("fread")
{
	unreachable("'fread' not available.");
}

fn CFile fclose(CFile) @weak @extname("fclose")
{
	unreachable("'fclose' not available.");
}

fn int fflush(CFile stream) @weak @extname("fflush")
{
	unreachable("'fflush' not available.");
}

fn int fputc(int c, CFile stream) @weak @extname("fputc")
{
	unreachable("'fputc' not available.");
}

fn char* fgets(char* str, int n, CFile stream) @weak @extname("fgets")
{
	unreachable("'fgets' not available.");
}

fn int fgetc(CFile stream) @weak @extname("fgetc")
{
	unreachable("'fgetc' not available.");
}

fn int feof(CFile stream) @weak @extname("feof")
{
	unreachable("'feof' not available.");
}

fn int putc(int c, CFile stream) @weak @extname("putc")
{
	unreachable("'putc' not available.");
}
fn int putchar(int c) @weak @extname("putchar")
{
	unreachable("'putchar' not available.");
}
fn int puts(char* str) @weak @extname("puts")
{
	unreachable("'puts' not available.");
}

$endif;
// vsprintf vprintf not supported

// time.h

define TimeOffset = CLong;

struct Tm
{
	int	tm_sec;		/* seconds after the minute [0-60] */
	int	tm_min;		/* minutes after the hour [0-59] */
	int	tm_hour;	/* hours since midnight [0-23] */
	int	tm_mday;	/* day of the month [1-31] */
	int	tm_mon;		/* months since January [0-11] */
	int	tm_year;	/* years since 1900 */
	int	tm_wday;	/* days since Sunday [0-6] */
	int	tm_yday;	/* days since January 1 [0-365] */
	int	tm_isdst;	/* Daylight Savings Time flag */
	TimeOffset tm_gmtoff;	/* offset from UTC in seconds */
    char *tm_zone;	/* timezone abbreviation */
}

struct TimeSpec
{
	Time s;
	ulong ns;
}

const int TIME_UTC = 1;


extern fn int timespec_get(TimeSpec* ts, int base);
extern fn int nanosleep(TimeSpec* req, TimeSpec* remaining);

// Likely wrong, must be per platform.
const CLOCKS_PER_SEC = 1000000;

// Time also needs to be per platform
define Time = long;
define Clock = ulong;

extern fn char* asctime(Tm *timeptr);
extern fn Clock clock();
extern fn char* ctime(Time *timer);
extern fn double difftime(Time time1, Time time2);
extern fn Tm* gmtime(Time *timer);
extern fn Tm* localtime(Time *timer);
extern fn Time mktime(Tm *timeptr);
extern fn usz strftime(char* str, usz maxsize, char* format, Tm *timeptr);
extern fn Time time(Time *timer);

// signal
define SignalFunction = fn void(int);
extern fn SignalFunction signal(int sig, SignalFunction function);
// Incomplete


module libc::errno;

const Errno OK = 0;
const Errno EPERM = 1;      // Operation not permitted
const Errno ENOENT = 2;     // No such file or directory
const Errno ESRCH = 3;      // No such process
const Errno EINTR = 4;      // Interrupted system call
const Errno EIO = 5;        // I/O error
const Errno ENXIO = 6;      // No such device or address
const Errno E2BIG = 7;      // Argument list too long
const Errno ENOEXEC = 8;    // Exec format error
const Errno EBADF = 9;      // Bad file number
const Errno ECHILD = 10;    // No child processes

$if (env::OS_TYPE == OsType.MACOSX):
const Errno EAGAIN = 35;    // Try again Macos
$else:
const Errno EAGAIN = 11;    // Try again
$endif;

const Errno ENOMEM = 12;    // Out of memory
const Errno EACCES = 13;    // Permission denied
const Errno EFAULT = 14;    // Bad address
const Errno ENOTBLK = 15;   // Block device required, not on Win32
const Errno EBUSY = 16;     // Device or resource busy
const Errno EEXIST = 17;    // File exists
const Errno EXDEV = 18;     // Cross-device link
const Errno ENODEV = 19;    // No such device
const Errno ENOTDIR = 20;   // Not a directory
const Errno EISDIR = 21;    // Is a directory
const Errno EINVAL = 22;    // Invalid argument
const Errno ENFILE = 23;    // File table overflow
const Errno EMFILE = 24;    // Too many open files
const Errno ENOTTY = 25;    // Not a typewriter
const Errno ETXTBSY = 26;   // Text file busy, not on Win32
const Errno EFBIG = 27;     // File too large
const Errno ENOSPC = 28;    // No space left on device
const Errno ESPIPE = 29;    // Illegal seek
const Errno EROFS = 30;     // Read-only file system
const Errno EMLINK = 31;    // Too many links
const Errno EPIPE = 32;     // Broken pipe
const Errno EDOM = 33;      // Math argument out of domain of func
const Errno ERANGE = 34;    // Math result not representable

$if (env::OS_TYPE == OsType.MACOSX):
const Errno EDEADLK = 11;       // Resource deadlock would occur MacOS
const Errno ENAMETOOLONG = 63;  // File name too long MacOS
const Errno ELOOP = 62;         // Too many symbolic links encountered
const Errno EOVERFLOW = 84;     // Value too large for defined data type Macos
const Errno ECONNRESET = 54;    // Connection reset by peer Macos
const Errno ENETDOWN = 50;      // Network is down MacOS
const Errno ENETUNREACH = 51;   // Network is unreachable MacOS
const Errno ENETRESET = 52;     // Network dropped connection because of reset MacOS

$elif (env::OS_TYPE == OsType.WIN32):
const Errno EDEADLK = 36;       // Resource deadlock would occur Win32
const Errno ENAMETOOLONG = 38;  // File name too long Win32
const Errno ELOOP = 114;        // Too many symbolic links encountered
const Errno EOVERFLOW = 132;    // Value too large for defined data type
const Errno ENETDOWN = 116;     // Network is down
const Errno ECONNRESET = 108;   // Connection reset by peer
const Errno ENETUNREACH = 118;  // Network is unreachable
const Errno ENETRESET = 117;    // Network dropped connection because of reset

$else:
const Errno EDEADLK = 35;       // Resource deadlock would occur Linux (others?)
const Errno ENAMETOOLONG = 36;  // File name too long Linux (others?)
const Errno ELOOP = 40;         // Too many symbolic links encountered
const Errno EOVERFLOW = 75;     // Value too large for defined data type
const Errno ENETDOWN = 100;     // Network is down
const Errno ECONNRESET = 104;   // Connection reset by peer
const Errno ENETUNREACH = 101;  // Network is unreachable
const Errno ENETRESET = 102;    // Network dropped connection because of reset

$endif;


/*
const Errno ENOLCK = 37;  /* No record locks available */
const Errno ENOSYS = 38;  /* Function not implemented */
const Errno ENOTEMPTY = 39;  /* Directory not empty */

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
const Errno EOPNOTSUPP = 95;  /* Operation not supported on transport endpoint */
const Errno EPFNOSUPPORT = 96;  /* Protocol family not supported */
const Errno EAFNOSUPPORT = 97;  /* Address family not supported by protocol */
const Errno EADDRINUSE = 98;  /* Address already in use */
const Errno EADDRNOTAVAIL = 99;  /* Cannot assign requested address */
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
$if (env::OS_TYPE == OsType.MACOSX):
const Errno ETIMEDOUT = 60;     // Connection timed out
const Errno EINPROGRESS = 36;   // Operation now in progress MacOS
const Errno EALREADY = 37;      // Operation already in progress MacOS
const Errno EDQUOT = 69;        // Quota exceeded, MacOS
const Errno EWOULDBLOCK = 35;   // Operation would block

$elif (env::OS_TYPE == OsType.WIN32):
const Errno ETIMEDOUT = 138;    // Connection timed out
const Errno EALREADY = 103;     // Operation already in progress
const Errno EINPROGRESS = 112;  // Operation now in progress Win32
const Errno EDQUOT = -122;      // Quota exceeded, not in Win32
const Errno EWOULDBLOCK = 140;  // Operation would block

$else:
const Errno ETIMEDOUT = 110;    // Connection timed out
const Errno EALREADY = 114;     // Operation already in progress
const Errno EINPROGRESS = 115;  // Operation now in progress
const Errno EDQUOT = 122;       // Quota exceeded
const Errno EWOULDBLOCK = 41;   // Operation would block
$endif;

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

