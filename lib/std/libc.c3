// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module libc;
import std::cinterop;
import std::env;
import std::os::linux;
import std::os::macos;
import std::os::windows;
// stdlib


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

enum Errno : ErrnoType
{
    EPERM = 1,  /* Operation not permitted */
    ENOENT = 2,  /* No such file or directory */
    ESRCH = 3,  /* No such process */
    EINTR = 4,  /* Interrupted system call */
    EIO = 5,  /* I/O error */
    ENXIO = 6,  /* No such device or address */
    E2BIG = 7,  /* Argument list too long */
    ENOEXEC = 8,  /* Exec format error */
    EBADF = 9,  /* Bad file number */
    ECHILD = 10,  /* No child processes */
    EAGAIN = 11,  /* Try again */
    ENOMEM = 12,  /* Out of memory */
    EACCES = 13,  /* Permission denied */
    EFAULT = 14,  /* Bad address */
    ENOTBLK = 15,  /* Block device required */
    EBUSY = 16,  /* Device or resource busy */
    EEXIST = 17,  /* File exists */
    EXDEV = 18,  /* Cross-device link */
    ENODEV = 19,  /* No such device */
    ENOTDIR = 20,  /* Not a directory */
    EISDIR = 21,  /* Is a directory */
    EINVAL = 22,  /* Invalid argument */
    ENFILE = 23,  /* File table overflow */
    EMFILE = 24,  /* Too many open files */
    ENOTTY = 25,  /* Not a typewriter */
    ETXTBSY = 26,  /* Text file busy */
    EFBIG = 27,  /* File too large */
    ENOSPC = 28,  /* No space left on device */
    ESPIPE = 29,  /* Illegal seek */
    EROFS = 30,  /* Read-only file system */
    EMLINK = 31,  /* Too many links */
    EPIPE = 32,  /* Broken pipe */
    EDOM = 33,  /* Math argument out of domain of func */
    ERANGE = 34,  /* Math result not representable */
    EDEADLK = 35,  /* Resource deadlock would occur */
    ENAMETOOLONG = 36,  /* File name too long */
    ENOLCK = 37,  /* No record locks available */
    ENOSYS = 38,  /* Function not implemented */
    ENOTEMPTY = 39,  /* Directory not empty */
    ELOOP = 40,  /* Too many symbolic links encountered */

    ENOMSG = 42,  /* No message of desired type */
    EIDRM = 43,  /* Identifier removed */
    ECHRNG = 44,  /* Channel number out of range */
    EL2NSYNC = 45,  /* Level 2 not synchronized */
    EL3HLT = 46,  /* Level 3 halted */
    EL3RST = 47,  /* Level 3 reset */
    ELNRNG = 48,  /* Link number out of range */
    EUNATCH = 49,  /* Protocol driver not attached */
    ENOCSI = 50,  /* No CSI structure available */
    EL2HLT = 51,  /* Level 2 halted */
    EBADE = 52,  /* Invalid exchange */
    EBADR = 53,  /* Invalid request descriptor */
    EXFULL = 54,  /* Exchange full */
    ENOANO = 55,  /* No anode */
    EBADRQC = 56,  /* Invalid request code */
    EBADSLT = 57,  /* Invalid slot */

    EBFONT = 59,  /* Bad font file format */
    ENOSTR = 60,  /* Device not a stream */
    ENODATA = 61,  /* No data available */
    ETIME = 62,  /* Timer expired */
    ENOSR = 63,  /* Out of streams resources */
    ENONET = 64,  /* Machine is not on the network */
    ENOPKG = 65,  /* Package not installed */
    EREMOTE = 66,  /* Object is remote */
    ENOLINK = 67,  /* Link has been severed */
    EADV = 68,  /* Advertise error */
    ESRMNT = 69,  /* Srmount error */
    ECOMM = 70,  /* Communication error on send */
    EPROTO = 71,  /* Protocol error */
    EMULTIHOP = 72,  /* Multihop attempted */
    EDOTDOT = 73,  /* RFS specific error */
    EBADMSG = 74,  /* Not a data message */
    EOVERFLOW = 75,  /* Value too large for defined data type */
    ENOTUNIQ = 76,  /* Name not unique on network */
    EBADFD = 77,  /* File descriptor in bad state */
    EREMCHG = 78,  /* Remote address changed */
    ELIBACC = 79,  /* Can not access a needed shared library */
    ELIBBAD = 80,  /* Accessing a corrupted shared library */
    ELIBSCN = 81,  /* .lib section in a.out corrupted */
    ELIBMAX = 82,  /* Attempting to link in too many shared libraries */
    ELIBEXEC = 83,  /* Cannot exec a shared library directly */
    EILSEQ = 84,  /* Illegal byte sequence */
    ERESTART = 85,  /* Interrupted system call should be restarted */
    ESTRPIPE = 86,  /* Streams pipe error */
    EUSERS = 87,  /* Too many users */
    ENOTSOCK = 88,  /* Socket operation on non-socket */
    EDESTADDRREQ = 89,  /* Destination address required */
    EMSGSIZE = 90,  /* Message too long */
    EPROTOTYPE = 91,  /* Protocol wrong type for socket */
    ENOPROTOOPT = 92,  /* Protocol not available */
    EPROTONOSUPPORT = 93,  /* Protocol not supported */
    ESOCKTNOSUPPORT = 94,  /* Socket type not supported */
    EOPNOTSUPP = 95,  /* Operation not supported on transport endpoint */
    EPFNOSUPPORT = 96,  /* Protocol family not supported */
    EAFNOSUPPORT = 97,  /* Address family not supported by protocol */
    EADDRINUSE = 98,  /* Address already in use */
    EADDRNOTAVAIL = 99,  /* Cannot assign requested address */
    ENETDOWN = 100, /* Network is down */
    ENETUNREACH = 101, /* Network is unreachable */
    ENETRESET = 102, /* Network dropped connection because of reset */
    ECONNABORTED = 103, /* Software caused connection abort */
    ECONNRESET = 104, /* Connection reset by peer */
    ENOBUFS = 105, /* No buffer space available */
    EISCONN = 106, /* Transport endpoint is already connected */
    ENOTCONN = 107, /* Transport endpoint is not connected */
    ESHUTDOWN = 108, /* Cannot send after transport endpoint shutdown */
    ETOOMANYREFS = 109, /* Too many references: cannot splice */
    ETIMEDOUT = 110, /* Connection timed out */
    ECONNREFUSED = 111, /* Connection refused */
    EHOSTDOWN = 112, /* Host is down */
    EHOSTUNREACH = 113, /* No route to host */
    EALREADY = 114, /* Operation already in progress */
    EINPROGRESS = 115, /* Operation now in progress */
    ESTALE = 116, /* Stale NFS file handle */
    EUCLEAN = 117, /* Structure needs cleaning */
    ENOTNAM = 118, /* Not a XENIX named type file */
    ENAVAIL = 119, /* No XENIX semaphores available */
    EISNAM = 120, /* Is a named type file */
    EREMOTEIO = 121, /* Remote I/O error */
    EDQUOT = 122, /* Quota exceeded */

    ENOMEDIUM = 123, /* No medium found */
    EMEDIUMTYPE = 124, /* Wrong medium type */
    ECANCELED = 125, /* Operation Canceled */
    ENOKEY = 126, /* Required key not available */
    EKEYEXPIRED = 127, /* Key has expired */
    EKEYREVOKED = 128, /* Key has been revoked */
    EKEYREJECTED = 129, /* Key was rejected by service */

    EOWNERDEAD = 130, /* Owner died */
    ENOTRECOVERABLE = 131, /* State not recoverable */
}

fn Errno errno()
{
    $if (env::OS_TYPE == OsType.WIN32):
        return (Errno)windows::errno();
    $elif (env::OS_TYPE == OsType.MACOSX):
        return (Errno)macos::errno();
    $elif (env::OS_TYPE == OsType.LINUX):
        return (Errno)linux::errno();
    $else:
        return Errno.ENOTRECOVERABLE;
    $endif;
}


define TerminateFunction = fn void();
define CompareFunction = fn int(void*, void*);
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
extern fn void bsearch(void* key, void *base, usize items, usize size, CompareFunction compare);
extern fn void qsort(void* base, usize items, usize size, CompareFunction compare);
extern fn int abs(int x);
extern fn DivResult div(int numer, int denom);
extern fn long labs(long x);
extern fn LongDivResult ldiv(long number, long denom);
extern fn int rand();
extern fn void srand(uint seed);

// MB functions omitted

// string
extern fn void* memchr(void* str, int c, usize n);
extern fn int memcmp(void* str1, void* str2, usize n);
extern fn void* memcpy(void* dest, void* src, usize n);
extern fn void* memmove(void* dest, void* src, usize n);
extern fn void* memset(void* dest, usize n);
extern fn char* strcat(char* dest, char* src);
extern fn char* strncat(char* dest, char* src, usize n);
extern fn char* strchr(char* str, int c);
extern fn int strcmp(char* str1, char* str2);
extern fn int strncmp(char* str1, char* str2, usize n);
extern fn int strcoll(char* str1, char* str2);
extern fn char* strcpy(char* dst, char* src);
extern fn char* strncpy(char* dst, char* src, usize n);
extern fn usize strcspn(char* str1, char* str2);
extern fn char* strerror(int errn);
extern fn usize strlen(char* str);
extern fn char* strpbrk(char* str1, char* str2);
extern fn usize strspn(char* str1, char* str2);
extern fn char* strstr(char* haystack, char* needle);
extern fn char* strtok(char* str, char* delim);
extern fn usize strxfrm(char* dest, char* src, usize n);

// malloc
extern fn void* malloc(usize size);
extern fn void* calloc(usize count, usize size);
extern fn void* free(void*);
extern fn void* realloc(void* ptr, usize size);

// stdio

define Fpos = long;
define CFile = void*;
$switch (env::OS_TYPE):
$case OsType.LINUX:
    extern CFile __stdin @extname("stdin");
    extern CFile __stdout @extname("stdout");
    extern CFile __stderr @extname("stderr");
    macro CFile stdin() { return __stdin; }
    macro CFile stdout() { return __stdout; }
    macro CFile stderr() { return __stderr; }
$case OsType.MACOSX:
    extern CFile __stdinp;
    extern CFile __stdoutp;
    extern CFile __stderrp;
    macro CFile stdin() { return __stdinp; }
    macro CFile stdout() { return __stdoutp; }
    macro CFile stderr() { return __stderrp; }
$case OsType.WIN32:
    extern fn CFile __acrt_iob_func(CInt c);
    macro CFile stdin() { return __acrt_iob_func(0); }
    macro CFile stdout() { return __acrt_iob_func(1); }
    macro CFile stderr() { return __acrt_iob_func(2); }
$default:
$endswitch;

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

define ErrnoType = CInt;
define SeekIndex = CLong;

extern fn int fclose(CFile stream);
extern fn void clearerr(CFile stream);
extern fn int feof(CFile stream);
extern fn int ferror(CFile stream);
extern fn int fflush(CFile stream);
extern fn int fgetpos(CFile stream, Fpos* pos);
extern fn CFile fopen(char* filename, char* mode);
extern fn usize fread(void* ptr, usize size, usize nmemb, CFile stream);
extern fn CFile freopen(char* filename, char* mode, CFile stream);
extern fn int fseek(CFile stream, SeekIndex offset, int whence);
extern fn int fsetpos(CFile stream, Fpos* pos);
extern fn SeekIndex ftell(CFile stream);
extern fn usize fwrite(void* ptr, usize size, usize nmemb, CFile stream);
extern fn int remove(char* filename);
extern fn int rename(char* old_name, char* new_name);
extern fn void rewind(CFile stream);
extern fn void setbuf(CFile stream, char* buffer);
extern fn void setvbuf(CFile stream, char* buffer, int mode, usize size);
extern fn CFile tmpnam(char* str);
extern fn int fprintf(CFile stream, char* format, ...);
extern fn int printf(char* format, ...);
extern fn int sprintf(char* str, char* format, ...);
extern fn int fscanf(CFile stream, char* format, ...);
extern fn int scanf(char* format, ...);
extern fn int sscanf(char* str, char* format, ...);
extern fn int fgetc(CFile stream);
extern fn char* fgets(char* str, int n, CFile stream);
extern fn int fputc(int c, CFile stream);
extern fn int getc(CFile stream);
extern fn int getchar();
extern fn int putc(char c, CFile stream);
extern fn int putchar(int c);
extern fn int puts(char* str);
extern fn int ungetc(int c, CFile stream);
extern fn void perror(char* str);

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
extern fn usize strftime(char* str, usize maxsize, char* format, Tm *timeptr);
extern fn Time time(Time *timer);

// signal
define SignalFunction = fn void(int);
extern fn SignalFunction signal(int sig, SignalFunction function);
// Incomplete