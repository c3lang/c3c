module libc;

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

define TerminateFunction = fn void();
define CompareFunction = fn int(void*, void*);
extern fn double atof(char* str);
extern fn int atoi(char* str);
extern fn long atol(char* str);
extern fn double strtod(char* str, char** endptr);
extern fn long strtol(char* str, char** endptr, int base);
extern fn ulong stroul(char* str, char** endptr, int base);
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
extern fn char* strerror(int errnum);
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

$if (${C_LONG_SIZE} == 64):
define SeekIndex = long;
$else:
define SeekIndex = int;
$endif;

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

$if (${C_LONG_SIZE} == 64):
define TimeOffset = long;
$else:
define TimeOffset = int;
$endif;

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