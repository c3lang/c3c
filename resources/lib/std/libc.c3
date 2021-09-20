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

define TerminateFunction = func void();
define CompareFunction = func int(void*, void*);
extern func double atof(char* str);
extern func long atol(char* str);
extern func double strtod(char* str, char** endptr);
extern func long strtol(char* str, char** endptr, int base);
extern func ulong stroul(char* str, char** endptr, int base);
extern func void abort();
extern func void atexit(TerminateFunction f);
extern func void exit(int status);
extern func char* getenv(char* name);
extern func int system(char* str);
extern func void bsearch(void* key, void *base, usize items, usize size, CompareFunction compare);
extern func void qsort(void* base, usize items, usize size, CompareFunction compare);
extern func int abs(int x);
extern func DivResult div(int numer, int denom);
extern func long labs(long x);
extern func LongDivResult ldiv(long number, long denom);
extern func int rand();
extern func void srand(uint seed);

// MB functions omitted

// string
extern func void* memchr(void* str, int c, usize n);
extern func int memcmp(void* str1, void* str2, usize n);
extern func void* memcpy(void* dest, void* src, usize n);
extern func void* memmove(void* dest, void* src, usize n);
extern func void* memset(void* dest, usize n);
extern func char* strcat(char* dest, char* src);
extern func char* strncat(char* dest, char* src, usize n);
extern func char* strchr(char* str, int c);
extern func int strcmp(char* str1, char* str2);
extern func int strncmp(char* str1, char* str2, usize n);
extern func int strcoll(char* str1, char* str2);
extern func char* strcpy(char* dst, char* src);
extern func char* strncpy(char* dst, char* src, usize n);
extern func usize strcspn(char* str1, char* str2);
extern func char* strerror(int errnum);
extern func usize strlen(char* str);
extern func char* strpbrk(char* str1, char* str2);
extern func usize strspn(char* str1, char* str2);
extern func char* strstr(char* haystack, char* needle);
extern func char* strtok(char* str, char* delim);
extern func usize strxfrm(char* dest, char* src, usize n);

// malloc
extern func void* malloc(usize size);
extern func void* calloc(usize count, usize size);
extern func void* free(void*);
extern func void* realloc(void* ptr, usize size);

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

extern func int fclose(CFile stream);
extern func void clearerr(CFile stream);
extern func int feof(CFile stream);
extern func int ferror(CFile stream);
extern func int fflush(CFile stream);
extern func int fgetpos(CFile stream, Fpos* pos);
extern func CFile fopen(char* filename, char* mode);
extern func usize fread(void* ptr, usize size, usize nmemb, CFile stream);
extern func CFile freopen(char* filename, char* mode, CFile stream);
extern func int fseek(CFile stream, long offset, int whence);
extern func int fsetpos(CFile stream, Fpos* pos);
extern func long ftell(CFile stream);
extern func usize fwrite(void* ptr, usize size, usize nmemb, CFile stream);
extern func int remove(char* filename);
extern func int rename(char* old_name, char* new_name);
extern func void rewind(CFile stream);
extern func void setbuf(CFile stream, char* buffer);
extern func void setvbuf(CFile stream, char* buffer, int mode, usize size);
extern func CFile tmpnam(char* str);
extern func int fprintf(CFile stream, char* format, ...);
extern func int printf(char* format, ...);
extern func int sprintf(char* str, char* format, ...);
extern func int fscanf(CFile stream, char* format, ...);
extern func int scanf(char* format, ...);
extern func int sscanf(char* str, char* format, ...);
extern func int fgetc(CFile stream);
extern func char* fgets(char* str, int n, CFile stream);
extern func int fputc(int c, CFile stream);
extern func int getc(CFile stream);
extern func int getchar();
extern func int putc(char c, CFile stream);
extern func int putchar(int c);
extern func int puts(char* str);
extern func int ungetc(int c, CFile stream);
extern func void perror(char* str);

// vsprintf vprintf not supported

// time.h

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
	long tm_gmtoff;	/* offset from UTC in seconds */
    char *tm_zone;	/* timezone abbreviation */
}

// Likely wrong, must be per platform.
const CLOCKS_PER_SEC = 1000000;

// Time also needs to be per platform
define Time = long;
define Clock = ulong;

extern func char* asctime(Tm *timeptr);
extern func Clock clock();
extern func char* ctime(Time *timer);
extern func double difftime(Time time1, Time time2);
extern func Tm* gmtime(Time *timer);
extern func Tm* localtime(Time *timer);
extern func Time mktime(Tm *timeptr);
extern func usize strftime(char* str, usize maxsize, char* format, Tm *timeptr);
extern func Time time(Time *timer);

// signal
define SignalFunction = func void(int);
extern func SignalFunction signal(int sig, SignalFunction fn);
// Incomplete