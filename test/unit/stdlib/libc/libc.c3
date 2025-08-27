module libc_tests;
import libc;

// Functions from libc.c3 which do not have a test are still present in
// comments here in the same order that they occur within libc.c3

fn void abort() @test
{
	// Cannot actually test this, so just checking if it's defined.
	assert($defined(libc::abort));
}

fn void abs() @test
{
	CInt x = 21;
	assert(libc::abs(x) == 21);
	assert(libc::abs(-x) == 21);
	$assert $typeof(libc::abs(x)) == CInt;
}

fn void asctime() @test
{
	Tm time = {
		.tm_sec = 13,
		.tm_min = 42,
		.tm_hour = 3,
		.tm_mday = 5,
		.tm_mon = 8,
		.tm_year = 11,
		.tm_wday = 1,
		.tm_yday = 41,
		.tm_isdst = 0
	};
	ZString formatted_time = libc::asctime(&time);
	assert(libc::strcmp(formatted_time, "Mon Sep  5 03:42:13 1911\n") == 0);
}

fn void asctime_r() @test @if(!env::WIN32)
{
	Tm time = {
		.tm_sec = 13,
		.tm_min = 42,
		.tm_hour = 3,
		.tm_mday = 5,
		.tm_mon = 8,
		.tm_year = 11,
		.tm_wday = 1,
		.tm_yday = 41,
		.tm_isdst = 0
	};
	char[26] formatted_time;
	libc::asctime_r(&time, &formatted_time);
	assert(libc::strcmp((ZString) &formatted_time, "Mon Sep  5 03:42:13 1911\n") == 0);
}

fn void test_atexit_function()
{
	assert(5 == 5);
	// This correctly asserts, but does not fail the tests.
	// char c = 2;
	// assert(c == 5);
}

fn void atexit() @test
{
	// No idea how to test this, but we can at least check if this compiles
	libc::atexit(&test_atexit_function);
	libc::atexit(fn void() => 4);
}

fn void atof() @test
{
	assert(libc::atof("123.45") == 123.45);
	$assert $typeof(libc::atof("123.45")) == double;
	assert(libc::atof("-3.14") == -3.14);
	assert(libc::atof("x") == 0.0);
	assert(libc::atof("") == 0.0);
}

fn void atoi() @test
{
	assert(libc::atoi("123") == 123);
	$assert $typeof(libc::atoi("123")) == CInt;
	assert(libc::atoi("-3.14") == -3);
	assert(libc::atoi("x") == 0);
	assert(libc::atoi("") == 0);
}

fn void atoll() @test
{
	assert(libc::atoll("123") == 123);
	assert(libc::atoll("-3.14") == -3);
	$assert $typeof(libc::atoll("-3.14")) == CLongLong;
	assert(libc::atoll("x") == 0);
	assert(libc::atoll("") == 0);
}

// Compare function, struct and function for libc::bsearch test
fn int compare_cint(void* a, void* b)
{
	CInt a_i = *((CInt*) a);
	CInt b_i = *((CInt*) b);
	return a_i - b_i;
}

struct Event
{
	CInt day;
	CInt month;
	String desc;
}

fn int compare_event(void* a, void* b)
{
  Event* a_e = a; // Avoid casts if possible
  Event* b_e = b;
  return (a_e.month - b_e.month) * 100 + a_e.day - b_e.day;
}

fn void bsearch() @test
{
	CInt[] int_ar = { 0, 1, 2, 3, 4, 5, 6 };
	CInt key = 0;
	CInt* found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(*found == 0);
	key = 1;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(*found == 1);
	key = 2;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(*found == 2);
	key = 5;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(*found == 5);
	key = 6;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(*found == 6);
	CInt non_existant_key = 12;
	found = (CInt*) libc::bsearch(&non_existant_key, int_ar, 7, CInt.sizeof, &compare_cint);
	assert(found == null);

	Event[] events = {
		{ 2, 1, "2nd of February" },
		{ 5, 1, "Some day" },
		{ 1, 5, "Begin of May" },
		{ 1, 7, "Summer holiday start" },
		{ 21, 7, "A fine warm day" },
		{ 11, 11, "Peace" },
		{ 25, 12, "Family and sharing" },
		{ 31, 12, "End of the year" }
	};
	Event searching = { .day = 1, .month = 5 };
	Event* found1 = (Event*) libc::bsearch(&searching, events, 7, Event.sizeof, &compare_event);
	assert(found1.desc == "Begin of May");
	searching = { .day = 4, .month = 4 };
	found1 = (Event*) libc::bsearch(&searching, events, 7, Event.sizeof, &compare_event);
	assert(found1 == null);
}

fn void calloc() @test
{
	usz amount = 10;
	CInt* int_ar = libc::calloc(amount, CInt.sizeof);
	for (usz i = 0; i < amount; i++)
	{
		assert(int_ar[i] == 0);
	}
	libc::free(int_ar);
}

fn void clearerr_feof_ferror() @test
{
	CFile* tmpf = libc::tmpfile();
	// Example from cppreference.com
	ZString test_string = (ZString) "cppreference.com\n" +++ '\0';
	libc::fputs(test_string, tmpf);
	libc::rewind(tmpf);

	for (int i = 0; i < test_string.len(); i++)
	{
		int c = libc::fgetc(tmpf);
		assert(c != libc::EOF);
		assert(c == test_string[i]);
	}

	assert(libc::fgetc(tmpf) == libc::EOF);
	assert(libc::feof(tmpf) != 0);
	// there should be no errors (EOF is not an error)
	assert(libc::ferror(tmpf) == 0);
	libc::clearerr(tmpf);
	assert(libc::feof(tmpf) == 0);
}

fn void clock() @test
{
	Clock_t time1 = libc::clock();
	if (time1 < 0)
	{
		// If getting processor-time is not available, do nothing
		return;
	}
	// Waist bit of time
	int i = 0;
	for (int j = 0; j <= 1000; j++)
	{
		i += j;
	}
	assert(i == (1000 / 2) * (1000 + 1)); // Gauss, prevent compiler-optimisation
	Clock_t time2 = libc::clock();
	assert(time2 >= time1);
}

fn void close() @test @if(!env::WIN32)
{
	assert(libc::close(13) == -1);
	// NOTE: errno is part of libc, and is thus recursively imported
	assert(libc::errno() == errno::EBADF);
}

fn void difftime() @test
{
	Time_t t1 = 1293;
	Time_t t2 = 919919;
	assert(libc::difftime(t2, t1) > 0);
	assert(libc::difftime(t1, t2) < 0);
}

fn void div() @test
{
	DivResult res = libc::div(13, 2);
	assert(res.quot == 6);
	assert(res.rem == 1);
	res = libc::div(-5, 3);
	assert(res.quot == -1);
	assert(res.rem == -2);
}

fn void exit() @test
{
	// Cannot actually test this, so just checking if it's defined.
	assert($defined(libc::exit));
}

fn void fclose() @test
{
	CFile random_file = libc::tmpfile();
	assert(libc::fclose(random_file) == 0);
}

fn void fdopen() @test @if(!env::WIN32)
{
	CFile stdin = libc::fdopen(-1, "w");
	assert(libc::errno() == errno::EBADF);
}

fn void fflush() @test
{
	CFile stdin = libc::stdin();
	assert(libc::fflush(stdin) == 0);
}

fn void fgets_fget() @test
{
	CFile* tmpf = libc::tmpfile();
	// Example from cppreference.com
	ZString test_string = (ZString) "cppreference.com\n" +++ '\0';
	libc::rewind(tmpf);
	libc::fclose(tmpf);
}

import std::io;
fn void fseek_ftell_fgetpos_fsetpos_rewind() @test
{
	CFile* tmpf = libc::tmpfile();
	// Example from cppreference.com
	ZString test_string = (ZString) "cppreference.com\n" +++ '\0';
	libc::fputs(test_string, tmpf);
	libc::rewind(tmpf);

	Fpos_t pos;
	libc::fgetpos(tmpf, &pos);
	assert(libc::fgetc(tmpf) == 'c');
	assert(libc::fgetc(tmpf) == 'p');

	libc::fsetpos(tmpf, &pos);
	assert(libc::fgetc(tmpf) == 'c');
	assert(libc::fgetc(tmpf) == 'p');

	assert(libc::ftell(tmpf) == 2);

	libc::fseek(tmpf, -2, libc::SEEK_END);
	assert(libc::fgetc(tmpf) == 'm');
	assert(libc::fgetc(tmpf) == '\n');

	libc::fclose(tmpf);
}

// fn void fileno() @test {}
// fn void fopen() @test {}
// fn void fprintf() @test {}
// fn void fputc() @test {}
// fn void fputs() @test {}
// fn void fread() @test {}
// fn void freopen() @test {}
// fn void fscanf() @test {}
// fn void fseek() @test {}
// fn void ftell() @test {}
// fn void fwrite() @test {}
// fn void getc() @test {}
// fn void getchar() @test {}
// fn void getenv() @test {}
// fn void gets() @test {}
// fn void gmtime() @test {}
// fn void gmtime_r() @test {}
// fn void isatty() @test {}
// fn void labs() @test {}
// fn void ldiv() @test {}
// fn void localtime() @test {}
// fn void localtime_r() @test {}
// fn void longjmp() @test {}

fn void malloc_free() @test
{
	CInt* ar = libc::malloc(3 * CInt.sizeof);
	ar[0] = 10;
	ar[1] = -10;
	ar[2] = 4;
	assert(ar[0] == 10);
	assert(ar[1] == -10);
	assert(ar[2] == 4);
	libc::free(ar);
}

// fn void memchr() @test {}
// fn void memcmp() @test {}
// fn void memcpy() @test {}
// fn void memmove() @test {}
// fn void memset() @test {}
// fn void mktime() @test {}
// fn void perror() @test {}
// fn void printf() @test {}
// fn void putc() @test {}
// fn void putchar() @test {}
// fn void puts() @test {}
// fn void qsort() @test {}
// fn void raise() @test {}
// fn void rand() @test {}
// fn void read() @test {}
// fn void realloc() @test {}
// fn void remove() @test {}
// fn void rename() @test {}
// fn void scanf() @test {}
// fn void setbuf() @test {}
// fn void setenv() @test {}
// fn void setjmp() @test {}
// fn void setvbuf() @test {}
// fn void signal() @test {}
// fn void snprintf() @test {}
// fn void sprintf() @test {}
// fn void srand() @test {}
// fn void sscanf() @test {}
// fn void strcat() @test {}
// fn void strchr() @test {}
// fn void strcmp() @test {}
// fn void strcoll() @test {}
// fn void strcspn() @test {}
// fn void strcpy() @test {}
// fn void strdup() @test {}
// fn void strerror() @test {}
// fn void strftime() @test {}
// fn void strlen() @test {}
// fn void strncat() @test {}
// fn void strncmp() @test {}
// fn void strncpy() @test {}
// fn void stroul() @test {}
// fn void strpbrk() @test {}
// fn void strspn() @test {}
// fn void strptime() @test {}
// fn void strrchr() @test {}
// fn void strstr() @test {}
// fn void strtod() @test {}
// fn void strtof() @test {}
// fn void strtok() @test {}
// fn void strtol() @test {}
// fn void strtoul() @test {}
// fn void strxfrm() @test {}
// fn void system() @test {}
// fn void timegm() @test {}
// fn void tmpnam() @test {}
// fn void tmpfile() @test {}
// fn void ungetc() @test {}
// fn void unsetenv() @test {}
// fn void write() @test {}
//
// extern fn CFile fmemopen(void* ptr, usz size, ZString mode);
// extern fn isz getline(char** linep, usz* linecapp, CFile stream);
// extern fn CInt timespec_get(TimeSpec* ts, CInt base);
// extern fn CInt nanosleep(TimeSpec* req, TimeSpec* remaining);
// extern fn ZString ctime(Time_t *timer);
// extern fn Time_t time(Time_t *timer);
//
// const CInt STDIN_FD = 0;
// const CInt STDOUT_FD = 1;
// const CInt STDERR_FD = 2;
//
// module libc @if(env::LINUX);
// extern CFile __stdin @extern("stdin");
// extern CFile __stdout @extern("stdout");
// extern CFile __stderr @extern("stderr");
// extern fn usz malloc_usable_size(void* ptr);
// macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);
// extern fn void* aligned_alloc(usz align, usz size);
// macro CFile stdin() => __stdin;
// macro CFile stdout() => __stdout;
// macro CFile stderr() => __stderr;
//
// module libc @if(env::NETBSD || env::OPENBSD);
// extern fn int fcntl(CInt socket, int cmd, ...);
// extern fn int _setjmp(void*);
// macro int setjmp(void* ptr) => _setjmp(ptr);
// extern fn int _longjmp(void*, int);
// macro usz longjmp(void* ptr, CInt i) => _longjmp(ptr, i);
// extern fn usz malloc_size(void* ptr);
// extern fn void* aligned_alloc(usz align, usz size);
// macro CFile stdin() { return fdopen(0, "r"); }
// macro CFile stdout() { return fdopen(1, "w"); }
// macro CFile stderr() { return fdopen(2, "w"); }
//
// module libc @if(env::DARWIN || env::FREEBSD);
// extern CFile __stdinp;
// extern CFile __stdoutp;
// extern CFile __stderrp;
// extern fn usz malloc_size(void* ptr) @if(!env::FREEBSD);
// extern fn void* aligned_alloc(usz align, usz size);
// macro CFile stdin() => __stdinp;
// macro CFile stdout() => __stdoutp;
// macro CFile stderr() => __stderrp;
//
// module libc @if(env::FREEBSD);
// extern fn usz malloc_usable_size(void* ptr);
// macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);
//
// module libc @if(env::WIN32);
// macro usz malloc_size(void* ptr) => _msize(ptr);
// macro CFile stdin() => __acrt_iob_func(STDIN_FD);
// macro CFile stdout() => __acrt_iob_func(STDOUT_FD);
// macro CFile stderr() => __acrt_iob_func(STDERR_FD);
//
// module libc @if(env::LIBC && !env::WIN32 && !env::LINUX && !env::DARWIN && !env::BSD_FAMILY);
// macro CFile stdin() { return (CFile*)(uptr)STDIN_FD; }
// macro CFile stdout() { return (CFile*)(uptr)STDOUT_FD; }
// macro CFile stderr() { return (CFile*)(uptr)STDERR_FD; }
//
// module libc @if(!env::LIBC);
//
// fn void longjmp(JmpBuf* buffer, CInt value) @weak @extern("longjmp") @nostrip
// {
// 	unreachable("longjmp unavailable");
// }
//
// fn CInt setjmp(JmpBuf* buffer) @weak @extern("setjmp") @nostrip
// {
// 	unreachable("setjmp unavailable");
// }
//
// fn void* malloc(usz size) @weak @extern("malloc") @nostrip
// {
// 	unreachable("malloc unavailable");
// }
// fn void* calloc(usz count, usz size) @weak @extern("calloc") @nostrip
// {
// 	unreachable("calloc unavailable");
// }
// fn void* free(void*) @weak @extern("free")
// {
// 	unreachable("free unavailable");
// }
//
// fn void* realloc(void* ptr, usz size) @weak @extern("realloc") @nostrip
// {
// 	unreachable("realloc unavailable");
// }
//
// fn void* memcpy(void* dest, void* src, usz n) @weak @extern("memcpy") @nostrip
// {
// 	for (usz i = 0; i < n; i++) ((char*)dest)[i] = ((char*)src)[i];
// 	return dest;
// }
//
// fn void* memmove(void* dest, void* src, usz n) @weak @extern("memmove") @nostrip
// {
// 	return memcpy(dest, src, n) @inline;
// }
//
// fn void* memset(void* dest, CInt value, usz n) @weak @extern("memset") @nostrip
// {
// 	for (usz i = 0; i < n; i++) ((char*)dest)[i] = (char)value;
// 	return dest;
// }
//
// fn int fseek(CFile stream, SeekIndex offset, int whence) @weak @extern("fseek") @nostrip
// {
// 	unreachable("'fseek' not available.");
// }
// fn CFile fopen(ZString filename, ZString mode) @weak @extern("fopen") @nostrip
// {
// 	unreachable("'fopen' not available.");
// }
//
// fn CFile freopen(ZString filename, ZString mode, CFile stream) @weak @extern("fopen") @nostrip
// {
// 	unreachable("'freopen' not available.");
// }
//
// fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fwrite") @nostrip
// {
// 	unreachable("'fwrite' not available.");
// }
//
// fn usz fread(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fread") @nostrip
// {
// 	unreachable("'fread' not available.");
// }
//
// fn CFile fclose(CFile) @weak @extern("fclose") @nostrip
// {
// 	unreachable("'fclose' not available.");
// }
//
// fn int fflush(CFile stream) @weak @extern("fflush") @nostrip
// {
// 	unreachable("'fflush' not available.");
// }
//
// fn int fputc(int c, CFile stream) @weak @extern("fputc") @nostrip
// {
// 	unreachable("'fputc' not available.");
// }
//
// fn char* fgets(ZString str, int n, CFile stream) @weak @extern("fgets") @nostrip
// {
// 	unreachable("'fgets' not available.");
// }
//
// fn int fgetc(CFile stream) @weak @extern("fgetc") @nostrip
// {
// 	unreachable("'fgetc' not available.");
// }
//
// fn int feof(CFile stream) @weak @extern("feof") @nostrip
// {
// 	unreachable("'feof' not available.");
// }
//
// fn int putc(int c, CFile stream) @weak @extern("putc") @nostrip
// {
// 	unreachable("'putc' not available.");
// }
// fn int putchar(int c) @weak @extern("putchar") @nostrip
// {
// 	unreachable("'putchar' not available.");
// }
// fn int puts(ZString str) @weak @extern("puts") @nostrip
// {
// 	unreachable("'puts' not available.");
// }
//
// module libc;
