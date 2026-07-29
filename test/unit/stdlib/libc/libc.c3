module libc_tests;
import libc;
import std::io;

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
	$assert $Typeof(libc::abs(x)) == CInt;
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

fn void asctime_r() @test @feat(!WIN32)
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
	$assert $Typeof(libc::atof("123.45")) == double;
	assert(libc::atof("-3.14") == -3.14);
	assert(libc::atof("x") == 0.0);
	assert(libc::atof("") == 0.0);
}

fn void atoi() @test
{
	assert(libc::atoi("123") == 123);
	$assert $Typeof(libc::atoi("123")) == CInt;
	assert(libc::atoi("-3.14") == -3);
	assert(libc::atoi("x") == 0);
	assert(libc::atoi("") == 0);
}

fn void atoll() @test
{
	assert(libc::atoll("123") == 123);
	assert(libc::atoll("-3.14") == -3);
	$assert $Typeof(libc::atoll("-3.14")) == CLongLong;
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
	CInt* found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt::size, &compare_cint);
	assert(*found == 0);
	key = 1;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt::size, &compare_cint);
	assert(*found == 1);
	key = 2;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt::size, &compare_cint);
	assert(*found == 2);
	key = 5;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt::size, &compare_cint);
	assert(*found == 5);
	key = 6;
	found = (CInt*) libc::bsearch(&key, int_ar, 7, CInt::size, &compare_cint);
	assert(*found == 6);
	CInt non_existent_key = 12;
	found = (CInt*) libc::bsearch(&non_existent_key, int_ar, 7, CInt::size, &compare_cint);
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
	Event* found1 = (Event*) libc::bsearch(&searching, events, 7, Event::size, &compare_event);
	assert(found1.desc == "Begin of May");
	searching = { .day = 4, .month = 4 };
	found1 = (Event*) libc::bsearch(&searching, events, 7, Event::size, &compare_event);
	assert(found1 == null);
}

fn void calloc() @test
{
	sz amount = 10;
	CInt* int_ar = libc::calloc((usz)amount, CInt::size);
	for (sz i = 0; i < amount; i++)
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

fn void close() @test @feat(!WIN32)
{
	assert(libc::close(1313) != 0);
	// NOTE: errno is part of libc, and is thus recursively imported
	assert(libc::errno() == EBADF);
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

fn void fdopen() @test @feat(!WIN32)
{
	CFile stdin = libc::fdopen(-1, "w");
	assert(libc::errno() == EBADF);
}

fn void fflush() @test
{
	CFile stdout = libc::stdout();
	assert(libc::fflush(stdout) == 0);
}

fn void fgets_fget() @test
{
	CFile* tmpf = libc::tmpfile();
	// Example from cppreference.com
	ZString test_string = (ZString) "cppreference.com\n" +++ '\0';
	libc::rewind(tmpf);
	libc::fclose(tmpf);
}

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


fn void malloc_free() @test
{
	CInt* ar = libc::malloc(3 * CInt::size);
	ar[0] = 10;
	ar[1] = -10;
	ar[2] = 4;
	assert(ar[0] == 10);
	assert(ar[1] == -10);
	assert(ar[2] == 4);
	libc::free(ar);
}
