<*
Unit test module

This module provides a toolset of macros for running unit test checks 

Example:
```c3
module sample::m;
import std::io;

fault MathError
{
    DIVISION_BY_ZERO
}

fn double! divide(int a, int b)
{
    if (b == 0) return MathError.DIVISION_BY_ZERO?;
    return (double)(a) / (double)(b);
}

fn void! test_div() @test {
    test::equal(2, divide(6, 3)!);
    test::not_equal(1, 2);
    test::almost_equal(m::divide(1, 3)!, 0.333, places: 3);
    test::greater_equal(3, 3);
    test::greater(2, divide(3, 3)!);
    test::less(2, 3);
    test::less_equal(2, 3);
    test::@check(2 == 2, "divide: %d", divide(6, 3)!);
    test::@error(m::divide(3, 0), MathError.DIVISION_BY_ZERO);
}

```
*>
// Copyright (c) 2025 Alex Veden <i@alexveden.com>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::test;
import std::math;
import std::core::builtin;
import std::os::argparse;
import libc; 
import std::time;
import std::io;
import std::sort;

def TestFn = fn void!() @if($$OLD_TEST);
def TestFn = fn void() @if(!$$OLD_TEST);

struct TestUnit @private
{
	String name;
	TestFn func;
}

struct TestContext @private
{
	JmpBuf buf;
	// Allows filtering test cased or modules by substring, e.g. 'foo::', 'foo::test_add'  
	String test_filter;
	// Triggers debugger breakpoint when assert or test:: checks failed
	bool breakpoint_on_assert;

	// internal state
	bool assert_print_backtrace;
	bool has_ansi_codes;
	bool is_in_panic;
	String current_test_name;
	TestFn setup_fn;
	TestFn teardown_fn;

			
	char* error_buffer;
	usz error_buffer_capacity;
	File fake_buffer;
	File orig_stdout;
	File orig_stderr;
}

TestContext* test_context @private;

<*
Initializes test case context.

@param setup_fn `initializer function for test case`
@param teardown_fn `cleanup function for test context (may be null)`

@require setup_fn != null, "setup_fn must always be set"
*>
macro setup(TestFn setup_fn, TestFn teardown_fn = null) {
	$assert(env::TESTING);
	test_context.setup_fn = setup_fn;
	test_context.teardown_fn = teardown_fn;
	test_context.setup_fn();
}

<*
Checks condition and fails assertion if not true 

@param #condition `any boolean condition, will be expanded by text`
@param format `printf compatible format`
@param args `vargs for format`
*>
macro @check(#condition, format = "", args...) { 
	$assert(env::TESTING);
    if(!#condition) {
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.new_init(allocator: allocator);
			s.appendf("check `%s` failed. ", $stringify(#condition));
			s.appendf(format, ...args);
    		test_context.assert_print_backtrace = false;
	    	builtin::panicf(s.str_view(), $$FILE, $$FUNC, $$LINE);
		};
    }
}

<*
Check if function returns specific error

@param #funcresult `result of function execution`
@param error_expected `expected error of function execution`
*>
macro @error(#funcresult, error_expected) {
	$assert(env::TESTING);
    if(catch err = #funcresult) {
    	if (err != error_expected){
    		test_context.assert_print_backtrace = false;
	    	builtin::panicf("`%s` expected to return error [%s], got [%s]",
	    					$$FILE, $$FUNC, $$LINE, 
	    					$stringify(#funcresult), error_expected, err);
    	}
    } else {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` error [%s] was not returned.", 
	    				$$FILE, $$FUNC, $$LINE,
	    				$stringify(#funcresult), error_expected);
    }
}

<*
Check if left == right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro equal(left, right) { 
	$assert(env::TESTING);
    if(!builtin::equals(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` != `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check left floating point value is approximately equals to right value

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@param places `number of decimal places to compare (default: 7)`
@param delta `minimal allowed difference (overrides places parameter)`
@param equal_nan `allows comparing nan values, if left and right both nans result is ok`

@require places > 0, places <= 20, "too many decimal places"
@require delta >= 0, delta <= 1, "delta must be a small number"
*>
macro almost_equal(left, right, uint places=7, double delta=0, bool equal_nan = true) { 
	$assert(env::TESTING);
	double diff = (double)left - (double)right;
	double eps = delta;
	if (eps == 0){
		eps = 1.0 / math::pow(10.0, places);
	} 

    if(!math::is_approx(left, right, eps)){
    	if(equal_nan && math::is_nan(left) && math::is_nan(right)) {
    		return;
    	}
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("Not almost equal: `%s` !~~ `%s` delta=%e diff: %e", $$FILE, $$FUNC, $$LINE, left, right, eps, diff);
    }
}

<*
Check if left != right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro not_equal(left, right) { 
	$assert(env::TESTING);
    if(builtin::equals(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` == `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left > right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro greater(left, right) { 
	$assert(env::TESTING);
    if(!builtin::greater(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` <= `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left >= right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro greater_equal(left, right) { 
	$assert(env::TESTING);
    if(!builtin::greater_eq(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` < `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left < right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro less(left, right) { 
	$assert(env::TESTING);
    if(!builtin::less(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` >= `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left <= right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
*>
macro less_equal(left, right) { 
	$assert(env::TESTING);
    if(!builtin::less_eq(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` > `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

//
//                  TEST RUNNER
//
fn TestUnit[] test_collection_create(Allocator allocator = allocator::heap()) 
{
	$assert(env::TESTING);
	TestFn[] fns = $$TEST_FNS;
	String[] names = $$TEST_NAMES;
	TestUnit[] tests = allocator::alloc_array(allocator, TestUnit, names.len);
	foreach (i, test : fns)
	{
		tests[i] = { names[i], fns[i] };
	}
	return tests;
}

// Sort the tests by their name in ascending order.
fn int cmp_test_unit(TestUnit a, TestUnit b) @private
{
	$assert(env::TESTING);
	usz an = a.name.len;
	usz bn = b.name.len;
	if (an > bn) @swap(a, b);
	foreach (i, ac : a.name)
	{
		char bc = b.name[i];
		if (ac != bc) return an > bn ? bc - ac : ac - bc;
	}
	return (int)(an - bn);
}

fn bool terminal_has_ansi_codes() @local {
	// TODO: implement terminal ansi code support check, currently colors can be enabled manually
	return false;
}

fn void test_panic(String message, String file, String function, uint line) @local
{
	$assert(env::TESTING);
	if(test_context.is_in_panic){
		return;
	}
	test_context.is_in_panic = true;

	unmute_output(true);
	(void)io::stdout().flush();
	if (test_context.assert_print_backtrace){
    	builtin::print_backtrace(message, 3);
	}
	io::printfn("[%sFAIL%s] %s ( %s:%s ) %s", 
		test_context.has_ansi_codes ? "\e[0;31m" : "",
		test_context.has_ansi_codes ? "\e[0m" : "",
		test_context.current_test_name, 
		file, 
		line,
		message);
	test_context.assert_print_backtrace = true;

	if (test_context.breakpoint_on_assert) {
		breakpoint();
	}

	if (test_context.teardown_fn){
		test_context.teardown_fn();
	}

	test_context.is_in_panic = false;
	libc::longjmp(&test_context.buf, 1);
}

fn void mute_output() @local {
	$assert(env::TESTING);
	assert(test_context.error_buffer != null);
	assert(test_context.error_buffer_capacity > 0);
	assert(!test_context.fake_buffer.file);
	assert(!test_context.orig_stderr.file);
	assert(!test_context.orig_stdout.file);

	test_context.fake_buffer = File {
		.file = libc::fmemopen(test_context.error_buffer, test_context.error_buffer_capacity, "r+w"),
	};

	File* stdout = io::stdout();
	File* stderr = io::stderr();

	test_context.orig_stderr = *stderr;
	test_context.orig_stdout = *stdout;

	*stderr = test_context.fake_buffer;
	*stdout = test_context.fake_buffer;
}

fn void unmute_output(bool has_error) @local {
	$assert(env::TESTING);
	assert(test_context.fake_buffer.file);
	assert(test_context.orig_stderr.file);
	assert(test_context.orig_stdout.file);


	File* stdout = io::stdout();
	File* stderr = io::stderr();

	*stderr = test_context.orig_stderr;
	*stdout = test_context.orig_stdout;
	test_context.orig_stderr.file = null;
	test_context.orig_stdout.file = null;

	usz log_size = test_context.fake_buffer.seek(0, Seek.CURSOR)!!;

	if (has_error && log_size > 0) {
		test_context.fake_buffer.write_byte('\n')!!;
		test_context.fake_buffer.write_byte('\0')!!;
		(void)test_context.fake_buffer.seek(0, Seek.SET)!!;

		io::printfn("========== TEST LOG ============");
		io::printfn("%s\n", test_context.current_test_name);
		while(true){
			char! c = test_context.fake_buffer.read_byte();
			if(catch c) {
				break; // EOF
			}
			if (@unlikely(c == '\0')){
				// ignore junk from previous tests 
				break;
			}
			libc::putchar(c);			
		}
		io::printfn("========== TEST END ============");
	}
	(void)stdout.flush();
	libc::fclose(test_context.fake_buffer.file);
	test_context.fake_buffer.file = null;
}

fn bool run_tests(String[] args, TestUnit[] tests) @private
{
	$assert(env::TESTING);

	usz max_name;
	foreach (&unit : tests)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}
	TestContext context = {
		.assert_print_backtrace = true,
		.breakpoint_on_assert = false,
		.test_filter = "",
		.has_ansi_codes = terminal_has_ansi_codes(),
	};
	bool sort_tests = true;
	test_context = &context;
	// io::printfn("args: %s", args);

	argparse::ArgParse agp = {
		.description = "c3c default test runner",
		.usage = " [options]",
		.options = { 
			argparse::group_opt("Test options"),
			argparse::help_opt(),
			{.short_name = 'f', .long_name = "filter", .value = &context.test_filter, 
				.help = "filter tests by name, e.g. some::test, or some"},
			{.short_name = 'b', .long_name = "breakpoint", .value = &context.breakpoint_on_assert,
				.help = "emits breakpoint if any assert() is failed"},
			{.long_name = "sort", .value = &sort_tests,
				.help = "sort all tests alphabetically by name, --no-sort - preserve natural order"},
			{.short_name = 'c', .long_name = "color", .value = &context.has_ansi_codes,
				.help = "test output with colors"},
		}, 
	};

	if(catch err = agp.parse(args)){
		agp.print_usage()!!;
		return false;
	}
	if (agp.arguments.len != 0){
		io::printfn("Error: no extra arguments allowed, got: %s", agp.arguments);
		return false;
	}

	if (sort_tests) {
		quicksort(tests, &cmp_test_unit);
	}

	// Buffer for hijacking the output
	test_context.error_buffer_capacity = 1024 * 1024;
	test_context.error_buffer = libc::calloc(1, test_context.error_buffer_capacity);
	assert(test_context.error_buffer, "out of mem");
	defer libc::free(test_context.error_buffer);

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
	int tests_skipped = 0;
	int test_count = tests.len;
	DString name = dstring::temp_with_capacity(64);
	usz len = max_name + 9;
	name.append_repeat('-', len / 2);
	name.append(" TESTS ");
	name.append_repeat('-', len - len / 2);
	io::printn(name);
	name.clear();
	foreach(unit : tests)
	{
		if (test_context.test_filter && !unit.name.contains(test_context.test_filter)){
			tests_skipped++;
			continue;
		}
		test_context.setup_fn = null;
		test_context.teardown_fn = null;

		test_context.current_test_name = unit.name;
		if (libc::setjmp(&context.buf) == 0)
		{
			mute_output();
			unit.func();
			unmute_output(false); // all good, discard output

			io::printfn("[%sPASS%s] %s", 
						test_context.has_ansi_codes ? "\e[0;32m" : "",
						test_context.has_ansi_codes ? "\e[0m" : "",
						unit.name);
			tests_passed++;

			if (test_context.teardown_fn){
				test_context.teardown_fn();
			}
		}
	}
	io::printfn("\n%d test%s run.\n", test_count-tests_skipped, test_count > 1 ? "s" : "");

	int n_failed = test_count - tests_passed - tests_skipped;
	io::printf("Test Result: %s%s%s: ",
				test_context.has_ansi_codes ? (n_failed ? "\e[0;31m" : "\e[0;32m") : "",
				n_failed ? "FAILED" : "PASSED",
				test_context.has_ansi_codes ? "\e[0m" : "",
				);

	io::printfn("%d passed, %d failed, %d skipped.",
				tests_passed,
				n_failed,
				tests_skipped);

	return n_failed == 0;
}

fn bool default_test_runner(String[] args)
{
	$assert(env::TESTING);
	@pool()
	{
		assert(test_context == null, "test suite is already running");
		return run_tests(args, test_collection_create(allocator::temp()));
	};
}

