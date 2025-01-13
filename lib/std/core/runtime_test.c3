// Copyright (c) 2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;
import std::os::argparse;
import std::core::test @public;
import libc, std::time, std::io, std::sort;

struct TestUnit @private
{
	String name;
	TestFn func;
}

//
//                  TEST RUNNER
//
fn TestUnit[] test_collection_create(Allocator allocator = allocator::heap()) 
{
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
	if(test::test_context.is_in_panic){
		return;
	}
	test::test_context.is_in_panic = true;

	unmute_output(true);
	(void)io::stdout().flush();
	if (test::test_context.assert_print_backtrace){
		$if env::NATIVE_STACKTRACE:
    		builtin::print_backtrace(message, 0);
		$endif
	}
	io::printf("\n[%sFAIL%s] ^^^ ( %s:%s ) %s\n", 
		test::test_context.has_ansi_codes ? "\e[0;31m" : "",
		test::test_context.has_ansi_codes ? "\e[0m" : "",
		file, 
		line,
		message);
	test::test_context.assert_print_backtrace = true;

	if (test::test_context.breakpoint_on_assert) {
		breakpoint();
	}

	if (test::test_context.teardown_fn){
		test::test_context.teardown_fn();
	}

	test::test_context.is_in_panic = false;
	libc::longjmp(&test::test_context.buf, 1);
}

fn void mute_output() @local  {
	if(!test::test_context.fake_stdout.file) {
		return;
	}
	assert(!test::test_context.orig_stderr.file);
	assert(!test::test_context.orig_stdout.file);


	File* stdout = io::stdout();
	File* stderr = io::stderr();

	test::test_context.orig_stderr = *stderr;
	test::test_context.orig_stdout = *stdout;

	*stderr = test::test_context.fake_stdout;
	*stdout = test::test_context.fake_stdout;

	(void)test::test_context.fake_stdout.seek(0, Seek.SET)!!;
}

fn void unmute_output(bool has_error) @local {
	if(!test::test_context.fake_stdout.file) {
		return;
	}
	assert(test::test_context.orig_stderr.file);
	assert(test::test_context.orig_stdout.file);


	File* stdout = io::stdout();
	File* stderr = io::stderr();

	*stderr = test::test_context.orig_stderr;
	*stdout = test::test_context.orig_stdout;
	test::test_context.orig_stderr.file = null;
	test::test_context.orig_stdout.file = null;

	usz log_size = test::test_context.fake_stdout.seek(0, Seek.CURSOR)!!;

	if (has_error && log_size > 0) {
		test::test_context.fake_stdout.write_byte('\n')!!;
		test::test_context.fake_stdout.write_byte('\0')!!;
		(void)test::test_context.fake_stdout.seek(0, Seek.SET)!!;

		io::printfn("\n========== TEST LOG ============");
		io::printfn("%s\n", test::test_context.current_test_name);
		while(true){
			char! c = test::test_context.fake_stdout.read_byte();
			if(catch c) {
				break; // EOF
			}
			if (@unlikely(c == '\0')){
				// ignore junk from previous tests 
				break;
			}
			libc::putchar(c);			
		}
		io::printf("========== TEST END ============");
	}
	(void)stdout.flush();
}

fn bool run_tests(String[] args, TestUnit[] tests) @private
{
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
	test::test_context = &context;
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
	$if (!env::NO_LIBC):
		test::test_context.fake_stdout.file = libc::tmpfile();
	$endif
	if (test::test_context.fake_stdout.file == null) {
		io::print("Failed to hijack stdout, tests will print everything");
	}

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
		if (test::test_context.test_filter && !unit.name.contains(test::test_context.test_filter)){
			tests_skipped++;
			continue;
		}
		test::test_context.setup_fn = null;
		test::test_context.teardown_fn = null;
		test::test_context.current_test_name = unit.name;

		defer name.clear();
		name.appendf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		if (libc::setjmp(&context.buf) == 0)
		{
			mute_output();
			$if(!$$OLD_TEST):
				unit.func();
			$else
				if (catch err = unit.func())
				{
					io::printf("[FAIL] Failed due to: %s", err);
					continue;
				}
			$endif
			unmute_output(false); // all good, discard output

			io::printfn("[%sPASS%s]", 
						test::test_context.has_ansi_codes ? "\e[0;32m" : "",
						test::test_context.has_ansi_codes ? "\e[0m" : "");
			tests_passed++;

			if (test::test_context.teardown_fn){
				test::test_context.teardown_fn();
			}
		}
	}
	io::printfn("\n%d test%s run.\n", test_count-tests_skipped, test_count > 1 ? "s" : "");

	int n_failed = test_count - tests_passed - tests_skipped;
	io::printf("Test Result: %s%s%s: ",
				test::test_context.has_ansi_codes ? (n_failed ? "\e[0;31m" : "\e[0;32m") : "",
				n_failed ? "FAILED" : "PASSED",
				test::test_context.has_ansi_codes ? "\e[0m" : "",
				);

	io::printfn("%d passed, %d failed, %d skipped.",
				tests_passed,
				n_failed,
				tests_skipped);

	// cleanup fake_stdout file
	if(test::test_context.fake_stdout.file) libc::fclose(test::test_context.fake_stdout.file);
	test::test_context.fake_stdout.file = null;

	return n_failed == 0;
}

fn bool default_test_runner(String[] args)
{
	@pool()
	{
		assert(test::test_context == null, "test suite is already running");
		return run_tests(args, test_collection_create(allocator::temp()));
	};
}

