// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::testing;

define TestFn = fn void!();

fn void test_panic(char[] message, char[] file, char[] function, uint line)
{
	io::println("[error]");
	io::printfln("\n Error: %s", message);
	io::printfln("  - in %s %s:%s.\n", function, file, line);
}

fn bool __run_default_test_runner()
{
	TestFn[] test_fns = $$TEST_FNS;
	char[][] test_names = $$TEST_NAMES;

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;

	int tests_passed = 0;
	int tests = test_names.len;
	io::println("----- TESTS -----");
	foreach(i, char[] name : test_names)
	{
		io::printf("Testing %s ... ", name);
		if (catch err = test_fns[i]())
		{
			io::println("[failed]");
			continue;
		}
		io::println("[ok]");
		tests_passed++;
	}
	io::printfln("\n%d test(s) run.\n", tests);
	io::print("Test Result: ");
	if (tests_passed < tests)
	{
		io::print("FAILED");
	}
	else
	{
		io::print("ok");
	}
	io::printfln(". %d passed, %d failed.", tests_passed, tests - tests_passed);
	return tests == tests_passed;
}
