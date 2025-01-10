// Copyright (c) 2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;
import libc, std::time, std::io, std::sort;

def TestFn = fn void!() @if($$OLD_TEST);
def TestFn = fn void() @if(!$$OLD_TEST);

struct TestUnit
{
	String name;
	TestFn func;
}

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

struct TestContext
{
	JmpBuf buf;
}

// Sort the tests by their name in ascending order.
fn int cmp_test_unit(TestUnit a, TestUnit b)
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

TestContext* test_context @private;

fn void test_panic(String message, String file, String function, uint line)
{
	io::printn("[error]");
	io::print("\n Error: ");
	io::print(message);
	io::printn();
	io::printfn("  - in %s %s:%s.\n", function, file, line);
	libc::longjmp(&test_context.buf, 1);
}

fn bool run_tests(TestUnit[] tests) @if($$OLD_TEST)
{
	usz max_name;
	foreach (&unit : tests)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}
	quicksort(tests, &cmp_test_unit);

	TestContext context;
	test_context = &context;

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
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
		defer name.clear();
		name.appendf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());
		(void)io::stdout().flush();
		if (libc::setjmp(&context.buf) == 0)
		{
			if (catch err = unit.func())
			{
				io::printfn("[failed] Failed due to: %s", err);
				continue;
			}
			io::printn("[ok]");
			tests_passed++;
		}
	}
	io::printfn("\n%d test%s run.\n", test_count, test_count > 1 ? "s" : "");
	io::printfn("Test Result: %s. %d passed, %d failed.",
		tests_passed < test_count ? "FAILED" : "ok", tests_passed, test_count - tests_passed);
	return test_count == tests_passed;
}

fn bool run_tests(TestUnit[] tests) @if(!$$OLD_TEST)
{
	usz max_name;
	foreach (&unit : tests)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}
	quicksort(tests, &cmp_test_unit);

	TestContext context;
	test_context = &context;

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
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
		defer name.clear();
		name.appendf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());
		(void)io::stdout().flush();
		if (libc::setjmp(&context.buf) == 0)
		{
			unit.func();
			io::printn("[ok]");
			tests_passed++;
		}
	}
	io::printfn("\n%d test%s run.\n", test_count, test_count > 1 ? "s" : "");
	io::printfn("Test Result: %s. %d passed, %d failed.",
		tests_passed < test_count ? "FAILED" : "ok", tests_passed, test_count - tests_passed);
	return test_count == tests_passed;
}

fn bool default_test_runner(String[] args)
{
	@pool()
	{
		return run_tests(test_collection_create(allocator::temp()));
	};
}
