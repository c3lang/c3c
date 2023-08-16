// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;
import libc;

struct StackTrace
{
	StackTrace* prev;
	String file;
	String function;
	uint line;
}

struct VirtualAny
{
	void* ptr;
	typeid type_id;
}

struct SubArrayContainer
{
	void* ptr;
	usz len;
}

def TestFn = fn void!();

struct TestUnit
{
	String name;
	TestFn func;
}

fn TestUnit[] test_collection_create(Allocator* using = mem::heap())
{
	TestFn[] fns = $$TEST_FNS;
	String[] names = $$TEST_NAMES;
	TestUnit[] tests = malloc(TestUnit, names.len, .using = using);
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
fn int cmp_unit(TestUnit a, TestUnit b)
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

fn bool run_tests(TestUnit[] tests)
{
	usz max_name;
	foreach (&unit : tests)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}
	quicksort(tests, &cmp_unit);

	TestContext context;
	test_context = &context;

	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
	int test_count = tests.len;
	DString name;
	name.tinit();
	usz len = max_name + 9;
	name.append_repeat('-', len / 2);
	name.append(" TESTS ");
	name.append_repeat('-', len - len / 2);
	io::printn(name);
	name.clear();
	foreach(unit : tests)
	{
		defer name.clear();
		name.printf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.as_str());
		CallstackElement* stack = $$stacktrace();
		if (stack) stack.prev = null;
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

fn bool __run_default_test_runner()
{
	@pool()
	{
		return run_tests(test_collection_create(mem::temp()));
	};
}


module std::core::runtime @if(WASM_NOLIBC);

extern fn void __wasm_call_ctors();
fn void wasm_initialize() @extern("_initialize") @wasm
{
	// The linker synthesizes this to call constructors.
	__wasm_call_ctors();
}