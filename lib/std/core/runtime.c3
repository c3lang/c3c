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

struct TestRunner
{
	String[] test_names;
	TestFn[] test_fns;
	JmpBuf buf;
}

fn TestRunner test_runner_create()
{
	return TestRunner {
		.test_fns = $$TEST_FNS,
		.test_names = $$TEST_NAMES,
	};
}

struct TestUnit
{
	String name;
	TestFn func;
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

TestRunner* current_runner @private;

fn void test_panic(String message, String file, String function, uint line)
{
	io::printn("[error]");
	io::print("\n Error: ");
	io::print(message);
	io::printn();
	io::printfn("  - in %s %s:%s.\n", function, file, line);
	libc::longjmp(&current_runner.buf, 1);
}

fn bool TestRunner.run(&self)
{
	assert(self.test_names.len == self.test_fns.len);
	TestUnit[] units = malloc(TestUnit, self.test_names.len);
	defer free(units);

	usz max_name;
	foreach (i, name: self.test_names)
	{
		units[i] = { name, self.test_fns[i] };
		max_name = max(max_name, name.len);
	}
	quicksort(units, &cmp_unit);

	current_runner = self;
	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
	int tests = units.len;
	io::printn("----- TESTS -----");
	DString name;
	name.tinit();
	foreach(unit : units)
	{
		defer name.clear();
		name.printf("Testing %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.as_str());
		CallstackElement* stack = $$stacktrace();
		if (stack) stack.prev = null;
		if (libc::setjmp(&self.buf) == 0)
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
	io::printfn("\n%d test%s run.\n", tests, tests > 1 ? "s" : "");
	io::printfn("Test Result: %s. %d passed, %d failed.",
		tests_passed < tests ? "FAILED" : "ok", tests_passed, tests - tests_passed);
	return tests == tests_passed;
}

fn bool __run_default_test_runner()
{
	return test_runner_create().run();
}


module std::core::runtime @if(WASM_NOLIBC);

extern fn void __wasm_call_ctors();
fn void wasm_initialize() @extern("_initialize") @wasm
{
	// The linker synthesizes this to call constructors.
	__wasm_call_ctors();
}