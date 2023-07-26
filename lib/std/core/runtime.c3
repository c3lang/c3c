// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;

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

import libc;

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

fn bool TestRunner.run(TestRunner* runner)
{
	current_runner = runner;
	PanicFn old_panic = builtin::panic;
	defer builtin::panic = old_panic;
	builtin::panic = &test_panic;
	int tests_passed = 0;
	int tests = runner.test_names.len;
	io::printn("----- TESTS -----");
	foreach(i, String name : runner.test_names)
	{
		io::printf("Testing %s ... ", name);
		CallstackElement* stack = $$stacktrace();
		if (stack) stack.prev = null;
		if (libc::setjmp(&runner.buf) == 0)
		{
			if (catch err = runner.test_fns[i]())
			{
				io::printfn("[failed] Failed due to: %s", err);
				continue;
			}
			io::printn("[ok]");
			tests_passed++;
		}
	}
	io::printfn("\n%d test(s) run.\n", tests);
	io::print("Test Result: ");
	if (tests_passed < tests)
	{
		io::print("FAILED");
	}
	else
	{
		io::print("ok");
	}
	io::printfn(". %d passed, %d failed.", tests_passed, tests - tests_passed);
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