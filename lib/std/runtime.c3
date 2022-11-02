// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::runtime;

struct VirtualAny
{
    void* ptr;
    typeid type_id;
}

struct VirtualContainer
{
    void* ptr;
    void* impl_ptr;
}

struct SubArrayContainer
{
    void* ptr;
    usz len;
}

struct VarArrayHeader
{
    usz size;
    usz capacity;
    void *allocator;
}

struct TestRunner
{
	char** test_names;
	void** test_fns;
	uint count;
}

fn TestRunner test_runner_create()
{
	return TestRunner {
		.test_names = $$TEST_NAMES,
		.test_fns = $$TEST_FNS,
		.count = $$TEST_COUNT,
	};
}

extern fn void puts(char* c);

fn void TestRunner.run(TestRunner* runner)
{
	for (uint i = 0; i < runner.count; i++)
	{
		puts(runner.test_names[i]);
	}
}

fn void run_test_runner()
{
	test_runner_create().run();
}