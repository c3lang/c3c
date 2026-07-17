module threadgroup_test;
import std::atomic, std::thread::experimental::threadgroup;

alias VoidGroup = ThreadGroup{int, void};

fn void increment_task(int task, void* ctx)
{
	Atomic{int}* counter = ctx;
	counter.add(1);
}

fn void void_result_runs_tasks() @test
{
	Atomic{int} counter;
	VoidGroup group;
	group.init(&allocators::LIBC_ALLOCATOR, &increment_task, thread_count: 4, context: &counter)!!;
	group.start()!!;
	for (int i = 0; i < 100; i++) group.add_task(i);
	group.join();
	group.destroy();
	assert(counter.load() == 100, "expected 100 tasks to run, got %d", counter.load());
}
