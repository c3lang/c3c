module thread_pool_test;
import std;

fn void noop_task(void* arg) {}

fn void increment_task(void* arg)
{
	Atomic{int}* counter = arg;
	counter.add(1);
}

fn void block_until_gated(void* arg)
{
	Atomic{bool}* gate = arg;
	while (!gate.load()) thread::sleep_ms(1);
}

struct GatedIncrement
{
	Atomic{bool}* gate;
	Atomic{int}* counter;
}

fn void block_then_increment(void* arg)
{
	GatedIncrement* gi = arg;
	while (!gi.gate.load()) thread::sleep_ms(1);
	gi.counter.add(1);
}

fn int call_destroy_now(void* arg)
{
	ThreadPool* pool = arg;
	pool.destroy_now();
	return 0;
}

fn void init_destroy() @test
{
	for (int i = 0; i < 20; i++)
	{
		ThreadPool pool;
		pool.init(mem, 4)!!;
		pool.destroy();
	}
}

fn void add_task_runs_all_work() @test
{
	Atomic{int} counter;
	counter.store(0);

	ThreadPool pool;
	pool.init(mem, 4)!!;
	for (int i = 0; i < 50; i++)
	{
		pool.add_task(&increment_task, &counter)!!;
	}

	pool.destroy();

	test::eq(counter.load(), 50);
}

fn void try_add_task_reports_queue_full() @test
{
	Atomic{bool} gate;
	gate.store(false);

	ThreadPool pool;
	pool.init(mem, 1, 1)!!;
	defer pool.destroy_now();
	defer gate.store(true);

	pool.add_task(&block_until_gated, &gate)!!;
	for (int i = 0; i < 2000 && pool.queue_count() > 0; i++) thread::sleep_ms(1);
	test::eq(pool.queue_count(), 0);

	pool.try_add_task(&noop_task, null)!!;
	test::eq(pool.queue_count(), 1);

	test::@error(pool.try_add_task(&noop_task, null), thread::THREAD_QUEUE_FULL);
}

struct BlockedPush
{
	ThreadPool* pool;
	Atomic{bool}* completed;
}

fn int blocked_push_worker(void* arg)
{
	BlockedPush* bp = arg;
	(void)bp.pool.add_task(&noop_task, null);
	bp.completed.store(true);
	return 0;
}

fn void add_task_blocks_until_space_available() @test
{
	Atomic{bool} gate;
	gate.store(false);

	ThreadPool pool;
	pool.init(mem, 1, 1)!!;
	defer pool.destroy_now();
	defer gate.store(true);

	pool.add_task(&block_until_gated, &gate)!!;
	for (int i = 0; i < 2000 && pool.queue_count() > 0; i++) thread::sleep_ms(1);
	test::eq(pool.queue_count(), 0);

	pool.add_task(&noop_task, null)!!;
	test::eq(pool.queue_count(), 1);

	Atomic{bool} completed;
	completed.store(false);
	BlockedPush bp = { &pool, &completed };
	Thread t;
	t.create(&blocked_push_worker, &bp)!!;

	// The queue is at capacity, so the blocked push should not complete yet.
	thread::sleep_ms(20);
	test::eq(completed.load(), false);

	gate.store(true);
	for (int i = 0; i < 2000 && !completed.load(); i++) thread::sleep_ms(1);
	test::eq(completed.load(), true);

	t.join();
}

fn void destroy_now_drops_pending_work() @test
{
	Atomic{bool} gate;
	gate.store(false);
	Atomic{int} counter;
	counter.store(0);
	GatedIncrement gi = { &gate, &counter };

	ThreadPool pool;
	pool.init(mem, 1)!!;

	pool.add_task(&block_then_increment, &gi)!!;
	for (int i = 0; i < 2000 && pool.queue_count() > 0; i++) thread::sleep_ms(1);
	test::eq(pool.queue_count(), 0);

	for (int i = 0; i < 10; i++)
	{
		pool.add_task(&increment_task, &counter)!!;
	}
	test::eq(pool.queue_count(), 10);

	Thread shutdown_thread;
	shutdown_thread.create(&call_destroy_now, &pool)!!;
	// Give destroy_now() time to mark the pool stopping and drop the queued work
	// before we let the currently running task finish.
	thread::sleep_ms(50);
	gate.store(true);
	shutdown_thread.join();

	test::eq(counter.load(), 1);
}

fn void busy_count_reflects_running_task() @test
{
	Atomic{bool} gate;
	gate.store(false);

	ThreadPool pool;
	pool.init(mem, 2)!!;

	pool.add_task(&block_until_gated, &gate)!!;
	for (int i = 0; i < 2000 && pool.queue_count() > 0; i++) thread::sleep_ms(1);
	test::eq(pool.queue_count(), 0);
	test::eq(pool.busy_count(), 1);

	gate.store(true);
	pool.destroy();
}

fn void prewarm_spins_up_workers() @test
{
	ThreadPool pool;
	pool.init(mem, 5)!!;
	defer pool.destroy();

	pool.prewarm(3)!!;

	test::ge(pool.worker_count(), 3);
	test::le(pool.worker_count(), 5);
}
