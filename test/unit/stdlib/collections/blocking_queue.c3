module blocking_queue_test;
import std::collections::blockingqueue;
import std::thread;
import std::time;
import std::atomic;

alias TestQueue = LinkedBlockingQueue {int};

// Basic functionality tests
fn void test_init_free() @test
{
	TestQueue q;
	q.tinit(10);
	assert(q.is_initialized());
	assert(q.size() == 0);
	assert(q.is_empty());
	 q.free();
}

fn void test_single_thread_operations() @test
{
	TestQueue q;
	q.tinit(3);  // Capacity of 3

	// Test add and remove
	q.push(1);
	q.push(2);
	assert(q.size() == 2);
	assert(!q.is_empty());

	assert(q.pop()!! == 1);
	assert(q.pop()!! == 2);
	assert(q.size() == 0);
	assert(q.is_empty());

	// Test try_add and try_remove
	q.try_push(3)!!;
	q.try_push(4)!!;
	q.try_push(5)!!;
	test::@error(q.try_push(6), CAPACITY_EXCEEDED);  // Should fail - queue full

	assert(q.pop()!! == 3);
	assert(q.pop()!! == 4);
	assert(q.pop()!! == 5);
	test::@error(q.pop(), NO_MORE_ELEMENT);

	// Test peek
	q.push(7);
	assert(q.peek()!! == 7);
	assert(q.size() == 1);  // Peek shouldn't remove
	q.free();
}

fn void test_timeout_operations() @test
{
	TestQueue q;
	q.tinit(2);

	// Test remove_timeout
	q.push(1);
	q.push(2);

	// Should fail to add within timeout
	test::@error(q.push_timeout(3, time::MS * 10), CAPACITY_EXCEEDED);  // 10ms timeout

	// Should succeed to remove
	assert(q.poll_timeout(time::MS * 10)!! == 1);
	assert(q.poll_timeout(time::MS * 10)!! == 2);

	// Should timeout on empty queue
	assert(@catch(q.poll_timeout(time::MS * 10)) == NO_MORE_ELEMENT);

	q.free();
}

// Multi-threaded tests
const THREAD_COUNT = 4;
const ITEMS_PER_THREAD = 40;

fn int producer(void* arg)
{
	TestQueue* q = (TestQueue*)arg;
	for (int i = 0; i < ITEMS_PER_THREAD; i++)
	{
		q.push(i);
	}
	return 0;
}

fn int consumer(void* arg)
{
	TestQueue* q = (TestQueue*)arg;
	for (int i = 0; i < ITEMS_PER_THREAD; i++)
	{
		q.poll();
	}
	return 0;
}

fn void test_producer_consumer() @test
{
	TestQueue q;
	q.tinit(0);  // Unbounded queue

	Thread[THREAD_COUNT] producers;
	Thread[THREAD_COUNT] consumers;

	// Create producer threads
	foreach (i, &thread: producers)
	{
		assert(@ok(thread.create(&producer, &q)));
	}

	// Create consumer threads
	foreach (i, &thread: consumers)
	{
		assert(@ok(thread.create(&consumer, &q)));
	}

	// Wait for all threads
	foreach (i, &thread: producers)
	{
		assert((thread.join()??1) == 0);
	}

	foreach (i, &thread: consumers)
	{
		assert((thread.join()??1) == 0);
	}

	assert(q.is_empty());
	(void) q.free();
}

fn int bounded_producer(void* arg)
{
	TestQueue* q = (TestQueue*)arg;
	for (int i = 0; i < ITEMS_PER_THREAD; i++)
	{
		while (catch q.try_push(i))
		{
			thread::sleep_ms(10);  // Brief sleep if queue is full
		}
	}
	return 0;
}

fn void test_bounded_queue() @test
{
	TestQueue q;
	q.tinit(10);  // Small bounded queue

	Thread[THREAD_COUNT] producers;
	Thread[THREAD_COUNT] consumers;

	// Create producer threads
	foreach (i, &thread: producers)
	{
		assert(@ok(thread.create(&bounded_producer, &q)));
	}

	// Create consumer threads
	foreach (i, &thread: consumers)
	{
		assert(@ok(thread.create(&consumer, &q)));
	}

	// Wait for all threads
	foreach (i, &thread: producers)
	{
		assert((thread.join()??1) == 0);
	}

	foreach (i, &thread: consumers)
	{
		assert((thread.join()??1) == 0);
	}

	// Queue should be empty
	assert(q.is_empty());
	(void) q.free();
}
// Track produced and consumed counts
int produced = 0;
int consumed = 0;

fn void test_timeout_operations_threaded() @test
{
	TestQueue q;
	q.tinit(5);  // Small bounded queue

	Thread[THREAD_COUNT] producers;
	Thread[THREAD_COUNT] consumers;

	// Create threads
	foreach (i, &thread: producers)
	{
		thread.create(fn int(void* arg) {
			TestQueue* q = (TestQueue*)arg;
			for (int i = 0; i < ITEMS_PER_THREAD; i++)
			{
				while (@catch(q.poll_timeout(time::MS * 10)))
				{
					// Retry if timeout occurs
				}
				atomic::fetch_add(&consumed, 1);
			}
			return 0;
		}, &q)!!;
	}
	foreach (i, &thread: consumers)
	{
		thread.create(fn int(void* arg)
		{
			TestQueue* q = (TestQueue*)arg;
			for (int i = 0; i < ITEMS_PER_THREAD; i++)
			{
				while (catch q.push_timeout(i, time::MS * 10))
				{
					// Retry if offer fails
				}
				atomic::fetch_add(&produced, 1);
			}
			return 0;
		}, &q)!!;
	}

	// Wait with timeout
	bool all_ok = true;
	Clock timer = clock::now();
	foreach (i, &thread: producers)
	{
		if (timer.to_now() > time::sec(5).to_nano())
		{
			all_ok = false;
			break;
		}
		all_ok &= (thread.join() ?? 1) == 0;
	}
	foreach (i, &thread: consumers)
	{
		if (timer.to_now() > time::sec(5).to_nano())
		{
			all_ok = false;
			break;
		}
		all_ok &= (thread.join() ?? 1) == 0;
	}

	// Verify counts
	assert(all_ok);
	assert(produced == consumed + q.size(), "Production/consumption mismatch");
	assert(q.is_empty());
	q.free();
}