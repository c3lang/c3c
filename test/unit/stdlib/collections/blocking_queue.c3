module blocking_queue_test;

import std::collections::queue;
import std::thread;
import std::time;
import std::atomic;

alias TestQueue = LinkedBlockingQueue {int};

// Basic functionality tests
fn void test_init_free() @test
{
    TestQueue q;
    assert(@ok(q.tinit(10)));
    assert(q.is_initialized());
    assert(q.size() == 0);
    assert(q.is_empty());
    (void) q.free();
}

fn void test_single_thread_operations() @test
{
    TestQueue q;
    assert(@ok(q.tinit(3)));  // Capacity of 3
    
    // Test add and remove
    assert(@ok(q.add(1)));
    assert(@ok(q.add(2)));
    assert(q.size() == 2);
    assert(!q.is_empty());
    
    assert(q.remove()!! == 1);
    assert(q.remove()!! == 2);
    assert(q.size() == 0);
    assert(q.is_empty());
    
    // Test try_add and try_remove
    assert(q.try_add(3)!! == true);
    assert(q.try_add(4)!! == true);
    assert(q.try_add(5)!! == true);
    assert(q.try_add(6)!! == false);  // Should fail - queue full
    
    assert(q.try_remove()!! == 3);
    assert(q.try_remove()!! == 4);
    assert(q.try_remove()!! == 5);
    assert(@catch(q.try_remove()) == err::EMPTY);
    
    // Test peek
    assert(@ok(q.add(7)));
    assert(q.peek()!! == 7);
    assert(q.size() == 1);  // Peek shouldn't remove
    (void) q.free();
}

fn void test_timeout_operations() @test
{
    TestQueue q;
    assert(@ok(q.tinit(2)));
    
    // Test remove_timeout
    assert(@ok(q.add(1)));
    assert(@ok(q.add(2)));
    
    // Should fail to add within timeout
    assert(q.offer(3, 10)!! == false);  // 10ms timeout
    
    // Should succeed to remove
    assert(q.remove_timeout(10)!! == 1);
    assert(q.remove_timeout(10)!! == 2);
    
    // Should timeout on empty queue
    assert(@catch(q.remove_timeout(10)) == err::TIMEOUT);
    
    (void) q.free();
}

// Multi-threaded tests
const THREAD_COUNT = 4;
const ITEMS_PER_THREAD = 40;

fn int producer(void* arg)
{
    TestQueue* q = (TestQueue*)arg;
    for (int i = 0; i < ITEMS_PER_THREAD; i++)
    {
        assert(@ok(q.add(i)));
    }
    return 0;
}

fn int consumer(void* arg)
{
    TestQueue* q = (TestQueue*)arg;
    for (int i = 0; i < ITEMS_PER_THREAD; i++)
    {
        assert(@ok(q.remove()));
    }
    return 0;
}

fn void test_producer_consumer() @test
{
    TestQueue q;
    assert(@ok(q.tinit(0)));  // Unbounded queue
    
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
        while (q.try_add(i)!! == false)
        {
            thread::sleep_ms(10);  // Brief sleep if queue is full
        }
    }
    return 0;
}

fn void test_bounded_queue() @test
{
    TestQueue q;
    assert(@ok(q.tinit(10)));  // Small bounded queue
    
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
    assert(@ok(q.tinit(5)));  // Small bounded queue
    
    Thread[THREAD_COUNT] producers;
    Thread[THREAD_COUNT] consumers;
    
    // Create threads
    foreach (i, &thread: producers) {
        assert(@ok(thread.create(fn int(void* arg) {
            TestQueue* q = (TestQueue*)arg;
            for (int i = 0; i < ITEMS_PER_THREAD; i++)
            {
                while (@catch(q.remove_timeout(10)) == err::TIMEOUT)
                {
                    // Retry if timeout occurs
                }
                atomic::fetch_add(&consumed, 1);
            }
            return 0;
        }, &q)));
    }
    foreach (i, &thread: consumers) {
        assert(@ok(thread.create(fn int(void* arg) {
            TestQueue* q = (TestQueue*)arg;
            for (int i = 0; i < ITEMS_PER_THREAD; i++)
            {
                while ((q.offer(i, 10)??false) == false)
                {
                    // Retry if offer fails
                }
                atomic::fetch_add(&produced, 1);
            }
            return 0;
        }, &q)));
    }
    
    // Wait with timeout
    bool all_ok = true;
    Clock timer = clock::now();
    foreach (i, &thread: producers) {
        if (timer.to_now() > (NanoDuration)5000000000) {
            all_ok = false;
            break;
        }
        all_ok &= ((thread.join()??1) == 0);
    }
    foreach (i, &thread: consumers) {
        if (timer.to_now() > (NanoDuration)5000000000) {
            all_ok = false;
            break;
        }
        all_ok &= ((thread.join()??1) == 0);
    }
    
    // Verify counts
    assert(all_ok);
    assert(produced == consumed + q.size(), "Production/consumption mismatch");
    assert(q.is_empty());
    (void) q.free();
}