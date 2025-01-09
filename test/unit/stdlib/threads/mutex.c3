module thread_test;

import std::thread;
import std::os;

const TEST_MAGNITUDE = 10;


fn void lock_control_test() @test
{
    Mutex m;
    m.init()!!;
    m.lock()!!;
    assert(@catch(m.lock()));
}

fn void unlock_control_test() @test
{
    Mutex m;
    m.init()!!;
    assert(@catch(m.unlock()));
}

fn void lock_with_double_unlock_test() @test
{
    Mutex m;
    m.init()!!;

    m.lock()!!;
    m.unlock()!!;
    assert(@catch(m.unlock()));
}

fn void! own_mutex(Mutex* m)
{
    m.lock()!;
    m.unlock()!;
}

fn void ensure_owner_checks() @test
{
    Mutex m;
    m.init()!!;

    Thread[3 * TEST_MAGNITUDE] threads;
    
    foreach(&t : threads)
    {
        t.create((ThreadFn)&own_mutex, &m)!!;
    }

    foreach(&t : threads)
    {
        t.join()!!;
    }

    own_mutex(&m)!!;
}

struct ArgsWrapper1
{
    Mutex* m;
    ulong* v;
}

fn void shared_mutex_increment(ArgsWrapper1* args)
{
    args.m.lock()!!;
    args.v++;
    args.m.unlock()!!;
}

fn void shared_mutex_decrement(ArgsWrapper1* args)
{
    args.m.lock()!!;
    args.v--;
    args.m.unlock()!!;
}

fn void shared_mutex() @test
{
    Mutex m;
    m.init()!!;
    m.lock()!!;

    ulong v;

    ArgsWrapper1 args =
    {
        .m = &m,
        .v = &v
    };

    // An even number of threads must be chosen
    Thread[6 * TEST_MAGNITUDE] threads;
    for (int i = 0; i < threads.len / 2; i++)
    {
        (&threads[i]).create((ThreadFn)&shared_mutex_increment, &args)!!;
    }
    for (int i = (threads.len / 2); i < threads.len; i++)
    {
        (&threads[i]).create((ThreadFn)&shared_mutex_decrement, &args)!!;
    }

    m.unlock()!!;
    foreach(&t : threads)
    {
        t.join()!!;
    }
    assert(v == 0);
}

// Recursive mutex

fn void acquire_recursively(RecursiveMutex* m)
{
    // TODO: The recursive mutex functions can not directly be called via pointer

    for (usz i = 0; i < 5 * TEST_MAGNITUDE; i++)
    {
        ((Mutex*)m).lock()!!;
    }

    for (usz i = 0; i < 5 * TEST_MAGNITUDE; i++)
    {
        ((Mutex*)m).unlock()!!;
    }
}

fn void test_recursive_mutex() @test
{
    RecursiveMutex m;
    m.init()!!;
    defer m.destroy()!!;

    Thread[3 * TEST_MAGNITUDE] threads;
    foreach(&t : threads)
    {
        t.create((ThreadFn)&acquire_recursively, &m)!!;
    }

    foreach(&t : threads)
    {
        t.join()!!;
    }

    return acquire_recursively(&m);
}
