module priorityqueue_test @test;
import std::collections;
import std::collections::priorityqueue;

def Queue = PriorityQueue(<int>);

fn void priorityqueue()
{
    Queue q;
    assert(q.is_empty());

    q.push(1);
    q.push(2);
    assert(q.len() == 2);

    int x;
    x = q.pop()!!;
    assert(x == 1, "got %d; want %d", x, 1);
    x = q.pop()!!;
    assert(x == 2, "got %d; want %d", x, 2);

    q.push(3);
    q.push(2);
    q.push(1);
    x = q.pop()!!;
    assert(x == 1, "got %d; want %d", x, 1);
    x = q.pop()!!;
    assert(x == 2, "got %d; want %d", x, 2);
    x = q.pop()!!;
    assert(x == 3, "got %d; want %d", x, 3);
}

def QueueMax = PriorityQueueMax(<int>);

fn void priorityqueue_max()
{
    QueueMax q;
    assert(q.is_empty());

    q.push(1);
    q.push(2);
    assert(q.len() == 2);

    int x;
    x = q.pop()!!;
    assert(x == 2, "got %d; want %d", x, 2);
    x = q.pop()!!;
    assert(x == 1, "got %d; want %d", x, 1);

    q.push(3);
    q.push(2);
    q.push(1);
    x = q.pop()!!;
    assert(x == 3, "got %d; want %d", x, 3);
    x = q.pop()!!;
    assert(x == 2, "got %d; want %d", x, 2);
    x = q.pop()!!;
    assert(x == 1, "got %d; want %d", x, 1);
}