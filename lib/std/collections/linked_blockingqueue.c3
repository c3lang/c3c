module std::collections::queue::err;

faultdef EMPTY, TIMEOUT;

module std::collections::queue { Value };
import std::thread;

const INITIAL_CAPACITY = 16;

struct QueueEntry 
{
    Value value;
    QueueEntry* next;    // Next in queue order
    QueueEntry* prev;    // Previous in queue order
}

struct LinkedBlockingQueue 
{
    QueueEntry* head;      // First element in queue
    QueueEntry* tail;      // Last element in queue
    usz count;             // Current number of elements
    usz capacity;          // Maximum capacity (0 for unbounded)
    Mutex lock;
    ConditionVariable not_empty;
    ConditionVariable not_full;
    Allocator allocator;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param capacity : "Maximum capacity (0 for unbounded)"
 @require !self.is_initialized() : "Queue was already initialized"
*>
fn void? LinkedBlockingQueue.init(&self, Allocator allocator, usz capacity = 0)
{
    self.allocator = allocator;
    self.capacity = capacity;
    self.count = 0;
    self.head = null;
    self.tail = null;
    
    self.lock.init()!;
    self.not_empty.init()!;
    self.not_full.init()!;
}

fn void? LinkedBlockingQueue.tinit(&self, usz capacity = 0) {
    return self.init(tmem, capacity)!;
}

<*
 @require self.is_initialized() : "Queue must be initialized"
*>
fn void? LinkedBlockingQueue.free(&self)
{
    self.lock.@in_lock()
	{
        // Free all remaining entries
        QueueEntry* entry = self.head;
        while (entry != null)
	    {
            QueueEntry* next = entry.next;
            allocator::free(self.allocator, entry);
            entry = next;
        }
    };
    
    self.lock.destroy()!;
    self.not_empty.destroy()!;
    self.not_full.destroy()!;
}

fn void LinkedBlockingQueue.link_entry(&self, QueueEntry* entry) @private 
{
    entry.next = null;
    entry.prev = self.tail;
    
    if (self.tail == null)
    {
        // First element in queue
        self.head = entry;
    }
    else
    {
        // Append to tail
        self.tail.next = entry;
    }
    self.tail = entry;
    self.count++;
}


fn QueueEntry* LinkedBlockingQueue.unlink_head(&self) @private 
{
    if (self.head == null) return null;
    
    QueueEntry* entry = self.head;
    self.head = entry.next;
    
    if (self.head != null)
    {
        self.head.prev = null;
    }
    else
    {
        // Queue is now empty
        self.tail = null;
    }
    
    self.count--;
    return entry;
}

<*
 @param value : "Value to add to the queue"
 @require self.is_initialized() : "Queue must be initialized"
*>
fn void? LinkedBlockingQueue.add(&self, Value value)
{
    self.lock.@in_lock()
	{
        while (self.capacity > 0 && self.count >= self.capacity)
	    {
            self.not_full.wait(&self.lock)!;
        }
        
        QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
            .value = value,
            .next = null,
            .prev = null
        });
        self.link_entry(entry);
        
        // Signal that queue is no longer empty
        self.not_empty.signal()!;
    };
}

<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value"
*>
fn Value? LinkedBlockingQueue.remove(&self)
{
    self.lock.@in_lock()
	{
        while (self.count == 0)
	    {
            self.not_empty.wait(&self.lock)!;
        }
        
        QueueEntry* entry = self.unlink_head();
        Value value = entry.value;
        allocator::free(self.allocator, entry);
        
        if (self.capacity > 0)
	    {
            self.not_full.signal()!;
        }
        return value;
    };
}

<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value or null if queue was empty"
*>
fn Value? LinkedBlockingQueue.try_remove(&self)
{
    self.lock.@in_lock()
	{
        if (self.count == 0)
	    {
            self.lock.unlock()!;
            return err::EMPTY?;
        }
        
        QueueEntry* entry = self.unlink_head();
        Value value = entry.value;
        allocator::free(self.allocator, entry);
        
        if (self.capacity > 0)
	    {
            self.not_full.signal()!;
        }
        
        return value;
    };
}

<*
 @param ms : "Timeout in milliseconds"
 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value or null if timeout occurred"
*>
fn Value? LinkedBlockingQueue.remove_timeout(&self, ulong ms)
{
    self.lock.@in_lock()
    {
        // Use while loop to handle spurious wakeups
        while (self.count == 0)
        {
            if (catch err = self.not_empty.wait_timeout(&self.lock, ms))
            {
                return err::TIMEOUT?;
            }
            // No need to recheck count here - while loop handles it
        }
        
        QueueEntry* entry = self.unlink_head();
        Value value = entry.value;
        allocator::free(self.allocator, entry);
        
        // Must signal not_full after removing an item
        if (self.capacity > 0)
        {
            self.not_full.signal()!;
        }

        return value;
    };
}


<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "Current size of the queue"
*>
fn usz LinkedBlockingQueue.size(&self)
{
    self.lock.@in_lock()
	{
        return self.count;
    };
}

<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "True if queue is empty"
*>
fn bool LinkedBlockingQueue.is_empty(&self)
{
    self.lock.@in_lock()
	{
        return self.count == 0;
    };
}

<*
 @param value : "Value to add to the queue"
 @require self.is_initialized() : "Queue must be initialized"
 @return "True if value was added, false if queue was full"
*>
fn bool? LinkedBlockingQueue.try_add(&self, Value value)
{
    self.lock.@in_lock()
	{
        if (self.capacity > 0 && self.count >= self.capacity)
	    {
            return false;
        }
        
        QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
            .value = value,
            .next = null,
            .prev = null
        });
        self.link_entry(entry);
        
        self.not_empty.signal()!;
        return true;
    };
}

<*
 @param value : "Value to add to the queue"
 @param ms : "Timeout in milliseconds"
 @require self.is_initialized() : "Queue must be initialized"
 @return "True if value was added, false if timeout occurred"
*>
fn bool? LinkedBlockingQueue.offer(&self, Value value, ulong ms)
{
    self.lock.@in_lock()
	{
        if (self.capacity > 0 && self.count >= self.capacity)
	    {
            if (catch err = self.not_full.wait_timeout(&self.lock, ms))
	        {
                self.lock.unlock()!;
                return false;
            }
            // Recheck after wait
            if (self.capacity > 0 && self.count >= self.capacity)
	        {
                self.lock.unlock()!;
                return false;
            }
        }
        
        QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
            .value = value,
            .next = null,
            .prev = null
        });
        self.link_entry(entry);
        
        self.not_empty.signal()!;
        return true;
    };
}

<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "The head value or EMPTY? if queue is empty"
*>
fn Value? LinkedBlockingQueue.peek(&self)
{
    self.lock.@in_lock()
	{
        return (self.head != null) ? self.head.value : err::EMPTY?;
    };
}

<*
 @return "True if queue is initialized"
*>
fn bool LinkedBlockingQueue.is_initialized(&self)
{
    return self.allocator && self.lock.initialized;
}
