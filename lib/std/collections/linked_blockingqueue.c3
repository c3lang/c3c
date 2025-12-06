module std::collections::blockingqueue { Value };
import std::thread, std::time;


const INITIAL_CAPACITY = 16;

struct QueueEntry 
{
	Value value;
	<* Next in queue order *>
	QueueEntry* next;
	<* Previous in queue order *>
	QueueEntry* prev;
}

struct LinkedBlockingQueue 
{
	<* First element in queue *>
	QueueEntry* head;
	<* Last element in queue *>
	QueueEntry* tail;
	<* Current number of elements *>
	usz count;
	<* Maximum capacity (0 for unbounded) *>
	usz capacity;
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
fn LinkedBlockingQueue* LinkedBlockingQueue.init(&self, Allocator allocator, usz capacity = 0)
{
	self.allocator = allocator;
	self.capacity = capacity;
	self.count = 0;
	self.head = null;
	self.tail = null;

	self.lock.init()!!;
	self.not_empty.init()!!;
	self.not_full.init()!!;
	return self;
}

fn LinkedBlockingQueue* LinkedBlockingQueue.tinit(&self, usz capacity = 0)
{
	return self.init(tmem, capacity) @inline;
}

<*
 @require self.is_initialized() : "Queue must be initialized"
*>
fn void LinkedBlockingQueue.free(&self)
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

	self.lock.destroy();
	self.not_empty.destroy();
	self.not_full.destroy();
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
fn void LinkedBlockingQueue.push(&self, Value value)
{
	self.lock.@in_lock()
	{
		while (self.capacity > 0 && self.count >= self.capacity)
		{
			self.not_full.wait(&self.lock);
		}

		QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
			.value = value,
			.next = null,
			.prev = null
		});
		self.link_entry(entry);

		// Signal that queue is no longer empty
		self.not_empty.signal();
	};
}

<*
 Get a value from the queue, blocking if there is no element in the queue.

 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value"
*>
fn Value LinkedBlockingQueue.poll(&self)
{
	self.lock.@in_lock()
	{
		while (self.count == 0)
		{
			self.not_empty.wait(&self.lock);
		}

		QueueEntry* entry = self.unlink_head();
		Value value = entry.value;
		allocator::free(self.allocator, entry);
		if (self.capacity > 0)
		{
			self.not_full.signal();
		}
		return value;
	};
}

<*
 Pop an element from the queue, fail is it is empty.

 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value"
 @return? NO_MORE_ELEMENT : "If the queue is empty"
*>
fn Value? LinkedBlockingQueue.pop(&self)
{
	self.lock.@in_lock()
	{
		if (self.count == 0) return NO_MORE_ELEMENT?;

		QueueEntry* entry = self.unlink_head();
		Value value = entry.value;
		allocator::free(self.allocator, entry);

		if (self.capacity > 0)
		{
			self.not_full.signal();
		}
		return value;
	};
}

<*
 Poll with a timeout.

 @param timeout : "Timeout in microseconds"
 @require self.is_initialized() : "Queue must be initialized"
 @return "The removed value or null if timeout occurred"
 @return? NO_MORE_ELEMENT : "If we reached the timeout"
*>
fn Value? LinkedBlockingQueue.poll_timeout(&self, Duration timeout)
{
	self.lock.@in_lock()
	{
		// Use while loop to handle spurious wakeups
		if (!self.count)
		{
			Time start = time::now();
			Time end = start + timeout;
			while (!self.count)
			{
        		if (end <= time::now()) break;
        		if (catch self.not_empty.wait_until(&self.lock, end)) break;
			}
			if (!self.count) return NO_MORE_ELEMENT?;
		}

		QueueEntry* entry = self.unlink_head();
		Value value = entry.value;
		allocator::free(self.allocator, entry);

		// Must signal not_full after removing an item
		if (self.capacity > 0)
		{
			self.not_full.signal();
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
 Try to push, return CAPACITY_EXCEEDED if the queue is full.

 @param value : "Value to add to the queue"
 @require self.is_initialized() : "Queue must be initialized"
 @return? CAPACITY_EXCEEDED : "If the queue is full"
*>
fn void? LinkedBlockingQueue.try_push(&self, Value value)
{
	self.lock.@in_lock()
	{
		if (self.capacity > 0 && self.count >= self.capacity) return CAPACITY_EXCEEDED?;

		QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
			.value = value,
			.next = null,
			.prev = null
		});
		self.link_entry(entry);
		self.not_empty.signal();
	};
}

<*
 Try to push, return CAPACITY_EXCEEDED if the queue is still full after timeout is reached.

 @param value : "Value to add to the queue"
 @param timeout : "Timeout in microseconds"
 @require self.is_initialized() : "Queue must be initialized"
 @return? CAPACITY_EXCEEDED : "If the queue is full"
*>
fn void? LinkedBlockingQueue.push_timeout(&self, Value value, Duration timeout)
{
	self.lock.@in_lock()
	{
		if (self.capacity > 0 && self.count >= self.capacity)
		{
			Time start = time::now();
			Time end = start + timeout;
        	while (self.capacity > 0 && self.count >= self.capacity)
			{
        		if (end <= time::now()) break;
				if (catch self.not_empty.wait_until(&self.lock, end)) break;
			}
			if (self.capacity > 0 && self.count >= self.capacity) return CAPACITY_EXCEEDED?;
		}

		QueueEntry* entry = allocator::new(self.allocator, QueueEntry, {
			.value = value,
			.next = null,
			.prev = null
		});
		self.link_entry(entry);
		self.not_empty.signal();
	};
}

<*
 @require self.is_initialized() : "Queue must be initialized"
 @return "The head value or NO_MORE_ELEMENT? if queue is empty"
*>
fn Value? LinkedBlockingQueue.peek(&self)
{
	self.lock.@in_lock()
	{
		return (self.head != null) ? self.head.value : NO_MORE_ELEMENT?;
	};
}

<*
 @return "True if queue is initialized"
*>
fn bool LinkedBlockingQueue.is_initialized(&self)
{
	return self.allocator && self.lock.initialized;
}
