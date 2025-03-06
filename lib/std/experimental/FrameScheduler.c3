module std::experimental::scheduler{Event};
import std::collections, std::thread, std::time;

struct DelayedSchedulerEvent @local
{
	inline Event event;
	Clock execution_time;
}

fn int DelayedSchedulerEvent.compare_to(self, DelayedSchedulerEvent other) @local
{
	switch
	{
		case self.execution_time < other.execution_time: return -1;
		case self.execution_time > other.execution_time: return 1;
		default: return 0;
	}
}

struct FrameScheduler
{
	PriorityQueue{DelayedSchedulerEvent} delayed_events;
	List{Event} events;
	List{Event} pending_events;
	bool pending;
	Mutex mtx;
}

fn void FrameScheduler.init(&self)
{
	self.events.init(mem);
	self.pending_events.init(mem);
	self.delayed_events.init(mem);
	(void)self.mtx.init();
	bool pending;
}

macro void FrameScheduler.@destroy(&self; @destruct(Event e))
{
	foreach (e : self.events) @destruct(e);
	foreach (e : self.pending_events) @destruct(e);
	foreach (e : self.delayed_events.heap) @destruct(e.event);
	self.events.free();
	self.pending_events.free();
	self.delayed_events.free();
	(void)self.mtx.destroy();
}

fn void FrameScheduler.queue_delayed_event(&self, Event event, Duration delay)
{
	self.mtx.@in_lock()
	{
		self.delayed_events.push({ event, clock::now().add_duration(delay)});
		@atomic_store(self.pending, true);
	};
}

fn bool FrameScheduler.has_delayed(&self)
{
	self.mtx.@in_lock()
    {
        return @ok(self.delayed_events.first());
    };
}

fn void FrameScheduler.queue_event(&self, Event event)
{
	self.mtx.@in_lock()
	{
		self.pending_events.push(event);
		@atomic_store(self.pending, true);
	};
}
fn Event? FrameScheduler.pop_event(&self)
{
	while (true)
	{
		if (try event = self.events.pop()) return event;
    	if (!@atomic_load(self.pending)) return NO_MORE_ELEMENT?;
        self.mtx.@in_lock()
        {
            self.events.add_all(&self.pending_events);
            self.pending_events.clear();
            Clock c = clock::now();
            while (try top = self.delayed_events.first())
            {
                if (top.execution_time > c) break;
                self.events.push(self.delayed_events.pop()!!);
            }
            @atomic_store(self.pending, self.delayed_events.len() > 0);
            if (!self.events.len()) return NO_MORE_ELEMENT?;
        };
	}
}
