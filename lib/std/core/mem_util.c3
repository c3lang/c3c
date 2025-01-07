module std::core::mem;
import std::core::mem::allocator @public;

<*
 Assert on memory leak in the scope of the macro body.

 @param $report "Set to false to disable memory report"
*>
macro void @assert_leak($report = true; @body())
{
	$if env::DEBUG_SYMBOLS || $feature(MEMORY_ASSERTS):
		TrackingAllocator tracker;
		tracker.init(allocator::thread_allocator);
		Allocator old_allocator = allocator::thread_allocator;
		allocator::thread_allocator = &tracker;
		defer
		{
			allocator::thread_allocator = old_allocator;
			defer tracker.free();

			usz allocated = tracker.allocated();
			if (allocated)
			{
				DString report = dstring::new();
				defer report.free();

				$if $report:
				report.append_char('\n');
				(void)tracker.fprint_report(&report);
				$endif

				assert(allocated == 0, "Memory leak detected"
					" (%d bytes allocated).%s",
					allocated, report.str_view());
			}
		}
	$endif
	@body();
}
