module compiler_rt;

<*
 All possible atomic orderings
*>
constdef AtomicOrderingRT : inline CInt
{
	NOT_ATOMIC,         // Not atomic
	UNORDERED,          // No lock
	RELAXED,            // Consistent ordering
	ACQUIRE,            // Barrier locking load/store
	RELEASE,            // Barrier releasing load/store
	ACQUIRE_RELEASE,    // Barrier fence to load/store
	SEQ_CONSISTENT,     // Acquire semantics, ordered with other seq_consistent
}

macro @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT $success, AtomicOrderingRT failure, $alignment)
{
	switch (failure)
	{
		case RELAXED: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrderingRT.RELAXED, $alignment);
		case ACQUIRE: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrderingRT.ACQUIRE, $alignment);
		case SEQ_CONSISTENT: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrderingRT.SEQ_CONSISTENT, $alignment);
		default: $$unreachable();
	}
	return 0;
}

macro @__atomic_compare_exchange_ordering_success(ptr, expected, desired, success, AtomicOrderingRT failure, $alignment)
{
	switch (success)
	{
		case RELAXED: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT.RELAXED, failure, $alignment);
		case ACQUIRE: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT.ACQUIRE, failure, $alignment);
		case RELEASE: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT.RELEASE, failure, $alignment);
		case ACQUIRE_RELEASE: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT.ACQUIRE_RELEASE, failure, $alignment);
		case SEQ_CONSISTENT: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrderingRT.SEQ_CONSISTENT, failure, $alignment);
		default: $$unreachable();
	}
	return 0;
}

fn CInt __atomic_compare_exchange(CInt size, any ptr, any expected, any desired, CInt success, CInt failure) @weak @nostrip @cname("__atomic_compare_exchange")
{
	switch (size)
	{
		case 1:
			char* pt = (char*)ptr;
			char ex = *(char*)expected;
			char de = *(char*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 1)) return 1;
		case 2:
			short* pt = (short*)ptr;
			short ex = *(short*)expected;
			short de = *(short*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 2)) return 1;
		case 4:
			int* pt = (int*)ptr;
			int ex = *(int*)expected;
			int de = *(int*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 4)) return 1;
		case 8:
			$if iptr::size >= 8:
				long* pt = (long*)ptr;
				long ex = *(long*)expected;
				long de = *(long*)desired;
				if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 8)) return 1;
			$else
				nextcase;
			$endif
		default:
			$$unreachable();
	}
	return 0;
}
