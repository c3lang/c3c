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

// --- Runtime functions ---

fn bool __atomic_is_lock_free(sz size, void* ptr) @weak @nostrip @cname("__atomic_is_lock_free")
{
	switch (size)
	{
		case 1: return true;
		case 2: return true;
		case 4: return true;
		case 8: return iptr::size >= 8;
		default: return false;
	}
}

fn void __atomic_load(sz size, void* src, void* dest, CInt model) @weak @nostrip @cname("__atomic_load")
{
	switch (size)
	{
		case 1:
			char* s = (char*)src;
			*(char*)dest = @__atomic_load_dispatch(s, (AtomicOrderingRT)model, 1);
		case 2:
			short* s = (short*)src;
			*(short*)dest = @__atomic_load_dispatch(s, (AtomicOrderingRT)model, 2);
		case 4:
			int* s = (int*)src;
			*(int*)dest = @__atomic_load_dispatch(s, (AtomicOrderingRT)model, 4);
		case 8:
			$if iptr::size >= 8:
				long* s = (long*)src;
				*(long*)dest = @__atomic_load_dispatch(s, (AtomicOrderingRT)model, 8);
			$else
				nextcase;
			$endif
		default:
			int* l = lock_for_pointer(src);
			atomic_spin_lock(l);
			char* s = (char*)src;
			char* d = (char*)dest;
			for (sz i = 0; i < size; i++) d[i] = s[i];
			atomic_spin_unlock(l);
	}
}

fn void __atomic_store(sz size, void* dest, void* src, CInt model) @weak @nostrip @cname("__atomic_store")
{
	switch (size)
	{
		case 1:
			char* d = (char*)dest;
			@__atomic_store_dispatch(d, *(char*)src, (AtomicOrderingRT)model, 1);
		case 2:
			short* d = (short*)dest;
			@__atomic_store_dispatch(d, *(short*)src, (AtomicOrderingRT)model, 2);
		case 4:
			int* d = (int*)dest;
			@__atomic_store_dispatch(d, *(int*)src, (AtomicOrderingRT)model, 4);
		case 8:
			$if iptr::size >= 8:
				long* d = (long*)dest;
				@__atomic_store_dispatch(d, *(long*)src, (AtomicOrderingRT)model, 8);
			$else
				nextcase;
			$endif
		default:
			int* l = lock_for_pointer(dest);
			atomic_spin_lock(l);
			char* d = (char*)dest;
			char* s = (char*)src;
			for (sz i = 0; i < size; i++) d[i] = s[i];
			atomic_spin_unlock(l);
	}
}

fn void __atomic_exchange(sz size, void* ptr, void* val, void* old, CInt model) @weak @nostrip @cname("__atomic_exchange")
{
	switch (size)
	{
		case 1:
			char* p = (char*)ptr;
			*(char*)old = @__atomic_exchange_dispatch(p, *(char*)val, (AtomicOrderingRT)model, 1);
		case 2:
			short* p = (short*)ptr;
			*(short*)old = @__atomic_exchange_dispatch(p, *(short*)val, (AtomicOrderingRT)model, 2);
		case 4:
			int* p = (int*)ptr;
			*(int*)old = @__atomic_exchange_dispatch(p, *(int*)val, (AtomicOrderingRT)model, 4);
		case 8:
			$if iptr::size >= 8:
				long* p = (long*)ptr;
				*(long*)old = @__atomic_exchange_dispatch(p, *(long*)val, (AtomicOrderingRT)model, 8);
			$else
				nextcase;
			$endif
		default:
			int* l = lock_for_pointer(ptr);
			atomic_spin_lock(l);
			char* p = (char*)ptr;
			char* v = (char*)val;
			char* o = (char*)old;
			for (sz i = 0; i < size; i++)
			{
				o[i] = p[i];
				p[i] = v[i];
			}
			atomic_spin_unlock(l);
	}
}

fn CInt __atomic_compare_exchange(sz size, void* ptr, void* expected, void* desired, CInt weak, CInt success, CInt failure) @weak @nostrip @cname("__atomic_compare_exchange")
{
	switch (size)
	{
		case 1:
			char* pt = (char*)ptr;
			char ex = *(char*)expected;
			char de = *(char*)desired;
			char actual = @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 1);
			if (ex == actual) return 1;
			*(char*)expected = actual;
		case 2:
			short* pt = (short*)ptr;
			short ex = *(short*)expected;
			short de = *(short*)desired;
			short actual = @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 2);
			if (ex == actual) return 1;
			*(short*)expected = actual;
		case 4:
			int* pt = (int*)ptr;
			int ex = *(int*)expected;
			int de = *(int*)desired;
			int actual = @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 4);
			if (ex == actual) return 1;
			*(int*)expected = actual;
		case 8:
			$if iptr::size >= 8:
				long* pt = (long*)ptr;
				long ex = *(long*)expected;
				long de = *(long*)desired;
				long actual = @__atomic_compare_exchange_ordering_success(pt, ex, de, (AtomicOrderingRT)success, (AtomicOrderingRT)failure, 8);
				if (ex == actual) return 1;
				*(long*)expected = actual;
			$else
				nextcase;
			$endif
		default:
			// Lock-based fallback for sizes without hardware CAS support
			// (e.g. 8 bytes on 32-bit, 16+ byte structs).
			int* l = lock_for_pointer(ptr);
			atomic_spin_lock(l);
			char* p = (char*)ptr;
			char* e = (char*)expected;
			char* d = (char*)desired;
			for (sz i = 0; i < size; i++)
			{
				if (p[i] != e[i])
				{
					// Mismatch: copy current value into expected.
					for (sz j = 0; j < size; j++) e[j] = p[j];
					atomic_spin_unlock(l);
					return 0;
				}
			}
			// Match: copy desired value into ptr.
			for (sz i = 0; i < size; i++) p[i] = d[i];
			atomic_spin_unlock(l);
			return 1;
	}
	return 0;
}

macro @__atomic_load_dispatch(ptr, ordering, $alignment)
{
	switch (ordering)
	{
		case RELAXED:        return $$atomic_load(ptr, false, AtomicOrderingRT.RELAXED,        $alignment);
		case ACQUIRE:        return $$atomic_load(ptr, false, AtomicOrderingRT.ACQUIRE,        $alignment);
		case SEQ_CONSISTENT: return $$atomic_load(ptr, false, AtomicOrderingRT.SEQ_CONSISTENT, $alignment);
		default: $$unreachable();
	}
	return 0;
}

macro @__atomic_store_dispatch(ptr, value, ordering, $alignment)
{
	switch (ordering)
	{
		case RELAXED:        $$atomic_store(ptr, value, false, AtomicOrderingRT.RELAXED,        $alignment);
		case RELEASE:        $$atomic_store(ptr, value, false, AtomicOrderingRT.RELEASE,        $alignment);
		case SEQ_CONSISTENT: $$atomic_store(ptr, value, false, AtomicOrderingRT.SEQ_CONSISTENT, $alignment);
		default: $$unreachable();
	}
}

// Spinlock table for non-lock-free atomic operations.
// Sized as a power of two; pointers are hashed to pick a lock.
const LOCK_TABLE_SIZE = 1024;
int[LOCK_TABLE_SIZE] atomic_lock_table @local;

macro int* lock_for_pointer(void* ptr) @local
{
	return &atomic_lock_table[((uptr)ptr >> 4) & (LOCK_TABLE_SIZE - 1)];
}

macro void atomic_spin_lock(int* l) @local
{
	// Spin until we change 0 → 1 with acquire ordering.
	while (true)
	{
		if ($$compare_exchange(l, 0, 1, true, false, AtomicOrderingRT.ACQUIRE, AtomicOrderingRT.RELAXED, 4) == 0) return;
	}
}

macro void atomic_spin_unlock(int* l) @local
{
	// Release store: CAS 1 → 0. Always succeeds because we hold the lock.
	$$compare_exchange(l, 1, 0, false, false, AtomicOrderingRT.RELEASE, AtomicOrderingRT.RELAXED, 4);
}

macro @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT $ordering, $alignment)
{
	return $$atomic_fetch_exchange(ptr, value, false, $ordering, $alignment);
}

macro @__atomic_exchange_dispatch(ptr, value, ordering, $alignment)
{
	switch (ordering)
	{
		case RELAXED: return @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT.RELAXED, $alignment);
		case ACQUIRE: return @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT.ACQUIRE, $alignment);
		case RELEASE: return @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT.RELEASE, $alignment);
		case ACQUIRE_RELEASE: return @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT.ACQUIRE_RELEASE, $alignment);
		case SEQ_CONSISTENT: return @__atomic_exchange_ordering(ptr, value, AtomicOrderingRT.SEQ_CONSISTENT, $alignment);
		default: $$unreachable();
	}
	return 0;
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
