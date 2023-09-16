// Copyright (c) 2023 Eduardo José Gómez Hernández. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic;

macro @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, $success, failure) {
	switch(failure)
	{
		case AtomicOrdering.RELAXED.ordinal: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrdering.RELAXED.ordinal, 0);
		case AtomicOrdering.ACQUIRE.ordinal: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrdering.ACQUIRE.ordinal, 0);
		case AtomicOrdering.SEQ_CONSISTENT.ordinal: return $$compare_exchange(ptr, expected, desired, false, false, $success, AtomicOrdering.SEQ_CONSISTENT.ordinal, 0);
		default: assert(false, "Unrecognized failure ordering");
	}
	return 0;
}

macro @__atomic_compare_exchange_ordering_success(ptr, expected, desired, success, failure)
{
	switch(success)
	{
		case AtomicOrdering.RELAXED.ordinal: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrdering.RELAXED.ordinal, failure);
		case AtomicOrdering.ACQUIRE.ordinal: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrdering.ACQUIRE.ordinal, failure);
		case AtomicOrdering.RELEASE.ordinal: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrdering.RELEASE.ordinal, failure);
		case AtomicOrdering.ACQUIRE_RELEASE.ordinal: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrdering.ACQUIRE_RELEASE.ordinal, failure);
		case AtomicOrdering.SEQ_CONSISTENT.ordinal: return @__atomic_compare_exchange_ordering_failure(ptr, expected, desired, AtomicOrdering.SEQ_CONSISTENT.ordinal, failure);
		default: assert(false, "Unrecognized success ordering");
	}
	return 0;
}

fn CInt __atomic_compare_exchange(CInt size, any* ptr, any* expected, any* desired, CInt success, CInt failure) @extern("__atomic_compare_exchange") @export
{
	switch (size)
	{
		case 1:
			char* pt = (char*)ptr;
			char ex = *(char*)expected;
			char de = *(char*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, success, failure)) return 1;
		case 2:
			short* pt = (short*)ptr;
			short ex = *(short*)expected;
			short de = *(short*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, success, failure)) return 1;
		case 4:
			int* pt = (int*)ptr;
			int ex = *(int*)expected;
			int de = *(int*)desired;
			if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, success, failure)) return 1;
		case 8:
			$if iptr.sizeof >= 8:
				long* pt = (long*)ptr;
				long ex = *(long*)expected;
				long de = *(long*)desired;
				if (ex == @__atomic_compare_exchange_ordering_success(pt, ex, de, success, failure)) return 1;
			$else
				nextcase;
			$endif
		default:
			assert(false, "Unsuported size (%d) for atomic_compare_exchange", size);
	}
	return 0;
}