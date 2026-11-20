module foreach_slice_reassigned;

struct Holder { int[] items; }

fn void reassign_in_body() @test
{
	int[5] arr = { 1, 2, 3, 4, 5 };
	int[] foos = arr[..];
	int sum;
	int count;
	foreach (foo : foos)
	{
		sum = sum * 10 + foo;
		count++;
		foos = foos[1..];
	}
	assert(sum == 12345 && count == 5);
}

fn void reassign_in_body_reverse() @test
{
	int[5] arr = { 1, 2, 3, 4, 5 };
	int[] foos = arr[..];
	int sum;
	foreach_r (foo : foos)
	{
		sum = sum * 10 + foo;
		foos = foos[1..];
	}
	assert(sum == 54321);
}

fn void by_ref_writes_original() @test
{
	int[5] arr = { 1, 2, 3, 4, 5 };
	int[] foos = arr[..];
	foreach (&p : foos)
	{
		*p *= 2;
		foos = arr[3..];
	}
	assert(arr == { 2, 4, 6, 8, 10 });
}

fn void field_reassigned() @test
{
	int[4] arr = { 1, 2, 3, 4 };
	Holder h = { arr[..] };
	int sum;
	foreach (i, x : h.items)
	{
		sum = sum * 10 + x;
		h.items = h.items[1..];
	}
	assert(sum == 1234);
}
