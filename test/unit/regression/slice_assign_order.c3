// `a[s..e] = v` evaluates v first, then the start and end, as in the LLVM backend.
module slice_assign_order;

int[8] trace;
int trace_len;

fn int note(int id, int result)
{
	trace[trace_len++] = id;
	return result;
}

fn void value_then_start_then_end() @test
{
	trace_len = 0;
	int[6] a;
	a[note(1, 1)..note(2, 3)] = note(3, 9);
	assert(trace_len == 3);
	assert(trace[0] == 3 && trace[1] == 1 && trace[2] == 2);
	assert(a == (int[6]){ 0, 9, 9, 9, 0, 0 });
}

fn void exclusive_and_empty_ranges() @test
{
	int[6] a;
	a[2:3] = 7;
	assert(a == (int[6]){ 0, 0, 7, 7, 7, 0 });
	int[6] b;
	b[2:0] = 7;
	assert(b == (int[6]){ 0, 0, 0, 0, 0, 0 });
	int[] s = b[..];
	s[4..] = 5;
	assert(b == (int[6]){ 0, 0, 0, 0, 5, 5 });
}
