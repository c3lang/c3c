const int[3] CA = { 1, 2, 3 };
const int[] CS = { 1, 2, 3 };

<* @param [out] p *>
fn void set(int* p) { *p = 0; }

fn void write_const_array()
{
	foreach (&x : CA) *x = 5; // #error: 'x' points to constant data, which may not be assigned to
}

fn void increment_const_array()
{
	foreach (&x : CA) (*x)++; // #error: 'x' points to constant data, which may not be assigned to
}

fn void write_subslice_copy(int i)
{
	int[] s = CA[i..];
	foreach (&x : s) *x = 5;
}

fn void pass_const_as_out()
{
	foreach (&x : CA) set(x); // #error: const parameter may not be passed
}

fn int read_const()
{
	int c;
	foreach (&x : CA) c += *x;
	foreach (x : CA) c += x;
	return c;
}

fn void write_through_flagged_pointer()
{
	int[3]* p = &CA;
	foreach (&x : *p) *x = 1; // #error: 'x' points to constant data, which may not be assigned to
}

fn void write_through_plain_pointer()
{
	int[3] buf;
	int[3]* p = &buf;
	foreach (&x : *p) *x = 1;
	foreach (&x : p) *x = 2;
}

fn void write_const_slice()
{
	foreach (&x : CS) *x = 5; // #error: 'x' points to constant data, which may not be assigned to
}

fn void write_const_slice_var()
{
	int[] c = CS;
	c[0] = 5; // #error: 'c' points to constant data, which may not be assigned to
}
