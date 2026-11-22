const int[3] CA = { 1, 2, 3 };
const int[] CS = { 1, 2, 3 };

<* @param [out] p *>
fn void set(int* p) { *p = 0; }

fn int read_const()
{
	int c;
	foreach (&x : CA) c += *x;
	foreach (x : CA) c += x;
	foreach (&x : CS) c += *x;
	return c;
}

fn void write_through_plain_pointer()
{
	int[3] buf;
	int[3]* p = &buf;
	foreach (&x : *p) *x = 1;
	foreach (&x : p) *x = 2;
}

fn void pass_element_as_out()
{
	set(&CA[1]); // #error: const parameter may not be passed
}
