module defer_vaarg_splat;

int total;
faultdef BAD;

macro void add_all(...)
{
	$for var $i = 0; $i < $vaarg.len; $i++:
		total += $vaarg[$i];
	$endfor
}

macro void forward(...) => add_all(...$vaarg);

fn int two_exits(int r)
{
	defer { int[3] y = { 1, 2, 3 }; add_all(0, ...y); }
	if (r) return 1;
	return 0;
}

fn void? on_fault(bool fail)
{
	defer (catch e) { int[2] y = { 10, 20 }; add_all(...y); }
	if (fail) return BAD~;
}

fn void nested(int r)
{
	defer
	{
		int[2] y = { 100, 200 };
		forward(1, ...y);
	}
	if (r) return;
}

fn void defer_with_vaarg_splat() @test
{
	total = 0;
	two_exits(1);
	two_exits(0);
	assert(total == 12);
	total = 0;
	(void)on_fault(false);
	assert(total == 0);
	(void)on_fault(true);
	assert(total == 30);
	total = 0;
	nested(1);
	nested(0);
	assert(total == 602);
}
