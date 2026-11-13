module optional_compound_assign;

faultdef BAD, BAD2;

fn int? bad() => BAD~;
fn int? good(int x) => x;

fn void compound_assign_fault_rhs() @test
{
	int? e = BAD~;
	int? a = 4;
	a += e;
	assert(@catch(a) == BAD);
	int? b = 4;
	b += bad();
	assert(@catch(b) == BAD);
	int? c = 4;
	c *= e;
	assert(@catch(c) == BAD);
	int? d = 4;
	d <<= bad();
	assert(@catch(d) == BAD);
}

fn void compound_assign_matches_plain() @test
{
	int? e = BAD~;
	int? a = 4;
	int? b = 4;
	a += e;
	b = b + e;
	assert(@catch(a) == @catch(b));
}

fn void compound_assign_ok_rhs() @test
{
	int? a = 4;
	a += good(3);
	assert(a!! == 7);
	a -= good(2);
	assert(a!! == 5);
}

fn void compound_assign_fault_lhs() @test
{
	int? a = BAD~;
	a += 2;
	assert(@catch(a) == BAD);
	int? e = BAD2~;
	a += e;
	assert(@catch(a) == BAD);
}

fn void compound_assign_value_of_expr() @test
{
	int? a = 4;
	int? e = BAD~;
	int? r = (a += e);
	assert(@catch(r) == BAD);
	assert(@catch(a) == BAD);
	int? b = 1;
	r = (b += good(2));
	assert(r!! == 3);
	assert(b!! == 3);
}

fn int? compound_rethrow(bool fail)
{
	int? a = 4;
	a += (fail ? bad() : good(2))!;
	return a;
}

fn void compound_assign_rethrow() @test
{
	assert(@catch(compound_rethrow(true)) == BAD);
	assert(compound_rethrow(false)!! == 6);
}
