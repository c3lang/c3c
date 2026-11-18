module always_failing_operand;
faultdef BAD;

fn int? rethrow_init() { int v = BAD~!;  }
fn int? rethrow_operand() { return 1 + BAD~!; }
fn void? rethrow_statement(int* reached) { BAD~!; }
fn int? rethrow_element() { int[2] arr = { 1, BAD~! };  }
fn int? else_value() { int x = BAD~ ?? 5; return x; }
fn int? optional_operand() { int? z = BAD~; return z + 1; }

fn void always_failing_operands() @test
{
	int reached;
	assert(@catch(rethrow_init()) == BAD);
	assert(@catch(rethrow_operand()) == BAD);
	assert(@catch(rethrow_statement(&reached)) == BAD && reached == 0);
	assert(@catch(rethrow_element()) == BAD);
	assert(else_value()!! == 5);
	assert(@catch(optional_operand()) == BAD);
}
