module optional_inline_subscript;

faultdef BAD;

struct Box { int[4] v; }
fn int Box.get(&self, usz i) @operator([]) => self.v[i];
fn usz Box.len(&self) @operator(len) => 4;
struct Outer { inline Box b; }
struct Arr { inline int[4] a; }
struct Nested { inline Arr arr; }

fn void index_through_inline_member() @test
{
	Outer? oo = { { { 1, 2, 3, 4 } } };
	Arr? oa = { { 5, 6, 7, 8 } };
	Nested? on = { { { 9, 10, 11, 12 } } };
	int? x = oo[1];
	int? y = oa[^1];
	int? z = on[2];
	int? w = oo[^1];
	assert(x!! == 2);
	assert(y!! == 8);
	assert(z!! == 11);
	assert(w!! == 4);
}

fn void index_faulted_inline_member() @test
{
	Outer? oo = BAD~;
	Arr? oa = BAD~;
	int? x = oo[1];
	int? y = oa[1];
	assert(@catch(x) == BAD);
	assert(@catch(y) == BAD);
}
