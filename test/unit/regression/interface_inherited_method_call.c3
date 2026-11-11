// Calling a method declared in a parent interface through a child interface, where the parent
// interface itself is never used directly.
module interface_inherited_method_call @test;

interface Node
{
	fn int depth_plus(int depth);
}

interface Statement : Node {}

interface Expression : Statement {}

struct Leaf (Expression) { int value; }
fn int Leaf.depth_plus(&self, int depth) @dynamic => self.value + depth;

fn void call_through_grandchild_interface()
{
	Leaf leaf = { 40 };
	Expression e = &leaf;
	assert(e.depth_plus(2) == 42);
	Statement s = &leaf;
	assert(s.depth_plus(3) == 43);
}
