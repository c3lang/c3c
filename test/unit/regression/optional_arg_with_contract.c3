// A faulted optional passed to a function with contracts must skip the call.
// In safe mode sema binds such arguments to generated locals.
module optional_arg_with_contract;
import std::collections::object;

faultdef MISSING;

struct Node { int v; }

fn Node*? Node.find(&self, bool ok) => ok ? self : MISSING~;

<*
 @require key > 0
*>
fn int? Node.val(&self, int key) => self.v + key;

fn void receiver() @test
{
	Node n = { 7 };
	assert((n.find(false).val(1) ?? -1) == -1);
	assert((n.find(true).val(1) ?? -1) == 8);
	Node*? missing = n.find(false);
	assert((missing.val(1) ?? -1) == -1);
}

fn void std_object() @test
{
	Object* o = object::new_obj(tmem);
	o.set("name", "x");
	assert((o.get("decl").get_string("id") ?? "none") == "none");
	assert((o.get_string("name") ?? "none") == "x");
}
