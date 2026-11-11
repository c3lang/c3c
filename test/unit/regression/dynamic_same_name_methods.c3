// Methods sharing the name of an interface method must not be picked by its dispatcher
// unless they are @dynamic with the same signature, including whether the result is optional.
module dynamic_same_name_methods @test;

faultdef EMPTY;

interface Sizer
{
	fn usz? size();
}

struct WithDyn (Sizer) { usz n; }
fn usz? WithDyn.size(&self) @dynamic
{
	if (!self.n) return EMPTY~;
	return self.n;
}

// Same name and lowered types as Sizer.size, not @dynamic.
struct Plain { usz n; }
fn usz Plain.size(&self) => self.n + 100;

// @dynamic and same name, but not optional and not implementing Sizer.
struct NonOpt { usz n; }
fn usz NonOpt.size(&self) @dynamic => self.n + 200;

fn void dispatch_ignores_same_name_methods()
{
	WithDyn w = { 5 };
	WithDyn empty = { 0 };
	Sizer s = &w;
	assert(s.size()!! == 5);
	s = &empty;
	assert(@catch(s.size()) == EMPTY);
	Plain p = { 7 };
	NonOpt q = { 9 };
	assert(p.size() == 107);
	assert(q.size() == 209);
}
