// Conversions to `any` keep the source type (typedef, enum, bitstruct, String) in every position,
// and a struct with an inline `any`-like first member (an interface) converts to that member.
module any_conversion_typeid;
import std::io;

typedef Meters = int;
enum Color { RED, GREEN }
bitstruct Bits : char { bool a; bool b; }
struct Holder { any x; }

fn any ret(Meters* m) => m;
fn typeid take(any a) => a.type;
fn typeid[] takev(any... a) @local
{
	typeid[] ids = mem::talloc_array(typeid, a.len);
	foreach (i, x : a) ids[i] = x.type;
	return ids;
}
fn typeid[] takef(String fmt, args...) @local
{
	typeid[] ids = mem::talloc_array(typeid, args.len);
	foreach (i, x : args) ids[i] = x.type;
	return ids;
}

fn void positions() @test
{
	String s = "hi";
	Meters m = (Meters)3;
	Color c = GREEN;
	Bits b;
	any a = &s;
	assert(a.type == String::typeid);
	a = &m;
	assert(a.type == Meters::typeid);
	a = &c;
	assert(a.type == Color::typeid);
	a = &b;
	assert(a.type == Bits::typeid);
	Holder h = { &m };
	assert(h.x.type == Meters::typeid);
	h.x = &c;
	assert(h.x.type == Color::typeid);
	any[2] arr = { &s, &b };
	assert(arr[0].type == String::typeid && arr[1].type == Bits::typeid);
	assert(ret(&m).type == Meters::typeid);
	assert(take(&s) == String::typeid && take(&c) == Color::typeid);
	typeid[] v = takev(&s, &m, &c, &b);
	assert(v[0] == String::typeid && v[1] == Meters::typeid && v[2] == Color::typeid && v[3] == Bits::typeid);
	typeid[] f = takef("", s, m, c, b);
	assert(f[0] == String::typeid && f[1] == Meters::typeid && f[2] == Color::typeid && f[3] == Bits::typeid);
}

interface Named
{
	fn String name();
}
struct Dog (Named) { int legs; }
fn String Dog.name(&self) @dynamic => "dog";

// The inline interface is the first member: converting the struct reads that member.
struct Wrapped
{
	inline Named named;
	int extra;
}

fn String wrapped_name(Wrapped[] list, usz i) => list[i].name();

fn void inline_interface_member() @test
{
	Dog d = { 4 };
	Wrapped[2] list = { { &d, 1 }, { &d, 2 } };
	assert(wrapped_name(&list, 1) == "dog");
	Named n = list[0];
	assert(n.type == Dog::typeid);
	@pool()
	{
		ByteReader r;
		r.init("hello world");
		char[4] storage;
		ReadBuffer rb;
		rb.init(&r, &storage);
		char[3] buf;
		sz got = rb.read(&buf)!!;
		assert((String)buf[:got] == "hel");
	};
}
