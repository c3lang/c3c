module gen <Type>;
struct Foo
{
	union { Type a; }
	struct { struct { Type b; } }
	struct inner { Type c; }
}

module other;
struct Item
{
	union { char c; }
}

module test;
import gen, other;

struct Item
{
	union { int i; }
	struct { struct { long j; } }
	struct inner { short s; }
}

fn void main()
{
	$assert Item::members[0].type.name == "Item.0";
	$assert Item::members[1].type.name == "Item.1";
	$assert Item::members[1].type.members[0].type.name == "Item.1.0";
	$assert Item::members[2].type.name == "Item.inner";
	$assert Item::members[0].type.qname == "test::Item.0";
	$assert other::Item::members[0].type.qname == "other::Item.0";
	$assert Item::members[0].type != other::Item::members[0].type;
	$assert Item::members[0].type.size == 4;
	$assert Item::members[1].type.size == 8;

	$assert Foo{int}::members[0].type.name == "Foo{int}.0";
	$assert Foo{int}::members[1].type.members[0].type.name == "Foo{int}.1.0";
	$assert Foo{int}::members[2].type.name == "Foo{int}.inner";
	$assert Foo{long}::members[0].type.qname == "gen::Foo{long}.0";
	$assert Foo{int}::members[0].type != Foo{long}::members[0].type;
	$assert Foo{int}::members[0].type.size == 4;
	$assert Foo{long}::members[0].type.size == 8;
}
