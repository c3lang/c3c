module optional_typeid_properties;

import std::core::types;

faultdef BAD;

enum Letters { A, B }

fn typeid? get_type(bool fail, typeid type) @noinline
{
	if (fail) return BAD~;
	return type;
}

fn void optional_typeid_properties() @test
{
	sz? size = get_type(false, int::typeid).size;
	assert(size!! == int::size);

	TypeKind? kind = get_type(false, int::typeid).kind;
	assert(kind!! == int::kind);

	sz? len = get_type(false, int[4]::typeid).len;
	assert(len!! == 4);

	typeid? inner = get_type(false, int[4]::typeid).inner;
	assert(inner!! == int::typeid);

	typeid? parent = get_type(false, int::typeid).parent;
	assert(parent!! == void::typeid);

	String[]? names = get_type(false, Letters::typeid).names;
	assert(names!!.len == 2 && names!![0] == "A" && names!![1] == "B");

	assert(@catch(get_type(true, int::typeid).size) == BAD);
	assert(@catch(get_type(true, int::typeid).kind) == BAD);
	assert(@catch(get_type(true, int[4]::typeid).len) == BAD);
	assert(@catch(get_type(true, int[4]::typeid).inner) == BAD);
	assert(@catch(get_type(true, int::typeid).parent) == BAD);
	assert(@catch(get_type(true, Letters::typeid).names) == BAD);
}

fn void optional_any_typeid_property() @test
{
	int number = 1;
	any? value = &number;
	sz? size = value.type.size;
	assert(size!! == int::size);

	any? bad = BAD~;
	size = bad.type.size;
	assert(@catch(size) == BAD);
}
