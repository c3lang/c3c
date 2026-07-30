
module json_regress_test @test;
import std::encoding, std::encoding::json, std::collections::object, std::io;

struct Point       { int x; int y; }
struct Nested      { Point p; bool flag; }
struct WithSlice   { int[] xs; bool flag; }
struct Deep        { String[] tags; Point[] pts; bool flag; }
struct Longs       { long v; }
struct Ulongs      { ulong v; }
struct FixedNames  { String[3] names; bool flag; }
struct OneKey      { int a; }
struct TwoKeys     { int a; int b; }
struct Tagged      { String[] tags; bool flag; }

typedef Celsius = int;
struct Reading     { Celsius temp; bool ok; }

enum Mode { ACTIVE, IDLE }
struct WithMode    { Mode s; }

fn void marshal_long_min()
{
	Longs l = { long::min };
	test::eq(json::marshal_try(tmem, l)!!, `{"v":-9223372036854775808}`);
}

fn void marshal_long_max_and_zero()
{
	Longs hi = { long::max };
	test::eq(json::marshal_try(tmem, hi)!!, `{"v":9223372036854775807}`);
	Longs z = { 0 };
	test::eq(json::marshal_try(tmem, z)!!, `{"v":0}`);
}

fn void marshal_ulong_max()
{
	Ulongs u = { ulong::max };
	test::eq(json::marshal_try(tmem, u)!!, `{"v":18446744073709551615}`);
}

fn void unmarshal_long_min_roundtrip()
{
	Longs l = json::unmarshal(tmem, `{"v":-9223372036854775808}`, Longs)!!;
	test::eq(l.v, long::min);
}

fn void dom_int128_out_of_range_faults()
{
	Object* o = object::new_obj(tmem);
	o.set("big", (int128)long::max + 1);
	test::@error(json::marshal_try(tmem, o), json::INVALID_NUMBER);

	Object* o2 = object::new_obj(tmem);
	o2.set("small", (int128)long::min - 1);
	test::@error(json::marshal_try(tmem, o2), json::INVALID_NUMBER);
}

fn void dom_int128_in_range_ok()
{
	Object* o = object::new_obj(tmem);
	o.set("v", (int128)long::max);
	test::eq(json::marshal_try(tmem, o)!!, `{"v":9223372036854775807}`);
}

fn void empty_containers_roundtrip()
{
	test::eq(json::marshal_try(tmem, json::tparse(`{}`)!!)!!, `{}`);
	test::eq(json::marshal_try(tmem, json::tparse(`[]`)!!)!!, `[]`);
	test::eq(json::marshal_try(tmem, json::tparse(`{"a":[]}`)!!)!!, `{"a":[]}`);
	test::eq(json::marshal_try(tmem, json::tparse(`{"m":{}}`)!!)!!, `{"m":{}}`);
	test::eq(json::marshal_try(tmem, json::tparse(`[[],{}]`)!!)!!, `[[],{}]`);
}

fn void empty_containers_to_format()
{
	test::eq(string::tformat("%s", json::tparse(`{"a":[]}`)!!), `{"a":[]}`);
	test::eq(string::tformat("%s", json::tparse(`{"m":{}}`)!!), `{"m":{}}`);
}

fn void parsed_empty_array_is_array()
{
	Object* a = json::tparse(`[]`)!!;
	test::eq(a.is_array(), true);
	test::eq(a.is_map(), false);
	test::eq(a.len(), 0);

	Object* m = json::tparse(`{}`)!!;
	test::eq(m.is_map(), true);
	test::eq(m.is_array(), false);
	test::eq(m.len(), 0);
}

fn void unmarshal_empty_slice()
{
	WithSlice w = json::unmarshal(tmem, `{"xs":[],"flag":true}`, WithSlice)!!;
	test::eq(w.xs.len, 0);
	test::eq(w.flag, true);
}

fn void unmarshal_empty_struct()
{
	Point p = object::unmarshal{Point}(tmem, json::tparse(`{}`)!!)!!;
	test::eq(p.x, 0);
	test::eq(p.y, 0);
}

fn void unmarshal_nested_empty_struct()
{
	Nested n = object::unmarshal{Nested}(tmem, json::tparse(`{"p":{},"flag":true}`)!!)!!;
	test::eq(n.p.x, 0);
	test::eq(n.flag, true);
}

fn void dom_unmarshal_populates()
{
	Point p = object::unmarshal{Point}(tmem, json::tparse(`{"x":3,"y":4}`)!!)!!;
	test::eq(p.x, 3);
	test::eq(p.y, 4);
}

fn void dom_unmarshal_nested_populates()
{
	Nested n = object::unmarshal{Nested}(tmem, json::tparse(`{"p":{"x":1,"y":2},"flag":true}`)!!)!!;
	test::eq(n.p.x, 1);
	test::eq(n.p.y, 2);
	test::eq(n.flag, true);
}

fn void dom_unmarshal_missing_members_ok()
{
	TwoKeys t = object::unmarshal{TwoKeys}(tmem, json::tparse(`{"a":1}`)!!)!!;
	test::eq(t.a, 1);
	test::eq(t.b, 0);
}

fn void string_path_missing_members_ok()
{
	TwoKeys t = json::unmarshal(tmem, `{"a":1}`, TwoKeys)!!;
	test::eq(t.a, 1);
	test::eq(t.b, 0);
}

fn void dom_type_mismatch_faults()
{
	// string where an int is expected
	test::@error(object::unmarshal{Point}(tmem, json::tparse(`{"x":"nope","y":1}`)!!),
	             encoding::MALFORMED_DATA);
	// int where a struct is expected
	test::@error(object::unmarshal{Nested}(tmem, json::tparse(`{"p":42,"flag":true}`)!!),
	             encoding::MALFORMED_DATA);
	// map where a slice is expected
	test::@error(object::unmarshal{WithSlice}(tmem, json::tparse(`{"xs":{},"flag":true}`)!!),
	             encoding::MALFORMED_DATA);
	// scalar where a struct is expected
	test::@error(object::unmarshal{Point}(tmem, json::tparse(`"hello"`)!!),
	             encoding::MALFORMED_DATA);
	// int where a bool is expected
	test::@error(object::unmarshal{WithSlice}(tmem, json::tparse(`{"xs":[],"flag":7}`)!!),
	             encoding::MALFORMED_DATA);
}

fn void trailing_comma_object_and_array_agree()
{
	test::eq(json::unmarshal(tmem, `{"a":1,}`, OneKey)!!.a, 1);
	test::eq(json::unmarshal(tmem, `{"xs":[1,2,],"flag":true}`, WithSlice)!!.xs.len, 2);
	test::eq(json::unmarshal(tmem, `{"xs":[ 1 , 2 , ],"flag":true}`, WithSlice)!!.xs.len, 2);
}

fn void dom_flavor_still_honoured()
{
	json::tparse(`{"a":1,}`)!!;
	json::tparse(`[1,]`)!!;
	test::@error(json::tparse(`{"a":1,}`, JSON), json::UNEXPECTED_CHARACTER);
	test::@error(json::tparse(`[1,]`, JSON), json::UNEXPECTED_CHARACTER);
}

fn void scan_counts()
{
	test::eq(json::unmarshal(tmem, `{"xs":[1,2,3],"flag":true}`, WithSlice)!!.xs.len, 3);
	test::eq(json::unmarshal(tmem, `{"xs":[ ],"flag":true}`, WithSlice)!!.xs.len, 0);
}

fn void scan_ignores_strings_and_comments()
{
	Deep d = json::unmarshal(tmem,
		`{"tags":["a,b","c]d","e{f"],"pts":[/*,,,*/{"x":1,"y":2}],"flag":true}`, Deep)!!;
	test::eq(d.tags.len, 3);
	test::eq(d.tags[1], "c]d");
	test::eq(d.pts.len, 1);
	test::eq(d.pts[0].y, 2);
}

fn void skip_unmapped_containers()
{
	test::eq(json::unmarshal(tmem, `{"other":["}]",{"x":[1,2]}],"a":7}`, OneKey)!!.a, 7);
	test::eq(json::unmarshal(tmem, `{"other":{"k":"v//x"},"a":8}`, OneKey)!!.a, 8);
}

fn void typedef_string_path()
{
	Reading r = json::unmarshal(tmem, `{"temp":21,"ok":true}`, Reading)!!;
	test::eq((int)r.temp, 21);
}

fn void typedef_marshal()
{
	Reading r = { (Celsius)21, true };
	test::eq(json::marshal_try(tmem, r)!!, `{"temp":21,"ok":true}`);
}

fn void enum_roundtrip()
{
	WithMode w = { Mode.IDLE };
	String s = json::marshal_try(tmem, w)!!;
	test::eq(s, `{"s":"IDLE"}`);
	test::eq(json::unmarshal(tmem, s, WithMode)!!.s, Mode.IDLE);
	test::eq(json::unmarshal(tmem, `{"s":0}`, WithMode)!!.s, Mode.ACTIVE);
}

fn void enum_out_of_range_faults()
{
	test::@error(json::unmarshal(tmem, `{"s":99}`, WithMode), encoding::MALFORMED_DATA);
	test::@error(json::unmarshal(tmem, `{"s":"NOPE"}`, WithMode), encoding::MALFORMED_DATA);
}

fn void cleanup_on_error_releases_slice()
{
	Object* dom = json::tparse(`{"xs":[1,2,3],"flag":"nope"}`)!!;
	test::@error(object::unmarshal{WithSlice}(mem, dom), encoding::MALFORMED_DATA);
}

fn void cleanup_on_error_releases_nested()
{
	Object* dom = json::tparse(
		`{"tags":["a","bb","ccc"],"pts":[{"x":1,"y":2},{"x":3,"y":4}],"flag":"nope"}`)!!;
	test::@error(object::unmarshal{Deep}(mem, dom), encoding::MALFORMED_DATA);
}

fn void cleanup_on_error_releases_fixed_array()
{
	// exercises recursively_free's ARRAY branch (String[3] elements are copied)
	Object* dom = json::tparse(`{"names":["a","b","c"],"flag":"nope"}`)!!;
	test::@error(object::unmarshal{FixedNames}(mem, dom), encoding::MALFORMED_DATA);
}

fn void recursively_free_is_public()
{
	Object* dom = json::tparse(
		`{"tags":["a","bb"],"pts":[{"x":1,"y":2}],"flag":true}`)!!;
	Deep d = object::unmarshal{Deep}(mem, dom)!!;
	test::eq(d.tags.len, 2);
	test::eq(d.tags[1], "bb");
	test::eq(d.pts[0].x, 1);
	object::recursively_free{Deep}(mem, &d);
}


fn void new_array_is_empty_array()
{
	Object* a = object::new_array(mem);
	defer a.free();
	test::eq(a.is_array(), true);
	test::eq(a.len(), 0);
}

fn void new_obj_push_coercion_still_works()
{
	Object* o = object::new_obj(mem);
	defer o.free();
	o.push("hello");
	o.push(132);
	test::eq(o.is_array(), true);
	test::eq(o.len(), 2);
	test::eq(o.get_string_at(0)!!, "hello");
}

fn void trailing_garbage_rejected()
{
	test::@error(json::tparse(`{"a":1} xyz`), json::UNEXPECTED_CHARACTER);
	test::@error(json::unmarshal(tmem, `{"a":1} xyz`, OneKey), json::UNEXPECTED_CHARACTER);
}


struct TwoSlices    { String[] a; String[] b; bool flag; }
struct ThreeSlices  { String[] a; String[] b; String[] c; bool flag; }
struct SliceOfSlice { String[][] g; bool flag; }
struct InnerSlice   { String[] xs; }
struct OuterSlice   { InnerSlice inner; String[] outer; bool flag; }

fn void cleanup_two_slices_second_fails()
{
	test::@error(json::unmarshal(mem, `{"a":["x","y"],"b":["p",7],"flag":true}`, TwoSlices),
	             json::UNEXPECTED_CHARACTER);
}

fn void cleanup_three_slices_third_fails()
{
	test::@error(json::unmarshal(mem, `{"a":["x"],"b":["y"],"c":["z",7],"flag":true}`, ThreeSlices),
	             json::UNEXPECTED_CHARACTER);
}

fn void cleanup_slice_of_slices_fails()
{
	test::@error(json::unmarshal(mem, `{"g":[["a","b"],["c",7]],"flag":true}`, SliceOfSlice),
	             json::UNEXPECTED_CHARACTER);
}

fn void cleanup_nested_struct_with_slice()
{
	test::@error(json::unmarshal(mem, `{"inner":{"xs":["a","b"]},"outer":["c","d"],"flag":9}`, OuterSlice),
	             json::UNEXPECTED_CHARACTER);
}

fn void cleanup_stream_input()
{
	ByteReader r;
	r.init(`{"tags":["a","bb"],"flag":"nope"}`);
	test::@error(json::unmarshal(mem, &r, Tagged), json::UNEXPECTED_CHARACTER);
}


fn void cleanup_dom_path_same_shapes()
{
	test::@error(object::unmarshal{ThreeSlices}(mem,
		json::tparse(`{"a":["x"],"b":["y"],"c":["z",7],"flag":true}`)!!),
		encoding::MALFORMED_DATA);
	test::@error(object::unmarshal{SliceOfSlice}(mem,
		json::tparse(`{"g":[["a"],["c",7]],"flag":true}`)!!),
		encoding::MALFORMED_DATA);
}


fn void recursively_free_zeroes_and_is_idempotent()
{
	Object* dom = json::tparse(`{"tags":["a","bb"],"pts":[{"x":1,"y":2}],"flag":true}`)!!;
	Deep d = object::unmarshal{Deep}(mem, dom)!!;
	test::eq(d.tags.len, 2);

	object::recursively_free{Deep}(mem, &d);
	test::eq(d.tags.len, 0);
	test::eq(d.pts.len, 0);

	// second pass must not double free
	object::recursively_free{Deep}(mem, &d);
}


fn void string_path_leaks_on_error()
{
	test::@error(json::unmarshal(mem, `{"tags":["a","bb"],"flag":"nope"}`, Tagged),
	             json::UNEXPECTED_CHARACTER);
}

fn void typedef_dom_path()
{
	Reading r = object::unmarshal{Reading}(tmem, json::tparse(`{"temp":21,"ok":true}`)!!)!!;
	test::eq((int)r.temp, 21);
}

fn void trailing_garbage_after_success_leaks()
{
	test::@error(json::unmarshal(mem, `{"tags":["a","bb"],"flag":true} xyz`, Tagged),
	             json::UNEXPECTED_CHARACTER);
}
