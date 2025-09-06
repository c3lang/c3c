module array_test;
import std::io;

struct ArrTestStruct (Printable)
{
	int a;
	int b;
}

fn ArrTestStruct ArrTestStruct.mult(self, ArrTestStruct other) @operator(*)
{
	self.a *= other.a;
	self.b *= other.b;
	return self;
}

fn ArrTestStruct ArrTestStruct.add(self, ArrTestStruct other) @operator(+)
{
	self.a += other.a;
	self.b += other.b;
	return self;
}

fn bool ArrTestStruct.equals(self, ArrTestStruct other) @operator(==)
{
	return self.a == other.a && self.b == other.b;
}

fn usz? ArrTestStruct.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("{ %d, %d }", self.a, self.b);
}

module array_test @test;
import std::collections;


fn void contains()
{
	int[3] a = { 1, 2, 3 };
	assert(array::contains(a, 2));
	assert(!array::contains(a, 15));
}

fn void find()
{
	int[3] a = { 1, 2, 3 };
	test::eq(array::index_of(a, 2)!!, 1);
	test::eq(array::index_of(a, 1)!!, 0);
	test::eq(array::index_of(a, 3)!!, 2);
	test::@error(array::index_of(a, 4), NOT_FOUND);
}

fn void find_subarray()
{
	int[] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == NOT_FOUND);
}

fn void concat()
{
	int[3] a = { 1, 2, 3 };
	free(array::concat(mem, a, a));
	free(array::concat(mem, a[..], a[..]));
	free(array::concat(mem, a[:0], a[:0]));
	free(array::concat(mem, (int[2]) { 1, 2 }, a[:0]));
	free(array::concat(mem, a[:0], (int[2]) { 1, 2 }));
	int[] c = array::concat(mem, a[1..2], a);
	defer free(c);
	assert(c == (int[]){ 2, 3, 1, 2, 3 });
}


fn void reduce() => @pool()
{
	int[] int_slice = { 1, 8, 12 };
	test::eq(2 * 1 * 8 * 12, array::@reduce(int_slice, 2, fn (i, e, u) => i * e));
	test::eq(array::@product(int_slice, 2), array::@reduce(int_slice, 2, fn (i, e, u) => i * e));

	String[] c3_is_great = { "hello,", " world.", " C3 ", "is ", "great!" };
	test::eq("Ahem! hello, world. C3 is great!", array::@reduce(c3_is_great, "Ahem! ", fn (i, e, u) => i.tconcat(e)));

	List {String} l;
	l.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@reduce(l, (usz)0, fn (acc, e, u) => acc + e.len + u), 0 + (3 + 0) + (4 + 1) + (3 + 2) + (6 + 3));

	LinkedList {String} ll;
	ll.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@reduce(ll.array_view(), (usz)0, fn (acc, e, u) => acc + e.len + u), 0 + (3 + 0) + (4 + 1) + (3 + 2) + (6 + 3));
}

fn void sum()
{
	test::eq(7, array::@sum((int[]){1, 2, 4}));
	test::eq(12, array::@sum((int[]){1, 2, 4}, 5));
	test::eq(1, array::@sum((int[]){1, 2, 4}, -6));
	test::eq_approx(array::@sum((double[]){1.236, 2.38709, 4, -5.9}, 8.5), 10.223090);
	test::eq((ArrTestStruct){10,10}, array::@sum((ArrTestStruct[]){{1,1}, {4,4}, {-2,7}, {17,8}}, (ArrTestStruct){-10,-10}));

	List {int} l;
	l.push_all({ 1, 2, 3, 4 });
	test::eq(array::@sum(l), 10);
}

fn void product()
{
	test::eq(0, array::@product((int[]){0, 23534, 2224}));
	test::eq(64, array::@product((int[]){1, 2, 4, 8}));
	test::eq_approx(array::@product((double[]){1, 2, 4, -0.5}, -3), 12.0);
	test::eq((ArrTestStruct){1360,-2240}, array::@product((ArrTestStruct[]){{1,1}, {4,4}, {-2,7}, {17,8}}, (ArrTestStruct){-10,-10}));

	List {int} l;
	l.push_all({ 1, 2, 3, 4 });
	test::eq(array::@product(l), 24);
}

fn void indices_of() => @pool()
{
	int[] int_slice = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	test::eq(array::@tindices_of(int_slice, fn (e, u) => !!(e % 2)), (usz[]){0, 2, 4, 6, 8});
	test::eq(array::@tindices_of(int_slice, fn (e, u) => !(e % 2)), (usz[]){1, 3, 5, 7, 9});
	test::eq(array::@tindices_of(int_slice, fn (e, u) => e > 5), (usz[]){5, 6, 7, 8, 9});

	ArrTestStruct[] struct_slice = { {40, 5}, {0, 0}, {12, 13}, {3, 5} };
	test::eq(array::@tindices_of(struct_slice, fn (e, u) => e.a > 10), (usz[]){0, 2});
	test::eq(array::@tindices_of(struct_slice, fn (e, u) => e.b == 0 || e.a < 10), (usz[]){1, 3});

	List {String} l;
	l.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@tindices_of(l, fn (e, u) => e.len <= 3), (usz[]){0, 2});

	LinkedList {String} ll;
	ll.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@tindices_of(ll.array_view(), fn (e, u) => e.len <= 3), (usz[]){0, 2});
}

fn void filter() => @pool()
{
	int[] int_slice = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	test::eq(array::@tfilter(int_slice, fn (e, u) => !!(e % 2)), (int[]){1, 3, 5, 7, 9});
	test::eq(array::@tfilter(int_slice, fn (e, u) => !(e % 2)), (int[]){2, 4, 6, 8, 10});
	test::eq(array::@tfilter(int_slice, fn (e, u) => e > 5), (int[]){6, 7, 8, 9, 10});

	ArrTestStruct[] struct_slice = { {40, 5}, {0, 0}, {12, 13}, {3, 5} };
	foreach (i, &e : array::@tfilter(struct_slice, fn (e, u) => e.a > 10)) test::eq(*e, (ArrTestStruct[2]){ {40, 5}, {12, 13} }[i]);
	foreach (i, &e : array::@tfilter(struct_slice, fn (e, u) => e.b == 0 || e.a < 10)) test::eq(*e, (ArrTestStruct[2]){ {0, 0}, {3, 5} }[i]);

	List {String} l;
	l.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@tfilter(l, fn (e, u) => e.len <= 3), (String[]){ "abc", "ghi" });

	LinkedList {String} ll;
	ll.push_all({ "abc", "adef", "ghi", "a12345" });
	test::eq(array::@tfilter(ll.array_view(), fn (e, u) => e.len <= 3), (String[]){ "abc", "ghi" });
}

fn void any_true() => @pool()
{
	int[] int_slice = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	test::@check(array::@any(int_slice, fn (e, u) => e == 7));
	test::@check(!array::@any(int_slice, fn (e, u) => e > 10));

	ArrTestStruct[] struct_slice = { {0, 0}, {1, 1} };
	test::@check(array::@any(struct_slice, fn (e, u) => !e.a));
	test::@check(!array::@any(struct_slice, fn (e, u) => e.b != e.a));

	List {String} l;
	l.push_all({ "abc", "adef", "ghi", "a12345" });
	test::@check(array::@any(l, fn (e, u) => e.len > 5));
	test::@check(!array::@any(l, fn (e, u) => e == "hi"));

	LinkedList {String} ll;
	ll.push_all({ "abc", "adef", "ghi", "a12345" });
	test::@check(array::@any(ll.array_view(), fn (e, u) => e.len > 5));
	test::@check(!array::@any(ll.array_view(), fn (e, u) => e == "hi"));
}

fn void all_true() => @pool()
{
	int[] int_slice = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	test::@check(array::@all(int_slice, fn (e, u) => e < 100));
	test::@check(!array::@all(int_slice, fn (e, u) => e > 5));

	ArrTestStruct[] struct_slice = { {0, 0}, {1, 1} };
	test::@check(array::@all(struct_slice, fn (e, u) => e.a != 2));
	test::@check(!array::@all(struct_slice, fn (e, u) => (e.a + e.b) > 0));

	List {String} l;
	l.push_all({ "abc", "adef", "ghi", "a12345" });
	test::@check(array::@all(l, fn (e, u) => e.len > 1));
	test::@check(!array::@all(l, fn (e, u) => e.len > 3));

	LinkedList {String} ll;
	ll.push_all({ "abc", "adef", "ghi", "a12345" });
	test::@check(array::@all(ll.array_view(), fn (e, u) => e.len > 1));
	test::@check(!array::@all(ll.array_view(), fn (e, u) => e.len > 3));
}


fn void zip() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0} };

	Pair{char, long}[] zipped = array::@tzip(left, right);

	test::eq(zipped.len, 3);
	foreach (i, c : zipped) assert(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_list() => @pool()
{
	char[] left = "abcde";
	List{long} l;
	l.push(-1);
	l.push(0x8000);
	l.push(0);

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0} };

	Pair{char, long}[] zipped = array::@tzip(left, l);

	test::eq(zipped.len, 3);
	foreach (i, c : zipped) assert(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_fill_with_default() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0}, {'d', 0}, {'e', 0} };

	Pair{char, long}[] zipped = array::@tzip(left, right, fill_with: 0);

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_fill_with_char() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0}, {'d', 0x40}, {'e', 0x40} };

	Pair{char, long}[] zipped = array::@tzip(left, right, fill_with: 0x40);

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_fill_with_string() => @pool()
{
	String[] left = { "abcde", "123456" };
	long[] right = { -1, 0x8000, 20, 30, 40 };

	Pair{String, long}[] expected = { {"abcde", -1}, {"123456", 0x8000}, {"aaa", 20}, {"aaa", 30}, {"aaa", 40} };

	Pair{String, long}[] zipped = array::@tzip(left, right, fill_with: "aaa");

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_fill_with_struct() => @pool()
{
	String[] left = { "abcde", "123456", "zzz" };
	ArrTestStruct[] right = { {1, 2} };

	Pair{String, ArrTestStruct}[] expected = { {"abcde", {1, 2}}, {"123456", {100, 200}}, {"zzz", {100, 200}} };

	Pair{String, ArrTestStruct}[] zipped = array::@tzip(left, right, fill_with: (ArrTestStruct){100, 200});

	test::eq(zipped.len, 3);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with() => @pool()
{
	char[] left = "abcde";
	char[4] right = { 0x05, 0x04, 0x03, 0x00 };

	char[] expected = "fffd";

	char[] zipped = array::@tzip(left, right, fn char (char a, char b) => a + b);

	test::eq(zipped.len, 4);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_fill_with_default() => @pool()
{
	char[] left = "abcde";
	char[] right = { 0x05, 0x04 };

	char[] expected = "ffcde";

	char[] zipped = array::@tzip(left, right, fn char (char a, char b) => a + b, 0);

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_fill_with_char() => @pool()
{
	char[] left = "abcde";
	char[] right = { 0x05, 0x04 };

	char[] expected = "ffghi";

	char[] zipped = array::@tzip(left, right, fn char (char a, char b) => a + b, 0x04);

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_fill_with_pointers() => @pool()
{
	ZString field = "0123456789abcdefghijklmnopqrstuvwxyz-_=!";

	char*[] left = { &field[3], &field[1] };
	char[] right = { 0x05, 0x04, 0x0A, 0x10, 0x11 };

	char[] expected = "85agh";

	char[] zipped = array::@tzip(left, right, fn char (char* a, char b) => a[b], &field[0]);

	test::eq(zipped.len, 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_fill_with_string() => @pool()
{
	String[] left = { "Hello", "World", "Foo", "Bazzy" };
	String[] right = { " there", "!" };

	String[] expected = { "Hello there", "World!", "FooBar", "BazzyBar" };

	String[] zipped = array::@tzip(left, right, fn String (String a, String b) => a.tconcat(b), "Bar");

	test::eq(zipped.len, 4);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_fill_with_struct() => @pool()
{
	ArrTestStruct[] left = { {1, 2}, {300, 400} };
	ArrTestStruct[] right = { {-1, -1} };

	ArrTestStruct[] expected = { {-1, -2}, {600, 1200} };

	ArrTestStruct[] zipped = array::@tzip(left, right, fn ArrTestStruct (ArrTestStruct a, ArrTestStruct b) => a * b, (ArrTestStruct){2, 3});

	test::eq(zipped.len, 2);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_into()
{
	char[] left = { '1', '2', '3', '4' };
	String[6] right = { "one", "two", "three", "four", "five", "six" };

	char[] expected = { '4', '5', '8', '8' };

	array::@zip_into(left, right, fn (a, b) => a + (char)b.len);

	test::eq(left.len, 4);
	foreach (i, c : left) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_into_list() => @pool()
{
	List{char} l;
	l.push_all({ '1', '2', '3', '4' });
	String[6] right = { "one", "two", "three", "four", "five", "six" };

	char[] expected = { '4', '5', '8', '8' };

	array::@zip_into(l, right, fn (a, b) => a + (char)b.len);

	test::eq(l.len(), 4);
	foreach (i, c : l) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_into_linked_list() => @pool()
{
	LinkedList{char} l;
	l.push_all({ '1', '2', '3', '4' });
	String[6] right = { "one", "two", "three", "four", "five", "six" };

	char[] expected = { '4', '5', '8', '8' };

	array::@zip_into(l.array_view(), right, fn (a, b) => a + (char)b.len);

	test::eq(l.len(), 4);
	foreach (i, c : l.array_view()) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}
