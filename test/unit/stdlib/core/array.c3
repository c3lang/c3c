module array_test;
import std::io;

struct TestStructZip (Printable)
{
	int a;
	int b;
}

fn TestStructZip TestStructZip.mult(self, TestStructZip other) @operator(*)
{
	self.a *= other.a;
	self.b *= other.b;
	return self;
}

fn bool TestStructZip.eq(self, TestStructZip other) @operator(==)
{
	return self.a == other.a && self.b == other.b;
}

fn usz? TestStructZip.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("{ %d, %d }", self.a, self.b);
}

module array_test @test;
import std::collections::pair, std::collections::list;


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
	TestStructZip[] right = { {1, 2} };

	Pair{String, TestStructZip}[] expected = { {"abcde", {1, 2}}, {"123456", {100, 200}}, {"zzz", {100, 200}} };

	Pair{String, TestStructZip}[] zipped = array::@tzip(left, right, fill_with: (TestStructZip){100, 200});

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
	TestStructZip[] left = { {1, 2}, {300, 400} };
	TestStructZip[] right = { {-1, -1} };

	TestStructZip[] expected = { {-1, -2}, {600, 1200} };

	TestStructZip[] zipped = array::@tzip(left, right, fn TestStructZip (TestStructZip a, TestStructZip b) => a * b, (TestStructZip){2, 3});

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
