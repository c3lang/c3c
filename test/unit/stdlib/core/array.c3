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
	=> self.a == other.a && self.b == other.b;

fn usz? TestStructZip.to_format(&self, Formatter* f) @dynamic
	=> f.printf("{ %d, %d }", self.a, self.b);


module array_test @test;

import std::collections::pair;


fn void contains()
{
	int[3] a = { 1, 2, 3 };
	assert(array::contains(a, 2) == true);
	assert(array::contains(a, 15) == false);
}

fn void find()
{
	int[3] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == NOT_FOUND);
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
	assert (c == (int[]){ 2, 3, 1, 2, 3 });
}


fn void zip() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0} };

	Pair{char, long}[] zipped = array::tzip(left, right);

	assert(zipped.len == 3);
	foreach (i, c : zipped) assert(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_longest() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0}, {'d', 0}, {'e', 0} };

	Pair{char, long}[] zipped = array::tzip_longest(left, right);

	assert(zipped.len == 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_longest_fill() => @pool()
{
	char[] left = "abcde";
	long[] right = { -1, 0x8000, 0 };

	Pair{char, long}[] expected = { {'a', -1}, {'b', 0x8000}, {'c', 0}, {'d', 0x40}, {'e', 0x40} };

	Pair{char, long}[] zipped = array::tzip_longest(left, right, 0x40);

	assert(zipped.len == 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_longest_fill_string() => @pool()
{
	String[] left = { "abcde", "123456" };
	long[] right = { -1, 0x8000, 20, 30, 40 };

	Pair{String, long}[] expected = { {"abcde", -1}, {"123456", 0x8000}, {"aaa", 20}, {"aaa", 30}, {"aaa", 40} };

	Pair{String, long}[] zipped = array::tzip_longest(left, right, "aaa");

	assert(zipped.len == 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_longest_fill_struct() => @pool()
{
	String[] left = { "abcde", "123456", "zzz" };
	TestStructZip[] right = { {1, 2} };

	Pair{String, TestStructZip}[] expected = { {"abcde", {1, 2}}, {"123456", {100, 200}}, {"zzz", {100, 200}} };

	Pair{String, TestStructZip}[] zipped = array::tzip_longest(left, right, (TestStructZip){100, 200});

	assert(zipped.len == 3);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with() => @pool()
{
	char[] left = "abcde";
	char[] right = { 0x05, 0x04, 0x03, 0x00 };

	char[] expected = "fffd";

	char[] zipped = array::tzip_with(left, right, fn char (char a, char b) => a + b);

	assert(zipped.len == 4);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_longest() => @pool()
{
	char[] left = "abcde";
	char[] right = { 0x05, 0x04 };

	char[] expected = "ffcde";

	char[] zipped = array::tzip_with_longest(left, right, fn char (char a, char b) => a + b);

	assert(zipped.len == 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_longest_fill() => @pool()
{
	char[] left = "abcde";
	char[] right = { 0x05, 0x04 };

	char[] expected = "ffghi";

	char[] zipped = array::tzip_with_longest(left, right, fn char (char a, char b) => a + b, 0x04);

	assert(zipped.len == 5);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_longest_fill_string() => @pool()
{
	String[] left = { "Hello", "World", "Foo", "Bazzy" };
	String[] right = { " there", "!" };

	String[] expected = { "Hello there", "World!", "FooBar", "BazzyBar" };

	String[] zipped = array::tzip_with_longest(left, right, fn String (String a, String b) => a.tconcat(b), fill_value: "Bar");

	assert(zipped.len == 4);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_longest_fill_struct() => @pool()
{
	TestStructZip[] left = { {1, 2}, {300, 400} };
	TestStructZip[] right = { {-1, -1} };

	TestStructZip[] expected = { {-1, -2}, {600, 1200} };

	TestStructZip[] zipped = array::tzip_with_longest(
		left, right, fn TestStructZip (TestStructZip a, TestStructZip b) => a * b, fill_value: (TestStructZip){2, 3});

	assert(zipped.len == 2);
	foreach (i, c : zipped) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}

fn void zip_with_into()
{
	char[] left = { '1', '2', '3', '4' };
	String[] right = { "one", "two", "three", "four", "five", "six" };

	char[] expected = { '4', '5', '8', '8' };

	array::zip_with_into(left, right, fn char (char a, String b) => a + (char)b.len);

	assert(left.len == 4);
	foreach (i, c : left) test::@check(c == expected[i], "Mismatch on index %d: %s (actual) != %s (expected)", i, c, expected[i]);
}
