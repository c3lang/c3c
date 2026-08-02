module csv_test @test;
import std::encoding::csv;
import std::io;

import std::collections;

struct TestCase
{
	String   input;
	String[] want;
	String   sep;
}

fn void line_endings()
{
	String[] test_cases = {
		"name,age\nAlice,30\nBob,25\n",
		"name,age\r\nAlice,30\r\nBob,25\r\n",
		"name,age\rAlice,30\rBob,25\r",
		"name,age\r\nAlice,30\nBob,25\r",
	};

	foreach (test : test_cases)
	{
		CsvReader r;
		r.init((ByteReader) {}.init(test));

		CsvRow row = r.tread_row()!!;

		test::@check(row.len() == 2, "Line ending was not correctly handled on case %s", test.tescape());
	}
}

// TODO correct the test
fn void trailing_newline()
{
	String with_trailing = "name,age\r\nAlice,30\r\n";
	String without_trailing = "name,age\r\nAlice,30";

	CsvReader with, without;
	with.init((ByteReader) {}.init(with_trailing));
	without.init((ByteReader) {}.init(without_trailing));

	test::@check(with.tread_row()!!.len() == without.tread_row()!!.len(), "Incorrectly handled trailing newline");
}

fn void string_escape()
{
	String csv = "name,age,occupation\r\nAlice,30,\"Cashier,Waitress\"";

	CsvReader r;
	r.init((ByteReader){}.init(csv));

	r.skip_row();

	test::@check(r.tread_row()!!.len() == 3, "Incorrectly escaped quotes in %s", csv.tescape());
}

fn void unique_header_entries()
{
	String csv = "name,age,gender,age\r\nAlice,30,female,20";

	CsvReader r;
	r.init((ByteReader) {}.init(csv));
	CsvRow header = r.tread_row()!!;

	for (sz i = 0; i < header.len() - 1; i++)
	{
		for (sz j = i + 1; j < header.len(); j++)
		{
			test::@check(header.get_col(i) != header.get_col(j), "Duplicate header entry found");
		}
	}
}

fn void csv_each_row()
{
	TestCase[] tests = {
		{
			"aaa,bbb,ccc",
			{"aaa","bbb","ccc"},
			","
		},
		{
			"aaa;bbb;ccc",
			{"aaa", "bbb", "ccc"},
			";"
		},
		{
			"aaa,bbb,ccc\n111,222,333",
			{"aaa", "bbb", "ccc", "111", "222", "333"},
			","
		},
		{
			"aaa , bbb\t ,ccc\r\n 111,222,333\r\n",
			{"aaa ", " bbb\t ", "ccc", " 111", "222", "333"},
			","
		},
	};
	foreach (t : tests)
	{
		String[] want = t.want;

		CsvReader r;
		r.init((ByteReader){}.init(t.input), t.sep);
		r.@each_row(; String[] row) {
			foreach (i, s : row)
			{
				assert(want.len > 0, "more records than we want");
				assert(s == want[0], "columns do not match; got: '%s', want: '%s'", s, want[0]);
				want = want[1..];
			}
		};
		assert(want.len == 0, "not enough records found");
	}
	foreach (t : tests)
	{
		String[] want = t.want;
		CsvReader r;

		csv::@each_row((ByteReader){}.init(t.input), t.sep; String[] row)
		{
			foreach (i, s : row)
			{
				assert(want.len > 0, "more records than we want");
				assert(s == want[0], "columns do not match; got: '%s', want: '%s'", s, want[0]);
				want = want[1..];
			}
		};
		assert(want.len == 0, "not enough records found");
	}
}

fn void csv_row()
{
	TestCase t = {
		"aaa,bbb,ccc",
		{"aaa", "bbb", "ccc"},
		","
	};
	CsvReader r;
	r.init((ByteReader){}.init(t.input), t.sep);
	CsvRow row = r.tread_row()!!;
	assert(row.list.len == t.want.len, "not enough records found");
	for (int i = 0; i < row.list.len; i++) {
		assert(row.list[i] == t.want[i],"columns do not match; "
			"got: '%s', want: '%s'", row.list[i], t.want[i]);
	}
}
