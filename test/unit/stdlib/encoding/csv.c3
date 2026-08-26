module csv_test @test;
import std::encoding::csv;
import std::io;

import std::collections;

struct TestCase
{
	String input;
	String[] want;
	String sep;
}

fn void line_endings()
{
	String[] test_cases = {
		"Alice,30\nBob,25\n",
		"Alice,30\rBob,25\r",
		"Alice,30\nBob,25\r",
	};

	foreach (test : test_cases)
	{
		CsvReader r;
		r.tinit((ByteReader) { }.init(test), ",", false, false)!!;

		CsvRow row = r.read_row()!!;
		test::@check(row.len() != 2, "Line ending was not correctly handled on case %s", test.tescape());
		r.free();

		r.tinit((ByteReader) { }.init(test), ",", false, true)!!;

		row = r.read_row()!!;
		test::@check(row.len() == 2, "Line ending was not correctly handled on case %s", test.tescape());
	}
}

fn void trailing_newline()
{
	String with_trailing    = "name,age\r\nAlice,30\r\n";
	String without_trailing = "name,age\r\nAlice,30";

	CsvReader with, without;
	with.tinit((ByteReader) { }.init(with_trailing))!!;
	without.tinit((ByteReader) { }.init(without_trailing))!!;
	sz with_count, without_count;

	with.@each_row(; row)
	{
		with_count++;
	};
	without.@each_row(; row)
	{
		without_count++;
	};

	test::@check(with_count == without_count, "Incorrectly handled trailing newline");
}

fn void string_escape()
{
	String csv = "name,age,occupation\r\nAlice,30,\"Cashier,Waitress\"";

	CsvReader r;
	r.tinit((ByteReader) { }.init(csv))!!;

	test::@check(r.read_row()!!.len() == 3, "Incorrectly escaped quotes in %s", csv.tescape());
}

fn void unique_header_entries()
{
	String csv = "name,age,gender,age\r\nAlice,30,female,20";

	CsvReader r;
	test::@error(r.tinit((ByteReader) { }.init(csv)), csv::NON_UNIQUE_ENTRY);
}

fn void csv_each_row()
{
	TestCase[] tests = {
		{
			"aaa,bbb,ccc",
			{ "aaa", "bbb", "ccc" },
			"," 
		},
		{
			"aaa;bbb;ccc",
			{ "aaa", "bbb", "ccc" },
			";"
		},
		{
			"aaa,bbb,ccc\n111,222,333",
			{ "aaa", "bbb", "ccc", "111", "222", "333" },
			","
		},
		{
			"aaa , bbb\t ,ccc\r\n 111,222,333\r\n",
			{ "aaa ", " bbb\t ", "ccc", " 111", "222", "333" },
			","
		},
	};
	foreach (t : tests)
	{
		String[] want = t.want;

		CsvReader r;
		r.tinit((ByteReader) { }.init(t.input), t.sep, false, true)!!;
		r.@each_row(; String[] row)
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
	foreach (t : tests)
	{
		String[] want = t.want;

		csv::@each_row(tmem, (ByteReader) { }.init(t.input), t.sep, true ; String[] row)
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
	TestCase t = { "aaa,bbb,ccc", { "aaa", "bbb", "ccc" }, "," };
	CsvReader r;
	r.tinit(
		(ByteReader) { }.init(t.input),
		t.sep,
		false,
	)!!;
	CsvRow row = r.read_row()!!;
	assert(row.list.len == t.want.len, "not enough records found");
	for (int i = 0; i < row.list.len; i++)
	{
		assert(row.list[i] == t.want[i], "columns do not match; " "got: '%s', want: '%s'", row.list[i], t.want[i]);
	}
}
