module csv_test @test;
import std::encoding::csv;
import std::io;

struct TestCase
{
	String   input;
	String[] want;
	String   sep;
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

macro String[] parse_row(String input, String sep = ",", CsvQuoteParsing parsing = REMOVE_QUOTES, char quote = '"')
{
	CsvReader r;
	r.init((ByteReader){}.init(input), sep, parsing, quote);
	return r.tread_row()!!.list;
}

fn void csv_quotes()
{
	assert(parse_row(`"a,b",c,""`) == { "a,b", "c", "" });
	assert(parse_row(`"say ""hi""",5" screen`) == { `say "hi"`, `5" screen` });
	assert(parse_row("'a;b';'it''s'", ";", quote: '\'') == { "a;b", "it's" });
	assert(parse_row(`"a,b",c`, parsing: NONE) == { `"a`, `b"`, "c" });
	assert(parse_row(`"x::y"::z::`, "::") == { "x::y", "z", "" });
}

fn void csv_quoted_row()
{
	CsvReader r;
	r.init((ByteReader){}.init(`"a,b",c`));
	assert(r.tread_row()!!.list == { `"a`, `b"`, "c" });
	r.init((ByteReader){}.init(`"a,b",c` "\n" `'x,y',z`), quote_parsing: REMOVE_QUOTES);
	assert(r.tread_row()!!.list == { "a,b", "c" });
	assert(r.tread_row()!!.list == { "'x", "y'", "z" });
	sz n;
	csv::@each_row((ByteReader){}.init(`"1,2",3` "\n" `"4,5",6`), quote_parsing: REMOVE_QUOTES; String[] cols)
	{
		assert(cols == (n ? (String[]){ "4,5", "6" } : (String[]){ "1,2", "3" }));
		n++;
	};
	assert(n == 2);
	n = 0;
	r.init((ByteReader){}.init("'1,2',3"), quote_parsing: REMOVE_QUOTES, quote_character: '\'');
	r.@each_row(; String[] cols)
	{
		assert(cols == { "1,2", "3" });
		n++;
	};
	assert(n == 1);
}
