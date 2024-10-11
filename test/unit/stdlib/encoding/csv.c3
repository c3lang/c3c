module csv_test @test;
import std::encoding::csv;
import std::io;

struct TestCase {
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
	foreach (t : tests) {
		String[] want = t.want;

		CsvReader r;
		r.init(ByteReader{}.init(t.input), t.sep);
		r.@each_row(; String[] row) {
			foreach (i, s : row) {
				assert(want.len > 0,
					"more records than we want");
				assert(s == want[0], "columns do not match; "
					"got: '%s', want: '%s'", s, want[0]);
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
	r.init(ByteReader{}.init(t.input), t.sep);
	CsvRow row = r.read_temp_row()!!;
	assert(row.list.len == t.want.len, "not enough records found");
	for (int i = 0; i < row.list.len; i++) {
		assert(row.list[i] == t.want[i],"columns do not match; "
			"got: '%s', want: '%s'", row.list[i], t.want[i]);
	}
}
