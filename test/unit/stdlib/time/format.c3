module timeformat_test @test;

import std::time::datetime, std::collections::list, std::collections::triple;

def FormatTzTestSpec = Triple(<TzDateTime, DateTimeFormat, String>);
def FormatTestSpec = Triple(<DateTime, DateTimeFormat, String>);

fn void test_with_tz()
{
	FormatTzTestSpec[*] tests = {
		{ datetime::from_date(1970, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(0), RFC1123, "Thu, 01 Jan 1970 00:00:00 GMT" },
		{ datetime::from_date(1994, Month.from_ordinal(10), 6, 8, 49, 37).with_gmt_offset(0), RFC1123, "Sun, 06 Nov 1994 08:49:37 GMT" },
		{ datetime::from_date(2020, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(0), RFC1123, "Wed, 01 Jan 2020 00:00:00 GMT" },
		{ datetime::from_date(2020, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(-3600), RFC1123, "Wed, 01 Jan 2020 01:00:00 GMT" },
		{ datetime::from_date(2020, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(-3600), RFC1123Z, "Wed, 01 Jan 2020 00:00:00 -0100" },
		{ datetime::from_date(2020, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(0), RFC1123Z, "Wed, 01 Jan 2020 00:00:00 -0000" },
		{ datetime::from_date(2020, Month.JANUARY, 1, 0, 0, 0).with_gmt_offset(0), RFC3339, "2020-01-01T00:00:00Z" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 5, 999998).with_gmt_offset(0), RFC3339MS, "2006-01-02T15:04:05.999998Z" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 5, 999998).with_gmt_offset(25200), RFC3339Z, "2006-01-02T15:04:05+07:00" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 5, 999998).with_gmt_offset(25200), RFC3339ZMS, "2006-01-02T15:04:05.999998+07:00" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05).with_gmt_offset(0), RFC822, "02 Jan 06 15:04 GMT" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05).with_gmt_offset(0), RFC822Z, "02 Jan 06 15:04 -0000" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05).with_gmt_offset(0), RFC850, "Monday, 02-Jan-06 15:04:05 GMT" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05).with_gmt_offset(0), UNIXDATE, "Mon Jan  2 15:04:05 GMT 2006" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05).to_gmt_offset(0), RUBYDATE, "Mon Jan  2 15:04:05 -0000 2006" },
	};

	foreach (test : tests)
	{
		String candidate = test.first.new_format(test.second);
		assert(candidate == test.third, "got: '%s', expected: '%s'", candidate, test.third);
	}
}

fn void test_without_tz()
{
	FormatTestSpec[*] tests = {
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05), ANSIC, "Mon Jan  2 15:04:05 2006" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05), DATETIME, "2006-01-02 15:04:05" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05), DATEONLY, "2006-01-02" },
		{ datetime::from_date(2006, Month.JANUARY, 2, 15, 4, 05), TIMEONLY, "15:04:05" },
	};

	foreach (test : tests)
	{
		String candidate = test.first.new_format(test.second);
		assert(candidate == test.third, "got: '%s', expected: '%s'", candidate, test.third);
	}
}
