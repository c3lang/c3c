module datetime_test @test;
import std::time;

fn void test_parse_and_add()
{
DateTime d = datetime::from_date(1973, APRIL, 27);
	assert(d.year == 1973);
	assert(d.month == APRIL);
	assert(d.day == 27);
	assert(d.hour == 0);
	assert(d.min == 0);
	d = d.add_weeks(1);
	assert(d.year == 1973);
    assert(d.month == MAY);
    assert(d.day == 4);
    assert(d.hour == 0);
    assert(d.min == 0);
	d = d.add_years(2);
	assert(d.year == 1975);
    assert(d.month == MAY);
    assert(d.day == 4);
    assert(d.hour == 0);
    assert(d.min == 0);
    DateTime x = d.add_months(-14);
    assert(x.year == 1974);
    assert(x.month == MARCH);
    assert(x.day == 4);
    assert(x.hour == 0);
    assert(x.min == 0);
    x = d.add_months(-12);
    assert(x.year == 1974);
    assert(x.month == MAY);
    x = d.add_months(12);
    assert(x.year == 1976);
    assert(x.month == MAY);
    x = d.add_months(3);
    assert(x.year == 1975);
    assert(x.month == AUGUST);
    x = d.add_months(15);
    assert(x.year == 1976);
    assert(x.month == AUGUST);
    x = d.add_months(0);
    assert(x.year == 1975);
    assert(x.month == MAY);
}

fn void test_timezone()
{
	int offset_hours = 7;
	int offset = offset_hours * 3600;

	int target_offset_hours = -4;
	int target_offset = target_offset_hours * 3600;

	DateTime d1 = datetime::from_date(2022, OCTOBER, 15);
	TzDateTime d2 = datetime::from_date_tz(2022, OCTOBER, 15, gmt_offset: offset);

	DateTime dt;
	TzDateTime tz;

	Time t1 = d1.time;
	Time t2 = d2.time;

	assert(t1 == 1665792000000000);
	assert(t2 == 1665766800000000);

	// to_gmt_offset should keep the timesampt value
	tz = d1.to_gmt_offset(offset);
	assert(tz.time == t1);
	assert(tz.year == 2022);
	assert(tz.month == OCTOBER);
	assert(tz.day == 15);
	assert(tz.hour == offset_hours);
	assert(tz.min == 0);
	assert(tz.gmt_offset == offset);

	// with_gmt_offset should keep the date/time values and adjust the timestamp
	tz = d1.with_gmt_offset(offset);
	assert(tz.time == t2);
	assert(tz.year == d1.year);
	assert(tz.month == d1.month);
	assert(tz.day == d1.day);
	assert(tz.hour == d1.hour);
	assert(tz.min == d1.min);
	assert(tz.gmt_offset == offset);

	dt = datetime::from_time(t1);
	assert(dt.day == 15);
	assert(dt.hour == 0);
	dt = datetime::from_time(t2);
	assert(dt.day == 14);
	assert(dt.hour == 24 - offset_hours);

	// from_time_tz should keep the timesampt value
	tz = datetime::from_time_tz(t1, offset);
	assert(tz.time == t1);
	assert(tz.day == 15);
	assert(tz.hour == offset_hours);
	assert(tz.gmt_offset == offset);
	tz = datetime::from_time_tz(t2, offset);
	assert(tz.time == t2);
	assert(tz.day == 15);
	assert(tz.hour == 0);
	assert(tz.gmt_offset == offset);

	// The add_* methods should adjust the targeted date/time values while
	//   keeping the others unchanged. The gmt_offset should be kept as well.
	dt = d1.add_hours(1);
	assert(dt.day == 15);
	assert(dt.hour == 1);
	tz = d2.add_hours(1);
	assert(tz.day == 15);
	assert(tz.hour == 1);
	assert(tz.gmt_offset == offset);

	dt = d1.add_days(1);
	assert(dt.day == 16);
	assert(dt.hour == 0);
	tz = d2.add_days(1);
	assert(tz.day == 16);
	assert(tz.hour == 0);
	assert(tz.gmt_offset == offset);

	dt = d1.add_months(1);
	assert(dt.month == NOVEMBER);
	assert(dt.day == 15);
	assert(dt.hour == 0);
	tz = d2.add_months(1);
	assert(tz.month == NOVEMBER);
	assert(tz.day == 15);
	assert(tz.hour == 0);
	assert(tz.gmt_offset == offset);

	// Conversion from GMT+7 to GMT-4
	tz = d2.to_gmt_offset(target_offset);
	assert(tz.year == d2.year);
	assert(tz.month == d2.month);
	assert(tz.day == d2.day - 1);
	assert(tz.hour == 24 - (offset_hours - target_offset_hours));
	assert(tz.time == t2);
	assert(tz.gmt_offset == target_offset);

	// Conversion with the same offset
	tz = d2.to_gmt_offset(offset);
	assert(tz.year == d2.year);
	assert(tz.month == d2.month);
	assert(tz.day == d2.day);
	assert(tz.hour == d2.hour);
	assert(tz.time == t2);
	assert(tz.gmt_offset == offset);
}
