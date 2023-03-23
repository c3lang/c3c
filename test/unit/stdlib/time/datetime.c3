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