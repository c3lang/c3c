module time_saturation_test @test;
import std::time;

// A fixed, mid-range timestamp, far from both rails.
const Time T = (Time)1_700_000_000_000_000;

// --- Duration constructors -------------------------------------------------

fn void duration_constructors()
{
	test::eq((long)time::us(7), 7);
	test::eq((long)time::ms(7), 7 * (long)time::MS);
	test::eq((long)time::sec(7), 7 * (long)time::SEC);
	test::eq((long)time::min(7), 7 * (long)time::MIN);
	test::eq((long)time::hour(7), 7 * (long)time::HOUR);
	test::eq((long)time::day(7), 7 * (long)time::DAY);
	test::eq((long)time::week(7), 7 * (long)time::WEEK);
	test::eq((long)time::month(7), 7 * (long)time::MONTH);
	test::eq((long)time::year(7), 7 * (long)time::YEAR);
}

fn void duration_constructors_negative()
{
	test::eq((long)time::sec(-7), -7 * (long)time::SEC);
	test::eq((long)time::day(-7), -7 * (long)time::DAY);
	test::eq((long)time::us(long::min), long::min);
	test::eq((long)time::us(long::max), long::max);
}

// --- Time + Duration -------------------------------------------------------

fn void add_duration_identities()
{
	// FAR_FUTURE + duration = FAR_FUTURE
	test::eq((long)(time::FAR_FUTURE + time::HOUR), long::max);
	// FAR_PAST + duration = FAR_PAST
	test::eq((long)(time::FAR_PAST + time::HOUR), long::min);
	// time + FOREVER = FAR_FUTURE
	test::eq((long)(T + time::FOREVER), long::max);
	// time + FOREVER_PAST = FAR_PAST
	test::eq((long)(T + time::FOREVER_PAST), long::min);
	// FAR_FUTURE + FOREVER_PAST = FAR_FUTURE (left operand wins)
	test::eq((long)(time::FAR_FUTURE + time::FOREVER_PAST), long::max);
	// FAR_PAST + FOREVER = FAR_PAST (left operand wins)
	test::eq((long)(time::FAR_PAST + time::FOREVER), long::min);
}

fn void add_duration_saturates_without_sentinel()
{
	// Overflow with two ordinary operands still saturates rather than wrapping.
	Time near_max = (Time)(long::max - 10);
	test::eq((long)(near_max + (Duration)1000), long::max);

	Time near_min = (Time)(long::min + 10);
	test::eq((long)(near_min + (Duration)(-1000)), long::min);
}

fn void sub_duration_identities()
{
	// FAR_FUTURE - duration = FAR_FUTURE
	test::eq((long)(time::FAR_FUTURE - time::HOUR), long::max);
	// FAR_PAST - duration = FAR_PAST
	test::eq((long)(time::FAR_PAST - time::HOUR), long::min);
	// time - FOREVER = FAR_PAST
	test::eq((long)(T - time::FOREVER), long::min);
	// time - FOREVER_PAST = FAR_FUTURE
	test::eq((long)(T - time::FOREVER_PAST), long::max);
	// Left operand wins when both are sentinels.
	test::eq((long)(time::FAR_FUTURE - time::FOREVER), long::max);
	test::eq((long)(time::FAR_PAST - time::FOREVER_PAST), long::min);
}

fn void sub_duration_saturates_without_sentinel()
{
	Time near_min = (Time)(long::min + 10);
	test::eq((long)(near_min - (Duration)1000), long::min);

	Time near_max = (Time)(long::max - 10);
	test::eq((long)(near_max - (Duration)(-1000)), long::max);
}

// --- Time difference -------------------------------------------------------

fn void diff_us_identities()
{
	// FAR_FUTURE - time = FOREVER
	test::eq((long)time::FAR_FUTURE.diff_us(T), long::max);
	// FAR_FUTURE - FAR_FUTURE = FOREVER (not zero)
	test::eq((long)time::FAR_FUTURE.diff_us(time::FAR_FUTURE), long::max);
	// FAR_PAST - time = FOREVER_PAST
	test::eq((long)time::FAR_PAST.diff_us(T), long::min);
	// FAR_PAST - FAR_PAST = FOREVER_PAST (not zero)
	test::eq((long)time::FAR_PAST.diff_us(time::FAR_PAST), long::min);
}

fn void diff_us_saturates_without_sentinel()
{
	// The original bug: this used to wrap to -1.
	test::eq((long)time::FAR_FUTURE.diff_us(time::FAR_PAST), long::max);
	test::eq((long)time::FAR_PAST.diff_us(time::FAR_FUTURE), long::min);
}

fn void diff_us_exact_in_range()
{
	Time later = T + time::HOUR;
	test::eq((long)later.diff_us(T), (long)time::HOUR);
	test::eq((long)T.diff_us(later), -(long)time::HOUR);
	test::eq((long)T.diff_us(T), 0);
}

// --- Float conversions -----------------------------------------------------

fn void time_to_float_rails()
{
	test::eq(time::FAR_FUTURE.to_seconds(), double::inf);
	test::eq(time::FAR_PAST.to_seconds(), -double::inf);
	test::eq(T.to_seconds(), (double)(long)T / (double)time::SEC);
}

fn void diff_float_rails()
{
	test::eq(time::FAR_FUTURE.diff_sec(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_min(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_hours(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_days(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_weeks(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_months(T), double::inf);
	test::eq(time::FAR_FUTURE.diff_years(T), double::inf);

	test::eq(time::FAR_PAST.diff_sec(T), -double::inf);
	test::eq(time::FAR_PAST.diff_years(T), -double::inf);
}

fn void diff_float_exact_in_range()
{
	Time later = T + time::DAY;
	test::eq_approx(later.diff_sec(T), 86_400.0);
	test::eq_approx(later.diff_min(T), 1440.0);
	test::eq_approx(later.diff_hours(T), 24.0);
	test::eq_approx(later.diff_days(T), 1.0);
}

// --- Duration conversions --------------------------------------------------

fn void duration_conversions_at_rails()
{
	// FOREVER behaves as an infinity across every conversion.
	test::eq(time::FOREVER.to_sec(), double::inf);
	test::eq(time::FOREVER.to_ms(), long::max);
	test::eq((long)time::FOREVER.to_nano(), long::max);

	test::eq(time::FOREVER_PAST.to_sec(), -double::inf);
	test::eq(time::FOREVER_PAST.to_ms(), long::min);
	test::eq((long)time::FOREVER_PAST.to_nano(), long::min);
}

fn void duration_conversions_in_range()
{
	Duration d = time::sec(3);
	test::eq_approx(d.to_sec(), 3.0);
	test::eq(d.to_ms(), 3_000);
	test::eq((long)d.to_nano(), 3_000_000_000);
}

fn void duration_to_nano_saturates()
{
	// Anything beyond ~292 years of microseconds overflows nanoseconds.
	Duration big = (Duration)(long::max / 100);
	test::eq((long)big.to_nano(), long::max);

	Duration small = (Duration)(long::min / 100);
	test::eq((long)small.to_nano(), long::min);
}

fn void duration_mult_saturates()
{
	test::eq((long)(time::FOREVER * 1000), long::max);
	test::eq((long)(time::FOREVER_PAST * 1000), long::min);
	test::eq((long)(time::sec(3) * 4), 12 * (long)time::SEC);
	test::eq((long)(time::sec(3) * -4), -12 * (long)time::SEC);
}

// --- from_float ------------------------------------------------------------

fn void from_float_infinities()
{
	test::eq((long)time::from_float(double::inf), long::max);
	test::eq((long)time::from_float(-double::inf), long::min);
}

fn void from_float_in_range()
{
	test::eq((long)time::from_float(1.5), 1_500_000);
	test::eq((long)time::from_float(-1.5), -1_500_000);
	test::eq((long)time::from_float(0.0), 0);
}

// --- Time.add_* helpers ----------------------------------------------------

fn void add_helpers_in_range()
{
	test::eq((long)T.add_seconds(2), (long)T + 2 * (long)time::SEC);
	test::eq((long)T.add_minutes(2), (long)T + 2 * (long)time::MIN);
	test::eq((long)T.add_hours(2), (long)T + 2 * (long)time::HOUR);
	test::eq((long)T.add_days(2), (long)T + 2 * (long)time::DAY);
	test::eq((long)T.add_weeks(2), (long)T + 2 * (long)time::WEEK);
}

fn void add_helpers_at_rails()
{
	test::eq((long)time::FAR_FUTURE.add_seconds(1), long::max);
	test::eq((long)time::FAR_FUTURE.add_days(1), long::max);
	test::eq((long)time::FAR_PAST.add_seconds(-1), long::min);
	test::eq((long)time::FAR_PAST.add_days(-1), long::min);
}

// --- Ordering --------------------------------------------------------------

fn void rails_order_correctly()
{
	test::lt((long)time::FAR_PAST, (long)T);
	test::lt((long)T, (long)time::FAR_FUTURE);
	test::eq(time::FAR_FUTURE.compare_to(T), 1);
	test::eq(time::FAR_PAST.compare_to(T), -1);
	test::eq(T.compare_to(T), 0);
}
