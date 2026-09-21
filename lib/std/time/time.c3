module std::time;
import std::io, std::time::os;

typedef Time = long;
typedef Duration = long;
typedef Clock = ulong;
typedef NanoDuration (Printable) = long;

const Time FAR_FUTURE = (Time)long::max;
const Time FAR_PAST = (Time)long::min;

const NanoDuration NANO_DURATION_ZERO = (NanoDuration)0;
const Duration US            = (Duration)1;
const Duration MS            = (Duration)1_000;
const Duration SEC           = (Duration)1_000_000;
const Duration MIN           = (Duration)60 * SEC;
const Duration HOUR          = (Duration)60 * MIN;
const Duration DAY           = (Duration)24 * HOUR;
const Duration WEEK          = (Duration)7 * DAY;
const Duration MONTH         = (Duration)30 * DAY;
const Duration YEAR          = (Duration)36525 * DAY / 100;
const Duration FOREVER       = (Duration)long::max;
const Duration FOREVER_PAST  = (Duration)long::min;
const Duration DURATION_ZERO = (Duration)0;


<*
 @param us : "The number of microseconds"
 @return "The resulting duration"
*>
fn Duration us(long us) @inline   => (Duration)us;


<*
 @param ms : "The number of milliseconds"
 @return "The resulting duration"

 @require [ms] @ok(ms.overflow_mul((long)MS)) : "The number of milliseconds would overflow the duration"
*>
fn Duration ms(long ms) @inline   => (Duration)(ms * (long)MS);

<*
 @param s : "The number of seconds"
 @return "The resulting duration"

 @require [s] @ok(s.overflow_mul((long)SEC)) : "The number of seconds would overflow the duration"
*>
fn Duration sec(long s) @inline  => (Duration)(s * (long)SEC);

<*
 @param m : "The number of minutes"
 @return "The resulting duration"

 @require [m] @ok(m.overflow_mul((long)MIN)) : "The number of minutes would overflow the duration"
*>
fn Duration min(long m) @inline  => (Duration)(m * (long)MIN);

<*
 @param h : "The number of hours"
 @return "The resulting duration"

 @require [h] @ok(h.overflow_mul((long)HOUR)) : "The number of hours would overflow the duration"
*>
fn Duration hour(long h) @inline => (Duration)(h * (long)HOUR);

<*
 @param d : "The number of days"
 @return "The resulting duration"

 @require [d] @ok(d.overflow_mul((long)DAY)) : "The number of days would overflow the duration"
*>
fn Duration day(long d) @inline => (Duration)(d * (long)DAY);

<*
 @param w : "The number of weeks"
 @return "The resulting duration"

 @require [w] @ok(w.overflow_mul((long)WEEK)) : "The number of weeks would overflow the duration"
*>
fn Duration week(long w) @inline => (Duration)(w * (long)WEEK);

<*
 @param m : "The number of months"
 @return "The resulting duration, assuming 1 month = 30 days"

 @require [m] @ok(m.overflow_mul((long)MONTH)) : "The number of months would overflow the duration"
*>
fn Duration month(long m) @inline => (Duration)(m * (long)MONTH);

<*
 @param y : "The number of years"
 @return "The resulting duration, assuming 1 year = 365.25 days"

 @require [y] @ok(y.overflow_mul((long)YEAR)) : "The number of years would overflow the duration"
*>
fn Duration year(long y) @inline => (Duration)(y * (long)YEAR);

<*
 @param s : "The duration in a floating point second"

 @require s.is_inf() || (s * (double)SEC < long::max && s * (double)SEC > long::min): "The value is out of range"
*>
fn Duration from_float(double s) @inline
{
	switch (s)
	{
		case double::inf:  return FOREVER;
		case -double::inf: return FOREVER_PAST;
		default:           return (Duration)(s * (double)SEC);
	}
}

struct DateTime (Printable)
{
	int usec;
	char sec;
	char min;
	char hour;
	char day;
	Month month;
	Weekday weekday;
	int year;
	ushort year_day;
	Time time;
}

struct TzDateTime (Printable)
{
	inline DateTime date_time;
	int gmt_offset;
}

enum Weekday : char (String name, String abbrev)
{
	MONDAY    { "Monday",    "Mon" },
	TUESDAY   { "Tuesday",   "Tue" },
	WEDNESDAY { "Wednesday", "Wed" },
	THURSDAY  { "Thursday",  "Thu" },
	FRIDAY    { "Friday",    "Fri" },
	SATURDAY  { "Saturday",  "Sat" },
	SUNDAY    { "Sunday",    "Sun" },
}

enum Month : char (String name, String abbrev, int days, bool leap)
{
	JANUARY    { "January",   "Jan", 31, false },
	FEBRUARY   { "February",  "Feb", 28, true  },
	MARCH      { "March",     "Mar", 31, false },
	APRIL      { "April",     "Apr", 30, false },
	MAY        { "May",       "May", 31, false },
	JUNE       { "June",      "Jun", 30, false },
	JULY       { "July",      "Jul", 31, false },
	AUGUST     { "August",    "Aug", 31, false },
	SEPTEMBER  { "September", "Sep", 30, false },
	OCTOBER    { "October",   "Oct", 31, false },
	NOVEMBER   { "November",  "Nov", 30, false },
	DECEMBER   { "December",  "Dec", 31, false }
}

fn Time now()
{
	$if $defined(os::native_timestamp):
		return os::native_timestamp();
	$else
		return (Time)0;
	$endif
}

fn Time Time.add_seconds(time, long seconds) => time + sec(seconds);
fn Time Time.add_minutes(time, long minutes) => time + min(minutes);
fn Time Time.add_hours(time, long hours)     => time + hour(hours);
fn Time Time.add_days(time, long days)       => time + day(days);
fn Time Time.add_weeks(time, long weeks)     => time + week(weeks);

<*
 Add a duration.

 The following behaviour applies:
 FAR_FUTURE + duration = FAR_FUTURE
 FAR_PAST + duration = FAR_PAST
 time + FOREVER = FAR_FUTURE
 time + FOREVER_PAST = FAR_PAST
 FAR_FUTURE + FOREVER_PAST = FAR_FUTURE
 FAR_PAST + FOREVER = FAR_PAST

 Addition and subtraction will saturate to FAR_FUTURE and FAR_PAST.
*>
fn Time Time.add_duration(time, Duration duration) @operator_s(+) @inline => _time_add(Time, time, duration);

<*
 Subtract a duration.

 The following behaviour applies:
 FAR_FUTURE - duration = FAR_FUTURE
 FAR_PAST - duration = FAR_PAST
 time - FOREVER = FAR_PAST
 time - FOREVER_PAST = FAR_FUTURE
 FAR_FUTURE - FOREVER = FAR_FUTURE
 FAR_PAST - FOREVER_PAST = FAR_PAST

 Addition and subtraction will saturate to FAR_FUTURE and FAR_PAST.
*>
fn Time Time.sub_duration(time, Duration duration) @operator(-) @inline   => _time_sub(Time, time, duration);

fn int Time.compare_to(time, Time other)
{
	if ((long)time == (long)other) return 0;
	return time > other ? 1 : -1;
}

<*
 Convert to seconds.

 Far past => -double::inf
 Far future => double::inf
*>
fn double Time.to_seconds(time) => _div_time_to_float(time, SEC);

<*
 Return the difference between two times.

 FAR_FUTURE - time = FOREVER
 FAR_FUTURE - FAR_FUTURE = FOREVER
 FAR_PAST - time = FOREVER_PAST
 FAR_PAST - FAR_PAST = FOREVER_PAST

 Value will saturate, so any value exceeding FOREVER and FOREVER_PAST sentinels
 will saturate to those values.
*>
fn Duration Time.diff_us(time, Time other) @operator(-) => _time_sub(Duration, time, other);
fn double Time.diff_sec(time, Time other)               => _div_time_to_float(time.diff_us(other), SEC);
fn double Time.diff_min(time, Time other)               => _div_time_to_float(time.diff_us(other), MIN);
fn double Time.diff_hours(time, Time other)             => _div_time_to_float(time.diff_us(other), HOUR);
fn double Time.diff_days(time, Time other)              => _div_time_to_float(time.diff_us(other), DAY);
fn double Time.diff_weeks(time, Time other)             => _div_time_to_float(time.diff_us(other), WEEK);
fn double Time.diff_months(time, Time other)            => _div_time_to_float(time.diff_us(other), MONTH);
fn double Time.diff_years(time, Time other)             => _div_time_to_float(time.diff_us(other), YEAR);

fn double NanoDuration.to_sec(nd)        => (double)nd / 1_000_000_000.0;
fn long NanoDuration.to_ms(nd)           => (long)nd / 1_000_000;
fn Duration NanoDuration.to_duration(nd) => (Duration)((long)nd / 1_000);

fn NanoDuration Duration.to_nano(td)     => _time_mul(NanoDuration, td, 1_000);
fn double Duration.to_sec(nd)            => _div_time_to_float(nd, SEC);
fn long Duration.to_ms(td)               => _time_div(long, td, 1_000);

macro Duration Duration.mult(#td, long #val) @operator_s(*) @safemacro
{
	$if $reflect(#td).is_const && $reflect(#val).is_const:
		var $td = (int128)#td;
		var $val = (int128)#val;
		int128 $mul = $td * $val;
		$switch:
			$case $mul < long::min: return (Duration)long::min;
			$case $mul > long::max: return (Duration)long::max;
			$default:               return (Duration)$mul;
		$endswitch
	$else
		return _time_mul(Duration, #td, #val);
	$endif
}

fn String NanoDuration.unit_str(&self, Allocator allocator, bool use_short_units = true)
{
	static String[] units = { "nanoseconds", "microseconds", "milliseconds", "seconds" };
	static String[] short_units = { "ns", "us", "ms", "s" }; // using Mu here causes spacing oddities, just using 'us'
	sz units_idx = 0;

	double reduced_value = (double)*self;
	for (; reduced_value > 1_000 && units_idx < units.len - 1; reduced_value /= 1_000, units_idx++);

	DString out_str = dstring::new_with_capacity(allocator, 32);
	out_str.appendf(
		use_short_units ? "%.2f%s" : "%.2f %s",
		reduced_value,
		(use_short_units ? short_units : units)[units_idx]
	);
	return out_str.str_view();
}

fn String NanoDuration.tunit_str(&self, bool use_short_units = true)
{
	return self.unit_str(tmem, use_short_units);
}

fn sz? NanoDuration.to_format(&self, Formatter* formatter) @dynamic
{
	NanoDuration nd = *self;
	if (nd == NANO_DURATION_ZERO)
	{
		return formatter.printf("0s")!;
	}

	bool neg = nd < NANO_DURATION_ZERO;
	if (neg) nd = -nd;

	DString str = dstring::temp_with_capacity(64);
	if (nd < (NanoDuration)1_000_000_000)
	{
		// Less than 1s: print milliseconds, microseconds and nanoseconds.
		NanoDuration ms = nd / 1_000_000;
		if (ms > NANO_DURATION_ZERO)
		{
			str.appendf("%dms", ms);
			nd -= ms * 1_000_000;
		}
		NanoDuration us = nd / 1000;
		if (us > NANO_DURATION_ZERO)
		{
			str.appendf("%dµs", us);
			nd -= us * 1000;
		}
		if (nd > NANO_DURATION_ZERO)
		{
			str.appendf("%dns", nd);
		}
	}
	else
	{
		// More than 1s: print hours, minutes and seconds.
		NanoDuration ms = (nd - nd / 1_000_000_000 * 1_000_000_000) / 1_000_000;

		nd /= 1_000_000_000;
		NanoDuration hour = nd / 3600;
		if (hour > NANO_DURATION_ZERO)
		{
			str.appendf("%dh", hour);
			nd -= hour * 3600;
		}
		NanoDuration min = nd / 60;
		if (min > NANO_DURATION_ZERO)
		{
			str.appendf("%dm", min);
			nd -= min * 60;
		}
		NanoDuration sec = nd;
		if (ms > NANO_DURATION_ZERO)
		{
			// Ignore trailing zeroes.
			while (ms / 10 * 10 == ms) ms /= 10;
			str.appendf("%d.%ds", sec, ms);
		}
		else
		{
			str.appendf("%ds", sec);
		}
	}
	return formatter.printf(str.str_view())!;
}

// --- helper macros
macro _time_add($Type, a, b) @local
{
	long as = (long)a;
	long bs = (long)b;
	switch
	{
		case as == long::max || as == long::min: return ($Type)a;
		case bs == long::max || bs == long::min: return ($Type)bs;
		default:                                 return ($Type)as.sat_add(bs);
	}
}

macro _time_sub($Type, a, b) @local
{
	long as = (long)a;
	long bs = (long)b;
	switch
	{
		case as == long::max || as == long::min: return ($Type)a;
		case bs == long::max: return ($Type)long::min;
		case bs == long::min: return ($Type)long::max;
		default:              return ($Type)as.sat_sub(bs);
	}
}

macro _time_mul($Type, a, b) @local
{
	return ($Type)((long)a).sat_mul((long)b);
}

macro _time_div($Type, a, b) @local
{
	long al = (long)a;
	if (al == long::max) return ($Type)long::max;
	if (al == long::min) return ($Type)long::min;
	return ($Type)(((long)a) / (long)b);
}

macro double _div_time_to_float(a, b) @local
{
	switch ((long)a)
	{
		case long::max: return double::inf;
		case long::min: return -double::inf;
		default: return (double)a / (double)b;
	}
}
