module std::time;
import std::io, std::time::os;

typedef Time @structlike = long;
typedef Duration @structlike = long;
typedef Clock @structlike = ulong;
typedef NanoDuration (Printable) @structlike = long;

const Time FAR_FUTURE = long.max;
const Time FAR_PAST = long.min;

const NanoDuration NANO_DURATION_ZERO = 0;
const Duration US = 1;
const Duration MS = 1_000;
const Duration SEC = 1_000_000;
const Duration MIN = 60 * SEC;
const Duration HOUR = 60 * MIN;
const Duration DAY = 24 * HOUR;
const Duration WEEK = 7 * DAY;
const Duration MONTH = 30 * DAY;
const Duration YEAR = 36525 * DAY / 100;
const Duration FOREVER = long.max;
const Duration DURATION_ZERO = 0;

fn Duration us(long l) @inline => l * US;
fn Duration ms(long l) @inline => l * MS;
fn Duration sec(long l) @inline =>l * SEC;
fn Duration min(long l) @inline => l * MIN;
fn Duration hour(long l) @inline => l * HOUR;
fn Duration from_float(double s) @inline => (Duration)(s * (double)SEC);

struct DateTime
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

struct TzDateTime
{
	inline DateTime date_time;
	int gmt_offset;
}

enum Weekday : char (String name, String abbrev)
{
	MONDAY    = { "Monday",    "Mon" },
	TUESDAY   = { "Tuesday",   "Tue" },
	WEDNESDAY = { "Wednesday", "Wed" },
	THURSDAY  = { "Thursday",  "Thu" },
	FRIDAY    = { "Friday",    "Fri" },
	SATURDAY  = { "Saturday",  "Sat" },
	SUNDAY    = { "Sunday",    "Sun" },
}

enum Month : char (String name, String abbrev, int days, bool leap)
{
	JANUARY    = { "January",   "Jan", 31, false },
	FEBRUARY   = { "February",  "Feb", 28, true  },
	MARCH      = { "March",     "Mar", 31, false },
	APRIL      = { "April",     "Apr", 30, false },
	MAY        = { "May",       "May", 31, false },
	JUNE       = { "June",      "Jun", 30, false },
	JULY       = { "July",      "Jul", 31, false },
	AUGUST     = { "August",    "Aug", 31, false },
	SEPTEMBER  = { "September", "Sep", 30, false },
	OCTOBER    = { "October",   "Oct", 31, false },
	NOVEMBER   = { "November",  "Nov", 30, false },
	DECEMBER   = { "December",  "Dec", 31, false }
}

fn Time now()
{
	$if $defined(os::native_timestamp):
		return os::native_timestamp();
	$else
		return (Time)0;
	$endif
}

fn Time Time.add_seconds(time, long seconds) => time + seconds * SEC;
fn Time Time.add_minutes(time, long minutes) => time + minutes * MIN;
fn Time Time.add_hours(time, long hours) => time + hours * HOUR;
fn Time Time.add_days(time, long days) => time + days * DAY;
fn Time Time.add_weeks(time, long weeks) => time + weeks * WEEK;
fn Time Time.add_duration(time, Duration duration) @operator_s(+) @inline => (Time)((long)time + (long)duration);
fn Time Time.sub_duration(time, Duration duration) @operator(-) @inline => (Time)((long)time - (long)duration);

fn int Time.compare_to(time, Time other)
{
	if ((long)time == (long)other) return 0;
	return time > other ? 1 : -1;
}

fn double Time.to_seconds(time) => (long)time / (double)SEC;
fn Duration Time.diff_us(time, Time other) @operator(-) => (Duration)((long)time - (long)other);
fn double Time.diff_sec(time, Time other) => (long)time.diff_us(other) / (double)SEC;
fn double Time.diff_min(time, Time other) => (long)time.diff_us(other) / (double)MIN;
fn double Time.diff_hour(time, Time other) => (long)time.diff_us(other) / (double)HOUR;
fn double Time.diff_days(time, Time other) => (long)time.diff_us(other) / (double)DAY;
fn double Time.diff_weeks(time, Time other) => (long)time.diff_us(other) / (double)WEEK;

fn double NanoDuration.to_sec(nd) => (double)nd / 1_000_000_000.0;
fn long NanoDuration.to_ms(nd) => (long)nd / 1_000_000;
fn Duration NanoDuration.to_duration(nd) => (Duration)nd / 1_000;
fn NanoDuration Duration.to_nano(td) => (NanoDuration)td * 1_000;
fn long Duration.to_ms(td) => (long)td / 1_000;
macro Duration Duration.mult(#td, long #val) @operator_s(*) @safemacro => (Duration)((long)#td * #val);

fn usz? NanoDuration.to_format(&self, Formatter* formatter) @dynamic
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