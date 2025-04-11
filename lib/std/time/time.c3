module std::time;
import std::io, std::time::os;

typedef Time = long;
typedef Duration = long;
typedef Clock = ulong;
typedef NanoDuration (Printable) = long;

const Time FAR_FUTURE = long.max;
const Time FAR_PAST = long.min;
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

fn Duration us(long l) @inline => (Duration)l * US;
fn Duration ms(long l) @inline => (Duration)l * MS;
fn Duration sec(long l) @inline => (Duration)l * SEC;
fn Duration min(long l) @inline => (Duration)l * MIN;
fn Duration hour(long l) @inline => (Duration)l * HOUR;
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
	$if $defined(native_timestamp):
		return os::native_timestamp();
	$else
		return 0;
	$endif
}

fn Time Time.add_seconds(time, long seconds) => time + (Time)(seconds * (long)SEC);
fn Time Time.add_minutes(time, long minutes) => time + (Time)(minutes * (long)MIN);
fn Time Time.add_hours(time, long hours) => time + (Time)(hours * (long)HOUR);
fn Time Time.add_days(time, long days) => time + (Time)(days * (long)DAY);
fn Time Time.add_weeks(time, long weeks) => time + (Time)(weeks * (long)WEEK);
fn Time Time.add_duration(time, Duration duration) @operator(+) => time + (Time)duration;

fn int Time.compare_to(time, Time other)
{
	if (time == other) return 0;
	return time > other ? 1 : -1;
}

fn double Time.to_seconds(time) => (long)time / (double)SEC;
fn Duration Time.diff_us(time, Time other) @operator(-) => (Duration)(time - other);
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

fn usz? NanoDuration.to_format(&self, Formatter* formatter) @dynamic
{
	NanoDuration nd = *self;
	if (nd == 0)
	{
		return formatter.printf("0s")!;
	}

	bool neg = nd < 0;
	if (neg) nd = -nd;

	DString str = dstring::temp_with_capacity(64);
	if (nd < 1_000_000_000)
	{
		// Less than 1s: print milliseconds, microseconds and nanoseconds.
		NanoDuration ms = nd / 1_000_000;
		if (ms > 0)
		{
			str.appendf("%dms", ms);
			nd -= ms * 1_000_000;
		}
		NanoDuration us = nd / 1000;
		if (us > 0)
		{
			str.appendf("%dµs", us);
			nd -= us * 1000;
		}
		if (nd > 0)
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
		if (hour > 0)
		{
			str.appendf("%dh", hour);
			nd -= hour * 3600;
		}
		NanoDuration min = nd / 60;
		if (min > 0)
		{
			str.appendf("%dm", min);
			nd -= min * 60;
		}
		NanoDuration sec = nd;
		if (ms > 0)
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