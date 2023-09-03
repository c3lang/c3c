module std::time;

def Time = distinct long @inline;
def Duration = distinct long @inline;
def Clock = distinct ulong @inline;
def NanoDuration = distinct long @inline;

fn Duration ms(long l) @inline => (Duration)l * 1000;
fn Duration sec(long l) @inline => (Duration)l * MICROSECONDS_PER_SECOND;
fn Duration min(long l) @inline => (Duration)l * MICROSECONDS_PER_MINUTE;
fn Duration hour(long l) @inline => (Duration)l * MICROSECONDS_PER_HOUR;

const Duration MICROSECONDS_PER_SECOND = 1_000_000;
const Duration MICROSECONDS_PER_MINUTE = MICROSECONDS_PER_SECOND * 60;
const Duration MICROSECONDS_PER_HOUR = MICROSECONDS_PER_MINUTE * 60;
const Duration MICROSECONDS_PER_DAY = MICROSECONDS_PER_HOUR * 24;
const Duration MICROSECONDS_PER_WEEK = MICROSECONDS_PER_DAY * 7;

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

enum Weekday : char
{
	MONDAY,
	TUESDAY,
	WEDNESDAY,
	THURSDAY,
	FRIDAY,
	SATURDAY,
	SUNDAY,
}

enum Month : char
{
	JANUARY,
	FEBRUARY,
	MARCH,
	APRIL,
	MAY,
	JUNE,
	JULY,
	AUGUST,
	SEPTEMBER,
	OCTOBER,
	NOVEMBER,
	DECEMBER
}


fn Time now()
{
	$if $defined(native_timestamp):
		return os::native_timestamp();
	$else
		return 0;
	$endif
}

fn Time Time.add_seconds(time, long seconds) => time + (Time)(seconds * (long)MICROSECONDS_PER_SECOND);
fn Time Time.add_minutes(time, long minutes) => time + (Time)(minutes * (long)MICROSECONDS_PER_MINUTE);
fn Time Time.add_hours(time, long hours) => time + (Time)(hours * (long)MICROSECONDS_PER_HOUR);
fn Time Time.add_days(time, long days) => time + (Time)(days * (long)MICROSECONDS_PER_DAY);
fn Time Time.add_weeks(time, long weeks) => time + (Time)(weeks * (long)MICROSECONDS_PER_WEEK);
fn double Time.to_seconds(time) => (long)time / (double)MICROSECONDS_PER_SECOND;
fn Duration Time.diff_us(time, Time other) => (Duration)(time - other);
fn double Time.diff_sec(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_SECOND;
fn double Time.diff_min(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_MINUTE;
fn double Time.diff_hour(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_HOUR;
fn double Time.diff_days(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_DAY;
fn double Time.diff_weeks(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_WEEK;

fn double NanoDuration.to_sec(nd) => (double)nd / 1_000_000_000.0;
fn long NanoDuration.to_ms(nd) => (long)nd / 1_000_000;
fn Duration NanoDuration.to_duration(nd) => (Duration)nd / 1_000;
fn NanoDuration Duration.to_nano(td) => (NanoDuration)td * 1_000;
fn long Duration.to_ms(td) => (long)td / 1_000;

fn usz! NanoDuration.to_format(&self, Formatter* formatter) @dynamic
{
	NanoDuration nd = *self;
	if (nd == 0)
	{
		return formatter.printf("0s")!;
	}

	bool neg = nd < 0;
	if (neg) nd = -nd;

	DString str;
	str.tinit();
	if (nd < 1_000_000_000)
	{
		// Less than 1s: print milliseconds, microseconds and nanoseconds.
		NanoDuration ms = nd / 1_000_000;
		if (ms > 0)
		{
			str.printf("%dms", ms);
			nd -= ms * 1_000_000;
		}
		NanoDuration us = nd / 1000;
		if (us > 0)
		{
			str.printf("%dµs", us);
			nd -= us * 1000;
		}
		if (nd > 0)
		{
			str.printf("%dns", nd);
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
			str.printf("%dh", hour);
			nd -= hour * 3600;
		}
		NanoDuration min = nd / 60;
		if (min > 0)
		{
			str.printf("%dm", min);
			nd -= min * 60;
		}
		NanoDuration sec = nd;
		if (ms > 0)
		{
			// Ignore trailing zeroes.
			while (ms / 10 * 10 == ms) ms /= 10;
			str.printf("%d.%ds", sec, ms);
		}
		else
		{
			str.printf("%ds", sec);
		}
	}
	return formatter.printf(str.as_str())!;
}