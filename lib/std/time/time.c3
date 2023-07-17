module std::time;

def Time = distinct long @inline;
def TimeDuration = distinct long @inline;
def Clock = distinct ulong @inline;
def NanoDuration = distinct long @inline;

const TimeDuration MICROSECONDS_PER_SECOND = 1_000_000;
const TimeDuration MICROSECONDS_PER_MINUTE = MICROSECONDS_PER_SECOND * 60;
const TimeDuration MICROSECONDS_PER_HOUR = MICROSECONDS_PER_MINUTE * 60;
const TimeDuration MICROSECONDS_PER_DAY = MICROSECONDS_PER_HOUR * 24;
const TimeDuration MICROSECONDS_PER_WEEK = MICROSECONDS_PER_DAY * 7;

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
fn TimeDuration Time.diff_us(time, Time other) => (TimeDuration)(time - other);
fn double Time.diff_sec(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_SECOND;
fn double Time.diff_min(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_MINUTE;
fn double Time.diff_hour(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_HOUR;
fn double Time.diff_days(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_DAY;
fn double Time.diff_weeks(time, Time other) => (long)time.diff_us(other) / (double)MICROSECONDS_PER_WEEK;

fn double NanoDuration.to_sec(nd) => (double)nd / 1_000_000_000.0;
fn long NanoDuration.to_ms(nd) => (long)nd / 1_000_000;
fn TimeDuration NanoDuration.to_duration(nd) => (TimeDuration)nd / 1_000;
fn NanoDuration TimeDuration.to_nano(td) => (NanoDuration)td * 1_000;