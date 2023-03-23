module std::time::datetime;
import libc;

fn DateTime now()
{
	return from_time(time::now());
}

/**
 * @require day >= 1 && day < 32
 * @require hour >= 0 && hour < 24
 * @require min >= 0 && min < 60
 * @require sec >= 0 && sec < 60
 * @require us >= 0 && us < 999_999
 **/
fn DateTime from_date(int year, Month month = JANUARY, int day = 1, int hour = 0, int min = 0, int sec = 0, int us = 0)
{
	DateTime dt @noinit;
	dt.set_date(year, month, day, hour, min, sec, us) @inline;
	return dt;
}

fn TzDateTime DateTime.to_local(DateTime* this)
{
	Tm tm @noinit;
	Time_t time_t = (Time_t)this.time.to_seconds();
	libc::localtime_r(&time_t, &tm);
	TzDateTime dt;
	dt.usec = (int)((long)this.time % (long)time::MICROSECONDS_PER_SECOND);
	dt.sec = (char)tm.tm_sec;
	dt.min = (char)tm.tm_min;
	dt.hour = (char)tm.tm_hour;
	dt.day = (char)tm.tm_mday;
	dt.month = (Month)tm.tm_mon;
	dt.year = tm.tm_year + 1900;
	dt.weekday = !tm.tm_wday ? Weekday.SUNDAY : (Weekday)tm.tm_wday + 1;
	dt.year_day = (ushort)tm.tm_yday;
	dt.time = this.time;
$if ($defined(tm.tm_gmtoff))
	dt.gmt_offset = (int)tm.tm_gmtoff;
$else
	$assert($defined(libc::_get_timezone));
	CLong timezone;
	libc::_get_timezone(&timezone);
	dt.gmt_offset = (int)-timezone;
$endif
	return dt;
}

/**
 * @require day >= 1 && day < 32
 * @require hour >= 0 && hour < 24
 * @require min >= 0 && min <= 60
 * @require sec >= 0 && sec < 60
 * @require us >= 0 && us < 999_999
 **/
fn void DateTime.set_date(DateTime *this, int year, Month month = JANUARY, int day = 1, int hour = 0, int min = 0, int sec = 0, int us = 0)
{
	Tm tm;
	tm.tm_sec = sec;
	tm.tm_min = min;
	tm.tm_hour = hour;
	tm.tm_mon = month.ordinal;
	tm.tm_mday = day;
	tm.tm_year = year - 1900;
	Time_t time = libc::timegm(&tm);
	this.set_time((Time)(time * (long)time::MICROSECONDS_PER_SECOND + us));
}

fn void DateTime.set_time(DateTime* this, Time time)
{
	Tm tm @noinit;
	Time_t time_t = (Time_t)time.to_seconds();
	libc::gmtime_r(&time_t, &tm);
	this.usec = (int)((long)time % (long)time::MICROSECONDS_PER_SECOND);
	this.sec = (char)tm.tm_sec;
	this.min = (char)tm.tm_min;
	this.hour = (char)tm.tm_hour;
	this.day = (char)tm.tm_mday;
	this.month = (Month)tm.tm_mon;
	this.year = tm.tm_year + 1900;
	this.weekday = !tm.tm_wday ? Weekday.SUNDAY : (Weekday)tm.tm_wday + 1;
	this.year_day = (ushort)tm.tm_yday;
	this.time = time;
}

fn DateTime DateTime.add_seconds(DateTime* date, int seconds) => from_time(date.time.add_seconds(seconds));
fn DateTime DateTime.add_minutes(DateTime* date, int minutes) => from_time(date.time.add_minutes(minutes));
fn DateTime DateTime.add_hours(DateTime* date, int hours) => from_time(date.time.add_hours(hours));
fn DateTime DateTime.add_days(DateTime* date, int days) => from_time(date.time.add_days(days));
fn DateTime DateTime.add_weeks(DateTime* date, int weeks) => from_time(date.time.add_weeks(weeks));

fn DateTime DateTime.add_years(DateTime* date, int years)
{
	if (!years) return *date;
	return from_date(date.year + years, date.month, date.day, date.hour, date.min, date.sec, date.usec);
}

fn DateTime DateTime.add_months(DateTime* date, int months)
{
	if (months == 0) return *date;
	int year = date.year;
	int month = date.month.ordinal;
	switch
	{
		case months % 12 == 0:
			year += months / 12;
		case months < 0:
			month += months % 12;
			year += months / 12;
			if (month < 0)
			{
				year--;
				month += 12;
			}
		default:
			month += months;
			year += month / 12;
			month %= 12;
	}
	return from_date(year, (Month)month, date.day, date.hour, date.min, date.sec, date.usec);
}

fn DateTime from_time(Time time)
{
	DateTime dt @noinit;
	dt.set_time(time) @inline;
	return dt;
}

fn Time DateTime.to_time(DateTime* this) @inline
{
	return this.time;
}

fn bool DateTime.after(DateTime* this, DateTime compare)
{
	return this.time > compare.time;
}

fn bool DateTime.before(DateTime* this, DateTime compare)
{
	return this.time < compare.time;
}

fn int DateTime.compare_to(DateTime* this, DateTime compare)
{
	switch
	{
		case this.time > compare.time: return 1;
		case this.time < compare.time: return -1;
		default: return 0;
	}
}

fn int DateTime.diff_years(DateTime *to, DateTime from)
{
	int year_diff = to.year - from.year;
	switch
	{
		case year_diff < 0: return -from.diff_years(*to);
		case year_diff == 0: return 0;
	}
	if (to.year_day < from.year_day) year_diff--;
	return year_diff;
}

fn double DateTime.diff_sec(DateTime to, DateTime from)
{
	return (double)to.time.diff_us(from.time) / (double)time::MICROSECONDS_PER_SECOND;
}
fn TimeDuration DateTime.diff_us(DateTime to, DateTime from)
{
	return to.time.diff_us(from.time);
}