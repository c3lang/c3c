module std::time::datetime @if(env::LIBC);
import libc;

fn DateTime now()
{
	return from_time(time::now());
}

<*
 @require day >= 1 && day < 32
 @require hour >= 0 && hour < 24
 @require min >= 0 && min < 60
 @require sec >= 0 && sec < 60
 @require us >= 0 && us <= 999_999
*>
fn DateTime from_date(int year, Month month = JANUARY, int day = 1, int hour = 0, int min = 0, int sec = 0, int us = 0)
{
	DateTime dt @noinit;
	dt.set_date(year, month, day, hour, min, sec, us) @inline;
	return dt;
}

<*
 @require day >= 1 && day < 32
 @require hour >= 0 && hour < 24
 @require min >= 0 && min < 60
 @require sec >= 0 && sec < 60
 @require us >= 0 && us <= 999_999
 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn TzDateTime from_date_tz(int year, Month month = JANUARY, int day = 1, int hour = 0, int min = 0, int sec = 0, int us = 0, int gmt_offset = 0)
{
	return from_date(year, month, day, hour, min, sec, us).with_gmt_offset(gmt_offset);
}

fn TzDateTime DateTime.to_local(&self)
{
	Tm tm @noinit;
	Time_t time_t = (Time_t)self.time.to_seconds();
	libc::localtime_r(&time_t, &tm);
	TzDateTime dt;
	dt.usec = (int)((long)self.time % (long)time::SEC);
	dt.sec = (char)tm.tm_sec;
	dt.min = (char)tm.tm_min;
	dt.hour = (char)tm.tm_hour;
	dt.day = (char)tm.tm_mday;
	dt.month = Month.from_ordinal(tm.tm_mon);
	dt.year = tm.tm_year + 1900;
	dt.weekday = !tm.tm_wday ? Weekday.SUNDAY : Weekday.from_ordinal(tm.tm_wday - 1);
	dt.year_day = (ushort)tm.tm_yday;
	dt.time = self.time;
	$if $defined(tm.tm_gmtoff):
		dt.gmt_offset = (int)tm.tm_gmtoff;
	$else
		$assert $defined(libc::_get_timezone);
		CLong timezone;
		libc::_get_timezone(&timezone);
		dt.gmt_offset = (int)-timezone;
	$endif
	return dt;
}

<*
 Update timestamp to gmt_offset while keeping the date and time
 values unchanged.

 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn TzDateTime DateTime.with_gmt_offset(self, int gmt_offset)
{
	TzDateTime dt = { self, 0 };
	return dt.with_gmt_offset(gmt_offset);
}

<*
 Update timestamp to gmt_offset while keeping the date and time
 values unchanged.

 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn TzDateTime TzDateTime.with_gmt_offset(self, int gmt_offset)
{
	self.time -= (Time)(gmt_offset - self.gmt_offset) * (Time)time::SEC;
	return { self.date_time, gmt_offset };
}

<*
 Update the date and time values to gmt_offset while keeping the
 timestamp unchanged.

 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
 @ensure self.time == return.time
*>
fn TzDateTime DateTime.to_gmt_offset(self, int gmt_offset)
{
	TzDateTime dt = { self, 0 };
	return dt.to_gmt_offset(gmt_offset);
}

<*
 Update the date and time values to gmt_offset while keeping the
 timestamp unchanged.

 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
 @ensure self.time == return.time
*>
fn TzDateTime TzDateTime.to_gmt_offset(self, int gmt_offset) {
	if (self.gmt_offset == gmt_offset) return self;
	Time time = self.time + (Time)gmt_offset * (Time)time::SEC;
	DateTime dt = from_time(time);
	dt.time = self.time;
	return { dt, gmt_offset };
}

<*
 @require day >= 1 && day < 32
 @require hour >= 0 && hour < 24
 @require min >= 0 && min <= 60
 @require sec >= 0 && sec < 60
 @require us >= 0 && us <= 999_999
*>
fn void DateTime.set_date(&self, int year, Month month = JANUARY, int day = 1, int hour = 0, int min = 0, int sec = 0, int us = 0)
{
	Tm tm;
	tm.tm_sec = sec;
	tm.tm_min = min;
	tm.tm_hour = hour;
	tm.tm_mon = month.ordinal;
	tm.tm_mday = day;
	tm.tm_year = year - 1900;
	Time_t time = libc::timegm(&tm);
	self.set_time((Time)(time * (long)time::SEC + us));
}

fn void DateTime.set_time(&self, Time time)
{
	Tm tm @noinit;
	Time_t time_t = (Time_t)time.to_seconds();
	libc::gmtime_r(&time_t, &tm);
	self.usec = (int)((long)time % (long)time::SEC);
	self.sec = (char)tm.tm_sec;
	self.min = (char)tm.tm_min;
	self.hour = (char)tm.tm_hour;
	self.day = (char)tm.tm_mday;
	self.month = Month.from_ordinal(tm.tm_mon);
	self.year = tm.tm_year + 1900;
	self.weekday = !tm.tm_wday ? Weekday.SUNDAY : Weekday.from_ordinal(tm.tm_wday - 1);
	self.year_day = (ushort)tm.tm_yday;
	self.time = time;
}

fn DateTime DateTime.add_seconds(&self, int seconds) => from_time(self.time.add_seconds(seconds));
fn DateTime DateTime.add_minutes(&self, int minutes) => from_time(self.time.add_minutes(minutes));
fn DateTime DateTime.add_hours(&self, int hours) => from_time(self.time.add_hours(hours));
fn DateTime DateTime.add_days(&self, int days) => from_time(self.time.add_days(days));
fn DateTime DateTime.add_weeks(&self, int weeks) => from_time(self.time.add_weeks(weeks));

fn DateTime DateTime.add_years(&self, int years)
{
	if (!years) return *self;
	return from_date(self.year + years, self.month, self.day, self.hour, self.min, self.sec, self.usec);
}

fn DateTime DateTime.add_months(&self, int months)
{
	if (months == 0) return *self;
	int year = self.year;
	int month = self.month.ordinal;
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
	return from_date(year, Month.from_ordinal(month), self.day, self.hour, self.min, self.sec, self.usec);
}


fn TzDateTime TzDateTime.add_seconds(&self, int seconds) => self.date_time.add_seconds(seconds).to_gmt_offset(self.gmt_offset);
fn TzDateTime TzDateTime.add_minutes(&self, int minutes) => self.date_time.add_minutes(minutes).to_gmt_offset(self.gmt_offset);
fn TzDateTime TzDateTime.add_hours(&self, int hours) => self.date_time.add_hours(hours).to_gmt_offset(self.gmt_offset);
fn TzDateTime TzDateTime.add_days(&self, int days) => self.date_time.add_days(days).to_gmt_offset(self.gmt_offset);
fn TzDateTime TzDateTime.add_weeks(&self, int weeks) => self.date_time.add_weeks(weeks).to_gmt_offset(self.gmt_offset);

fn TzDateTime TzDateTime.add_years(&self, int years) => self.date_time.add_years(years).with_gmt_offset(self.gmt_offset);
fn TzDateTime TzDateTime.add_months(&self, int months) => self.date_time.add_months(months).with_gmt_offset(self.gmt_offset);

fn DateTime from_time(Time time)
{
	DateTime dt @noinit;
	dt.set_time(time) @inline;
	return dt;
}

<*
 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
 @ensure time == return.time
*>
fn TzDateTime from_time_tz(Time time, int gmt_offset)
{
	return from_time(time).to_gmt_offset(gmt_offset);
}

fn Time DateTime.to_time(&self) @inline
{
	return self.time;
}

fn bool DateTime.after(&self, DateTime compare) @inline
{
	return self.time > compare.time;
}

fn bool DateTime.before(&self, DateTime compare) @inline
{
	return self.time < compare.time;
}

fn int DateTime.compare_to(&self, DateTime compare)
{
	return compare_to(self.time, compare.time);
}

fn int DateTime.diff_years(&self, DateTime from)
{
	int year_diff = self.year - from.year;
	switch
	{
		case year_diff < 0: return -from.diff_years(*self);
		case year_diff == 0: return 0;
	}
	if (self.year_day < from.year_day) year_diff--;
	return year_diff;
}

fn double DateTime.diff_sec(self, DateTime from)
{
	return (double)self.time.diff_us(from.time) / (double)time::SEC;
}
fn Duration DateTime.diff_us(self, DateTime from)
{
	return self.time.diff_us(from.time);
}
