module std::time::datetime @if(env::LIBC);


enum DateTimeFormat
{
	ANSIC,	 	// "Mon Jan _2 15:04:05 2006"
	UNIXDATE,	// "Mon Jan _2 15:04:05 GMT 2006"
	RUBYDATE,	// "Mon Jan 02 15:04:05 -0700 2006"
	RFC822,		// "02 Jan 06 15:04 GMT"
	RFC822Z,	// "02 Jan 06 15:04 -0700"
	RFC850,		// "Monday, 02-Jan-06 15:04:05 GMT"
	RFC1123,	// "Mon, 02 Jan 2006 15:04:05 GMT"
	RFC1123Z,	// "Mon, 02 Jan 2006 15:04:05 -0700"
	RFC3339,	// "2006-01-02T15:04:05Z"
	RFC3339Z,	// "2006-01-02T15:04:05+07:00"
	RFC3339MS,	// "2006-01-02T15:04:05.999999Z"
	RFC3339ZMS,	// "2006-01-02T15:04:05.999999+07:00"
	DATETIME,	// "2006-01-02 15:04:05"
	DATEONLY,	// "2006-01-02"
	TIMEONLY,	// "15:04:05"
}

fn String format(Allocator allocator, DateTimeFormat type, TzDateTime dt)
{
	switch (type)
	{
		case ANSIC:
			return string::format(allocator, "%s %s %2d %02d:%02d:%02d %04d", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec, dt.year);
		case UNIXDATE:
			return string::format(allocator, "%s %s %2d %02d:%02d:%02d GMT %04d", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec, dt.year);
		case RUBYDATE:
			return string::format(allocator, "%s %s %2d %02d:%02d:%02d %s %04d", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix(dt.gmt_offset), dt.year);
		case RFC822:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format(allocator, "%02d %s %02d %02d:%02d GMT", dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min);
		case RFC822Z:
			return string::format(allocator, "%02d %s %02d %02d:%02d %s", dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min, temp_numeric_tzsuffix(dt.gmt_offset));
		case RFC850:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format(allocator, "%s, %02d-%s-%02d %02d:%02d:%02d GMT", dt.weekday.name, dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min, dt.sec);
		case RFC1123:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format(allocator, "%s, %02d %s %d %02d:%02d:%02d GMT", dt.weekday.abbrev, dt.day, dt.month.abbrev, dt.year, dt.hour, dt.min, dt.sec);
		case RFC1123Z:
			return string::format(allocator, "%s, %02d %s %d %02d:%02d:%02d %s", dt.weekday.abbrev, dt.day, dt.month.abbrev, dt.year, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix(dt.gmt_offset));
		case RFC3339:
			dt = dt.to_gmt_offset(0);
			return string::format(allocator, "%04d-%02d-%02dT%02d:%02d:%02dZ", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec);
		case RFC3339Z:
			return string::format(allocator, "%04d-%02d-%02dT%02d:%02d:%02d%s", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix_colon(dt.gmt_offset));
		case RFC3339MS:
			dt = dt.to_gmt_offset(0);
			return string::format(allocator, "%04d-%02d-%02dT%02d:%02d:%02d.%dZ", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec);
		case RFC3339ZMS:
			return string::format(allocator, "%04d-%02d-%02dT%02d:%02d:%02d.%d%s", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec, temp_numeric_tzsuffix_colon(dt.gmt_offset));
		case DATETIME:
			return string::format(allocator, "%04d-%02d-%02d %02d:%02d:%02d", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec);
		case DATEONLY:
			return string::format(allocator, "%04d-%02d-%02d", dt.year, dt.month.ordinal + 1, dt.day);
		case TIMEONLY:
			return string::format(allocator, "%02d:%02d:%02d", dt.hour, dt.min, dt.sec);
	}
}

fn String tformat(DateTimeFormat dt_format, TzDateTime dt) => format(tmem, dt_format, dt);

fn String TzDateTime.format(self, Allocator allocator, DateTimeFormat dt_format) => format(allocator, dt_format, self);

// .with_gmt_offset(0) instead of .to_local() is used to avoid surprises when user is formatting to a representation without a timezone
fn String DateTime.format(self, Allocator allocator, DateTimeFormat dt_format) => format(allocator, dt_format, self.with_gmt_offset(0));

<*
 Returns the timezone offset in the format of "+HHMM" or "-HHMM"
 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn String temp_numeric_tzsuffix(int gmt_offset) @private @inline
{
	if (gmt_offset == 0) return "-0000";
	return string::tformat("%+03d%02d", gmt_offset / 3600, (gmt_offset % 3600) / 60);
}

<*
 Returns the timezone offset in the format of "+HH:MM" or "-HH:MM"
 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn String temp_numeric_tzsuffix_colon(int gmt_offset) @private @inline
{
	if (gmt_offset == 0) return "-00:00";
	return string::tformat("%+03d:%02d", gmt_offset / 3600, (gmt_offset % 3600) / 60);
}