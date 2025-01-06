module std::time;


enum FormatSpec : char
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


fn String FormatSpec.format(self, TzDateTime dt, Allocator allocator = allocator::heap())
{
	switch (self)
	{
		case ANSIC:
			return string::format("%s %s %2d %02d:%02d:%02d %04d", dt.weekday.to_string_short(), dt.month.to_string_short(), dt.day, dt.hour, dt.min, dt.sec, dt.year, allocator: allocator);
		case UNIXDATE:
			return string::format("%s %s %2d %02d:%02d:%02d GMT %04d", dt.weekday.to_string_short(), dt.month.to_string_short(), dt.day, dt.hour, dt.min, dt.sec, dt.year, allocator: allocator);
		case RUBYDATE:
			return string::format("%s %s %2d %02d:%02d:%02d %s %04d", dt.weekday.to_string_short(), dt.month.to_string_short(), dt.day, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix(dt.gmt_offset), dt.year, allocator: allocator);
		case RFC822:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format("%02d %s %02d %02d:%02d GMT", dt.day, dt.month.to_string_short(), dt.year % 100, dt.hour, dt.min, allocator: allocator);
		case RFC822Z:
			return string::format("%02d %s %02d %02d:%02d %s", dt.day, dt.month.to_string_short(), dt.year % 100, dt.hour, dt.min, temp_numeric_tzsuffix(dt.gmt_offset), allocator: allocator);
		case RFC850:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format("%s, %02d-%s-%02d %02d:%02d:%02d GMT", dt.weekday.to_string_long(), dt.day, dt.month.to_string_short(), dt.year % 100, dt.hour, dt.min, dt.sec, allocator: allocator);
		case RFC1123:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return string::format("%s, %02d %s %d %02d:%02d:%02d GMT", dt.weekday.to_string_short(), dt.day, dt.month.to_string_short(), dt.year, dt.hour, dt.min, dt.sec, allocator: allocator);
		case RFC1123Z:
			return string::format("%s, %02d %s %d %02d:%02d:%02d %s", dt.weekday.to_string_short(), dt.day, dt.month.to_string_short(), dt.year, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix(dt.gmt_offset), allocator: allocator);
		case RFC3339:
			dt = dt.to_gmt_offset(0);
			return string::format("%04d-%02d-%02dT%02d:%02d:%02dZ", dt.year, dt.month + 1, dt.day, dt.hour, dt.min, dt.sec, allocator: allocator);
		case RFC3339Z:
			return string::format("%04d-%02d-%02dT%02d:%02d:%02d%s", dt.year, dt.month + 1, dt.day, dt.hour, dt.min, dt.sec, temp_numeric_tzsuffix_colon(dt.gmt_offset), allocator: allocator);
		case RFC3339MS:
			dt = dt.to_gmt_offset(0);
			return string::format("%04d-%02d-%02dT%02d:%02d:%02d.%dZ", dt.year, dt.month + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec, allocator: allocator);
		case RFC3339ZMS:
			return string::format("%04d-%02d-%02dT%02d:%02d:%02d.%d%s", dt.year, dt.month + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec, temp_numeric_tzsuffix_colon(dt.gmt_offset), allocator: allocator);
		case DATETIME:
			return string::format("%04d-%02d-%02d %02d:%02d:%02d", dt.year, dt.month + 1, dt.day, dt.hour, dt.min, dt.sec, allocator: allocator);
		case DATEONLY:
			return string::format("%04d-%02d-%02d", dt.year, dt.month + 1, dt.day, allocator: allocator);
		case TIMEONLY:
			return string::format("%02d:%02d:%02d", dt.hour, dt.min, dt.sec, allocator: allocator);
	}
}

fn String FormatSpec.tformat(self, TzDateTime dt, Allocator allocator = allocator::temp()) => self.format(dt, allocator);

fn String TzDateTime.format(self, FormatSpec spec, Allocator allocator = allocator::temp()) => spec.format(self, allocator);
fn String TzDateTime.tformat(self, FormatSpec spec, Allocator allocator = allocator::temp()) => spec.format(self, allocator);

// .with_gmt_offset(0) instead of .to_local() is used to avoid surprises when user is formatting to a representation without a timezone
fn String DateTime.format(self, FormatSpec spec, Allocator allocator = allocator::heap()) => spec.format(self.with_gmt_offset(0), allocator);
fn String DateTime.tformat(self, FormatSpec spec, Allocator allocator = allocator::temp()) => spec.format(self.with_gmt_offset(0), allocator);


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

fn String Weekday.to_string_short(self)
{
	switch(self)
	{
		case SUNDAY: return "Sun";
		case MONDAY: return "Mon";
		case TUESDAY: return "Tue";
		case WEDNESDAY: return "Wed";
		case THURSDAY: return "Thu";
		case FRIDAY: return "Fri";
		case SATURDAY: return "Sat";
	}
}

fn String Weekday.to_string_long(self)
{
	switch (self)
	{
		case SUNDAY: return "Sunday";
		case MONDAY: return "Monday";
		case TUESDAY: return "Tuesday";
		case WEDNESDAY: return "Wednesday";
		case THURSDAY: return "Thusday";
		case FRIDAY: return "Friday";
		case SATURDAY: return "Saturday";
	}
}


fn String Month.to_string_short(self)
{
	switch (self)
	{
		case JANUARY: return "Jan";
		case FEBRUARY: return "Feb";
		case MARCH: return "Mar";
		case APRIL: return "Apr";
		case MAY: return "May";
		case JUNE: return "Jun";
		case JULY: return "Jul";
		case AUGUST: return "Aug";
		case SEPTEMBER: return "Sep";
		case OCTOBER: return "Oct";
		case NOVEMBER: return "Nov";
		case DECEMBER: return "Dec";
	}
}

fn String Month.to_string_long(self)
{
	switch (self)
	{
		case JANUARY: return "January";
		case FEBRUARY: return "February";
		case MARCH: return "March";
		case APRIL: return "April";
		case MAY: return "May";
		case JUNE: return "June";
		case JULY: return "July";
		case AUGUST: return "August";
		case SEPTEMBER: return "September";
		case OCTOBER: return "October";
		case NOVEMBER: return "November";
		case DECEMBER: return "December";
	}
}