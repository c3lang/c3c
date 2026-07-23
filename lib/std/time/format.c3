module std::time::datetime @if(env::LIBC);
import std::io, std::math;

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

fn sz? to_format(Formatter* f, TzDateTime dt, DateTimeFormat type)
{
	switch (type)
	{
		case ANSIC:
			return f.printf("%s %s %2d %02d:%02d:%02d %04d", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec, dt.year);
		case UNIXDATE:
			return f.printf("%s %s %2d %02d:%02d:%02d GMT %04d", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec, dt.year);
		case RUBYDATE:
			return f.printf("%s %s %2d %02d:%02d:%02d ", dt.weekday.abbrev, dt.month.abbrev, dt.day, dt.hour, dt.min, dt.sec) + print_gmt(f, dt.gmt_offset, colon:false) + f.printf(" %04d", dt.year);
		case RFC822:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return f.printf("%02d %s %02d %02d:%02d GMT", dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min);
		case RFC822Z:
			return f.printf("%02d %s %02d %02d:%02d ", dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min) + print_gmt(f, dt.gmt_offset, colon:false);
		case RFC850:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return f.printf("%s, %02d-%s-%02d %02d:%02d:%02d GMT", dt.weekday.name, dt.day, dt.month.abbrev, dt.year % 100, dt.hour, dt.min, dt.sec);
		case RFC1123:
			dt = dt.to_gmt_offset(0); // For named representations of the timezone we always go for GMT, which is required by some RFCs
			return f.printf("%s, %02d %s %d %02d:%02d:%02d GMT", dt.weekday.abbrev, dt.day, dt.month.abbrev, dt.year, dt.hour, dt.min, dt.sec);
		case RFC1123Z:
			return f.printf("%s, %02d %s %d %02d:%02d:%02d ", dt.weekday.abbrev, dt.day, dt.month.abbrev, dt.year, dt.hour, dt.min, dt.sec) + print_gmt(f, dt.gmt_offset, colon:false);
		case RFC3339:
			dt = dt.to_gmt_offset(0);
			return f.printf("%04d-%02d-%02dT%02d:%02d:%02dZ", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec);
		case RFC3339Z:
			return f.printf("%04d-%02d-%02dT%02d:%02d:%02d", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec) + print_gmt(f, dt.gmt_offset, colon: true);
		case RFC3339MS:
			dt = dt.to_gmt_offset(0);
			return f.printf("%04d-%02d-%02dT%02d:%02d:%02d.%06dZ", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec);
		case RFC3339ZMS:
			return f.printf("%04d-%02d-%02dT%02d:%02d:%02d.%06d", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec, dt.usec) + print_gmt(f, dt.gmt_offset, colon: true);
		case DATETIME:
			return f.printf("%04d-%02d-%02d %02d:%02d:%02d", dt.year, dt.month.ordinal + 1, dt.day, dt.hour, dt.min, dt.sec);
		case DATEONLY:
			return f.printf("%04d-%02d-%02d", dt.year, dt.month.ordinal + 1, dt.day);
		case TIMEONLY:
			return f.printf("%02d:%02d:%02d", dt.hour, dt.min, dt.sec);
	}
}
fn String format(Allocator allocator, DateTimeFormat type, TzDateTime dt)
{
	DString s;
	s.init(tmem, 40);
	Formatter formatter;
	formatter.init(&dstring::append_fn, &s);
	to_format(&formatter, dt, type)!!;
	if (allocator == tmem) return s.str_view();
	return s.copy_str(allocator);
}

fn String tformat(DateTimeFormat dt_format, TzDateTime dt) => format(tmem, dt_format, dt);

fn String TzDateTime.format(self, Allocator allocator, DateTimeFormat dt_format) => format(allocator, dt_format, self);

// .with_gmt_offset(0) instead of .to_local() is used to avoid surprises when user is formatting to a representation without a timezone
fn String DateTime.format(self, Allocator allocator, DateTimeFormat dt_format) => format(allocator, dt_format, self.with_gmt_offset(0));

fn sz? DateTime.to_format(&self, Formatter* f) @dynamic
{
	return to_format(f, self.with_gmt_offset(0), DATETIME);
}

fn sz? TzDateTime.to_format(&self, Formatter* f) @dynamic
{
	return to_format(f, *self, RFC3339MS);
}

<*
 Prints the timezone offset
 @require gmt_offset >= -12 * 3600 && gmt_offset <= 14 * 3600
*>
fn sz? print_gmt(Formatter* formatter, int gmt_offset, bool colon) @private @inline
{
	if (gmt_offset == 0) return formatter.print(colon ? "-00:00" : "-0000");
	char sign = '+';
	if (gmt_offset < 0)
	{
		sign = '-';
		gmt_offset = -gmt_offset;
	}
	if (colon) return formatter.printf("%c%02d:%02d", sign, gmt_offset / 3600, (gmt_offset % 3600) / 60);
	return formatter.printf("%c%02d%02d", sign, gmt_offset / 3600, (gmt_offset % 3600) / 60);
}
