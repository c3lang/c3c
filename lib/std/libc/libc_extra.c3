module libc;
import std::time;

<*
 Return a "timespec" from a duration.

 @require self >= time::NANO_DURATION_ZERO
*>
fn TimeSpec NanoDuration.to_timespec(self) @inline
{
	CLong ns = (CLong)(self % 1000_000_000);
	Time_t sec = (Time_t)(self / 1000_000_000);
	return { .s = sec, .ns = ns };
}

<*
 Convert a duration to a timespec.

 @require self >= time::DURATION_ZERO
*>
fn TimeSpec Duration.to_timespec(self) @inline
{
	CLong ns = (CLong)(1000 * (self % time::SEC));
	Time_t sec = (Time_t)(self / time::SEC);
	return { .s = sec, .ns = ns };
}

<*
 Convert a timestamp to a timespec.
*>
fn TimeSpec Time.to_timespec(self) @inline
{
	return ((Duration)self).to_timespec();
}
