module libc;
import std::time;

/**
 * @require self >= 0
 **/
fn TimeSpec NanoDuration.to_timespec(self) @inline
{
	CLong ns = (CLong)(self % 1000_000_000);
	Time_t sec = (Time_t)(self / 1000_000_000);
	return { .s = sec, .ns = ns };
}

/**
 * @require self >= 0
 **/
fn TimeSpec Duration.to_timespec(self) @inline
{
	CLong ns = (CLong)(1000 * (self % time::SEC));
	Time_t sec = (Time_t)(self / time::SEC);
	return { .s = sec, .ns = ns };
}
