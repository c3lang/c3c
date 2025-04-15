module std::time::os @if(env::POSIX);
import libc, std::os::posix;

fn Time native_timestamp()
{
	TimeSpec ts;
	posix::clock_gettime(posix::CLOCK_REALTIME, &ts);
	return (Time)(ts.s * 1_000_000i64 + ts.ns / 1_000i64);
}

fn Clock native_clock() @if(!env::DARWIN)
{
	TimeSpec ts;
	posix::clock_gettime(posix::CLOCK_MONOTONIC, &ts);
	return (Clock)((ulong)ts.s * 1_000_000_000u64 + (ulong)ts.ns);
}

