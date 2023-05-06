module std::time::os;
import libc;

$if env::os_is_posix() && env::COMPILER_LIBC_AVAILABLE:

$switch (env::OS_TYPE)
$case OPENBSD:
const CLOCK_REALTIME = 0;
const CLOCK_PROCESS_CPUTIME_ID = 2;
const CLOCK_MONOTONIC = 3;
const CLOCK_THREAD_CPUTIME_ID = 4;
const CLOCK_UPTIME = 5;
const CLOCK_BOOTTIME = 6;
$case FREEBSD:
const CLOCK_REALTIME = 0;
const CLOCK_VIRTUAL = 1;
const CLOCK_PROF = 2;
const CLOCK_MONOTONIC = 4;
const CLOCK_UPTIME = 5;
const CLOCK_UPTIME_PRECISE = 7;
const CLOCK_UPTIME_FAST = 8;
const CLOCK_REALTIME_PRECISE = 9;
const CLOCK_REALTIME_FAST = 10;
const CLOCK_MONOTONIC_PRECISE = 11;
const CLOCK_MONOTONIC_FAST = 12;
const CLOCK_SECOND = 13;
const CLOCK_THREAD_CPUTIME_ID = 14;
const CLOCK_PROCESS_CPUTIME_ID = 15;
const CLOCK_BOOTTIME = CLOCK_UPTIME;
const CLOCK_REALTIME_COARSE = CLOCK_REALTIME_FAST;
const CLOCK_MONOTONIC_COARSE = CLOCK_MONOTONIC_FAST;
$case NETBSD:
const CLOCK_REALTIME = 0;
const CLOCK_VIRTUAL = 1;
const CLOCK_PROF = 2;
const CLOCK_MONOTONIC = 3;
const CLOCK_THREAD_CPUTIME_ID = 0x20000000;
const CLOCK_PROCESS_CPUTIME_ID = 0x40000000;
$case WASI:
// Not implemented
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 0;
$case MACOS:
$case TVOS:
$case IOS:
$case WATCHOS:
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 6;
const CLOCK_MONOTONIC_RAW = 4;
const CLOCK_MONOTONIC_RAW_APPROX = 5;
const CLOCK_UPTIME_RAW = 8;
const CLOCK_UPTIME_RAW_APPROX = 9;
const CLOCK_PROCESS_CPUTIME_ID = 12;
const CLOCK_THREAD_CPUTIME_ID = 16;
$case LINUX:
$default:
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 1;
const CLOCK_PROCESS_CPUTIME_ID = 2;
const CLOCK_THREAD_CPUTIME_ID = 3;
const CLOCK_MONOTONIC_RAW = 4;
const CLOCK_REALTIME_COARSE = 5;
const CLOCK_MONOTONIC_COARSE = 6;
const CLOCK_BOOTTIME = 7;
const CLOCK_REALTIME_ALARM = 8;
const CLOCK_BOOTTIME_ALARM = 9;
const CLOCK_TAI = 11;
$endswitch

extern fn void clock_gettime(int type, TimeSpec *time);

fn Time native_timestamp()
{
	TimeSpec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	return (Time)(ts.s * 1_000_000i64 + ts.ns / 1_000i64);
}

$if !env::os_is_darwin():
fn Clock native_clock()
{
	TimeSpec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (Clock)((ulong)ts.s * 1_000_000_000u64 + (ulong)ts.ns);
}
$endif

$endif
