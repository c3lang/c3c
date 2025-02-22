module std::time::os @if(env::DARWIN);

struct Darwin_mach_timebase_info
{
	uint numer;
	uint denom;
}

def Darwin_mach_timebase_info_t = Darwin_mach_timebase_info;
def Darwin_mach_timebase_info_data_t = Darwin_mach_timebase_info;

extern fn void mach_timebase_info(Darwin_mach_timebase_info_data_t* timebase);
extern fn ulong mach_absolute_time();

fn Clock native_clock()
{
	static Darwin_mach_timebase_info_data_t timebase;
	if (!timebase.denom)
	{
		mach_timebase_info(&timebase);
	}
	return (Clock)(mach_absolute_time() * timebase.numer / timebase.denom);
}
