module std::time::os @if(env::DARWIN);
import std::os::darwin;

fn Clock native_clock()
{
	static Darwin_mach_timebase_info_data_t timebase;
	if (!timebase.denom)
	{
		darwin::mach_timebase_info(&timebase);
	}
	return (Clock)(darwin::mach_absolute_time() * timebase.numer / timebase.denom);
}
