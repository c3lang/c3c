module std::time::clock;
import std::time::os;

fn Clock now()
{
	$if $defined(os::native_clock):
		return os::native_clock();
	$else
		unreachable("Clock unsupported");
	$endif
}

fn NanoDuration Clock.mark(&self)
{
	Clock mark = now();
	NanoDuration diff = (NanoDuration)(mark - *self);
	*self = mark;
	return diff;
}

fn NanoDuration Clock.to_now(self)
{
	return (NanoDuration)(now() - self);
}