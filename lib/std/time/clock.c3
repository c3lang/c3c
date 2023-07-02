module std::time::clock;

fn Clock now()
{
	$if $defined(native_clock):
		return os::native_clock();
	$else
		return 0;
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