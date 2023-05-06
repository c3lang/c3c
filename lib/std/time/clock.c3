module std::time::clock;

fn Clock now()
{
$if $defined(native_clock):
	return os::native_clock();
$else
	return 0;
$endif
}

fn NanoDuration Clock.mark(Clock* this)
{
	Clock mark = now();
	NanoDuration diff = (NanoDuration)(mark - *this);
	*this = mark;
	return diff;
}

fn NanoDuration Clock.to_now(Clock this)
{
	return (NanoDuration)(now() - this);
}