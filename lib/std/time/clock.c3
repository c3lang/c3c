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
	NanoDuration diff = mark - *self;
	*self = mark;
	return diff;
}

fn Clock Clock.add_nano_duration(self, NanoDuration nano) @operator_s(+) @inline
{
	return (Clock)((NanoDuration)self + nano);
}

fn Clock Clock.sub_nano_duration(self, NanoDuration nano) @operator(-) @inline
{
	return (Clock)((NanoDuration)self - nano);
}

fn Clock Clock.add_duration(self, Duration duration) @operator_s(+) @inline
{
	return self.add_nano_duration(duration.to_nano());
}

fn Clock Clock.sub_duration(self, Duration duration) @operator(-) @inline
{
	return self.sub_nano_duration(duration.to_nano());
}

fn NanoDuration Clock.nano_diff(self, Clock other) @operator(-) @inline
{
	return (NanoDuration)self - (NanoDuration)other;
}

fn NanoDuration Clock.to_now(self) @inline
{
	return (NanoDuration)now() - (NanoDuration)self;
}