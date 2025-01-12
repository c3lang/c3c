module nanoduration_test @test;
import std::io;
import std::time;

fn void to_format()
{
	char[32] buffer;
	char[] buf;
	buf = io::bprintf(buffer[..], "%s", (NanoDuration)123)!!;
	assert(buf == "123ns", "got %s; want 123ns", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)123_000)!!;
	assert(buf == "123µs", "got %s; want 123µs", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)123_000_000)!!;
	assert(buf == "123ms", "got %s; want 123ms", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)13_000_000_000)!!;
	assert(buf == "13s", "got %s; want 13s", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)123_000_000_000)!!;
	assert(buf == "2m3s", "got %s; want 2m3s", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)12345_000_000_000)!!;
	assert(buf == "3h25m45s", "got %s; want 3h25m45s", buf);

	buf = io::bprintf(buffer[..], "%s", (NanoDuration)12_100_000_000)!!;
	assert(buf == "12.1s", "got %s; want 12.1s", buf);
}