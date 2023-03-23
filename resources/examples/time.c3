import std::io;
import std::time;
import std::math;

fn void main()
{
	Clock start = clock::now();
	DateTime d = datetime::now();
	io::printfn("Today is: %d-%02d-%02d %02d:%02d", d.year, d.month.ordinal + 1, d.day, d.hour, d.min);
	io::printfn("Epoch timestamp: %d", d.time / 1_000);
	TzDateTime td = d.to_local();
	int absolute_offset = math::abs(td.gmt_offset);
	int offset_hour = absolute_offset / 3600;
	int offset_min = (absolute_offset / 60) % 60;
	io::printfn("Local time is: %d-%02d-%02d %02d:%02d:%02d %c%02d:%02d", td.year, td.month.ordinal + 1, td.day, td.hour, td.min, td.sec, td.gmt_offset < 0 ? '-' : '+', offset_hour, offset_min);
	io::printfn("Executed the above in %d ns", start.to_now());
}