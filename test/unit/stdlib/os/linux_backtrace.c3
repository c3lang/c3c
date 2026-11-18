module linux_backtrace_test @test @feat(LINUX);
import std::os::linux, std::os::backtrace;

fn void parse_addr2line_line()
{
	Backtrace trace = linux::backtrace_line_parse(mem, "main at /src/a.c3:40", "obj", "fn", false)!!;
	assert(trace.file == "/src/a.c3" && trace.line == 40 && trace.function == "main");
	trace.free();
}

fn void parse_addr2line_discriminator()
{
	Backtrace trace = linux::backtrace_line_parse(mem, "main at /src/a.c3:40 (discriminator 3)", "obj", "fn", false)!!;
	assert(trace.file == "/src/a.c3" && trace.line == 40);
	trace.free();
}
