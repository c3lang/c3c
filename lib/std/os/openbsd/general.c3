module std::os::openbsd @if(env::OPENBSD);
import libc, std::os, std::collections::list;

extern fn ZString* backtrace_symbols_fmt(void **addrlist, usz len, ZString fmt);

fn Backtrace? backtrace_line_parse(Allocator allocator, String obj, String addr2line) @local
{
	@stack_mem(256; Allocator mem)
	{
		String[] parts = addr2line.trim().split(mem, " at ");
		if (parts.len != 2) return NOT_FOUND?;

		uint line   = 0;
		String source = "";
		if (!parts[1].contains("?") && parts[1].contains(":"))
		{
			usz index = parts[1].rindex_of_char(':')!;
			source = parts[1][:index];
			line   = parts[1][index + 1..].to_uint()!;
		}
		return {
			.function    = parts[0].copy(allocator),
			.object_file = obj.copy(allocator),
			.file        = source.copy(allocator),
			.line        = line,
			.allocator   = allocator,
			.is_inline   = false,
		};
	};
}

fn void? backtrace_add_addr2line(Allocator allocator, BacktraceList* list, String obj, String addr2line) @local
{
	list.push(backtrace_line_parse(allocator, obj, addr2line)!);
}

fn void? backtrace_add_from_exec(Allocator allocator, BacktraceList* list, String fun, String obj) @local
{
	char[1024] buf @noinit;
	String addr2line = process::execute_stdout_to_buffer(&buf, {"llvm-addr2line", "-fpe", obj, fun})!;
	return backtrace_add_addr2line(allocator, list, obj, addr2line);
}

fn void? backtrace_add_element(Allocator allocator, BacktraceList *list, String fun, String obj) @local
{
	return backtrace_add_from_exec(allocator, list, fun, obj);
}

fn BacktraceList? symbolize_backtrace(Allocator allocator, void*[] backtrace)
{
	BacktraceList list;
	list.init(allocator, backtrace.len);
	defer catch
	{
		foreach (trace : list)
		{
			trace.free();
		}
		list.free();
	}

	ZString *strings = backtrace_symbols_fmt(backtrace.ptr, backtrace.len, "%n%D %f");

	for (int i = 0; i < backtrace.len; i++) {
		String full = strings[i].str_view();
		Splitter iter = full.tokenize(" ");
		String fun = iter.next()!;
		String obj = iter.next()!;
		backtrace_add_element(allocator, &list, fun, obj)!;
	}

	free(strings);
	return list;
}
