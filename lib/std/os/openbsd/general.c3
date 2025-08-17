module std::os::openbsd @if(env::OPENBSD);
import libc, std::os, std::collections::list;


// Native `sysctl` identifiers from <sys/sysctl.h>
const CTL_UNSPEC        =  0;   /* unused */
const CTL_KERN          =  1;   /* "high kernel": proc, limits */
const CTL_VM            =  2;   /* virtual memory */
const CTL_FS            =  3;   /* file system, mount type is next */
const CTL_NET           =  4;   /* network, see socket.h */
const CTL_DEBUG         =  5;   /* debugging parameters */
const CTL_HW            =  6;   /* generic cpu/io */
const CTL_MACHDEP       =  7;   /* machine dependent */
const CTL_GAP_UNUSED    =  8;   /* was CTL_USER: removed 2013-04 */
const CTL_DDB           =  9;   /* DDB user interface, see db_var.h */
const CTL_VFS           = 10;   /* VFS sysctl's */
const CTL_MAXID         = 11;   /* number of valid top-level ids */

const HW_MACHINE        =  1;   /* string: machine class */
const HW_MODEL          =  2;   /* string: specific machine model */
const HW_NCPU           =  3;   /* int: number of configured cpus */
const HW_BYTEORDER      =  4;   /* int: machine byte order */
const HW_PHYSMEM        =  5;   /* int: total memory */
const HW_USERMEM        =  6;   /* int: non-kernel memory */
const HW_PAGESIZE       =  7;   /* int: software page size */
const HW_DISKNAMES      =  8;   /* strings: disk drive names */
const HW_DISKSTATS      =  9;   /* struct: diskstats[] */
const HW_DISKCOUNT      = 10;   /* int: number of disks */
const HW_SENSORS        = 11;   /* node: hardware monitors */
const HW_CPUSPEED       = 12;   /* get CPU frequency */
const HW_SETPERF        = 13;   /* set CPU performance % */
const HW_VENDOR         = 14;   /* string: vendor name */
const HW_PRODUCT        = 15;   /* string: product name */
const HW_VERSION        = 16;   /* string: hardware version */
const HW_SERIALNO       = 17;   /* string: hardware serial number */
const HW_UUID           = 18;   /* string: universal unique id */
const HW_PHYSMEM64      = 19;   /* quad: total memory */
const HW_USERMEM64      = 20;   /* quad: non-kernel memory */
const HW_NCPUFOUND      = 21;   /* int: number of cpus found */
const HW_ALLOWPOWERDOWN = 22;   /* allow power button shutdown */
const HW_PERFPOLICY     = 23;   /* set performance policy */
const HW_SMT            = 24;   /* int: enable SMT/HT/CMT */
const HW_NCPUONLINE     = 25;   /* int: number of cpus being used */
const HW_POWER          = 26;   /* int: machine has wall-power */
const HW_BATTERY        = 27;   /* node: battery */
const HW_UCOMNAMES      = 28;   /* strings: ucom names */
const HW_MAXID          = 29;   /* number of valid hw ids */


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
