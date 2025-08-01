module std::os::darwin @if(env::DARWIN);
import std::collections::list, std::os;

const CTL_UNSPEC	= 0; /* unused */
const CTL_KERN		= 1; /* "high kernel": proc, limits */
const CTL_VM		= 2; /* virtual memory */
const CTL_VFS		= 3; /* file system, mount type is next */
const CTL_NET		= 4; /* network, see socket.h */
const CTL_DEBUG		= 5; /* debugging parameters */
const CTL_HW		= 6; /* generic cpu/io */
const CTL_MACHDEP	= 7; /* machine dependent */
const CTL_USER		= 8; /* user-level */
const CTL_MAXID		= 9; /* number of valid top-level ids */

const HW_MACHINE		= 1; /* string: machine class */
const HW_MODEL			= 2; /* string: specific machine model */
const HW_NCPU			= 3; /* int: number of cpus */
const HW_BYTEORDER		= 4; /* int: machine byte order */
const HW_PHYSMEM		= 5; /* int: total memory */
const HW_USERMEM		= 6; /* int: non-kernel memory */
const HW_PAGESIZE		= 7; /* int: software page size */
const HW_DISKNAMES		= 8; /* strings: disk drive names */
const HW_DISKSTATS		= 9; /* struct: diskstats[] */
const HW_EPOCH			= 10; /* int: 0 for Legacy, else NewWorld */
const HW_FLOATINGPT		= 11; /* int: has HW floating point? */
const HW_MACHINE_ARCH	= 12; /* string: machine architecture */
const HW_VECTORUNIT		= 13; /* int: has HW vector unit? */
const HW_BUS_FREQ		= 14; /* int: Bus Frequency */
const HW_CPU_FREQ		= 15; /* int: CPU Frequency */
const HW_CACHELINE		= 16; /* int: Cache Line Size in Bytes */
const HW_L1ICACHESIZE	= 17; /* int: L1 I Cache Size in Bytes */
const HW_L1DCACHESIZE	= 18; /* int: L1 D Cache Size in Bytes */
const HW_L2SETTINGS		= 19; /* int: L2 Cache Settings */
const HW_L2CACHESIZE	= 20; /* int: L2 Cache Size in Bytes */
const HW_L3SETTINGS		= 21; /* int: L3 Cache Settings */
const HW_L3CACHESIZE	= 22; /* int: L3 Cache Size in Bytes */
const HW_MAXID			= 23; /* number of valid hw ids */

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);
extern fn CInt darwin_NSGetExecutablePath(char* buffer, uint *size) @extern("_NSGetExecutablePath") @builtin;
extern fn Darwin_segment_command_64* getsegbyname(ZString segname);
extern fn uint _dyld_image_count();
extern fn ZString _dyld_get_image_name(uint image_index);
extern fn iptr _dyld_get_image_vmaddr_slide(uint image_index);
extern fn CInt dladdr(void* addr, Darwin_Dl_info* info);

struct Darwin_Dl_info
{
	ZString     dli_fname;     /* Pathname of shared object */
	void*       dli_fbase;     /* Base address of shared object */
	ZString     dli_sname;     /* Name of nearest symbol */
	void*       dli_saddr;     /* Address of nearest symbol */
}

struct Darwin_segment_command_64
{
	uint	    cmd;		/* LC_SEGMENT_64 */
	uint	    cmdsize;	/* includes sizeof section_64 structs */
	char[16]    segname;	/* segment name */
	ulong   	vmaddr;		/* memory address of this segment */
	ulong   	vmsize;		/* memory size of this segment */
	ulong   	fileoff;	/* file offset of this segment */
	ulong   	filesize;	/* amount to map from the file */
	int  		maxprot;	/* maximum VM protection */
	int 		initprot;	/* initial VM protection */
	uint    	nsects;		/* number of sections in segment */
	uint    	flags;		/* flags */
}

struct Darwin_mach_timebase_info
{
	uint numer;
	uint denom;
}

alias Darwin_mach_timebase_info_t = Darwin_mach_timebase_info;
alias Darwin_mach_timebase_info_data_t = Darwin_mach_timebase_info;

extern fn void mach_timebase_info(Darwin_mach_timebase_info_data_t* timebase);
extern fn ulong mach_absolute_time();

fn String? executable_path()
{
	static char[4096] path;
	static uint len = 0;
	if (!len)
	{
		char[4096] buf;
		uint temp_len = buf.len;
    	if (darwin_NSGetExecutablePath(&buf, &temp_len) < 0) return NOT_FOUND?;
    	path[:len] = buf[:len];
    	len = (int)((ZString)&buf).len();
    }
    return (String)path[:len];
}

fn uptr? load_address() @local
{
	Darwin_segment_command_64* cmd = darwin::getsegbyname("__TEXT");
	if (!cmd) return backtrace::SEGMENT_NOT_FOUND?;
	String path = env::executable_path() ?? backtrace::EXECUTABLE_PATH_NOT_FOUND?!;
	uint dyld_count = darwin::_dyld_image_count();
	for (uint i = 0; i < dyld_count; i++)
	{
		ZString image_name = darwin::_dyld_get_image_name(i);
		if (!image_name) continue;
		if (image_name.str_view() != path) continue;
		return cmd.vmaddr + darwin::_dyld_get_image_vmaddr_slide(i);
	}
	return backtrace::IMAGE_NOT_FOUND?;
}

fn Backtrace? backtrace_load_element(Allocator allocator, String execpath, void* buffer, void* load_address) @local
{
	if (buffer)
	{
		char[1024] buf;
		String s = process::execute_stdout_to_buffer(&buf,
			{ "atos", "-o", execpath, "-arch", env::AARCH64 ? "arm64" : "x86_64", "-l",
				string::bformat(&&(char[64]){}, "%p", load_address),
				string::bformat(&&(char[64]){}, "%p", buffer - 1),
		        "-fullPath" })!;
		@stack_mem(512; Allocator mem)
        {
			String[] parts = s.split(mem, " ", 5);
			if (parts.len == 4)
			{
				String[] path_parts = parts[3].split(mem, ":");
				return {
					.offset = (uptr)buffer,
					.function = parts[0].copy(allocator),
					.object_file = parts[2][..^2].copy(allocator),
					.file = path_parts[0][1..].copy(allocator),
					.line = path_parts[1][..^2].to_uint()!,
					.allocator = allocator
				};
			}
		};
	}
	Darwin_Dl_info info;
	if (!buffer || !darwin::dladdr(buffer, &info)) return backtrace::BACKTRACE_UNKNOWN;
	return {
		.offset = (uptr)buffer,
		.function = info.dli_sname ? info.dli_sname.copy(allocator) : "???".copy(allocator),
		.object_file = info.dli_fname.copy(allocator),
		.file = "".copy(allocator),
		.line = 0,
		.allocator = allocator
	};
}

fn BacktraceList? symbolize_backtrace(Allocator allocator, void*[] backtrace)
{
	void *load_addr = (void *)load_address()!;
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
	String execpath = executable_path()!;
	foreach (addr : backtrace)
	{
		list.push(backtrace_load_element(allocator, execpath, addr, load_addr) ?? backtrace::BACKTRACE_UNKNOWN);
	}
	return list;
}

