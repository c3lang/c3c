module std::os::darwin @if(env::DARWIN);
import std::collections::list;

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


fn String! executable_path(Allocator *using = mem::heap())
{
	char[4096] path;
	uint len = path.len;
	if (darwin_NSGetExecutablePath(&path, &len) < 0) return SearchResult.MISSING?;
	return ((ZString)&path).copy(.using = using);
}

def BacktraceList = List(<Backtrace>);

fn uptr! load_address() @local
{
	Darwin_segment_command_64* cmd = darwin::getsegbyname("__TEXT");
	if (!cmd) return BacktraceFault.SEGMENT_NOT_FOUND?;
	String path = env::executable_path(mem::temp()) ?? BacktraceFault.EXECUTABLE_PATH_NOT_FOUND?!;
    uint dyld_count = darwin::_dyld_image_count();
	for (uint i = 0; i < dyld_count; i++)
	{
		ZString image_name = darwin::_dyld_get_image_name(i);
		if (!image_name) continue;
		if (image_name.as_str() != path) continue;
		return cmd.vmaddr + darwin::_dyld_get_image_vmaddr_slide(i);
	}
	return BacktraceFault.IMAGE_NOT_FOUND?;
}


fn Backtrace! backtrace_load_element(String execpath, void* buffer, void* load_address, Allocator* using = mem::heap()) @local
{
	@pool(using)
	{
		if (buffer)
		{
			SubProcess process = process::create({ "atos",
				"-o", execpath, "-arch", env::AARCH64 ? "arm64" : "x86_64", "-l",
				string::tprintf("%p", load_address),
				string::tprintf("%p", buffer),
				 "-fullPath" })!;
			process.join()!;
			char* buf = tmalloc(1024);
			usz len = process.read_stdout(buf, 1024)!;
			String s = (String)buf[:len - 1];
			String[] parts = s.tsplit(" ");
			if (parts.len == 4)
			{
				String[] path_parts = parts[3].tsplit(":");
				return {
					.offset = (uptr)buffer,
                    .function = parts[0].copy(using),
                    .object_file = parts[2][..^2].copy(using),
                    .file = path_parts[0][1..].copy(using),
                    .line = path_parts[1][..^2].to_uint()!,
                    .allocator = using
				};
			}
		}
		Darwin_Dl_info info;
		if (!buffer || !darwin::dladdr(buffer, &info)) return backtrace::BACKTRACE_UNKNOWN;
		return {
			.offset = (uptr)buffer,
			.function = info.dli_sname ? info.dli_sname.copy(using) : "???".copy(using),
			.object_file = info.dli_fname.copy(using),
			.file = "".copy(using),
			.line = 0
		};
	};
}

fn BacktraceList! backtrace_load(Allocator* using = mem::heap())
{
	void*[256] bt_buffer;
	CInt size = posix::backtrace(&bt_buffer, 256);
	void *load_addr = (void *)load_address()!;
	BacktraceList list;
	list.init(size, .using = using);
	defer catch
	{
		foreach (trace : list)
		{
			trace.free();
		}
		list.free();
	}
	@pool(using)
	{
		String execpath = executable_path(mem::temp())!;
		for (usz i = 1; i < size; i++)
		{
			void* buffer = bt_buffer[i];
			Backtrace trace = backtrace_load_element(execpath, buffer, load_addr, .using = using)!;
			list.append(trace);
		}
	};
	return list;
}
