module std::os::linux @if(env::LINUX);
import libc, std::os, std::io, std::collections::list;

extern fn isz readlink(ZString path, char* buf, usz bufsize);

const PT_PHDR = 6;
const EI_NIDENT = 16;
alias Elf32_Half = ushort;
alias Elf32_Word = uint;
alias Elf32_Addr = uint;
alias Elf32_Off = uint;

struct Elf32_Ehdr
{
	char[EI_NIDENT] e_ident;
	Elf32_Half	    e_type;
	Elf32_Half	    e_machine;
	Elf32_Word	    e_version;
	Elf32_Addr	    e_entry;
	Elf32_Off	    e_phoff;
	Elf32_Off	    e_shoff;
	Elf32_Word	    e_flags;
	Elf32_Half	    e_ehsize;
	Elf32_Half	    e_phentsize;
	Elf32_Half	    e_phnum;
	Elf32_Half	    e_shentsize;
	Elf32_Half	    e_shnum;
	Elf32_Half	    e_shstrndx;
}

struct Elf32_Phdr
{
	Elf32_Word	p_type;
	Elf32_Off	p_offset;
	Elf32_Addr	p_vaddr;
	Elf32_Addr	p_paddr;
	Elf32_Word	p_filesz;
	Elf32_Word	p_memsz;
	Elf32_Word	p_flags;
	Elf32_Word	p_align;
}

alias Elf64_Addr = ulong;
alias Elf64_Half = ushort;
alias Elf64_Off = ulong;
alias Elf64_Word = uint;
alias Elf64_Sword = int;
alias Elf64_Sxword = long;
alias Elf64_Lword = ulong;
alias Elf64_Xword = ulong;

struct Elf64_Ehdr
{
	char[EI_NIDENT]     e_ident;
	Elf64_Half	        e_type;
	Elf64_Half	        e_machine;
	Elf64_Word	        e_version;
	Elf64_Addr	        e_entry;
	Elf64_Off	        e_phoff;
	Elf64_Off	        e_shoff;
	Elf64_Word	        e_flags;
	Elf64_Half	        e_ehsize;
	Elf64_Half	        e_phentsize;
	Elf64_Half	        e_phnum;
	Elf64_Half	        e_shentsize;
	Elf64_Half	        e_shnum;
	Elf64_Half	        e_shstrndx;
}

struct Elf64_Phdr
{
	Elf64_Word	p_type;
	Elf64_Word	p_flags;
	Elf64_Off	p_offset;
	Elf64_Addr	p_vaddr;
	Elf64_Addr	p_paddr;
	Elf64_Xword	p_filesz;
	Elf64_Xword	p_memsz;
	Elf64_Xword	p_align;
}

extern fn CInt dladdr(void* addr, Linux_Dl_info* info);

struct Linux_Dl_info
{
	ZString     dli_fname;     /* Pathname of shared object */
	void*       dli_fbase;     /* Base address of shared object */
	ZString     dli_sname;     /* Name of nearest symbol */
	void*       dli_saddr;     /* Address of nearest symbol */
}

fn ulong? elf_module_image_base(String path) @local
{
	File file = file::open(path, "rb")!;
	defer (void)file.close();
	char[4] buffer;
	io::read_all(&file, &buffer)!;
	if (buffer != { 0x7f, 'E', 'L', 'F'}) return backtrace::IMAGE_NOT_FOUND?;
	bool is_64 = file.read_byte()! == 2;
	bool is_little_endian = file.read_byte()! == 1;
	// Actually, not supported.
	if (!is_little_endian) return backtrace::IMAGE_NOT_FOUND?;
	file.seek(0)!;
	if (is_64)
	{
		Elf64_Ehdr file_header;
		io::read_any(&file, &file_header)!;
		if (file_header.e_ehsize != Elf64_Ehdr.sizeof) return backtrace::IMAGE_NOT_FOUND?;
		for (isz i = 0; i < file_header.e_phnum; i++)
		{
			Elf64_Phdr header;
			file.seek((usz)file_header.e_phoff + (usz)file_header.e_phentsize * i)!;
			io::read_any(&file, &header)!;
			if (header.p_type == PT_PHDR) return header.p_vaddr - header.p_offset;
		}
		return 0;
	}
	Elf32_Ehdr file_header;
	io::read_any(&file, &file_header)!;
	if (file_header.e_ehsize != Elf32_Ehdr.sizeof) return backtrace::IMAGE_NOT_FOUND?;
	for (isz i = 0; i < file_header.e_phnum; i++)
	{
		Elf32_Phdr header;
		file.seek(file_header.e_phoff + (usz)file_header.e_phentsize * i)!;
		io::read_any(&file, &header)!;
		if (header.p_type == PT_PHDR) return (ulong)header.p_vaddr - header.p_offset;
	}
	return 0;
}

fn void? backtrace_add_from_exec(Allocator allocator, BacktraceList* list, void* addr) @local
{
	char[] buf = mem::talloc_array(char, 1024);

	String exec_path = process::execute_stdout_to_buffer(buf, {"realpath", "-e", string::tformat("/proc/%d/exe", posix::getpid())})!;
	String obj_name = exec_path.copy(allocator);
	String addr2line = process::execute_stdout_to_buffer(buf, {"addr2line", "-p", "-i", "-C", "-f", "-e", exec_path, string::tformat("0x%x", addr)})!;
	return backtrace_add_addr2line(allocator, list, addr, addr2line, obj_name, "???");
}

fn void? backtrace_add_from_dlinfo(Allocator allocator, BacktraceList* list, void* addr, Linux_Dl_info* info) @local
{
	char[] buf = mem::talloc_array(char, 1024);

	void*   obj_addr  = addr - (uptr)info.dli_fbase + (uptr)elf_module_image_base(info.dli_fname.str_view())!;
	ZString obj_path  = info.dli_fname;
	String  sname     = info.dli_sname ? info.dli_sname.str_view() : "???";
	String  addr2line = process::execute_stdout_to_buffer(buf, {"addr2line", "-p", "-i", "-C", "-f", "-e", obj_path.str_view(), string::tformat("0x%x", obj_addr - 1)})!;
	return backtrace_add_addr2line(allocator, list, addr, addr2line, info.dli_fname.str_view(), sname);
}

fn Backtrace? backtrace_line_parse(Allocator allocator, String string, String obj_name, String func_name, bool is_inlined)
{
	String[] parts = string.trim().tsplit(" at ");
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
		.object_file = obj_name.copy(allocator),
		.file        = source.copy(allocator),
		.line        = line,
		.allocator   = allocator,
		.is_inline   = is_inlined
	};
}
fn void? backtrace_add_addr2line(Allocator allocator, BacktraceList* list, void* addr, String addr2line, String obj_name, String func_name) @local
{
	String[] inline_parts = addr2line.tsplit("(inlined by)");
	usz last = inline_parts.len - 1;
	foreach (i, part : inline_parts)
	{
		bool is_inline = i != last;
		Backtrace? trace = backtrace_line_parse(allocator, part, obj_name, func_name, is_inline);
		if (catch trace)
		{
			list.push({
				.function    = func_name.copy(allocator),
				.object_file = obj_name.copy(allocator),
				.offset      = (uptr)addr,
				.file        = "".copy(allocator),
				.line        = 0,
				.allocator   = allocator,
				.is_inline   = is_inline
			});
			continue;
		}
		list.push(trace);
	}
}

fn void? backtrace_add_element(Allocator allocator, BacktraceList *list, void* addr) @local
{
	if (!addr)
	{
		list.push(backtrace::BACKTRACE_UNKNOWN);
		return;
	}

	@pool()
	{
		Linux_Dl_info info;
		if (dladdr(addr, &info) == 0)
		{
			return backtrace_add_from_exec(allocator, list, addr);
		}
		return backtrace_add_from_dlinfo(allocator, list, addr, &info);
	};
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
	@pool()
	{
		foreach (addr : backtrace)
		{
			backtrace_add_element(allocator, &list, addr)!;
		}
	};
	return list;
}
