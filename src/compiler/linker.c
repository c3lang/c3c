#include "compiler_internal.h"
#include "c3_llvm.h"

#if PLATFORM_POSIX
#include <glob.h>
#endif


typedef enum
{
	LINKER_LINK_EXE,
	LINKER_LD,
	LINKER_LD64,
	LINKER_WASM,
	LINKER_CC,
	LINKER_UNKNOWN
} Linker;

#define add_arg(arg_) vec_add(*args_ref, (arg_))
#define add_arg2(arg_, arg_2) vec_add(*args_ref, str_cat((arg_), (arg_2)))

static inline bool is_no_pie(RelocModel reloc)
{
	return reloc == RELOC_NONE;
}
static inline bool is_pie(RelocModel reloc)
{
	return reloc == RELOC_BIG_PIE || reloc == RELOC_SMALL_PIE;
}

static const char *ld_target(ArchType arch_type)
{
	switch (arch_type)
	{
		case ARCH_TYPE_X86_64:
			return "elf_x86_64";
		case ARCH_TYPE_X86:
			return "elf_i386";
		case ARCH_TYPE_AARCH64:
			return "aarch64elf";
		case ARCH_TYPE_RISCV32:
			return "elf32lriscv";
		case ARCH_TYPE_RISCV64:
			return "elf64lriscv";
		default:
			error_exit("Architecture currently not available for cross linking.");
	}

}
static const char *string_esc(const char *str)
{
	scratch_buffer_clear();
	size_t len = strlen(str);
	for (size_t i = 0; i < len; i++)
	{
		if (i > 3 && !char_is_alphanum_(str[i])) scratch_buffer_append_char('\\');
		scratch_buffer_append_char(str[i]);
	}
	return strdup(scratch_buffer_to_string());
}

static void linker_setup_windows(const char ***args_ref, Linker linker_type)
{
	add_arg(active_target.win.use_win_subsystem ? "/SUBSYSTEM:WINDOWS" : "/SUBSYSTEM:CONSOLE");
	if (link_libc()) global_context_add_link("dbghelp");
	if (linker_type == LINKER_CC) return;
	//add_arg("/MACHINE:X64");
	bool is_debug = false;
	switch (active_target.debug_info)
	{
		case DEBUG_INFO_NOT_SET:
			break;
		case DEBUG_INFO_NONE:
			add_arg("/DEBUG:NONE");
			break;
		case DEBUG_INFO_LINE_TABLES:
		case DEBUG_INFO_FULL:
			add_arg("/DEBUG:FULL");
			is_debug = true;
			break;
		default:
			UNREACHABLE
	}
	if (!link_libc()) return;
	bool link_with_dynamic_debug_libc = true;
#if !PLATFORM_WINDOWS
	// The debug version of libc is usually not available on target machines,
	// so we do not link with debug dll versions of libc.
	link_with_dynamic_debug_libc = false;
#endif
	WinCrtLinking linking = active_target.win.crt_linking;
	FOREACH(Library *, library, active_target.library_list)
	{
		WinCrtLinking wincrt = library->target_used->win_crt;
		if (wincrt == WIN_CRT_DEFAULT) continue;
		if (linking != WIN_CRT_DEFAULT)
		{
			WARNING("Mismatch between CRT linking in library %s and previously selected type.", library->dir);
			continue;
		}
		linking = wincrt;
	}

	if (!active_target.win.sdk)
	{
		const char *path = windows_cross_compile_library();
		if (path)
		{
			switch (platform_target.arch)
			{
				case ARCH_TYPE_ARM:
					scratch_buffer_append("/arm");
					break;
				case ARCH_TYPE_AARCH64:
					scratch_buffer_append("/arm64");
					break;
				case ARCH_TYPE_X86_64:
					scratch_buffer_append("/x64");
					break;
				case ARCH_TYPE_X86:
					scratch_buffer_append("/x86");
					break;
				default:
					UNREACHABLE
			}
			if (file_exists(scratch_buffer_to_string()))
			{
				active_target.win.sdk = scratch_buffer_copy();
				// If we only use the msvc cross compile on windows, we
				// avoid linking with dynamic debug dlls.
				link_with_dynamic_debug_libc = false;
			}
		}
	}
	if (active_target.win.def)
	{
		add_arg(str_printf("/def:%s", active_target.win.def));
	}
	if (active_target.win.sdk)
	{
		add_arg(str_printf("/LIBPATH:%s", active_target.win.sdk));
	}
	else
	{
		WindowsSDK *windows_sdk = windows_get_sdk();
		if (!windows_sdk) error_exit("Windows applications cannot be cross compiled without --winsdk.");

		if (!file_is_dir(windows_sdk->vs_library_path)) error_exit("Failed to find windows sdk.");

		add_arg(str_printf("/LIBPATH:%s", windows_sdk->windows_sdk_um_library_path));
		add_arg(str_printf("/LIBPATH:%s", windows_sdk->windows_sdk_ucrt_library_path));
		add_arg(str_printf("/LIBPATH:%s", windows_sdk->vs_library_path));
	}
	// Do not link any.
	if (active_target.win.crt_linking == WIN_CRT_NONE) return;

	global_context_add_link("kernel32");
	global_context_add_link("ntdll");
	global_context_add_link("user32");
	global_context_add_link("shell32");
	global_context_add_link("Shlwapi");
	global_context_add_link("Ws2_32");
	global_context_add_link("legacy_stdio_definitions");

	if (active_target.win.crt_linking == WIN_CRT_STATIC)
	{
		if (is_debug)
		{
			global_context_add_link("libucrtd");
			global_context_add_link("libvcruntimed");
			global_context_add_link("libcmtd");
			global_context_add_link("libcpmtd");
		}
		else
		{
			global_context_add_link("libucrt");
			global_context_add_link("libvcruntime");
			global_context_add_link("libcmt");
			global_context_add_link("libcpmt");
		}
	}
	else
	{
		// When cross compiling we might not have the relevant debug libraries.
		// if so, then exclude them.
		if (is_debug && link_with_dynamic_debug_libc)
		{
			global_context_add_link("ucrtd");
			global_context_add_link("vcruntimed");
			global_context_add_link("msvcrtd");
			global_context_add_link("msvcprtd");
		}
		else
		{
			global_context_add_link("ucrt");
			global_context_add_link("vcruntime");
			global_context_add_link("msvcrt");
			global_context_add_link("msvcprt");
		}
	}
	add_arg("/NOLOGO");
}

static void linker_setup_macos(const char ***args_ref, Linker linker_type)
{
	if (linker_type == LINKER_CC)
	{
		add_arg("-target");
		add_arg(platform_target.target_triple);
		return;
	}
	add_arg("-arch");
	add_arg(arch_to_linker_arch(platform_target.arch));
	if (strip_unused() && active_target.type == TARGET_TYPE_EXECUTABLE)
	{
		add_arg("-no_exported_symbols");
		add_arg("-dead_strip");
	}

	// Skip if no libc.
	if (!link_libc()) return;

	if (!active_target.macos.sdk)
	{
		error_exit("Cannot crosslink MacOS without providing --macossdk.");
	}
	global_context_add_link("System");
	global_context_add_link("m");
	add_arg("-syslibroot");
	add_arg(active_target.macos.sysroot);
	if (is_no_pie(platform_target.reloc_model)) add_arg("-no_pie");
	if (is_pie(platform_target.reloc_model)) add_arg("-pie");
	add_arg("-platform_version");
	add_arg("macos");
	if (active_target.macos.min_version)
	{
		add_arg(active_target.macos.min_version);
	}
	else
	{
		add_arg(str_printf("%d.%d.0", active_target.macos.sdk->macos_min_deploy_target.major, active_target.macos.sdk->macos_min_deploy_target.minor));
	}
	if (active_target.macos.sdk_version)
	{
		add_arg(active_target.macos.sdk_version);
	}
	else
	{
		add_arg(str_printf("%d.%d", active_target.macos.sdk->macos_deploy_target.major, active_target.macos.sdk->macos_deploy_target.minor));
	}
}


static const char *find_freebsd_crt(void)
{
	if (file_exists("/usr/lib/crt1.o"))
	{
		return "/usr/lib/";
	}
	return NULL;
}

static const char *find_arch_glob_path(const char *glob_path, int file_len)
{
#if PLATFORM_POSIX
	glob_t globbuf;
	if (!glob(glob_path, 0, NULL, &globbuf))
	{
		for (int i = 0; i < globbuf.gl_pathc; i++)
		{
			const char *path = globbuf.gl_pathv[i];
			// Avoid qemu problems
			if (platform_target.arch != ARCH_TYPE_RISCV64
			    && platform_target.arch != ARCH_TYPE_RISCV32
			    && strstr(path, "riscv")) continue;
			size_t len = strlen(path);
			assert(len > file_len);
			const char *res = str_copy(path, len - file_len);
			globfree(&globbuf);
			return res;
		}
		globfree(&globbuf);
	}
#endif
	return NULL;
}
static const char *find_linux_crt(void)
{
	if (active_target.linuxpaths.crt) return active_target.linuxpaths.crt;
	const char *path = find_arch_glob_path("/usr/lib/*/crt1.o", 6);
	if (!path)
	{
		INFO_LOG("No crt in /usr/lib/*/");
		return NULL;
	}
	INFO_LOG("Found crt at %s", path);
	return path;
}

static const char *find_linux_crt_begin(void)
{
	if (active_target.linuxpaths.crtbegin) return active_target.linuxpaths.crtbegin;
	const char *path = find_arch_glob_path("/usr/lib/gcc/*/*/crtbegin.o", 10);
	if (!path)
	{
		INFO_LOG("No crtbegin in /usr/lib/gcc/*/*/");
		return NULL;
	}
	INFO_LOG("Found crtbegin at %s", path);
	return path;
}

static void linker_setup_linux(const char ***args_ref, Linker linker_type)
{
	global_context_add_link("dl");
	if (linker_type == LINKER_CC)
	{
		if (!link_libc())
		{
			add_arg("-nostdlib");
			return;
		}
		else
		{
			global_context_add_link("m");
		}
		if (active_target.debug_info == DEBUG_INFO_FULL)
		{
			add_arg("-rdynamic");
		}
		add_arg("-pthread");
		return;
	}
	if (active_target.debug_info == DEBUG_INFO_FULL)
	{
		add_arg("-export-dynamic");
	}
	if (is_no_pie(platform_target.reloc_model)) add_arg("-no-pie");
	if (is_pie(platform_target.reloc_model)) add_arg("-pie");
	if (platform_target.arch == ARCH_TYPE_X86_64) add_arg("--eh-frame-hdr");
	if (!link_libc()) return;
	const char *crt_begin_dir = find_linux_crt_begin();
	const char *crt_dir = find_linux_crt();

	if (strip_unused() && active_target.type == TARGET_TYPE_EXECUTABLE)
	{
		add_arg("--gc-sections");
	}

	if (!crt_begin_dir || !crt_dir)
	{
		error_exit("Failed to find the C runtime at link time.");
	}
	if (is_pie_pic(platform_target.reloc_model))
	{
		add_arg("-pie");
		add_arg2(crt_dir, "Scrt1.o");
		add_arg2(crt_begin_dir, "crtbeginS.o");
		add_arg2(crt_dir, "crti.o");
		add_arg2(crt_begin_dir, "crtendS.o");
	}
	else
	{
		add_arg2(crt_dir, "crt1.o");
		add_arg2(crt_begin_dir, "crtbegin.o");
		add_arg2(crt_dir, "crti.o");
		add_arg2(crt_begin_dir, "crtend.o");
	}
	add_arg2(crt_dir, "crtn.o");
	add_arg2("-L", crt_dir);
	add_arg("-L");
	add_arg("/usr/lib/x86_64-linux-gnu/libdl.so");
	add_arg("--dynamic-linker=/lib64/ld-linux-x86-64.so.2");
	global_context_add_link("m");
	global_context_add_link("pthread");
	global_context_add_link("c");
	add_arg("-L/usr/lib/");
	add_arg("-L/lib/");
	add_arg("-m");
	add_arg(ld_target(platform_target.arch));
}

static void linker_setup_freebsd(const char ***args_ref, Linker linker_type)
{
	if (linker_type == LINKER_CC) return;
	if (is_no_pie(platform_target.reloc_model)) add_arg("-no-pie");
	if (is_pie(platform_target.reloc_model)) add_arg("-pie");
	if (platform_target.arch == ARCH_TYPE_X86_64) add_arg("--eh-frame-hdr");

	if (!link_libc()) return;

	const char *crt_dir = find_freebsd_crt();
	if (!crt_dir)
	{
		error_exit("Failed to find the C runtime at link time.");
	}
	if (strip_unused() && active_target.type == TARGET_TYPE_EXECUTABLE)
	{
		add_arg("--gc-sections");
	}

	if (is_pie_pic(platform_target.reloc_model))
	{
		add_arg("-pie");
		add_arg2(crt_dir, "Scrt1.o");
		add_arg2(crt_dir, "crtbeginS.o");
		add_arg2(crt_dir, "crti.o");
		add_arg2(crt_dir, "crtendS.o");
	}
	else
	{
		add_arg2(crt_dir, "crt1.o");
		add_arg2(crt_dir, "crtbegin.o");
		add_arg2(crt_dir, "crti.o");
		add_arg2(crt_dir, "crtend.o");
	}
	add_arg2(crt_dir, "crtn.o");
	add_arg2("-L", crt_dir);
	add_arg("--dynamic-linker=/libexec/ld-elf.so.1");
	global_context_add_link("c");
	global_context_add_link("m");
	global_context_add_link("gcc");
	global_context_add_link("gcc_s");

	add_arg("-L/usr/lib/");
	add_arg("-m");
	add_arg(ld_target(platform_target.arch));
}

static void add_linked_libs(const char ***args_ref, const char **libs, bool is_win)
{
	FOREACH(const char *, lib, libs)
	{
		INFO_LOG("Linking %s", lib);
		const char *framework = str_remove_suffix(lib, ".framework");
		if (framework)
		{
			add_arg("-framework");
			add_arg(framework);
			continue;
		}
		if (is_win)
		{
			if (str_has_suffix(lib, ".lib"))
			{
				add_arg(lib);
			}
			else
			{
				add_arg2(lib, ".lib");
			}
		}
		else
		{
			if (str_has_suffix(lib, ".a") || str_has_suffix(lib, ".so") ||
			    str_has_suffix(lib, ".dylib") || str_has_suffix(lib, ".tbd"))
			{
				add_arg(lib);
			}
			else
			{
				add_arg2("-l", lib);
			}
		}
	}
}

static bool linker_setup(const char ***args_ref, const char **files_to_link, unsigned file_count,
                         const char *output_file, Linker linker_type)
{
	bool is_dylib = active_target.type == TARGET_TYPE_DYNAMIC_LIB;
	bool use_win = linker_type == LINKER_LINK_EXE;
	if (!use_win)
	{
		add_arg("-o");
		add_arg(output_file);
	}
	switch (linker_type)
	{
		case LINKER_UNKNOWN:
			break;
		case LINKER_WASM:
			if (!is_dylib && active_target.no_entry) add_arg("--no-entry");
			break;
		case LINKER_LD64:
			if (is_dylib) vec_add(*args_ref, "-dylib");
			break;
		case LINKER_LD:
			if (is_dylib) vec_add(*args_ref, "-shared");
			break;
		case LINKER_LINK_EXE:
			add_arg2("/OUT:", output_file);
			if (is_dylib)
			{
				add_arg("/DLL");
			}
			else
			{
				if (active_target.no_entry) add_arg("/NOENTRY");
			}
		case LINKER_CC:
			break;
		default:
			UNREACHABLE
	}
	const char *lib_path_opt = use_win ? "/LIBPATH:" : "-L";

	switch (platform_target.os)
	{
		case OS_UNSUPPORTED:
			UNREACHABLE
		case OS_TYPE_WIN32:
			linker_setup_windows(args_ref, linker_type);
			break;
		case OS_TYPE_MACOSX:
			linker_setup_macos(args_ref, linker_type);
			break;
		case OS_TYPE_WATCHOS:
		case OS_TYPE_IOS:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_NETBSD:
		case OS_TYPE_TVOS:
		case OS_TYPE_WASI:
			break;
		case OS_TYPE_FREE_BSD:
			linker_setup_freebsd(args_ref, linker_type);
			break;
		case OS_TYPE_LINUX:
			linker_setup_linux(args_ref, linker_type);
			break;
		case OS_TYPE_UNKNOWN:
			if (link_libc())
			{
				error_exit("Linking is not supported for unknown OS.");
			}
			break;
		case OS_TYPE_NONE:
			break;
	}
	for (unsigned i = 0; i < file_count; i++)
	{
		add_arg(files_to_link[i]);
	}

	FOREACH(const char *, dir, active_target.linker_libdirs)
	{
		add_arg2(lib_path_opt, dir);
	}
	FOREACH(const char *, arg, active_target.link_args)
	{
		add_arg(arg);
	}
	add_linked_libs(args_ref, active_target.linker_libs, use_win);
	FOREACH(Library *, library, active_target.library_list)
	{
		LibraryTarget *target = library->target_used;
		FOREACH(const char *, flag, target->link_flags) add_arg(flag);
		add_linked_libs(args_ref, target->linked_libs, use_win);
	}
	add_linked_libs(args_ref, global_context.links, use_win);
	return true;
}
#undef add_arg2
#undef add_arg

static void append_fpie_pic_options(RelocModel reloc, const char ***args_ref)
{
	switch (reloc)
	{
		case RELOC_DEFAULT:
			UNREACHABLE
		case RELOC_NONE:
			vec_add(*args_ref, "-fno-pic");
			vec_add(*args_ref, "-fno-pie");
			vec_add(*args_ref, "-fno-PIC");
			vec_add(*args_ref, "-fno-PIE");
			break;
		case RELOC_SMALL_PIC:
			vec_add(*args_ref, "-fpic");
			break;
		case RELOC_BIG_PIC:
			vec_add(*args_ref, "-fPIC");
			break;
		case RELOC_SMALL_PIE:
			vec_add(*args_ref, "-fpie");
			vec_add(*args_ref, "-fpic");
			break;
		case RELOC_BIG_PIE:
			vec_add(*args_ref, "-fPIE");
			vec_add(*args_ref, "-fPIC");
			break;
	}
}

Linker linker_find_linker_type(void)
{
	if (arch_is_wasm(platform_target.arch)) return LINKER_WASM;
	switch (platform_target.os)
	{
		case OS_UNSUPPORTED:
		case OS_TYPE_UNKNOWN:
		case OS_TYPE_NONE:
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_LINUX:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
			return LINKER_LD;
		case OS_TYPE_IOS:
		case OS_TYPE_MACOSX:
		case OS_TYPE_TVOS:
		case OS_TYPE_WATCHOS:
			return LINKER_LD64;
		case OS_TYPE_WIN32:
			return LINKER_LINK_EXE;
		case OS_TYPE_WASI:
			return LINKER_WASM;
	}
	UNREACHABLE
}

static bool link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	INFO_LOG("Using linker directly.");
	const char **args = NULL;
	Linker linker_type = linker_find_linker_type();
	linker_setup(&args, files_to_link, file_count, output_file, linker_type);

	const char *error = NULL;
	// This isn't used in most cases, but its contents should get freed after linking.

	bool success;
	const char *arg_list = "";
	FOREACH(const char *, arg, args)
	{
		arg_list = str_cat(arg_list, " ");
		arg_list = str_cat(arg_list, arg);
	}
	INFO_LOG("Linker arguments: %s to %d", arg_list, platform_target.object_format);
	if (active_target.print_linking) puts(arg_list);

	switch (platform_target.object_format)
	{
		case OBJ_FORMAT_COFF:
			success = llvm_link_coff(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_ELF:
			success = llvm_link_elf(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_MACHO:
			success = llvm_link_macho(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_WASM:
			success = llvm_link_wasm(args, (int)vec_size(args), &error);
			break;
		default:
			UNREACHABLE
	}
	if (!success)
	{
		error_exit("Failed to create an executable: %s", error);
	}
	INFO_LOG("Linking complete.");
	return true;
}

bool obj_format_linking_supported(ObjectFormatType format_type)
{
	switch (format_type)
	{
		case OBJ_FORMAT_XCOFF:
		case OBJ_FORMAT_AOUT:
		case OBJ_FORMAT_GOFF:
		case OBJ_FORMAT_UNSUPPORTED:
			return false;
		case OBJ_FORMAT_COFF:
		case OBJ_FORMAT_ELF:
		case OBJ_FORMAT_MACHO:
		case OBJ_FORMAT_WASM:
			return true;
	}
	UNREACHABLE

}

const char *concat_string_parts(const char **args)
{
	unsigned size_needed = 0;
	FOREACH(const char *, arg, args)
	{
		size_needed += strlen(arg) + 1;
	}
	char *output = malloc_string(size_needed);
	char *ptr = output;
	FOREACH(const char *, arg, args)
	{
		unsigned len = (unsigned)strlen(arg);
		memcpy(ptr, arg, len);
		ptr += len;
		*(ptr++) = ' ';
	}
	ptr[-1] = '\0';
	return output;
}


void platform_linker(const char *output_file, const char **files, unsigned file_count)
{
	const char **parts = NULL;
	Linker linker_type = LINKER_CC;
	if (active_target.linker_type == LINKER_TYPE_CUSTOM)
	{
		INFO_LOG("Using linker %s.", active_target.custom_linker_path);
		vec_add(parts, active_target.custom_linker_path);
		switch (platform_target.object_format)
		{
			case OBJ_FORMAT_UNSUPPORTED:
			case OBJ_FORMAT_GOFF:
			case OBJ_FORMAT_XCOFF:
			case OBJ_FORMAT_AOUT:
				linker_type = LINKER_UNKNOWN;
				break;
			case OBJ_FORMAT_COFF:
				linker_type = LINKER_LINK_EXE;
				break;
			case OBJ_FORMAT_ELF:
				linker_type = LINKER_LD;
				break;
			case OBJ_FORMAT_MACHO:
				linker_type = LINKER_LD64;
				break;
			case OBJ_FORMAT_WASM:
				linker_type = LINKER_WASM;
				break;
		}
	}
	else
	{
		INFO_LOG("Using cc linker.");
		vec_add(parts, active_target.cc ? active_target.cc : "cc");
		append_fpie_pic_options(platform_target.reloc_model, &parts);
	}

	linker_setup(&parts, files, file_count, output_file, linker_type);
	const char *output = concat_string_parts(parts);
	if (active_target.print_linking) puts(output);
	if (system(output) != 0)
	{
		error_exit("Failed to link executable '%s' using command '%s'.\n", output_file, output);
	}
	if (os_is_apple(platform_target.os) && active_target.debug_info == DEBUG_INFO_FULL)
	{
		// Create .dSYM
		scratch_buffer_clear();
		scratch_buffer_printf("dsymutil -arch %s %s", arch_to_linker_arch(platform_target.arch), output_file);
		if (active_target.print_linking) puts(scratch_buffer_to_string());
		if (system(scratch_buffer_to_string()) != 0)
		{
			puts("Failed to create .dSYM files, debugging will be impacted.");
		}
	}
	printf("Program linked to executable '%s'.\n", output_file);
}

const char *cc_compiler(const char *cc, const char *file, const char *flags, const char *output_subdir)
{
	const char *dir = active_target.object_file_dir;
	if (!dir) dir = active_target.build_dir;
	if (output_subdir) dir = file_append_path(dir, output_subdir);
	dir_make(dir);
	bool is_cl_exe = str_eq(cc, "cl.exe");
	char *filename = NULL;
	bool split_worked = file_namesplit(file, &filename, NULL);
	if (!split_worked) error_exit("Cannot compile '%s'", file);
	size_t len = strlen(filename);
	// Remove .cpp or .c
	if (len > 5 && memcmp(filename + len - 4, ".cpp", 4) == 0)
	{
		len -= 4;
		filename[len] = 0;
	}
	else if (len > 2 && memcmp(filename + len - 2, ".c", 2) == 0)
	{
		len -= 2;
		filename[len] = 0;
	}
	const char *out_name = dir
	                      ? str_printf("%s/%s%s", dir, filename, get_object_extension())
	                      : str_printf("%s%s", filename, get_object_extension());;
	const char **parts = NULL;
	vec_add(parts, cc);

	const bool pie_set =
			flags != NULL &&
			(strstr(flags, "-fno-PIE") || // This is a weird case, but probably don't set PIE if
			 strstr(flags, "-fno-pie") || // it is being set in user defined cflags.
			 strstr(flags, "-fpie") ||
			 strstr(flags, "-fPIE")); // strcasestr is apparently nonstandard >:(
	if (!pie_set && !is_cl_exe)
	{
		append_fpie_pic_options(platform_target.reloc_model, &parts);
	}

	vec_add(parts, is_cl_exe ? "/c" : "-c");
	if (flags) vec_add(parts, flags);
	vec_add(parts, file);
	if (is_cl_exe)
	{
		vec_add(parts, str_printf("/Fo:\"%s\"", out_name));
	}
	else
	{
		vec_add(parts, "-o");
		vec_add(parts, out_name);
	}

	const char *output = concat_string_parts(parts);
	DEBUG_LOG("Compiling c sources using '%s'", output);
	if (system(output) != 0)
	{
		error_exit("Failed to compile c sources using command '%s'.\n", output);
	}
	return out_name;
}

bool dynamic_lib_linker(const char *output_file, const char **files, unsigned file_count)
{
	INFO_LOG("Using linker directly.");
	const char **args = NULL;
	if (active_target.linker_type == LINKER_TYPE_CUSTOM) vec_add(args, active_target.custom_linker_path);
	Linker linker_type = linker_find_linker_type();
	linker_setup(&args, files, file_count, output_file, linker_type);

	const char *command = concat_string_parts(args);
	if (active_target.print_linking) puts(command);
	DEBUG_LOG("Linker arguments: %s to %d", command, platform_target.object_format);
	if (active_target.linker_type == LINKER_TYPE_CUSTOM)
	{
		if (system(command) != 0)
		{
			error_exit("Failed to create a dynamic library using command '%s'.", command);
		}
		return true;
	}
	bool success;
	const char *error = NULL;
	switch (platform_target.object_format)
	{
		case OBJ_FORMAT_COFF:
			success = llvm_link_coff(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_ELF:
			success = llvm_link_elf(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_MACHO:
			success = llvm_link_macho(args, (int)vec_size(args), &error);
			break;
		case OBJ_FORMAT_WASM:
			success = llvm_link_wasm(args, (int)vec_size(args), &error);
			break;
		default:
			UNREACHABLE
	}
	if (!success)
	{
		error_exit("Failed to create a dynamic library: %s", error);
	}
	INFO_LOG("Linking complete.");
	return true;
}

bool static_lib_linker(const char *output_file, const char **files, unsigned file_count)
{
	ArFormat format;
	switch (platform_target.os)
	{
		case OS_DARWIN_TYPES:
			format = AR_DARWIN;
			break;
		case OS_TYPE_WIN32:
			format = AR_COFF;
			break;
		case OS_TYPE_FREE_BSD:
		case OS_TYPE_NETBSD:
		case OS_TYPE_OPENBSD:
			format = AR_BSD;
			break;
		case OS_TYPE_LINUX:
		default:
			format = AR_GNU;
			break;
	}
	return llvm_ar(output_file, files, file_count, format);
}

bool linker(const char *output_file, const char **files, unsigned file_count)
{
	return link_exe(output_file, files, file_count);
}

/**
 * From Clang
 * .Cases("aarch64elf", "aarch64linux", {ELF64LEKind, EM_AARCH64})
		  .Cases("aarch64elfb", "aarch64linuxb", {ELF64BEKind, EM_AARCH64})
		  .Cases("armelf", "armelf_linux_eabi", {ELF32LEKind, EM_ARM})
		  .Case("elf32_x86_64", {ELF32LEKind, EM_X86_64})
		  .Cases("elf32btsmip", "elf32btsmipn32", {ELF32BEKind, EM_MIPS})
		  .Cases("elf32ltsmip", "elf32ltsmipn32", {ELF32LEKind, EM_MIPS})
		  .Case("elf32lriscv", {ELF32LEKind, EM_RISCV})
		  .Cases("elf32ppc", "elf32ppclinux", {ELF32BEKind, EM_PPC})
		  .Cases("elf32lppc", "elf32lppclinux", {ELF32LEKind, EM_PPC})
		  .Case("elf64btsmip", {ELF64BEKind, EM_MIPS})
		  .Case("elf64ltsmip", {ELF64LEKind, EM_MIPS})
		  .Case("elf64lriscv", {ELF64LEKind, EM_RISCV})
		  .Case("elf64ppc", {ELF64BEKind, EM_PPC64})
		  .Case("elf64lppc", {ELF64LEKind, EM_PPC64})
		  .Cases("elf_amd64", "elf_x86_64", {ELF64LEKind, EM_X86_64})
		  .Case("elf_i386", {ELF32LEKind, EM_386})
		  .Case("elf_iamcu", {ELF32LEKind, EM_IAMCU})
		  .Case("elf64_sparc", {ELF64BEKind, EM_SPARCV9})
		  .Case("msp430elf", {ELF32LEKind, EM_MSP430})
 */
