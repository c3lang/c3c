#include "compiler_internal.h"

#include <llvm/Config/llvm-config.h>  // for LLVM_VERSION_STRING
#if PLATFORM_POSIX
#include <glob.h>
#endif

// Copied from wrapper.cpp
typedef enum
{
	AR_GNU,
	AR_DARWIN,
	AR_DARWIN64,
	AR_BSD,
	AR_GNU64,
	AR_COFF,
} ArFormat;

extern bool llvm_link_elf(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_macho(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_coff(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_wasm(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_mingw(const char **args, int arg_count, const char **error_string);
extern bool llvm_ar(const char *out_name, const char **args, size_t count, int ArFormat);


typedef enum
{
	LINKER_LINK_EXE,
	LINKER_LD,
	LINKER_LD64,
	LINKER_WASM,
	LINKER_CC,
} LinkerType;

#define add_arg(arg_) vec_add(*args_ref, (arg_))
#define add_arg2(arg_, arg_2) vec_add(*args_ref, str_cat((arg_), (arg_2)))

static inline bool is_no_pie(RelocModel reloc)
{
	return reloc == RELOC_NONE;
}
static inline bool is_pie(RelocModel reloc)
{
	return reloc == RELOC_BIG_PIE || reloc == RELOC_BIG_PIE;
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
static void linker_setup_windows(const char ***args_ref, LinkerType linker_type)
{
	add_arg(active_target.win.use_win_subsystem ? "/SUBSYSTEM:WINDOWS" : "/SUBSYSTEM:CONSOLE");
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
	if (active_target.no_libc) return;
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
			}
		}
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

	add_arg("kernel32.lib");
	add_arg("ntdll.lib");
	add_arg("user32.lib");
	add_arg("shell32.lib");
	add_arg("legacy_stdio_definitions.lib");

	if (active_target.win.crt_linking == WIN_CRT_STATIC)
	{
		if (is_debug)
		{
			add_arg("libucrtd.lib");
			add_arg("libvcruntimed.lib");
			add_arg("libcmtd.lib");
			add_arg("libcpmtd.lib");
		}
		else
		{
			add_arg("libucrt.lib");
			add_arg("libvcruntime.lib");
			add_arg("libcmt.lib");
			add_arg("libcpmt.lib");
		}
	}
	else
	{
		if (is_debug)
		{
			add_arg("ucrtd.lib");
			add_arg("vcruntimed.lib");
			add_arg("msvcrtd.lib");
			add_arg("msvcprtd.lib");
		}
		else
		{
			add_arg("ucrt.lib");
			add_arg("vcruntime.lib");
			add_arg("msvcrt.lib");
			add_arg("msvcprt.lib");
		}
	}
	add_arg("/NOLOGO");
}
#ifdef mingw64_support
static void linker_setup_mingw64_gcc(const char ***args_ref)
{
	add_arg("-m");
	add_arg("i386pep");
	add_arg("-Bdynamic");
	const char *root = getenv("MSYSTEM_PREFIX");
	const char *gcc_base = strformat("%s/lib/gcc/x86_64-w64-mingw32", root);
	if (!file_exists(gcc_base)) error_exit("Missing GCC");
	const char *name = file_first(gcc_base);
	const char *gcc_path = strformat("%s/%s/", gcc_base, name);
	add_arg(strformat("-L%s/x86_64-w64-mingw32/lib", root));
	add_arg(strformat("-L%s/lib", root));
	add_arg2(gcc_path, "crtbegin.o");
	add_arg(strformat("%s/lib/crt2.o", root));
	add_arg(strformat("%s/lib/default-manifest.o", root));
	add_arg2("-L", gcc_path);
	add_arg("-lkernel32");
	add_arg("-lmingw32");
	add_arg("-lgcc");
	add_arg("-lgcc_eh");
	add_arg("-lmoldname");
	add_arg("-lmingwex");
	add_arg("-lmsvcrt");
	add_arg("-ladvapi32");
	add_arg("-lshell32");
	add_arg("-luser32");
	add_arg("-lpthread");

	add_arg2(gcc_path, "crtend.o");
}

static void linker_setup_windows_gnu(const char ***args_ref, LinkerType linker_type)
{
	if (linker_type == LINKER_CC) return;
	bool is_clang = strcmp(getenv("MSYSTEM"), "CLANG64") == 0;
	bool is_mingw = strcmp(getenv("MSYSTEM"), "MINGW64") == 0;
	if (!is_clang && !is_mingw)
	{
		error_exit("Crosslinking MSYS is not yet supported.");
	}
	if (is_mingw)
	{
		linker_setup_mingw64_gcc(args_ref);
		return;
	}
	const char *root = getenv("MSYSTEM_PREFIX");
	const char *compiler_prefix;
	if (is_clang)
	{
		char *filename;
		char *dir;
		path_get_dir_and_filename_from_full(root, &filename, &dir);
		compiler_prefix = filename;
		root = dir;
	}
	else
	{
		compiler_prefix = "x86_64-w64-mingw32";
	}
	add_arg("-m");
	add_arg("i386pep");
	add_arg("-Bdynamic");
	const char *lib = strformat("%s/%s/lib", root, compiler_prefix);
	if (!file_exists(lib))
	{
		error_exit("Cannot find '%s'.", lib);
	}
	add_arg2(lib, "/crt2.o");
	add_arg2(lib, "/crtbegin.o");
	add_arg2("-L", lib);
	add_arg(strformat("-L%s/lib", root));
	add_arg(strformat("-L%s/%s/sys-root/mingw/lib", root, compiler_prefix));
	const char *clang_dir = strformat("%s/lib/clang/" LLVM_VERSION_STRING, root);
	add_arg(strformat("-L%s/lib/windows", clang_dir));
	add_arg("-lmingw32");
	add_arg(strformat("%s/lib/windows/libclang_rt.builtins-x86_64.a", clang_dir));
	add_arg("-lmoldname");
	add_arg("-lmingwex");
	add_arg("-lmsvcrt");
	add_arg("-ladvapi32");
	add_arg("-lshell32");
	add_arg("-luser32");
	add_arg("-lkernel32");
	add_arg("-lmingw32");
	add_arg2(lib, "\\crtend.o");
}
*/
#endif

static void linker_setup_macos(const char ***args_ref, LinkerType linker_type)
{
	add_arg("-framework");
	add_arg("CoreFoundation");
	if (linker_type == LINKER_CC)
	{
		return;
	}
	add_arg("-arch");
	add_arg(arch_to_linker_arch(platform_target.arch));

	// Skip if no libc.
	if (active_target.no_libc) return;

	const char *sysroot = active_target.macos.sdk ? active_target.macos.sdk : macos_sysroot();
	if (!sysroot)
	{
		error_exit("Cannot crosslink MacOS without providing --macossdk.");
	}
	DEBUG_LOG("Macos SDK: %s", sysroot);
	MacSDK *mac_sdk = macos_sysroot_sdk_information(sysroot);
	add_arg("-lSystem");
	add_arg("-lm");
	add_arg("-syslibroot");
	add_arg(sysroot);
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
		add_arg(str_printf("%d.%d.0", mac_sdk->macos_min_deploy_target.major, mac_sdk->macos_min_deploy_target.minor));
	}
	if (active_target.macos.sdk_version)
	{
		add_arg(active_target.macos.sdk_version);
	}
	else
	{
		add_arg(str_printf("%d.%d", mac_sdk->macos_deploy_target.major, mac_sdk->macos_deploy_target.minor));
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

static const char *find_linux_crt(void)
{
#if PLATFORM_POSIX
	glob_t globbuf;
	if (!glob("/usr/lib/*/crt1.o", 0, NULL, &globbuf) && globbuf.gl_pathc)
	{
		const char *path = globbuf.gl_pathv[0];
		DEBUG_LOG("Found crt at %s", path);
		size_t len = strlen(path);
		assert(len > 6);
		const char *res = str_copy(path, len - 6);
		globfree(&globbuf);
		return res;
	}
	else
	{
		DEBUG_LOG("No crt in /usr/lib/*/");
	}
#endif
	return NULL;
}

static const char *find_linux_crt_begin(void)
{
#if PLATFORM_POSIX
	glob_t globbuf;
	if (!glob("/usr/lib/gcc/*/*/crtbegin.o", 0, NULL, &globbuf) && globbuf.gl_pathc)
	{
		const char *path = globbuf.gl_pathv[0];
		DEBUG_LOG("Found crtbegin at %s", path);
		size_t len = strlen(path);
		assert(len > 10);
		const char *res = str_copy(path, len - 10);
		globfree(&globbuf);
		return res;
	}
	else
	{
		DEBUG_LOG("No crtbegin in /usr/lib/gcc/*/*/");
	}
#endif
	return NULL;
}

static void linker_setup_linux(const char ***args_ref, LinkerType linker_type)
{
	if (linker_type == LINKER_CC) return;
	if (is_no_pie(platform_target.reloc_model)) add_arg("-no-pie");
	if (is_pie(platform_target.reloc_model)) add_arg("-pie");
	if (platform_target.arch == ARCH_TYPE_X86_64) add_arg("--eh-frame-hdr");
	if (active_target.no_libc) return;
	const char *crt_begin_dir = find_linux_crt_begin();
	const char *crt_dir = find_linux_crt();
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
	add_arg("--dynamic-linker=/lib64/ld-linux-x86-64.so.2");
	add_arg("-lc");
	add_arg("-lm");
	add_arg("-lpthread");
	add_arg("-L/usr/lib/");
	add_arg("-L/lib/");
	add_arg("-m");
	add_arg(ld_target(platform_target.arch));
}

static void linker_setup_freebsd(const char ***args_ref, LinkerType linker_type)
{
	if (linker_type == LINKER_CC) return;
	if (is_no_pie(platform_target.reloc_model)) add_arg("-no-pie");
	if (is_pie(platform_target.reloc_model)) add_arg("-pie");
	if (platform_target.arch == ARCH_TYPE_X86_64) add_arg("--eh-frame-hdr");

	if (active_target.no_libc) return;

	const char *crt_dir = find_freebsd_crt();
	if (!crt_dir)
	{
		error_exit("Failed to find the C runtime at link time.");
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
	add_arg("-lc");
	add_arg("-lm");
	add_arg("-lgcc");
	add_arg("-lgcc_s");
	add_arg("-L/usr/lib/");
	add_arg("-m");
	add_arg(ld_target(platform_target.arch));
}

static bool linker_setup(const char ***args_ref, const char **files_to_link, unsigned file_count,
                         const char *output_file, LinkerType linker_type)
{
	bool use_win = linker_type == LINKER_LINK_EXE;
	if (use_win)
	{
		add_arg2("/OUT:", output_file);
		if (active_target.no_entry) add_arg("/NOENTRY");
	}
	else
	{
		if (active_target.no_entry) add_arg("--no-entry");
		add_arg("-o");
		add_arg(output_file);
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
			if (!active_target.no_libc)
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

	VECEACH(active_target.linker_libdirs, i)
	{
		add_arg2(lib_path_opt, active_target.linker_libdirs[i]);
	}
	VECEACH(active_target.link_args, i)
	{
		add_arg(active_target.link_args[i]);
	}
	VECEACH(active_target.linker_libs, i)
	{
		const char *lib = active_target.linker_libs[i];
		const char *framework = str_remove_suffix(lib, ".framework");
		if (framework)
		{
			add_arg("-framework");
			add_arg(framework);
			continue;
		}
		if (use_win)
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
			add_arg2("-l", lib);
		}
	}
	VECEACH(active_target.library_list, i)
	{
		Library *library = active_target.library_list[i];
		LibraryTarget *target = library->target_used;
		VECEACH(target->link_flags, j)
		{
			add_arg(target->link_flags[j]);
		}
		VECEACH(target->linked_libs, j)
		{
			const char *lib = target->linked_libs[j];
			const char *framework = str_remove_suffix(lib, ".framework");
			if (framework)
			{
				add_arg("-framework");
				add_arg(framework);
				continue;
			}
			if (use_win)
			{
				add_arg2(lib, ".lib");
			}
			else
			{
				add_arg2("-l", lib);
			}
		}
	}
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

LinkerType linker_find_linker_type(void)
{
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
	DEBUG_LOG("Using linker directly.");
	const char **args = NULL;
	LinkerType linker_type = linker_find_linker_type();
	linker_setup(&args, files_to_link, file_count, output_file, linker_type);

	const char *error = NULL;
	// This isn't used in most cases, but its contents should get freed after linking.

	bool success;
	const char *arg_list = "";
	VECEACH(args, i)
	{
		arg_list = str_cat(arg_list, " ");
		arg_list = str_cat(arg_list, args[i]);
	}
	DEBUG_LOG("Linker arguments: %s to %d", arg_list, platform_target.object_format);
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
	DEBUG_LOG("Linking complete.");
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
	VECEACH(args, i)
	{
		size_needed += strlen(args[i]) + 1;
	}
	char *output = malloc_string(size_needed);
	char *ptr = output;
	VECEACH(args, i)
	{
		unsigned len = (unsigned)strlen(args[i]);
		memcpy(ptr, args[i], len);
		ptr += len;
		*(ptr++) = ' ';
	}
	ptr[-1] = '\0';
	return output;
}

void platform_linker(const char *output_file, const char **files, unsigned file_count)
{
	DEBUG_LOG("Using cc linker.");
	const char **parts = NULL;
	vec_add(parts, active_target.cc ? active_target.cc : "cc");
	append_fpie_pic_options(platform_target.reloc_model, &parts);
	linker_setup(&parts, files, file_count, output_file, LINKER_CC);
	if (!active_target.no_libc)
	{
		vec_add(parts, "-lm");
	}
	const char *output = concat_string_parts(parts);
	if (system(output) != 0)
	{
		error_exit("Failed to link executable '%s' using command '%s'.\n", output_file, output);
	}
	printf("Program linked to executable '%s'.\n", output_file);
}

const char *platform_compiler(const char *file, const char *flags)
{
	const char *dir = active_target.object_file_dir;
	if (!dir) dir = active_target.build_dir;

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
			: str_printf("%s%s", filename, get_object_extension());

	const char **parts = NULL;
	vec_add(parts, active_target.cc);

	const bool pie_set =
			flags != NULL &&
			(strstr(flags, "-fno-PIE") || // This is a weird case, but probably don't set PIE if
			 strstr(flags, "-fno-pie") || // it is being set in user defined cflags.
			 strstr(flags, "-fpie") ||
			 strstr(flags, "-fPIE")); // strcasestr is apparently nonstandard >:(
	if (!pie_set)
	{
		append_fpie_pic_options(platform_target.reloc_model, &parts);
	}

	vec_add(parts, "-c");
	if (flags) vec_add(parts, flags);
	vec_add(parts, file);
	vec_add(parts, "-o");
	vec_add(parts, out_name);

	const char *output = concat_string_parts(parts);
	if (system(output) != 0)
	{
		error_exit("Failed to compile c sources using command '%s'.\n", output);
	}
	return out_name;
}

bool dynamic_lib_linker(const char *output_file, const char **files, unsigned file_count)
{
	error_exit("Apologies, dynamic libs are still not supported.");
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
