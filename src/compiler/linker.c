#include "compiler_internal.h"

#include <llvm/Config/llvm-config.h>  // for LLVM_VERSION_STRING

#ifdef PLATFORM_WINDOWS

#include "utils/find_msvc.h"

#endif

extern bool llvm_link_elf(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_macho(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_coff(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_wasm(const char **args, int arg_count, const char **error_string);
extern bool llvm_link_mingw(const char **args, int arg_count, const char **error_string);

static void add_files(const char ***args, const char **files_to_link, unsigned file_count)
{
	for (unsigned i = 0; i < file_count; i++)
	{
		vec_add(*args, files_to_link[i]);
	}
}

static const char *join_strings(const char **args, unsigned count)
{
	char *res = "";
	for (unsigned i = 0; i < count; ++i)
	{
		res = strcat_arena(res, args[i]);
	}
	return res;
}

static void prepare_msys2_linker_flags(const char ***args, const char **files_to_link, unsigned file_count)
{
	const char *root = getenv("MSYSTEM_PREFIX");
#define add_arg(opt) vec_add(*args, opt)
	add_arg("-m");
	add_arg("i386pep");
	add_arg("-Bdynamic");
	add_arg(join_strings((const char *[]){ root, "\\x86_64-w64-mingw32\\lib\\crt2.o" }, 2));
	add_arg(join_strings((const char *[]){ root, "\\x86_64-w64-mingw32\\lib\\crtbegin.o" }, 2));
	add_arg(join_strings((const char *[]){ "-L", root, "\\x86_64-w64-mingw32\\lib" }, 3));
	add_arg(join_strings((const char *[]){ "-L", root, "\\lib" }, 3));
	add_arg(join_strings((const char *[]){ "-L", root, "\\x86_64-w64-mingw32\\sys-root\\mingw\\lib" }, 3));
	add_arg(join_strings((const char *[]){ "-L", root, "\\lib\\clang\\", LLVM_VERSION_STRING, "\\lib\\windows" }, 5));
	add_files(args, files_to_link, file_count);
	add_arg("-lmingw32");
	add_arg(join_strings((const char *[]){ root, "\\lib\\clang\\", LLVM_VERSION_STRING,
	                                       "\\lib\\windows\\libclang_rt.builtins-x86_64.a" }, 4));
	add_arg("-lunwind");
	add_arg("-lmoldname");
	add_arg("-lmingwex");
	add_arg("-lmsvcrt");
	add_arg("-ladvapi32");
	add_arg("-lshell32");
	add_arg("-luser32");
	add_arg("-lkernel32");
	add_arg("-lmingw32");
	add_arg(join_strings((const char *[]){ root, "\\lib\\clang\\", LLVM_VERSION_STRING,
	                                       "\\lib\\windows\\libclang_rt.builtins-x86_64.a" }, 4));
	add_arg("-lunwind");
	add_arg("-lmoldname");
	add_arg("-lmingwex");
	add_arg("-lmsvcrt");
	add_arg("-lkernel32");
	add_arg(join_strings((const char *[]){ root, "\\x86_64-w64-mingw32\\lib\\crtend.o" }, 2));
#undef add_arg
}

static bool link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	const char **args = NULL;
#ifdef _MSC_VER
	if (platform_target.os == OS_TYPE_WIN32)
	{
		vec_add(args, join_strings((const char* []) {"/out:", output_file}, 2));
	}
	else
	{
#endif
	vec_add(args, "-o");
	vec_add(args, output_file);
#ifdef _MSC_VER
	}
#endif
	VECEACH(active_target.link_args, i)
	{
		vec_add(args, active_target.link_args[i]);
	}
	const char *error = NULL;
	// This isn't used in most cases, but its contents should get freed after linking.
	WindowsLinkPathsUTF8 windows_paths = { 0 };

	switch (platform_target.os)
	{
		case OS_TYPE_WIN32:
			// TODO: properly detect if llvm-lld is available
			// TODO: check if running inside MSYS2, it could be done via getting MSYSTEM environment variable
			// https://stackoverflow.com/questions/65527286/how-to-check-if-my-program-is-running-on-mingwor-msys-shell-or-on-cmd
			if (NULL == getenv("MSYSTEM"))
			{
				// "native" windows


				// find paths to library directories.
				// ex:
				// C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\lib\\x64
				// C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\atlmfc\\lib\\x64
				// C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\ucrt\\x64
				// C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\um\\x64
#ifdef _MSC_VER
				windows_paths = get_windows_link_paths();
				vec_add(args, join_strings((const char* []) { "-libpath:", windows_paths.windows_sdk_um_library_path }, 2));
				vec_add(args, join_strings((const char* []) { "-libpath:", windows_paths.windows_sdk_ucrt_library_path }, 2));
				vec_add(args, join_strings((const char* []) { "-libpath:", windows_paths.vs_library_path }, 2));

				vec_add(args, "-defaultlib:libcmt");
				vec_add(args, "-nologo");
				add_files(&args, files_to_link, file_count);
#else
				error_exit("ERROR - c3c must be compiled with MSVC to target x64-windows\n");
#endif
			}
			else
			{
				if (!strcmp(getenv("MSYSTEM"), "CLANG64") || !strcmp(getenv("MSYSTEM"), "MINGW64"))
				{
					prepare_msys2_linker_flags(&args, files_to_link, file_count);
				}
				else
				{
					return false;
				}
			}
			break;
		case OS_TYPE_MACOSX:
			add_files(&args, files_to_link, file_count);
			vec_add(args, "-lSystem");
			vec_add(args, "-lm");
			vec_add(args, "-syslibroot");
			vec_add(args, "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk");
			if (platform_target.pie)
			{
				vec_add(args, "-macosx_version_min");
				vec_add(args, platform_target.arch == ARCH_TYPE_AARCH64 ? "11.0" : "10.8");
				vec_add(args, "-pie");
			}
			else
			{
				vec_add(args, "-no_pie");
			}
			break;
		case OS_TYPE_WATCHOS:
		case OS_TYPE_IOS:
			return false;
		case OS_TYPE_WASI:
			return false;
		case OS_TYPE_OPENBSD:
		case OS_TYPE_NETBSD:
		case OS_TYPE_FREE_BSD:
			return false;
		case OS_TYPE_LINUX:
			vec_add(args, "-m");
			switch (platform_target.arch)
			{
				case ARCH_TYPE_X86_64:
					vec_add(args, "elf_x86_64");
					if (platform_target.pie || platform_target.pic)
					{
						vec_add(args, "--eh-frame-hdr");
						vec_add(args, "/usr/lib/x86_64-linux-gnu/crt1.o");
						vec_add(args, "/usr/lib/gcc/x86_64-linux-gnu/10/crtbeginS.o");
						add_files(&args, files_to_link, file_count);
						vec_add(args, "/usr/lib/x86_64-linux-gnu/crti.o");
						vec_add(args, "/usr/lib/gcc/x86_64-linux-gnu/10/crtendS.o");
						vec_add(args, "-pie");
					}
					else
					{
						vec_add(args, "/usr/lib/x86_64-linux-gnu/Scrt1.o");
						vec_add(args, "/usr/lib/gcc/x86_64-linux-gnu/10/crtbegin.o");
						add_files(&args, files_to_link, file_count);
						vec_add(args, "-lc");
						vec_add(args, "-lm");
						vec_add(args, "/usr/lib/x86_64-linux-gnu/crti.o");
						vec_add(args, "/usr/lib/gcc/x86_64-linux-gnu/10/crtend.o");
						vec_add(args, "-no-pie");
					}
					vec_add(args, "/usr/lib/x86_64-linux-gnu/crtn.o");
					vec_add(args, "-L/usr/lib/x86_64-linux-gnu/");
					vec_add(args, "--dynamic-linker=/lib64/ld-linux-x86-64.so.2");
					break;
				case ARCH_TYPE_X86:
//					vec_add(args, "elf_i386");
					return false;
				case ARCH_TYPE_AARCH64:
					vec_add(args, "aarch64elf");
					return false;
				case ARCH_TYPE_RISCV32:
					vec_add(args, "elf32lriscv");
					return false;
				case ARCH_TYPE_RISCV64:
					vec_add(args, "elf64lriscv");
					return false;
				default:
					UNREACHABLE
			}
			vec_add(args, "-L/usr/lib/");
			vec_add(args, "-L/lib/");
			break;
		default:
			add_files(&args, files_to_link, file_count);
			vec_add(args, platform_target.pie ? "-pie" : "-no_pie");
			return false;
	}

	bool success;
	switch (platform_target.object_format)
	{
		case OBJ_FORMAT_COFF:
			if (platform_target.x64.is_mingw64)
			{
				success = llvm_link_mingw(args, (int)vec_size(args), &error);
			}
			else
			{
				success = llvm_link_coff(args, (int)vec_size(args), &error);
			}
			// This is only defined if compiling with MSVC
#ifdef _MSC_VER
			if (windows_paths.windows_sdk_um_library_path) {
				free_windows_link_paths(&windows_paths);
			}
#endif
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
	char *output = malloc_arena(size_needed);
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
	const char **parts = NULL;
	vec_add(parts, active_target.cc ? active_target.cc : "cc");
	VECEACH(active_target.link_args, i)
	{
		vec_add(parts, active_target.link_args[i]);
	}
	switch (platform_target.pie)
	{
		case PIE_DEFAULT:
			UNREACHABLE
		case PIE_NONE:
			vec_add(parts, "-fno-PIE");
			vec_add(parts, "-fno-pie");
			break;
		case PIE_SMALL:
			vec_add(parts, "-fpie");
			break;
		case PIE_BIG:
			vec_add(parts, "-fPIE");
			break;
	}
	vec_add(parts, "-o");
	vec_add(parts, output_file);
	for (unsigned i = 0; i < file_count; i++)
	{
		vec_add(parts, files[i]);
	}
	vec_add(parts, "-lm");
	const char *output = concat_string_parts(parts);
	if (system(output) != 0)
	{
		error_exit("Failed to link executable '%s' using command '%s'.\n", output_file, output);
	}
	printf("Program linked to executable '%s'.\n", output_file);
}

void platform_compiler(const char **files, unsigned file_count, const char *flags)
{
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
		switch (platform_target.pie)
		{
			case PIE_DEFAULT:
				UNREACHABLE
			case PIE_NONE:
				vec_add(parts, "-fno-PIE");
				vec_add(parts, "-fno-pie");
				break;
			case PIE_SMALL:
				vec_add(parts, "-fpie");
				break;
			case PIE_BIG:
				vec_add(parts, "-fPIE");
				break;
		}
	}

	vec_add(parts, "-c");
	if (flags) vec_add(parts, flags);
	for (unsigned i = 0; i < file_count; i++)
	{
		vec_add(parts, files[i]);
	}
	const char *output = concat_string_parts(parts);
	if (system(output) != 0)
	{
		error_exit("Failed to compile c sources using command '%s'.\n", output);
	}
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
