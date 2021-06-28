#include "compiler_internal.h"

extern bool llvm_link_elf(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_macho(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_coff(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_wasm(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_mingw(const char **args, int arg_count, const char** error_string);

static void add_files(const char ***args, const char **files_to_link, unsigned file_count)
{
	for (unsigned i = 0; i < file_count; i++)
	{
		vec_add(*args, files_to_link[i]);
	}
}

static bool link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	const char **args = NULL;
	vec_add(args, "-o");
	vec_add(args, output_file);
	const char *error = NULL;

	switch (platform_target.os)
	{
		case OS_TYPE_WIN32:
			return false;
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
		unsigned len = strlen(args[i]);
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
	vec_add(parts, "cc");
	vec_add(parts, "-lm");
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
	const char *output = concat_string_parts(parts);
	if (system(output) != 0)
	{
		error_exit("Failed to link executable '%s' using command '%s'.\n", output_file, output);
	}
	printf("Program linked to executable '%s'.\n", output_file);
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
