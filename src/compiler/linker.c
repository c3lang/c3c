#include "compiler_internal.h"

extern bool llvm_link_elf(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_macho(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_coff(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_wasm(const char **args, int arg_count, const char** error_string);
extern bool llvm_link_mingw(const char **args, int arg_count, const char** error_string);

static void link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	int arg_count = (int)file_count + 2;
	const char **args = NULL;
	vec_add(args, "-o");
	vec_add(args, output_file);
	vec_add(args, "-m");
	vec_add(args, platform_target.target_triple);
	for (unsigned i = 0; i < file_count; i++)
	{
		args[i + 2] = files_to_link[i];
	}
	const char *error = NULL;

	switch (platform_target.os)
	{
		case OS_TYPE_WIN32:
			break;
		case OS_TYPE_WATCHOS:
		case OS_TYPE_MACOSX:
			vec_add(args, "-lSystem");
			vec_add(args, "-lm");
			vec_add(args, "-syslibroot");
			vec_add(args, "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk");
			vec_add(args, platform_target.pie ? "-pie" : "-no_pie");
			break;
		case OS_TYPE_LINUX:
			vec_add(args, "-L/usr/lib/");
			vec_add(args, "-L/lib/");
			vec_add(args, "-lc");
			vec_add(args, "-lm");
			vec_add(args, platform_target.pie ? "-pie" : "-no-pie");
			break;
		default:
			break;
	}
	vec_add(args, platform_target.pie ? "-pie" : "-no_pie");

	bool success;
	switch (platform_target.object_format)
	{
		case OBJ_FORMAT_COFF:
			success = llvm_link_coff(args, arg_count, &error);
			break;
		case OBJ_FORMAT_ELF:
			success = llvm_link_elf(args, arg_count, &error);
			break;
		case OBJ_FORMAT_MACHO:
			success = llvm_link_macho(args, arg_count, &error);
			break;
		case OBJ_FORMAT_WASM:
			success = llvm_link_wasm(args, arg_count, &error);
			break;
		default:
			UNREACHABLE
	}
	if (!success)
	{
		error_exit("Failed to create an executable: %s", error);
	}
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
void linker(const char *output_file, const char **files, unsigned file_count)
{
	link_exe(output_file, files, file_count);
}
