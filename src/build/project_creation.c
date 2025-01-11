// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "build_internal.h"

const char* JSON_EXE =
		"{\n"
		"  // Language version of C3.\n"
		"  \"langrev\": \"1\",\n"
		"  // Warnings used for all targets.\n"
		"  \"warnings\": [ \"no-unused\" ],\n"
		"  // Directories where C3 library files may be found.\n"
		"  \"dependency-search-paths\": [ \"lib\" ],\n"
		"  // Libraries to use for all targets.\n"
		"  \"dependencies\": [ ],\n"
		"  // Authors, optionally with email.\n"
		"  \"authors\": [ \"John Doe <john.doe@example.com>\" ],\n"
		"  // Version using semantic versioning.\n"
		"  \"version\": \"0.1.0\",\n"
		"  // Sources compiled for all targets.\n"
		"  \"sources\": [ \"src/**\" ],\n"
		"  // Test sources compiled for all targets.\n"
		"  \"test-sources\": [ \"test/**\" ],\n"
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Include directories for C sources relative to the project file.\n"
		"  // \"c-include-dirs\": [ \"csource/include\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"build\",\n"
		"  // Architecture and OS target.\n"
		"  // You can use 'c3c --list-targets' to list all valid targets.\n"
		"  // \"target\": \"windows-x64\",\n"
		"  // Targets.\n"
		"  \"targets\": {\n"
		"    \"%s\": {\n"
		"      // Executable or library.\n"
		"      \"type\": \"executable\",\n"
		"      // Additional libraries, sources\n"
		"      // and overrides of global settings here.\n"
		"    },\n"
		"  },\n"
		"  // Global settings.\n"
		"  // CPU name, used for optimizations in the LLVM backend.\n"
		"  \"cpu\": \"generic\",\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\"\n"
		"  // See resources/examples/project_all_settings.json and 'c3c --list-project-properties' to see more properties.\n"
		"}";

const char* JSON_STATIC =
		"{\n"
		"  // Language version of C3.\n"
		"  \"langrev\": \"1\",\n"
		"  // Warnings used for all targets.\n"
		"  \"warnings\": [ \"no-unused\" ],\n"
		"  // Directories where C3 library files may be found.\n"
		"  \"dependency-search-paths\": [ \"lib\" ],\n"
		"  // Libraries to use for all targets.\n"
		"  \"dependencies\": [ ],\n"
		"  // Authors, optionally with email.\n"
		"  \"authors\": [ \"John Doe <john.doe@example.com>\" ],\n"
		"  // Version using semantic versioning.\n"
		"  \"version\": \"0.1.0\",\n"
		"  // Sources compiled for all targets.\n"
		"  \"sources\": [ \"src/**\" ],\n"
		"  // Test sources compiled for all targets.\n"
		"  \"test-sources\": [ \"test/**\" ],\n"
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Include directories for C sources relative to the project file.\n"
		"  // \"c-include-dirs\": [ \"csource/include\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"build\",\n"
		"  // Architecture and OS target.\n"
		"  // You can use 'c3c --list-targets' to list all valid targets.\n"
		"  // \"target\": \"windows-x64\",\n"
		"  // Targets.\n"
		"  \"targets\": {\n"
		"    \"%s\": {\n"
		"      // Executable or library.\n"
		"      \"type\": \"static-lib\",\n"
		"      // Additional libraries, sources\n"
		"      // and overrides of global settings here.\n"
		"    },\n"
		"  },\n"
		"  // Global settings.\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\"\n"
		"  // See resources/examples/project_all_settings.json and 'c3c --list-project-properties' to see more properties.\n"
		"}";

const char* JSON_DYNAMIC =
		"{\n"
		"  // Language version of C3.\n"
		"  \"langrev\": \"1\",\n"
		"  // Warnings used for all targets.\n"
		"  \"warnings\": [ \"no-unused\" ],\n"
		"  // Directories where C3 library files may be found.\n"
		"  \"dependency-search-paths\": [ \"lib\" ],\n"
		"  // Libraries to use for all targets.\n"
		"  \"dependencies\": [ ],\n"
		"  // Authors, optionally with email.\n"
		"  \"authors\": [ \"John Doe <john.doe@example.com>\" ],\n"
		"  // Version using semantic versioning.\n"
		"  \"version\": \"0.1.0\",\n"
		"  // Sources compiled for all targets.\n"
		"  \"sources\": [ \"src/**\" ],\n"
		"  // Test sources compiled for all targets.\n"
		"  \"test-sources\": [ \"test/**\" ],\n"
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Include directories for C sources relative to the project file.\n"
		"  // \"c-include-dirs\": [ \"csource/include\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"build\",\n"
		"  // Architecture and OS target.\n"
		"  // You can use 'c3c --list-targets' to list all valid targets.\n"
		"  // \"target\": \"windows-x64\",\n"
		"  // Targets.\n"
		"  \"targets\": {\n"
		"    \"%s\": {\n"
		"      // Executable or library.\n"
		"      \"type\": \"dynamic-lib\",\n"
		"      // Additional libraries, sources\n"
		"      // and overrides of global settings here.\n"
		"    },\n"
		"  },\n"
		"  // Global settings.\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\"\n"
		"  // See resources/examples/project_all_settings.json and 'c3c --list-project-properties' to see more properties.\n"
		"}";

const char *MANIFEST_TEMPLATE =
		"{\n"
		"  \"provides\" : \"%s\",\n"
		"  // \"sources\" : [ \"src/**\" ],\n"
		"  \"targets\" : {\n"
		"%s"
		"  }\n"
		"}";

const char *MANIFEST_TARGET =
		"    \"%s\" : {\n"
		"      // Extra flags to the linker for this target:\n"
		"      \"link-args\" : [],\n"
		"      // C3 libraries this target depends on:\n"
		"      \"dependencies\" : [],\n"
		"      // The external libraries to link for this target:\n"
		"      \"linked-libraries\" : []\n"
		"    },\n";

const char *MAIN_TEMPLATE =
		"module %s;\n"
		"import std::io;\n"
		"\n"
		"fn int main(String[] args)\n"
		"{\n"
		"\tio::printn(\"Hello, World!\");\n"
		"\treturn 0;\n"
		"}\n";

const char* MAIN_INTERFACE_TEMPLATE =
		"module %s;\n"
		"\n"
		"// extern fn int some_library_function();\n";

const char* DEFAULT_TARGETS[] = {
		"freebsd-x64",
		"linux-aarch64",
		"linux-riscv32",
		"linux-riscv64",
		"linux-x86",
		"linux-x64",
		"macos-aarch64",
		"macos-x64",
		"netbsd-x64",
		"openbsd-x64",
		"wasm32",
		"wasm64",
		"windows-aarch64",
		"windows-x64"
};

const char *LIB_README = "Welcome to the %s library.\n";

static bool check_name(const char *name);
static char* get_cwd_project_name();
static void exit_fail(const char *fmt, ...);
static void delete_dir_and_exit(BuildOptions *build_options, const char *fmt, ...);
static void mkdir_or_fail(BuildOptions *build_options, const char *name);
static void chdir_or_fail(BuildOptions *build_options, const char *name);
static void create_file_or_fail(BuildOptions *build_options, const char *filename, const char *fmt, ...);
static const char *module_name(BuildOptions *build_options);

void create_library(BuildOptions *build_options)
{
	if (str_has_suffix(build_options->project_name, ".c3l"))
	{
		exit_fail("Please remove the '.c3l' suffix from the project name.");
	}

	if (!check_name(build_options->project_name))
	{
		exit_fail("'%s' is not a valid library name.", build_options->project_name);
	}
	if (!dir_change(build_options->path))
	{
		exit_fail("Can't open path %s", build_options->path);
	}

	const char *dir = str_cat(build_options->project_name, ".c3l");
	if (!dir_make(dir))
	{
		exit_fail("Could not create directory %s.", dir);
	}

	chdir_or_fail(build_options, dir);

	create_file_or_fail(build_options, "LICENSE", NULL);
	create_file_or_fail(build_options, "README.md", LIB_README, build_options->project_name);
	mkdir_or_fail(build_options, "scripts");

	scratch_buffer_clear();
	scratch_buffer_printf("%s.c3i", build_options->project_name);
	const char *interface_file = scratch_buffer_copy();
	create_file_or_fail(build_options, interface_file, MAIN_INTERFACE_TEMPLATE, module_name(build_options));
	scratch_buffer_clear();
	for (int i = 0; i < sizeof(DEFAULT_TARGETS) / sizeof(char*); i++)
	{
		const char *target = DEFAULT_TARGETS[i];
		scratch_buffer_printf(MANIFEST_TARGET, target);
		mkdir_or_fail(build_options, target);
	}
	create_file_or_fail(build_options, "manifest.json", MANIFEST_TEMPLATE, build_options->project_name, scratch_buffer_to_string());
	printf("The '%s' library has been set up in the directory '%s'.\n", build_options->project_name, dir);
}

void create_project(BuildOptions *build_options)
{
	const char *template;
	if (!build_options->template || strcmp(build_options->template, "exe") == 0)
	{
		template = JSON_EXE;
	}
	else if (strcmp(build_options->template, "static-lib") == 0)
	{
		template = JSON_STATIC;
	}
	else if (strcmp(build_options->template, "dynamic-lib") == 0)
	{
		template = JSON_DYNAMIC;
	}
	else
	{
		size_t len;
		template = file_read_all(build_options->template, &len);
	}

	// Special case, a '.' is given
	if (str_eq(build_options->project_name, "."))
	{
		build_options->project_name = get_cwd_project_name();
		if (!check_name(build_options->project_name))
		{
			error_exit("The parent directory (which is '%s') is not a valid project name.", build_options->project_name);
		}
		goto CREATE;
	}

	if (!check_name(build_options->project_name))
	{
		error_exit("'%s' is not a valid project name.", build_options->project_name);
	}

	if (!dir_change(build_options->path))
	{
		error_exit("Can't open path '%s'.", build_options->path);
	}

	if (!dir_make(build_options->project_name))
	{
		error_exit("Could not create directory '%s'.", build_options->project_name);
	}
	chdir_or_fail(build_options, build_options->project_name);

CREATE:
	create_file_or_fail(build_options, "LICENSE", NULL);
	create_file_or_fail(build_options, "README.md", NULL);
	create_file_or_fail(build_options, "project.json", template, build_options->project_name);
	mkdir_or_fail(build_options, "build");
	mkdir_or_fail(build_options, "docs");
	mkdir_or_fail(build_options, "lib");
	mkdir_or_fail(build_options, "resources");
	mkdir_or_fail(build_options, "scripts");
	mkdir_or_fail(build_options, "src");
	chdir_or_fail(build_options, "src");

	create_file_or_fail(build_options, "main.c3", MAIN_TEMPLATE, module_name(build_options));
	chdir_or_fail(build_options, "..");
	mkdir_or_fail(build_options, "test");

	(void) printf("Project '%s' created.\n", build_options->project_name);
	exit_compiler(COMPILER_SUCCESS_EXIT);
}

// Helper functions:

static const char *module_name(BuildOptions *build_options)
{
	scratch_buffer_clear();
	size_t len = strlen(build_options->project_name);
	bool has_char = false;
	bool appended_underscore = false;
	for (size_t i = 0; i < len; i++)
	{
		char c = build_options->project_name[i];
		if (c >= '0' && c <= '9')
		{
			if (!has_char) scratch_buffer_append("m_");
			scratch_buffer_append_char(c);
			has_char = true;
			appended_underscore = false;
			continue;
		}
		if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
		{
			scratch_buffer_append_char(c | 0x20);
			has_char = true;
			appended_underscore = false;
			continue;
		}
		if (!appended_underscore)
		{
			scratch_buffer_append_char('_');
			appended_underscore = true;
		}
	}
	if (!has_char) scratch_buffer_append("module");
	return scratch_buffer_to_string();
}

static void create_file_or_fail(BuildOptions *build_options, const char *filename, const char *fmt, ...)
{
	if (!fmt)
	{
		if (!file_touch(filename))
		{
			delete_dir_and_exit(build_options, "Could not create '%s' file.", filename);
		}
		return;
	}
	FILE *file = fopen(filename, "a");
	if (!file)
	{
		delete_dir_and_exit(build_options, "Couldn't create '%s' file.", filename);
	}
	va_list list;
	va_start(list, fmt);
	(void) vfprintf(file, fmt, list);
	va_end(list);
	if (fclose(file))
	{
		delete_dir_and_exit(build_options, "Couldn't close the '%s' file.", filename);
	}
}

static bool check_name(const char *name)
{
	for (int i = 0; ; i++)
	{
		char c = name[i];
		if (c == '\0') break;
		if (!char_is_alphanum_(c)) return false;
	}
	return true;
}

static char* get_cwd_project_name()
{
	char *full_path = getcwd(NULL, 0);
	size_t len = strlen(full_path);
	for (size_t i = len; i > 0; i--)
	{
		switch (full_path[i])
		{
			case '/':
#if PLATFORM_WINDOWS
			case '\\':
#endif
			return &full_path[i + 1];
		}
	}
	return full_path;
}

static void chdir_or_fail(BuildOptions *build_options, const char *name)
{
	if (!dir_change(name))
	{
		delete_dir_and_exit(build_options, "Failed to open directory '%s'.", name);
	}
}

static void exit_fail(const char *fmt, ...)
{
	va_list list;
	va_start(list, fmt);
	vfprintf(stderr, fmt, list);
	va_end(list);
	fputs("", stderr);
	exit_compiler(EXIT_FAILURE);
}

static void delete_dir_and_exit(BuildOptions *build_options, const char *fmt, ...)
{
	va_list list;
	va_start(list, fmt);
	if (dir_change(build_options->path))
	{
		(void)rmdir(build_options->project_name);
	}
	vfprintf(stderr, fmt, list);
	va_end(list);
	fputs("", stderr);
	exit_compiler(EXIT_FAILURE);
}

static void mkdir_or_fail(BuildOptions *build_options, const char *name)
{
	if (!dir_make(name))
	{
		delete_dir_and_exit(build_options, "Failed to create directory '%s'.", name);
	}
	const char *path = file_append_path_temp(name, ".gitkeep");
	file_touch(path);
}
