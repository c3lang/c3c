// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <stdio.h>
#include <stdlib.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include "project_creation.h"
#include "build_options.h"
#include "../utils/lib.h"

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
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"../build\",\n"
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
		"  // C compiler if the project also compiles C sources\n"
		"  // defaults to 'cc'.\n"
		"  \"cc\": \"cc\",\n"
		"  // CPU name, used for optimizations in the LLVM backend.\n"
		"  \"cpu\": \"generic\",\n"
		"  // Debug information, may be \"none\", \"full\" and \"line-tables\".\n"
		"  \"debug-info\": \"full\",\n"
		"  // FP math behaviour: \"strict\", \"relaxed\", \"fast\".\n"
		"  \"fp-math\": \"strict\",\n"
		"  // Link libc other default libraries.\n"
		"  \"link-libc\": true,\n"
		"  // Memory environment: \"normal\", \"small\", \"tiny\", \"none\".\n"
		"  \"memory-env\": \"normal\",\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\",\n"
		"  // Code optimization level: \"none\", \"less\", \"more\", \"max\".\n"
		"  \"optlevel\": \"none\",\n"
		"  // Code size optimization: \"none\", \"small\", \"tiny\".\n"
		"  \"optsize\": \"none\",\n"
		"  // Relocation model: \"none\", \"pic\", \"PIC\", \"pie\", \"PIE\".\n"
		"  \"reloc\": \"none\",\n"
		"  // Trap on signed and unsigned integer wrapping for testing.\n"
		"  \"trap-on-wrap\": false,\n"
		"  // Turn safety (contracts, runtime bounds checking, null pointer checks etc).\n"
		"  \"safe\": true,\n"
		"  // Compile all modules together, enables more inlining.\n"
		"  \"single-module\": true,\n"
		"  // Use / don't use soft float, value is otherwise target default.\n"
		"  \"soft-float\": false,\n"
		"  // Strip unused code and globals from the output.\n"
		"  \"strip-unused\": true,\n"
		"  // The size of the symtab, which limits the amount\n"
		"  // of symbols that can be used. Should usually not be changed.\n"
		"  \"symtab\": 1048576,\n"
		"  // Use the system linker.\n"
		"  \"system-linker\": false,\n"
		"  // Include the standard library.\n"
		"  \"use-stdlib\": true,\n"
		"  // Set general level of x64 cpu: \"baseline\", \"ssse3\", \"sse4\", \"avx1\", \"avx2-v1\", \"avx2-v2\", \"avx512\", \"native\".\n"
		"  \"x86cpu\": \"native\",\n"
		"  // Set max type of vector use: \"none\", \"mmx\", \"sse\", \"avx\", \"avx512\", \"native\".\n"
		"  \"x86vec\": \"sse\",\n"
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
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"../build\",\n"
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
		"  // C compiler if the project also compiles C sources\n"
		"  // defaults to 'cc'.\n"
		"  \"cc\": \"cc\",\n"
		"  // CPU name, used for optimizations in the LLVM backend.\n"
		"  \"cpu\": \"generic\",\n"
		"  // Debug information, may be \"none\", \"full\" and \"line-tables\".\n"
		"  \"debug-info\": \"full\",\n"
		"  // FP math behaviour: \"strict\", \"relaxed\", \"fast\".\n"
		"  \"fp-math\": \"strict\",\n"
		"  // Link libc other default libraries.\n"
		"  \"link-libc\": true,\n"
		"  // Memory environment: \"normal\", \"small\", \"tiny\", \"none\".\n"
		"  \"memory-env\": \"normal\",\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\",\n"
		"  // Code optimization level: \"none\", \"less\", \"more\", \"max\".\n"
		"  \"optlevel\": \"none\",\n"
		"  // Code size optimization: \"none\", \"small\", \"tiny\".\n"
		"  \"optsize\": \"none\",\n"
		"  // Relocation model: \"none\", \"pic\", \"PIC\", \"pie\", \"PIE\".\n"
		"  \"reloc\": \"none\",\n"
		"  // Trap on signed and unsigned integer wrapping for testing.\n"
		"  \"trap-on-wrap\": false,\n"
		"  // Turn safety (contracts, runtime bounds checking, null pointer checks etc).\n"
		"  \"safe\": true,\n"
		"  // Compile all modules together, enables more inlining.\n"
		"  \"single-module\": true,\n"
		"  // Use / don't use soft float, value is otherwise target default.\n"
		"  \"soft-float\": false,\n"
		"  // Strip unused code and globals from the output.\n"
		"  \"strip-unused\": true,\n"
		"  // The size of the symtab, which limits the amount\n"
		"  // of symbols that can be used. Should usually not be changed.\n"
		"  \"symtab\": 1048576,\n"
		"  // Use the system linker.\n"
		"  \"system-linker\": false,\n"
		"  // Include the standard library.\n"
		"  \"use-stdlib\": true,\n"
		"  // Set general level of x64 cpu: \"baseline\", \"ssse3\", \"sse4\", \"avx1\", \"avx2-v1\", \"avx2-v2\", \"avx512\", \"native\".\n"
		"  \"x86cpu\": \"native\",\n"
		"  // Set max type of vector use: \"none\", \"mmx\", \"sse\", \"avx\", \"avx512\", \"native\".\n"
		"  \"x86vec\": \"sse\",\n"
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
		"  // C sources if the project also compiles C sources\n"
		"  // relative to the project file.\n"
		"  // \"c-sources\": [ \"csource/**\" ],\n"
		"  // Output location, relative to project file.\n"
		"  \"output\": \"../build\",\n"
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
		"  // C compiler if the project also compiles C sources\n"
		"  // defaults to 'cc'.\n"
		"  \"cc\": \"cc\",\n"
		"  // CPU name, used for optimizations in the LLVM backend.\n"
		"  \"cpu\": \"generic\",\n"
		"  // Debug information, may be \"none\", \"full\" and \"line-tables\".\n"
		"  \"debug-info\": \"full\",\n"
		"  // FP math behaviour: \"strict\", \"relaxed\", \"fast\".\n"
		"  \"fp-math\": \"strict\",\n"
		"  // Link libc other default libraries.\n"
		"  \"link-libc\": true,\n"
		"  // Memory environment: \"normal\", \"small\", \"tiny\", \"none\".\n"
		"  \"memory-env\": \"normal\",\n"
		"  // Optimization: \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", \"Oz\".\n"
		"  \"opt\": \"O0\",\n"
		"  // Code optimization level: \"none\", \"less\", \"more\", \"max\".\n"
		"  \"optlevel\": \"none\",\n"
		"  // Code size optimization: \"none\", \"small\", \"tiny\".\n"
		"  \"optsize\": \"none\",\n"
		"  // Relocation model: \"none\", \"pic\", \"PIC\", \"pie\", \"PIE\".\n"
		"  \"reloc\": \"none\",\n"
		"  // Trap on signed and unsigned integer wrapping for testing.\n"
		"  \"trap-on-wrap\": false,\n"
		"  // Turn safety (contracts, runtime bounds checking, null pointer checks etc).\n"
		"  \"safe\": true,\n"
		"  // Compile all modules together, enables more inlining.\n"
		"  \"single-module\": true,\n"
		"  // Use / don't use soft float, value is otherwise target default.\n"
		"  \"soft-float\": false,\n"
		"  // Strip unused code and globals from the output.\n"
		"  \"strip-unused\": true,\n"
		"  // The size of the symtab, which limits the amount\n"
		"  // of symbols that can be used. Should usually not be changed.\n"
		"  \"symtab\": 1048576,\n"
		"  // Use the system linker.\n"
		"  \"system-linker\": false,\n"
		"  // Include the standard library.\n"
		"  \"use-stdlib\": true,\n"
		"  // Set general level of x64 cpu: \"baseline\", \"ssse3\", \"sse4\", \"avx1\", \"avx2-v1\", \"avx2-v2\", \"avx512\", \"native\".\n"
		"  \"x86cpu\": \"native\",\n"
		"  // Set max type of vector use: \"none\", \"mmx\", \"sse\", \"avx\", \"avx512\", \"native\".\n"
		"  \"x86vec\": \"sse\",\n"
		"}";

const char* MAIN_TEMPLATE =
		"module %s;\n"
		"import std::io;\n"
		"\n"
		"fn int main(String[] args)\n"
		"{\n"
		"\tio::printn(\"Hello, World!\");\n"
		"\treturn 0;\n"
		"}\n";

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
	for (int i = 0; ; i++)
	{
		char c = build_options->project_name[i];
		if (c == '\0') break;
		if (!char_is_alphanum_(c))
		{
			fprintf(stderr, "'%s' is not a valid project name.\n", build_options->project_name);
			exit_compiler(EXIT_FAILURE);
		}
	}

	if (!dir_change(build_options->path))
	{
		fprintf(stderr, "Can't open path %s\n", build_options->path);
		exit_compiler(EXIT_FAILURE);
	}

	if (!dir_make(build_options->project_name))
	{
		fprintf(stderr, "Could not create directory %s.\n", build_options->project_name);
		exit_compiler(EXIT_FAILURE);
	}

	if (!dir_change(build_options->project_name)) goto ERROR;

	if (!file_touch("LICENSE")) goto ERROR;

	if (!file_touch("README.md")) goto ERROR;


	FILE *file = fopen("project.json", "a");
	if (!file) goto ERROR;
	(void) fprintf(file, template, build_options->project_name);
	if (fclose(file)) goto ERROR;

	if (!dir_make("build")) goto ERROR;

	if (!dir_make("docs")) goto ERROR;

	if (!dir_make("lib")) goto ERROR;

	if (!dir_make("resources")) goto ERROR;

	if (!dir_make("src")) goto ERROR;

	if (!dir_change("src")) goto ERROR;

	file = fopen("main.c3", "w");
	if (!file) goto ERROR;
	(void) fprintf(file, MAIN_TEMPLATE, build_options->project_name);
	if (fclose(file)) goto ERROR;

	if (!dir_change("..")) goto ERROR;

	if (!dir_make("test")) goto ERROR;

	(void) printf("Project '%s' created.\n", build_options->project_name);
	exit_compiler(COMPILER_SUCCESS_EXIT);

ERROR:
	fprintf(stderr, "Error creating the project.\n");
	if (dir_change(build_options->path))
	{
		(void)rmdir(build_options->project_name);
	}
	exit_compiler(EXIT_FAILURE);
}

