// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <string.h>
#include "project_creation.h"
#include "build_options.h"
#include "../utils/lib.h"

const char* JSON =
		"{\n"
		"  // language version of C3\n"
		"  \"langrev\": \"1\",\n"
		"  // warnings used for all targets\n"
		"  \"warnings\": [ \"no-unused\" ],\n"
		"  // libraries to use for all targets\n"
		"  \"libs\": [ \"lib/**\" ],\n"
		"  // authors, optionally with email\n"
		"  \"authors\": [ \"John Doe <john.doe@example.com>\" ],\n"
		"  // Version using semantic versioning\n"
		"  \"version\": \"0.1.0\",\n"
		"  // sources compiled for all targets\n"
		"  \"sources\": [ \"src/**\" ],\n"
		"  // Targets\n"
		"  \"targets\": {\n"
		"    \"%s\": {\n"
		"      // executable or library\n"
		"      \"type\": \"executable\"\n"
		"      // additional libraries, sources\n"
		"      // and overrides of global settings here\n"
		"    },\n"
		"  }\n"
		"  /*\n"
		"  // Debug information, may be 'none', 'full' and 'line-tables'\n"
		"  \"debug-info\": \"full\",\n"
		"  // Architecture and OS target:\n"
		"  \"target\": \"x64-windows\",\n"
		"  // The size of the symtab, which limits the amount\n"
		"  // of symbols that can be used. Should usually not\n"
		"  // be changed.\n"
		"  \"symtab\": 4194304,\n"
		"  // \"none\", \"pic\", \"PIC\", \"pie\", \"PIE\"\n"
		"  \"reloc\": \"none\",\n"
		"  // Trap on signed and unsigned integer wrapping\n"
		"  // for testing\n"
		"  \"trap-on-wrap\": false,\n"
		"  // Use / don't use soft float, value is otherwise target default\n"
		"  \"soft-float\": false,\n"
		"  // Vector settings on x86: none/mmx/sse/avx/avx512\n"
		"  \"x86vec\": \"sse\",\n"
		"  // CPU name, used for optimizations in the LLVM backend\n"
		"  \"cpu\": \"generic\",\n"
		"  // Output location, relative to project file\n"
		"  \"output\": \"../build\",\n"
		"  // C compiler if the project also compiles c sources\n"
		"  // defaults to 'cc'\n"
		"  \"cc\": \"cc\",\n"
		"  // c sources if the project also compiles c sources\n"
		"  // relative to the project file\n"
		"  \"csources\": [\n"
		"    \"csource/**\"\n"
		"  ]\n"
		"  */\n"
		"}";

void create_project(BuildOptions *build_options)
{
	for (int i = 0; ; i++)
	{
		char c = build_options->project_name[i];
		if (c == '\0') break;
		if (!is_alphanum_(c))
		{
			fprintf(stderr, "'%s' is not a valid project name.\n", build_options->project_name);
			exit_compiler(EXIT_FAILURE);
		}
	}

	if (chdir(build_options->path))
	{
		fprintf(stderr, "Can't open path %s\n", build_options->path);
		exit_compiler(EXIT_FAILURE);
	}

	int error = mkdir(build_options->project_name, 0755);
	if (error)
	{
		fprintf(stderr, "Could not create directory %s: %s\n", build_options->project_name, strerror(errno));
		exit_compiler(EXIT_FAILURE);
	}

	if (chdir(build_options->project_name)) goto ERROR;

	FILE *file = fopen("LICENCE", "a");
	if (!file) goto ERROR;
	if (fclose(file)) goto ERROR;

	file = fopen("README.md", "a");
	if (!file) goto ERROR;
	if (fclose(file)) goto ERROR;

	file = fopen("project.c3p", "a");
	if (!file) goto ERROR;
	(void) fprintf(file, JSON, build_options->project_name);
	if (fclose(file)) goto ERROR;

	if (mkdir("lib", 0755)) goto ERROR;

	if (mkdir("build", 0755)) goto ERROR;

	if (mkdir("resources", 0755)) goto ERROR;

	if (mkdir("test", 0755)) goto ERROR;

	if (chdir("test")) goto ERROR;

	if (mkdir(build_options->project_name, 0755)) goto ERROR;

	if (chdir("..")) goto ERROR;

	if (mkdir("directives", 0755)) goto ERROR;

	if (chdir("directives") == -1) goto ERROR;

	file = fopen("about.md", "a");
	if (!file) goto ERROR;
	if (fclose(file)) goto ERROR;

	if (mkdir("src", 0755)) goto ERROR;

	if (chdir("src")) goto ERROR;

	file = fopen("index.html", "a");
	if (!file) goto ERROR;
	if (fclose(file)) goto ERROR;

	if (chdir("../..")) goto ERROR;

	if (mkdir("src", 0755)) goto ERROR;

	if (chdir("src")) goto ERROR;

	if (mkdir(build_options->project_name, 0755)) goto ERROR;

	if (chdir(build_options->project_name)) goto ERROR;

	file = fopen("main.c3", "a");
	if (!file) goto ERROR;
	if (fclose(file)) goto ERROR;

	if (chdir("../..")) goto ERROR;

	(void) printf("Project '%s' created.\n", build_options->project_name);
	exit_compiler(COMPILER_SUCCESS_EXIT);

ERROR:
	fprintf(stderr, "Err: %s\n", strerror(errno));

	printf("Something went wrong creating the project.\n");
	if (!chdir(build_options->path))
	{
		(void)rmdir(build_options->project_name);
	}
	exit_compiler(EXIT_FAILURE);
}
