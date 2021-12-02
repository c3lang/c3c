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

const char* TOML =
	"[[executable]]\n"
    "# name of the target\n"
	"name = \"%s\"\n"
    "# version using semantic versioning\n"
	"version = \"0.1.0\"\n"
    "# authors, optionally with email\n"
	"authors = [\"John Doe <john.doe@example.com>\"]\n"
    "# language version of C3\n"
	"langrev = \"1\"\n"
    "# warnings used\n"
	"warnings = [\"no-unused\"]\n"
    "# sources compiled\n"
	"sources = [\"src/**\"]\n"
    "# libraries to use\n"
	"libs = [\"lib/**\"]\n";

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

	file = fopen("project.toml", "a");
	if (!file) goto ERROR;
	(void) fprintf(file, TOML, build_options->project_name);
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
	exit_compiler(EXIT_SUCCESS);

ERROR:
	fprintf(stderr, "Err: %s\n", strerror(errno));

	printf("Something went wrong creating the project.\n");
	if (!chdir(build_options->path))
	{
		(void)rmdir(build_options->project_name);
	}
	exit_compiler(EXIT_FAILURE);
}
