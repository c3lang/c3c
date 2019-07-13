// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "project_creation.h"
#include "build_options.h"
#include "string.h"
#include "../utils/string_utils.h"

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

void create_project(void)
{
	for (int i = 0; ; i++)
	{
		char c = build_options.project_name[i];
		if (c == '\0') break;
		if (!is_alphanum_(c))
		{
			fprintf(stderr, "'%s' is not a valid project name.\n", build_options.project_name);
			exit(EXIT_FAILURE);
		}
	}

	if (chdir(build_options.path))
	{
		fprintf(stderr, "Can't open path %s\n", build_options.path);
		exit(EXIT_FAILURE);
	}

	int error = mkdir(build_options.project_name, 0755);
	if (error)
	{
		fprintf(stderr, "Could not create directory %s: %s\n", build_options.project_name, strerror(errno));
		exit(EXIT_FAILURE);
	}

	if (chdir(build_options.project_name)) goto ERROR;

	FILE *file = fopen("LICENCE", "a");
	if (!file) goto ERROR;
	fclose(file);

	file = fopen("README.md", "a");
	if (!file) goto ERROR;
	fclose(file);

	file = fopen("project.toml", "a");
	if (!file) goto ERROR;
	fprintf(file, TOML, build_options.project_name);
	fclose(file);

	if (mkdir("lib", 0755)) goto ERROR;

	if (mkdir("build", 0755)) goto ERROR;

	if (mkdir("resources", 0755)) goto ERROR;

	if (mkdir("test", 0755)) goto ERROR;

	chdir("test");

	if (mkdir(build_options.project_name, 0755)) goto ERROR;

	chdir("..");

	if (mkdir("docs", 0755)) goto ERROR;

	chdir("docs");

	file = fopen("about.md", "a");
	if (!file) goto ERROR;
	fclose(file);

	if (mkdir("src", 0755)) goto ERROR;

	chdir("src");

	file = fopen("index.html", "a");
	if (!file) goto ERROR;
	fclose(file);

	chdir("../..");

	if (mkdir("src", 0755)) goto ERROR;

	chdir("src");

	if (mkdir(build_options.project_name, 0755)) goto ERROR;

	chdir(build_options.project_name);

	file = fopen("main.c3", "a");
	if (!file) goto ERROR;
	fclose(file);

	chdir("../..");

	printf("Project '%s' created.\n", build_options.project_name);
	exit(EXIT_SUCCESS);

ERROR:
	fprintf(stderr, "Err: %s\n", strerror(errno));

	printf("Something went wrong creating the project.");
	chdir(build_options.path);
	rmdir(build_options.project_name);
	exit(EXIT_FAILURE);
}
