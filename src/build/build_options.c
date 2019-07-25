// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "build_options.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>

#include "../utils/errors.h"

static const char* DEFAULT_TARGET = "default";

BuildOptions build_options;
static int arg_index;
static int arg_count;
static const char** args;
static const char* current_arg;


#define OUTPUT(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__)
#define FAIL_WITH_ERR(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); usage(); exit(EXIT_FAILURE); } while (0)

static void usage(void)
{
	OUTPUT("Usage: %s [<options>] <command> [<args>]", args[0]);
	OUTPUT("");
	OUTPUT("Commands:");
	OUTPUT("");
	OUTPUT("  compile <file1> [<file2> ...]      Compile files without a project into an executable.");
	OUTPUT("  init <project name>                Initialize a new project structure.");
	OUTPUT("  build [<target>]                   Build the target in the current project.");
	OUTPUT("  clean                              Clean all build files.");
	OUTPUT("  run [<target>]                     Run (and build if needed) the target in the current project.");
	OUTPUT("  dist [<target>]                    Clean and build a target for distribution.");
	OUTPUT("  docs [<target>]                    Generate documentation for the target.");
	OUTPUT("  bench [<target>]                   Benchmark a target.");
	OUTPUT("  clean-run [<target>]               Clean, then run the target.");
	OUTPUT("  compile-run <file1> [<file2> ...]  Compile files then immediately run the result.");
	OUTPUT("");
	OUTPUT("Options:");
	OUTPUT("  --lib <dir>           - Use this directory as the c3 library path.");
	OUTPUT("  --path <dir>          - Use this as the base directory for the current command.");
	OUTPUT("  --template <template> - Use a different template: \"lib\", \"staticlib\" or a path.");
	OUTPUT("  --about               - Prints a short description of C3.");
}



static const char* check_dir(const char *path)
{
	static char *original_path = NULL;
	if (!original_path)
	{
		original_path = getcwd(NULL, 0);
	}
	if (chdir(path) == -1) error_exit("The path \"%s\" does not point to a valid directory.", path);
	int err = chdir(original_path);
	assert(!err);
	return path;
}

static inline bool at_end()
{
	return arg_index == arg_count - 1;
}

static inline const char* next_arg()
{
	assert(!at_end());
	current_arg = args[++arg_index];
	return current_arg;
}


static inline bool next_is_opt()
{
	return args[arg_index + 1][0] == '-';
}

static inline bool match_longopt(const char* name)
{
	return strcmp(&current_arg[2], name) == 0;
}

static inline bool match_shortopt(const char* name)
{
	return strcmp(&current_arg[1], name) == 0;
}


void append_file()
{
	if (build_options.file_count == MAX_FILES)
	{
		fprintf(stderr, "Max %d files may be specified\n", MAX_FILES);
		exit(EXIT_FAILURE);
	}
	build_options.files[build_options.file_count++] = current_arg;
}

static bool arg_match(const char *candidate)
{
	return strcmp(current_arg, candidate) == 0;
}

static void parse_optional_target()
{
	if (at_end() || next_is_opt())
	{
		build_options.target = DEFAULT_TARGET;
	}
	else
	{
		build_options.target = next_arg();
	}
}

static void parse_command(void)
{
	if (arg_match("init"))
	{
		build_options.command = COMMAND_INIT;
		if (at_end() || next_is_opt()) error_exit("Expected a project name after init");
		build_options.project_name = next_arg();
		return;
	}
	if (arg_match("utest"))
	{
		build_options.command = COMMAND_UNIT_TEST;
		return;
	}
	if (arg_match("compile"))
	{
		build_options.command = COMMAND_COMPILE;
		return;
	}
	if (arg_match("build"))
	{
		build_options.command = COMMAND_BUILD;
		parse_optional_target();
		return;
	}
	if (arg_match("run"))
	{
		build_options.command = COMMAND_RUN;
		parse_optional_target();
		return;
	}
	if (arg_match("compile-run"))
	{
		build_options.command = COMMAND_COMPILE_RUN;
		parse_optional_target();
		return;
	}
	if (arg_match("clean-run"))
	{
		build_options.command = COMMAND_CLEAN_RUN;
		parse_optional_target();
		return;
	}
	if (arg_match("clean"))
	{
		build_options.command = COMMAND_CLEAN;
		return;
	}
	if (arg_match("dist"))
	{
		build_options.command = COMMAND_CLEAN_RUN;
		parse_optional_target();
		return;
	}
	if (arg_match("docs"))
	{
		build_options.command = COMMAND_DOCS;
		parse_optional_target();
		return;
	}
	if (arg_match("bench"))
	{
		build_options.command = COMMAND_BENCH;
		parse_optional_target();
		return;
	}
	FAIL_WITH_ERR("Cannot process the unknown command \"%s\".", current_arg);
}
static void parse_option()
{
	switch (current_arg[1])
	{
		case 'h':
			break;
		case '-':
			if (match_longopt("about"))
			{
				OUTPUT("The C3 Compiler");
				OUTPUT("C3 is low level programming language based on C.");
				exit(EXIT_SUCCESS);
			}
			if (match_longopt("lib"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --lib needs a directory.");
				if (build_options.lib_count == MAX_LIB_DIRS) error_exit("Max %d libraries may be specified.", MAX_LIB_DIRS);
				build_options.lib_dir[build_options.lib_count++] = check_dir(next_arg());
				return;
			}
			if (match_longopt("path"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --path needs a directory.");
				build_options.path = check_dir(next_arg());
				return;
			}
			if (match_longopt("help"))
			{
				break;
			}
			break;
		default:
			break;

	}
	FAIL_WITH_ERR("Cannot process the unknown option \"%s\".", current_arg);
}


void parse_arguments(int argc, const char *argv[])
{
	if (argc < 2)
	{
		usage();
		exit(EXIT_SUCCESS);
	}

	build_options.path = ".";
	build_options.command = COMMAND_MISSING;

	arg_count = argc;
	args = argv;
	for (arg_index = 1; arg_index < arg_count; arg_index++)
	{
		current_arg = args[arg_index];
		if (current_arg[0] == '-')
		{
			parse_option();
			continue;
		}
		if (build_options.command == COMMAND_MISSING)
		{
			parse_command();
			continue;
		}
		if (build_options.command == COMMAND_COMPILE_RUN || build_options.command == COMMAND_COMPILE)
		{
			append_file();
			continue;
		}
		FAIL_WITH_ERR("Found the unexpected argument \"%s\".", current_arg);
	}
	if (build_options.command == COMMAND_MISSING)
	{
		FAIL_WITH_ERR("No command found.");
	}
}
