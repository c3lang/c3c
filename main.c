#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <assert.h>
#include "build/build_options.h"
#include "build/project_creation.h"
#include "utils/errors.h"

static void usage(const char* name) {
	fprintf(stderr, "Usage: %s <options> <target>\n", name);
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "   --lib <dir>   - use this directory as the c3 library path\n");
	fprintf(stderr, "   --path <dir>  - use this as the base directory for the current command\n");
	exit(EXIT_SUCCESS);
}

typedef enum
{
	COMMAND_COMPILE,
	COMMAND_NEW,
} CompilerCommand;

static CompilerCommand command;

static int arg_index;
static int arg_count;
static const char** args;
static const char* current_arg;

static void select_compiler_command(CompilerCommand new_command)
{
	if (command != COMMAND_COMPILE)
	{
		fprintf(stderr, "Please select only one command\n");
		exit(EXIT_FAILURE);
	}
	command = new_command;
}

static const char* check_dir(const char *path)
{
	if (chdir(path) == -1) error_exit("The path \"%s\" does not point to a valid directory.", path);
	int err = chdir(build_options.original_path);
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


static inline bool next_is_arg()
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

static void parse_option()
{
	switch (current_arg[1])
	{
		case 'h':
			break;
		case 'n':
			if (match_shortopt("new"))
			{
				select_compiler_command(COMMAND_NEW);
				if (at_end() || next_is_arg()) error_exit("Expected a project name after -new");
				build_options.project_name = next_arg();
				return;
			}
			break;
		case '-':
			if (match_longopt("about"))
			{
				fprintf(stderr, "The C3 Compiler\n");
				fprintf(stderr, "\nC3 is an evolution of C.\n");
				exit(0);
			}
			if (match_longopt("lib"))
			{
				if (at_end() || next_is_arg()) error_exit("error: --lib needs a directory.");
				if (current_arg[0] == '-') error_exit("Expected a directory after --lib.");
				if (build_options.lib_count == MAX_LIB_DIRS) error_exit("Max %d libraries may be specified.", MAX_LIB_DIRS);
				build_options.lib_dir[build_options.lib_count++] = check_dir(next_arg());
				return;
			}
			if (match_longopt("path"))
			{
				if (at_end() || next_is_arg()) error_exit("error: --path needs a directory.");
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
	usage(args[0]);
}

static void handle_compile_command()
{
	printf("TODO\n");
	exit(EXIT_SUCCESS);
}

static void parse_arguments()
{
	command = COMMAND_COMPILE;
	for (arg_index = 1; arg_index < arg_count; arg_index++)
	{
		current_arg = args[arg_index];
		if (current_arg[0] == '-')
		{
			parse_option();
			continue;
		}
		append_file();
	}
	switch (command)
	{
		case COMMAND_NEW:
			create_project();
			break;
		case COMMAND_COMPILE:
			handle_compile_command();
			break;
		default:
			exit(EXIT_FAILURE);
	}
}


int main(int argc, const char *argv[])
{
	build_options.path = ".";
	build_options.original_path = getcwd(NULL, 0);
	arg_count = argc;
	args = argv;
	parse_arguments();
	return EXIT_SUCCESS;
}

