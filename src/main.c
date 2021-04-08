#include <stdio.h>
#include "compiler/compiler.h"
#include "build/build_options.h"
#include "build/project_creation.h"
#include "compiler_tests/tests.h"
#include "utils/lib.h"

static int version = 102;
int main(int argc, const char *argv[])
{
	// First setup memory
	memory_init();



	DEBUG_LOG("Version: %d", version);

	// Init the compiler
	compiler_init();

	// Parse arguments.
	BuildOptions build_options = parse_arguments(argc, argv);

	BuildTarget build_target = {};

	switch (build_options.command)
	{
		case COMMAND_INIT:
			create_project(&build_options);
			break;
		case COMMAND_UNIT_TEST:
			compiler_tests();
			break;
		case COMMAND_GENERATE_HEADERS:
		case COMMAND_COMPILE:
		case COMMAND_COMPILE_RUN:
			init_default_build_target(&build_target, &build_options, "foo.out");
			compile_files(&build_target);
			break;
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_CLEAN_RUN:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
			init_build_target(&build_target, &build_options);
			compile_files(&build_target);
			break;
		case COMMAND_MISSING:
			printf("TODO\n");
	}


	print_arena_status();
	free_arena();
	return 0;
}

