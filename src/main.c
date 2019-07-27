#include <stdio.h>
#include "build/build_options.h"
#include "build/project_creation.h"
#include "utils/errors.h"
#include "compiler_tests/tests.h"
#include "compiler/malloc.h"
int main(int argc, const char *argv[])
{
	init_arena();
	parse_arguments(argc, argv);
	switch (build_options.command)
	{
		case COMMAND_INIT:
			create_project();
			break;
		case COMMAND_UNIT_TEST:
			compiler_tests();
		case COMMAND_COMPILE:
		case COMMAND_COMPILE_RUN:
		case COMMAND_MISSING:
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_CLEAN_RUN:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
			printf("TODO\n");
	}
	free_arena();
	return 0;
}

