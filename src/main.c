#include <stdio.h>
#include "compiler/compiler.h"
#include "build/build_options.h"
#include "build/project_creation.h"
#include "compiler_tests/tests.h"
#include "utils/lib.h"

int main(int argc, const char *argv[])
{
	// First setup memory
	memory_init();

	// Parse arguments.
	parse_arguments(argc, argv);

	// Now we set up the symtab.
	symtab_init(build_options.symtab_size);

	switch (build_options.command)
	{
		case COMMAND_INIT:
			create_project();
			break;
		case COMMAND_UNIT_TEST:
			compiler_tests();
			break;
		case COMMAND_COMPILE:
			compile_files(NULL);
			break;
		case COMMAND_BUILD:
			build();
			break;
		case COMMAND_COMPILE_RUN:
		case COMMAND_MISSING:
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

