#include "compiler/compiler.h"
#include "build/build_options.h"
#include "build/project_creation.h"
#include "compiler_tests/tests.h"
#include "utils/lib.h"

bool debug_log = false;
bool debug_stats = false;

int main(int argc, const char *argv[])
{
	// First setup memory
	memory_init();

	puts("successfully initialized memory");

	// TODO: REMOVE THIS !!!!!!
	// this code is a workaround for it being difficult to set command line parameters.
	char* tmp = argv[0];
	argv = (char*[]){"c3c", "compile-only", "../../../resources/hw.c3", "-E"};
	argv[0] = tmp;
	argc = 3;

	// Parse arguments.
	BuildOptions build_options = parse_arguments(argc, argv);

	puts("parsed args");

	// Init the compiler
	compiler_init(build_options.std_lib_dir);

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
		case COMMAND_COMPILE_ONLY:
		case COMMAND_COMPILE_RUN:
			compile_target(&build_options);
			break;
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_CLEAN_RUN:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
			compile_file_list(&build_options);
			break;
		case COMMAND_MISSING:
			UNREACHABLE
	}

	puts("cleaning up");

	print_arena_status();
	free_arena();
	return 0;
}

