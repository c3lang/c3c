#include "compiler/compiler.h"
#include "build/build_options.h"
#include "build/project_creation.h"
#include "compiler_tests/tests.h"
#include "utils/lib.h"

bool debug_log = false;
bool debug_stats = false;

jmp_buf on_error_jump;

NORETURN void exit_compiler(int exit_value)
{
	assert(exit_value != 0);
	longjmp(on_error_jump, exit_value);
}

static void cleanup()
{
	memory_release();
}


int main_real(int argc, const char *argv[])
{
	int result = setjmp(on_error_jump);
	if (result)
	{
		cleanup();
		if (result == COMPILER_SUCCESS_EXIT) result = EXIT_SUCCESS;
		return result;
	}

	// First setup memory
	memory_init();

	// Parse arguments.
	BuildOptions build_options = parse_arguments(argc, argv);

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

	print_arena_status();
	memory_release();
	return 0;
}

int main(int argc, const char *argv[])
{
	main_real(argc, argv);
}
