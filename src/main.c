#include <compiler_tests/benchmark.h>
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
	symtab_destroy();
	memory_release();
}


int main_real(int argc, const char *argv[])
{
	bench_begin();

	// Setjmp will allow us to add things like fuzzing with
	// easy restarts.
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
		case COMMAND_PRINT_SYNTAX:
			print_syntax(&build_options);
			break;
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
		case COMMAND_CLEAN:
			compile_clean(&build_options);
			break;
		case COMMAND_CLEAN_RUN:
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
			compile_file_list(&build_options);
			break;
		case COMMAND_MISSING:
			UNREACHABLE
	}

	symtab_destroy();
	memory_release();
	return 0;
}

#if (_MSC_VER)

int wmain(int argc, const uint16_t *argv[])
{
	char** args = malloc(sizeof(void*) * (unsigned)argc);
	for (unsigned i = 0; i < (unsigned)argc; i++)
	{
		args[i] = win_utf16to8(argv[i]);
	}
	main_real(argc, (const char **)args);
}

#else

int main(int argc, const char *argv[])
{
	return main_real(argc, argv);
}

#endif