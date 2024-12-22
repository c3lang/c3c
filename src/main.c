#include "build/build.h"
#include "compiler/compiler.h"
#include "compiler_tests/tests.h"
#include "utils/lib.h"
#include <compiler_tests/benchmark.h>


bool debug_log = false;

jmp_buf on_error_jump;

NORETURN void exit_compiler(int exit_value)
{
	ASSERT0(exit_value != 0);
	longjmp(on_error_jump, exit_value);
}

static void cleanup()
{
	symtab_destroy();
	memory_release();
}

const char *compiler_exe_name;

int main_real(int argc, const char *argv[])
{
	compiler_exe_name = argv[0];
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

	// Memory must be handled here
	long long max_mem = 0;
	for (int i = 0; i < argc; i++)
	{
		if (str_eq(argv[i], "--max-mem") && i < argc - 1)
		{
			max_mem = atoll(argv[i + 1]);
			if (max_mem) max_mem = next_highest_power_of_2(max_mem);
			break;
		}
	}
	// First setup memory
	memory_init(max_mem);

	// Parse arguments.
	BuildOptions build_options = parse_arguments(argc, argv);

	// Init the compiler
	compiler_init(&build_options);

	switch (build_options.command)
	{
		case COMMAND_PRINT_SYNTAX:
			print_syntax(&build_options);
			break;
		case COMMAND_INIT:
			create_project(&build_options);
			break;
		case COMMAND_INIT_LIB:
			create_library(&build_options);
			break;
		case COMMAND_UNIT_TEST:
			compiler_tests();
			break;
		case COMMAND_COMPILE:
		case COMMAND_COMPILE_ONLY:
		case COMMAND_COMPILE_RUN:
		case COMMAND_DYNAMIC_LIB:
		case COMMAND_STATIC_LIB:
		case COMMAND_COMPILE_BENCHMARK:
		case COMMAND_COMPILE_TEST:
			compile_target(&build_options);
			break;
		case COMMAND_CLEAN:
			compile_clean(&build_options);
			break;
		case COMMAND_VENDOR_FETCH:
			vendor_fetch(&build_options);
			break;
		case COMMAND_CLEAN_RUN:
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
		case COMMAND_BENCHMARK:
		case COMMAND_TEST:
			compile_file_list(&build_options);
			break;
		case COMMAND_PROJECT:
			switch (build_options.project_options.command)
			{
				case SUBCOMMAND_VIEW:
					view_project(&build_options);
					break;
				case SUBCOMMAND_ADD:
					add_target_project(&build_options);
					break;
				case SUBCOMMAND_FETCH:
					fetch_project(&build_options);
					break;
				case SUBCOMMAND_MISSING:
					break;
			}
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
	char **args = malloc(sizeof(void *) * (unsigned)argc);
	for (unsigned i = 0; i < (unsigned)argc; i++)
	{
		args[i] = win_utf16to8(argv[i]);
	}
	return main_real(argc, (const char **)args);
}

#else

int main(int argc, const char *argv[]) { return main_real(argc, argv); }

#endif
