// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../build/build_options.h"

void compiler_init(void)
{
}

static void compiler_lex()
{
	VECEACH(build_options.files, i)
	{
		bool loaded = false;
		File *file = source_file_load(build_options.files[i], &loaded);
		if (loaded) continue;
		lexer_add_file_for_lexing(file);
		printf("# %s\n", file->full_path);
		while (1)
		{
			Token token = lexer_scan_token();
			printf("%s ", token_type_to_string(token.type));
			if (token.type == TOKEN_EOF) break;
		}
		printf("\n");
	}
	exit(EXIT_SUCCESS);
}

void compiler_parse()
{
	builtin_setup();
	VECEACH(build_options.files, i)
	{
		bool loaded = false;
		File *file = source_file_load(build_options.files[i], &loaded);
		if (loaded) continue;
		diag_reset();
		parse_file(file);
		context_print_ast(current_context, stdout);
	}
	exit(EXIT_SUCCESS);
}

void compiler_compile()
{
	builtin_setup();
	VECEACH(build_options.files, i)
	{
		bool loaded = false;
		File *file = source_file_load(build_options.files[i], &loaded);
		if (loaded) continue;
		diag_reset();
		parse_file(file);
		sema_analysis(current_context);
		if (diagnostics.errors > 0) exit(EXIT_FAILURE);
		FILE *f = fopen("test.c","w");
		fprintf(f, "#include <stdbool.h>\n#include <stdint.h>\n");
		current_context->codegen_output = f;
		codegen(current_context);
		fclose(f);
		system("cc test.c && ./a.out");

	}
	exit(EXIT_SUCCESS);
}

void compile_file()
{
	if (!vec_size(build_options.files)) error_exit("No files to compile.");
	switch (build_options.compile_option)
	{
		case COMPILE_LEX_ONLY:
			compiler_lex();
			break;
		case COMPILE_LEX_PARSE_ONLY:
			compiler_parse();
			break;
		default:
			compiler_compile();
			break;
	}
	TODO
}


