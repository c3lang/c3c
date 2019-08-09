// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <utils/errors.h>
#include <utils/file_utils.h>
#include "compiler.h"
#include "symtab.h"
#include "../build/build_options.h"
#include "../utils/lib.h"
#include "lexer.h"
#include "source_file.h"
#include "parser.h"
#include "diagnostics.h"
#include "semantic_analyser.h"

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
	type_setup(build_options.pointer_size);
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
	type_setup(build_options.pointer_size);
	VECEACH(build_options.files, i)
	{
		bool loaded = false;
		File *file = source_file_load(build_options.files[i], &loaded);
		if (loaded) continue;
		diag_reset();
		parse_file(file);
		sema_analysis(current_context);
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


