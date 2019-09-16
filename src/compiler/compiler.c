// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../build/build_options.h"

Compiler compiler;

void compiler_init(void)
{
	stable_init(&compiler.modules, 64);
	compiler.module_list = NULL;
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
		Context *context = context_create(file);
		parse_file(context);
		context_print_ast(context, stdout);
	}
	exit(EXIT_SUCCESS);
}

void compiler_compile()
{
	builtin_setup();
	Context **contexts = NULL;
	VECEACH(build_options.files, i)
	{
		bool loaded = false;
		File *file = source_file_load(build_options.files[i], &loaded);
		if (loaded) continue;
		diag_reset();
		Context *context = context_create(file);
		vec_add(contexts, context);
		parse_file(context);
	}
	VECEACH(contexts, i)
	{
		Context *context = contexts[i];
		sema_analysis(context);
		if (diagnostics.errors > 0) exit(EXIT_FAILURE);
		char buffer[255];
		sprintf(buffer, "%s_test.c", context->module_name.string);
		printf("%s\n", buffer);
		FILE *f = fopen(buffer,"w");
		fprintf(f, "#include <stdbool.h>\n#include <stdint.h>\n");
		context->codegen_output = f;
		codegen(context);
		fclose(f);
		sprintf(buffer, "cc %s_test.c && ./a.out", context->module_name.string);
		system(buffer);
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


Decl *compiler_find_symbol(Token token)
{
	Decl *candidate = NULL;
	VECEACH(compiler.module_list, i)
	{
		Module *module = compiler.module_list[i];
		Decl *decl = module_find_symbol(module, token.string);
		if (decl && candidate)
		{
			const char *previous = candidate->module->name;
			const char *current = decl->module->name;
			SEMA_ERROR(token, "Ambiguous use of '%s', matches both %s::%s and %s::%s.", token.string,
					previous, token.string, current, token.string);
			return &poisoned_decl;
		}
		candidate = decl;
	}
	return candidate;
}

Module *compiler_find_or_create_module(const char *module_name)
{
	Module *module = stable_get(&compiler.modules, module_name);
	if (module) return module;
	module = CALLOCS(Module);
	module->name = module_name;
	stable_init(&module->symbols, 0x10000);
	stable_set(&compiler.modules, module_name, module);
	vec_add(compiler.module_list, module);
	return module;
}
