// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../build/build_options.h"

Compiler compiler;

void compiler_init(void)
{
	stable_init(&compiler.modules, 64);
	stable_init(&compiler.global_symbols, 0x1000);
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
	/*
	const char *printf = "printf";
	TokenType t_type = TOKEN_IDENT;
	const char *interned = symtab_add(printf, (uint32_t) 6, fnv1a(printf, (uint32_t)6), &t_type);
	Decl *decl = decl_new(DECL_FUNC, wrap(interned), VISIBLE_PUBLIC);
	Type *type = type_new(TYPE_POINTER);
	type->base = type_char;
	sema_resolve_type(contexts[0], type);
	Decl *param = decl_new_var(wrap("str"), type, VARDECL_PARAM, VISIBLE_LOCAL);
	vec_add(decl->func.function_signature.params, param);
	decl->func.function_signature.rtype = type_void;
	decl->resolve_status = RESOLVE_DONE;
	context_register_global_decl(contexts[0], decl);
*/
	VECEACH(contexts, i)
	{
		sema_analysis_pass_conditional_compilation(contexts[i]);
	}
	VECEACH(contexts, i)
	{
		sema_analysis_pass_decls(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);
	VECEACH(contexts, i)
	{
		Context *context = contexts[i];
		llvm_codegen(context);
	}
	exit(EXIT_SUCCESS);
}

void compile_file()
{
	target_setup();
	builtin_setup();

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
	return stable_get(&compiler.global_symbols, token.string);
}

void compiler_add_type(Type *type)
{
	DEBUG_LOG("Created type %s.", type->name);
	assert(type_ok(type));
	VECADD(compiler.type, type);
}
Module *compiler_find_or_create_module(const char *module_name)
{
	Module *module = stable_get(&compiler.modules, module_name);
	if (module) return module;
	module = CALLOCS(Module);
	module->name = module_name;
	stable_init(&module->symbols, 0x10000);
	stable_set(&compiler.modules, module_name, module);
	return module;
}

void compiler_register_public_symbol(Decl *decl)
{
	Decl *prev = stable_get(&compiler.global_symbols, decl->name.string);
	// If the previous symbol was already declared globally, remove it.
	stable_set(&compiler.global_symbols, decl->name.string, prev ? &poisoned_decl : decl);
	STable *sub_module_space = stable_get(&compiler.qualified_symbols, decl->module->name);
	if (!sub_module_space)
	{
		sub_module_space = malloc_arena(sizeof(*sub_module_space));
		stable_init(sub_module_space, 0x100);
		stable_set(&compiler.qualified_symbols, decl->module->name, sub_module_space);
	}
	prev = stable_get(sub_module_space, decl->name.string);
	stable_set(sub_module_space, decl->name.string, prev ? &poisoned_decl : decl);
}
