// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <compiler/lexer.h>
#include "tests.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <compiler/compiler.h>
#include <utils/lib.h>
#include <utils/errors.h>
#include <utils/malloc.h>
#include "benchmark.h"
#include "../compiler/symtab.h"

static void test_lexer(void)
{
#ifdef __OPTIMIZE__
	printf("--- RUNNING OPTIMIZED ---\n");
#endif
	run_arena_allocator_tests();
	printf("Begin lexer testing.\n");
	printf("-- Check number of keywords...\n");
	int tokens_found = 0;
	const int EXPECTED_TOKENS = 12 + 73 + 9;
	const char* tokens[TOKEN_EOF];
	int len[TOKEN_EOF];
	lexer_check_init();
	for (int i = 1; i < TOKEN_EOF; i++)
	{
		const char* token = token_type_to_string((TokenType)i);
		tokens[i] = token;
		len[i] = strlen(token);
		TokenType lookup = TOKEN_VAR_IDENT;
		const char* interned = symtab_add(token, len[i], fnv1a(token, len[i]), &lookup);
		if (lookup != TOKEN_VAR_IDENT)
		{
			Token scanned = lexer_scan_ident_test(token);
			TEST_ASSERT(scanned.type == i, "Mismatch scanning: was '%s', expected '%s' - lookup: %s - interned: %s.",
					token_type_to_string(scanned.type),
					token_type_to_string(i),
					token_type_to_string(lookup),
					interned);
			tokens_found++;
		}
		else
		{
			tokens[i] = "casi";
			len[i] = 4;
		}
	}
	printf("-> %d keywords found.\n", tokens_found);
	EXPECT("Keywords", tokens_found, EXPECTED_TOKENS);

	const int BENCH_REPEATS = 100000;

	printf("-- Test keyword lexing speed...\n");
	bench_begin();
	for (int b = 0; b < BENCH_REPEATS; b++)
	{
		for (int i = 1; i < TOKEN_EOF; i++)
		{
			volatile TokenType t = lexer_scan_ident_test(tokens[i]).type;
		}
	}

	printf("-> Test complete in %fs, %.0f kkeywords/s\n", bench_mark(), (BENCH_REPEATS * (TOKEN_EOF - 1)) / (1000 * bench_mark()));

#include "shorttest.c"

	printf("-- Test token lexing speed...\n");
	const char *pointer = test_parse;
	int loc = 0;
	while (*pointer != '\0')
	{
		if (*(pointer++) == '\n') loc++;
	}

	bench_begin();
	int tokens_parsed = 0;
	size_t test_len = strlen(test_parse);
	for (int b = 0; b < BENCH_REPEATS; b++)
	{
		lexer_test_setup(test_parse, test_len);
		Token token;
		while (1)
		{
			token = lexer_scan_token();
			if (token.type == TOKEN_EOF) break;
			TEST_ASSERT(token.type != INVALID_TOKEN, "Got invalid token");
			tokens_parsed++;
		}
	}

	printf("-> Test complete in %fs, %.0f kloc/s, %.0f ktokens/s\n", bench_mark(),
			loc * BENCH_REPEATS / (1000 * bench_mark()), tokens_parsed / (1000 * bench_mark()));
}

void test_compiler(void)
{
	compiler_init();
}

void compiler_tests(void)
{
	test_lexer();
	test_compiler();

	exit(0);
}