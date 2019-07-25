// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <compiler/lexer.h>
#include "tests.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "benchmark.h"

#define TEST_ASSERT(cond, text, ...) do { if (!(cond)) { printf("\nTEST FAILED: " text "\n", ##__VA_ARGS__); exit(-1); } } while (0)
static void test_lexer(void)
{
#ifdef __OPTIMIZE__
	printf("--- RUNNING OPTIMIZED ---\n");
#endif
	printf("Begin lexer testing.\n");
	printf("1. Check number of keywords...");
	int tokens_found = 0;
	const int EXPECTED_TOKENS = 81;
	const char* tokens[INVALID_TOKEN];
	int len[INVALID_TOKEN];
	for (int i = 0; i < INVALID_TOKEN; i++)
	{
		const char* token = token_type_to_string((TokenType)i);
		tokens[i] = token;
		len[i] = strlen(token);
		TokenType type = identifier_type(token, len[i]);
		TokenType type2 = ident_type_fnv1(token, len[i]);

		if (type != TOKEN_VAR_IDENT)
		{
			tokens_found++;
			TEST_ASSERT(type == i, "Mismatch on token %s", token);
			if (type2 != type)
			{
				printf("\n(fnv1) Test mismatch on token %s, generated %s\n", token, token_type_to_string(type2));
			}
		}
		tokens[i] = "byte";
		len[i] = 4;
	}
	printf(" %d found.\n", tokens_found);
	TEST_ASSERT(ident_type_fnv1("alias ", 6) == TOKEN_VAR_IDENT, "Error in fnv1 ident");
	TEST_ASSERT(identifier_type("alias ", 6) == TOKEN_VAR_IDENT, "Error in switch ident");
	TEST_ASSERT(ident_type_fnv1("alias ", 5) != TOKEN_VAR_IDENT, "Error in fnv1 ident2");
	TEST_ASSERT(identifier_type("alias ", 5) != TOKEN_VAR_IDENT, "Error in switch ident2");
	TEST_ASSERT(tokens_found == EXPECTED_TOKENS, "Unexpected number of identifiers! Expected %d.", EXPECTED_TOKENS);

	const int BENCH_REPEATS = 10000000;

	printf("2. Test keyword lexing speed (switch)... ");
	bench_begin();
	for (int b = 0; b < BENCH_REPEATS; b++)
	{
		for (int i = 0; i < INVALID_TOKEN; i++)
		{
			identifier_type(tokens[i], len[i]);
		}
	}
	printf("complete in %fs\n", bench_mark());

	printf("3. Test keyword lexing speed (fnv1)... ");
	bench_begin();
	for (int b = 0; b < BENCH_REPEATS; b++)
	{
		for (int i = 0; i < INVALID_TOKEN; i++)
		{
			ident_type_fnv1(tokens[i], len[i]);
		}
	}
	printf("complete in %fs\n", bench_mark());


	exit(0);
}

void compiler_tests(void)
{
	test_lexer();
}