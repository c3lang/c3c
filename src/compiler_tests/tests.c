// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "tests.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "compiler/compiler_internal.h"
#include "benchmark.h"

static void test_lexer(void)
{
#ifdef __OPTIMIZE__
	printf("--- RUNNING OPTIMIZED ---\n");
#endif
	run_arena_allocator_tests();
	printf("Begin lexer testing.\n");
	printf("-- Check number of keywords...\n");
	int tokens_found = 0;
	const int EXPECTED_TOKENS = TOKEN_CT_SWITCH - TOKEN_ALIAS + 1 + TOKEN_TYPEID - TOKEN_VOID + 1;
	const char* tokens[TOKEN_EOF];
	int len[TOKEN_EOF];
	Lexer lexer;
	for (int i = 1; i < TOKEN_EOF; i++)
	{
		const char* token = token_type_to_string((TokenType)i);
		tokens[i] = token;
		len[i] = (int)strlen(token);
		TokenType lookup = TOKEN_IDENT;
		const char* interned = symtab_add(token, len[i], fnv1a(token, len[i]), &lookup);
		if (lookup != TOKEN_IDENT)
		{
			if (!lexer_scan_ident_test(&lexer, token))
			{
				TEST_ASSERT(false, "Failed to scan token %s", token);
			}
			int index = toktype_arena.allocated;
			TokenType type_scanned = (TokenType)(toktypeptr(index - 1))[0];
			TEST_ASSERT(type_scanned == (TokenType)i, "Mismatch scanning: was '%s', expected '%s' - lookup: %s - interned: %s.",
					token_type_to_string(type_scanned),
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

	const int BENCH_REPEATS = 10000;

	printf("-- Test keyword lexing speed...\n");
	bench_begin();
	for (int b = 0; b < BENCH_REPEATS; b++)
	{
		for (int i = 1; i < TOKEN_EOF; i++)
		{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
			volatile bool t = lexer_scan_ident_test(&lexer, tokens[i]);
#pragma clang diagnostic pop
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
		lexer_init_for_test(&lexer, test_parse, test_len);
		Token token;
		while (1)
		{
			token = lexer_advance(&lexer);
			if (token.type == TOKEN_EOF) break;
			TEST_ASSERT(token.type != TOKEN_INVALID_TOKEN, "Got invalid token");

			tokens_parsed++;
		}
	}

	printf("-> Test complete in %fs, %.0f kloc/s, %.0f ktokens/s\n", bench_mark(),
			loc * BENCH_REPEATS / (1000 * bench_mark()), tokens_parsed / (1000 * bench_mark()));
}

void test_compiler(void)
{
	const char **files = NULL;
	file_add_wildcard_files(&files, "tests", true);
	if (!vec_size(files))
	{
		error_exit("No test files could be found.");
	}

	const char **single_file = VECNEW(const char *, 1);
	vec_add(single_file, files[0]);

	VECEACH(files, i)
	{
		printf("Running %s...\n", files[i]);
		char *res = NULL;
		asprintf(&res, "tests/%s", files[i]);
		single_file[0] = res;
		active_target = (BuildTarget) { .type = TARGET_TYPE_EXECUTABLE, .sources = single_file, .name = "a.out" };
		free(res);
	}

}

void test_file(void)
{
	File file;
	memset(&file, 0, sizeof(file));
	file.start_id = 3;
	VECADD(file.lines, file.start_id);
	TEST_ASSERT(source_file_find_position_in_file(&file, 3).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 10).line == 1, "Expected first line");
	source_file_append_line_end(&file, 9);
	TEST_ASSERT(source_file_find_position_in_file(&file, 3).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 5).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 10).line == 2, "Expected second line");
	source_file_append_line_end(&file, 19);
	TEST_ASSERT(source_file_find_position_in_file(&file, 3).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 5).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 10).line == 2, "Expected second line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 15).line == 2, "Expected second line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 21).line == 3, "Expected third line");
	source_file_append_line_end(&file, 29);
	TEST_ASSERT(source_file_find_position_in_file(&file, 3).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 5).line == 1, "Expected first line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 10).line == 2, "Expected second line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 15).line == 2, "Expected second line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 21).line == 3, "Expected third line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 25).line == 3, "Expected third line");
	TEST_ASSERT(source_file_find_position_in_file(&file, 31).line == 4, "Expected fourth line");
}
void compiler_tests(void)
{
	symtab_init(0x100000);

	test_file();
	test_lexer();
	test_compiler();

	exit(0);
}