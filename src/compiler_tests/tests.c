// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "tests.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "compiler/compiler_internal.h"
#include "benchmark.h"



void test_file(void)
{
	File file;
	memset(&file, 0, sizeof(file));
	file.start_id = 3;
	vec_add(file.lines, file.start_id);
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
	run_arena_allocator_tests();

	exit(0);
}