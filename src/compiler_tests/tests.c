// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "tests.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "compiler/compiler_internal.h"
#include "benchmark.h"
#include "utils/json.h"

void test_file(void)
{
	File file;
	memset(&file, 0, sizeof(file));
}
#define i128(x_, y_) ((Int128){x_, y_})
void test128()
{
	printf("Begin i128 testing.\n");
	Int128 address = i128_add(i128(0x123, 0x123), i128(0x222, 0x333));
	TEST_ASSERTF(address.high == 0x345 && address.low == 0x456, "i128 add failed with small numbers was %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_add(i128(0x123, UINT64_MAX), i128(0x222, 0x1));
	TEST_ASSERT(address.high == 0x346 && address.low == 0, "i128 add failed with simple overflow");
	address = i128_add(i128(0x123, UINT64_MAX), i128(0x222, UINT64_MAX));
	TEST_ASSERT(address.high == 0x346 && address.low == UINT64_MAX - 1, "i128 add failed with simple overflow2");
	address = i128_add(i128(UINT64_MAX, UINT64_MAX), i128(0x0, 0x1));
	TEST_ASSERT(address.high == 0 && address.low == 0, "i128 add failed with wrap");
	address = i128_add(i128(UINT64_MAX, UINT64_MAX), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX - 1, "i128 add failed overflow with wrap");
	printf("-- i128 Add - Ok.\n");
	address = i128_sub(i128(0x345, 0x457), i128(0x222, 0x333));
	TEST_ASSERTF(address.high == 0x123 && address.low == 0x124, "i128 sub failed with small numbers was %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_sub(i128(0x346, 0), i128(0x222, 0x1));
	TEST_ASSERTF(address.high == 0x123 && address.low == UINT64_MAX, "i128 sub failed with simple overflow %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_sub(i128(0x346, UINT64_MAX - 1), i128(0x222, UINT64_MAX));
	TEST_ASSERT(address.high == 0x123 && address.low == UINT64_MAX, "i128 sub failed with simple overflow2");
	address = i128_sub(i128(0, 0), i128(0x0, 0x1));
	TEST_ASSERTF(address.high == UINT64_MAX && address.low == UINT64_MAX, "i128 sub failed with wrap %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_sub(i128(UINT64_MAX, UINT64_MAX - 1), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX, "i128 sub failed overflow with wrap");
	printf("-- i128 Sub - Ok.\n");
	address = i128_and(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == 0 && address.low == 0, "And failed");
	address = i128_and(i128(0x123, 0x123456789abcdef1), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == 0x123 && address.low == 0x123456789abcdef1, "And failed");
	address = i128_and(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0));
	TEST_ASSERT(address.high == 0x0b0d0f0203 && address.low == 0x1030507090, "And failed");
	printf("-- i128 And - Ok.\n");
	address = i128_or(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX, "Or failed");
	address = i128_or(i128(0x123, 0x123456789abcdef1), i128(0x123203, 0x0));
	TEST_ASSERT(address.high == 0x123323 && address.low == 0x123456789abcdef1, "Or failed");
	address = i128_or(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0F0));
	TEST_ASSERTF(address.high == 0x0FAFCFEF2F3F && address.low == 0xF0F0F2F4F6F8FA, "Or failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Or - Ok.\n");
	address = i128_xor(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX, "Xor failed");
	address = i128_xor(i128(0x123, 0x123456789abcdef1), i128(0x123223, 0x0));
	TEST_ASSERT(address.high == 0x123300 && address.low == 0x123456789abcdef1, "Xor failed");
	address = i128_xor(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0F0));
	TEST_ASSERTF(address.high == 0x0FA4C2E02d3c && address.low == 0xF0F0e2c4a6886A, "Xor failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Xor - Ok.\n");
	address = i128_neg(i128(0x0, 0x0));
	TEST_ASSERT(address.high == 0 && address.low == 0, "Neg failed");
	address = i128_neg(i128(0x123, 0x123456789abcdef1));
	TEST_ASSERT(address.high == ~((uint64_t)0x123) && address.low == ~(uint64_t)0x123456789abcdef0, "Neg failed");
	address = i128_neg(i128(0xabcdef2233, 0x123456789A));
	TEST_ASSERTF(address.high == ~(uint64_t)0xabcdef2233 && address.low == ~(uint64_t)0x1234567899, "Neg failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Neg - Ok.\n");

	address = i128_from_str("1123");
	TEST_ASSERT(address.high == 0 && address.low == 1123, "Init failed");
	address = i128_from_str("10000000000000000000012344434232");
	TEST_ASSERT(address.high == 0x7e37be2022 && address.low == 0xc0914b295fc91e38, "Init failed");

	address = i128_mult(i128(0x111, 0x222), i128(0, 2));
	TEST_ASSERTF(address.high == 0x222 && address.low == 0x444, "Mult failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_mult(i128(0x111, 0x222), i128(2, 0));
	TEST_ASSERTF(address.high == 0x444 && address.low == 0, "Mult failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_mult(i128_from_str("523871293871232000123"), i128_from_str("283712312938293299"));

	TEST_ASSERTF(i128_ucomp(i128_from_str("148628736466183585621117368965778075777"), address) == CMP_EQ, "Mult failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Mult ok.\n");

	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("123")) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("124")) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("121")) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128(0x222, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128(0x111, 0x222), i128(0x222, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128(0x222, 0x111), i128(0x222, 0x111)) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128(UINT64_MAX, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_ucomp(i128(0x111, 0x222), i128(UINT64_MAX, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Ucomp ok.\n");

	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("123")) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("124")) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("121")) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128(0x222, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128(0x111, 0x222), i128(0x222, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128(0x222, 0x111), i128(0x222, 0x111)) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128(UINT64_MAX, 0x111), i128(0x111, 0x222)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	TEST_ASSERTF(i128_scomp(i128(0x111, 0x222), i128(UINT64_MAX, 0x111)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Scomp ok.\n");


	address = i128_shl(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERT(address.high == 0x2340 && address.low == 0x1234560, "shl failed");
	address = i128_shl(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(address.high == 0 && address.low == 0, "shl failed");
	address = i128_shl(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(address.high == 0 && address.low == 0, "shl failed");
	address = i128_shl(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERT(address.high == 0x1234561 && address.low == 0, "shl failed");
	printf("-- i128 Shl ok.\n");

	address = i128_lshr(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(address.high == 0x23 && address.low == 0x4000000000012345, "lshr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_lshr(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(address.high == 0 && address.low == 0, "lshr failed");
	address = i128_lshr(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(address.high == 0 && address.low == 0, "lshr failed");
	address = i128_lshr(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(address.high == 0 && address.low == 0x234, "lshr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Lshr ok.\n");

	address = i128_ashr(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(address.high == 0x23 && address.low == 0x4000000000012345, "ashr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_ashr(i128(0xF000000000000234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(address.high == 0xFF00000000000023 && address.low == 0x4000000000012345, "ashr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_ashr(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(address.high == 0 && address.low == 0, "ashr failed");
	address = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(0, 128));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX, "ashr failed");
	address = i128_ashr(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(address.high == 0 && address.low == 0, "ashr failed");
	address = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(1, 1));
	TEST_ASSERT(address.high == UINT64_MAX && address.low == UINT64_MAX, "ashr failed");
	address = i128_ashr(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(address.high == 0 && address.low == 0x234, "ashr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	address = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(address.high == UINT64_MAX && address.low == 0xF000000000000234, "ashr failed %llx, %llx", (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Ashr ok.\n");

	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("123"), i128_from_str("123")), i128_from_str("1")) == CMP_EQ, "Div failed");
	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("123"), i128_from_str("124")), i128_from_str("0")) == CMP_EQ, "Div failed");
	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("245"), i128_from_str("123")), i128_from_str("1")) == CMP_EQ, "Div failed");
	address = i128_udiv(i128(0x12345, UINT64_MAX), i128(1, 0));
	TEST_ASSERT(address.low == 0x12345 && address.high == 0, "Div failed");
	address = i128_sdiv(i128(0x12345, UINT64_MAX), i128(1, 0));
	TEST_ASSERT(address.low == 0x12345 && address.high == 0, "Div failed");
	address = i128_udiv(i128(UINT64_MAX, 0), i128(1, 0));
	TEST_ASSERT(address.low == UINT64_MAX && address.high == 0, "Div failed");
	address = i128_sdiv(i128(UINT64_MAX - 1, UINT64_MAX - 1), i128(1, 0));
	TEST_ASSERTF(address.low == UINT64_MAX && address.high == UINT64_MAX, "Div failed %s", i128_to_string(address, 10,
	                                                                                                    true, false));
	address = i128_sdiv(i128(2, 0), i128(UINT64_MAX - 1, UINT64_MAX - 1));
	printf("-- i128 Div okfefe %x.\n", (unsigned)-2);
	TEST_ASSERTF(address.low == UINT64_MAX && address.high == UINT64_MAX, "Div failed: %s %llx, %llx", i128_to_string(
			address, 10, true, false), (unsigned long long)address.high, (unsigned long long)address.low);
	printf("-- i128 Div ok.\n");



}

static void test_json(void)
{
	printf("Begin json testing.\n");
	JsonParser parser;
	json_init_string(&parser, "123");
	JSONObject *obj = json_parse(&parser);
	TEST_ASSERT(obj->type == J_NUMBER, "Expected number");
	TEST_ASSERT(obj->f == 123.0, "Expected number match");
	json_init_string(&parser, "[123, 23.123]");
	JSONObject *array = json_parse(&parser);
	TEST_ASSERT(array->type == J_ARRAY, "Expected array");
	TEST_ASSERT(vec_size(array->elements) == 2, "Expected 2 elements");
	TEST_ASSERT(array->elements[0]->f == 123.0, "Matching element 1");
	TEST_ASSERT(array->elements[1]->f == 23.123, "Matching element 1");
	json_init_string(&parser, "[\"hello\\nworld\\t.\", 123]");
	array = json_parse(&parser);
	TEST_ASSERT(array->type == J_ARRAY, "Expected array");
	TEST_ASSERT(vec_size(array->elements) == 2, "Expected 2 elements");
	TEST_ASSERT(array->elements[1]->f == 123.0, "Matching element 1");
	TEST_ASSERT(array->elements[0]->type == J_STRING, "Matching element 0");
	TEST_ASSERT(strcmp(array->elements[0]->str, "hello\nworld\t.") == 0, "Mismatching string");
}

void compiler_tests(void)
{
	symtab_init(0x100000);

	test_file();
	test128();
	run_arena_allocator_tests();

	test_json();
	exit_compiler(COMPILER_SUCCESS_EXIT);
}