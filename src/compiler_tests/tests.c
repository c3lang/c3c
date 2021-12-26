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
}
#define i128(x_, y_) ((Int128){x_, y_})
void test128()
{
	printf("Begin i128 testing.\n");
	Int128 addres = i128_add(i128(0x123, 0x123), i128(0x222, 0x333));
	TEST_ASSERTF(addres.high == 0x345 && addres.low == 0x456, "i128 add failed with small numbers was %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_add(i128(0x123, UINT64_MAX), i128(0x222, 0x1));
	TEST_ASSERT(addres.high == 0x346 && addres.low == 0, "i128 add failed with simple overflow");
	addres = i128_add(i128(0x123, UINT64_MAX), i128(0x222, UINT64_MAX));
	TEST_ASSERT(addres.high == 0x346 && addres.low == UINT64_MAX - 1, "i128 add failed with simple overflow2");
	addres = i128_add(i128(UINT64_MAX, UINT64_MAX), i128(0x0, 0x1));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "i128 add failed with wrap");
	addres = i128_add(i128(UINT64_MAX, UINT64_MAX), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX - 1, "i128 add failed overflow with wrap");
	printf("-- i128 Add - Ok.\n");
	addres = i128_sub(i128(0x345, 0x457), i128(0x222, 0x333));
	TEST_ASSERTF(addres.high == 0x123 && addres.low == 0x124, "i128 sub failed with small numbers was %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_sub(i128(0x346, 0), i128(0x222, 0x1));
	TEST_ASSERTF(addres.high == 0x123 && addres.low == UINT64_MAX, "i128 sub failed with simple overflow %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_sub(i128(0x346, UINT64_MAX - 1), i128(0x222, UINT64_MAX));
	TEST_ASSERT(addres.high == 0x123 && addres.low == UINT64_MAX, "i128 sub failed with simple overflow2");
	addres = i128_sub(i128(0, 0), i128(0x0, 0x1));
	TEST_ASSERTF(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "i128 sub failed with wrap %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_sub(i128(UINT64_MAX, UINT64_MAX - 1), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "i128 sub failed overflow with wrap");
	printf("-- i128 Sub - Ok.\n");
	addres = i128_and(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "And failed");
	addres = i128_and(i128(0x123, 0x123456789abcdef1), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == 0x123 && addres.low == 0x123456789abcdef1, "And failed");
	addres = i128_and(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0));
	TEST_ASSERT(addres.high == 0x0b0d0f0203 && addres.low == 0x1030507090, "And failed");
	printf("-- i128 And - Ok.\n");
	addres = i128_or(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "Or failed");
	addres = i128_or(i128(0x123, 0x123456789abcdef1), i128(0x123203, 0x0));
	TEST_ASSERT(addres.high == 0x123323 && addres.low == 0x123456789abcdef1, "Or failed");
	addres = i128_or(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0F0));
	TEST_ASSERTF(addres.high == 0x0FAFCFEF2F3F && addres.low == 0xF0F0F2F4F6F8FA, "Or failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Or - Ok.\n");
	addres = i128_xor(i128(0x0, 0x0), i128(UINT64_MAX, UINT64_MAX));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "Xor failed");
	addres = i128_xor(i128(0x123, 0x123456789abcdef1), i128(0x123223, 0x0));
	TEST_ASSERT(addres.high == 0x123300 && addres.low == 0x123456789abcdef1, "Xor failed");
	addres = i128_xor(i128(0xabcdef2233, 0x123456789A), i128(0x0F0F0F0F0F0F, 0xF0F0F0F0F0F0F0));
	TEST_ASSERTF(addres.high == 0x0FA4C2E02d3c && addres.low == 0xF0F0e2c4a6886A, "Xor failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Xor - Ok.\n");
	addres = i128_neg(i128(0x0, 0x0));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "Neg failed");
	addres = i128_neg(i128(0x123, 0x123456789abcdef1));
	TEST_ASSERT(addres.high == ~((uint64_t)0x123) && addres.low == ~(uint64_t)0x123456789abcdef0, "Neg failed");
	addres = i128_neg(i128(0xabcdef2233, 0x123456789A));
	TEST_ASSERTF(addres.high == ~(uint64_t)0xabcdef2233 && addres.low == ~(uint64_t)0x1234567899, "Neg failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Neg - Ok.\n");

	addres = i128_from_str("1123");
	TEST_ASSERT(addres.high == 0 && addres.low == 1123, "Init failed");
	addres = i128_from_str("10000000000000000000012344434232");
	TEST_ASSERT(addres.high == 0x7e37be2022 && addres.low == 0xc0914b295fc91e38, "Init failed");

	addres = i128_mult(i128(0x111, 0x222), i128(0, 2));
	TEST_ASSERTF(addres.high == 0x222 && addres.low == 0x444, "Mult failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_mult(i128(0x111, 0x222), i128(2, 0));
	TEST_ASSERTF(addres.high == 0x444 && addres.low == 0, "Mult failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_mult(i128_from_str("523871293871232000123"), i128_from_str("283712312938293299"));

	TEST_ASSERTF(i128_ucomp(i128_from_str("148628736466183585621117368965778075777"), addres) == CMP_EQ, "Mult failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Mult ok.\n");

	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("123")) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("124")) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128_from_str("123"), i128_from_str("121")) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128(0x222, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128(0x111, 0x222), i128(0x222, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128(0x222, 0x111), i128(0x222, 0x111)) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128(UINT64_MAX, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_ucomp(i128(0x111, 0x222), i128(UINT64_MAX, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Ucomp ok.\n");

	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("123")) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("124")) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128_from_str("123"), i128_from_str("121")) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128(0x222, 0x111), i128(0x111, 0x222)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128(0x111, 0x222), i128(0x222, 0x111)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128(0x222, 0x111), i128(0x222, 0x111)) == CMP_EQ, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128(UINT64_MAX, 0x111), i128(0x111, 0x222)) == CMP_LT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	TEST_ASSERTF(i128_scomp(i128(0x111, 0x222), i128(UINT64_MAX, 0x111)) == CMP_GT, "Comp failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Scomp ok.\n");


	addres = i128_shl(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERT(addres.high == 0x2340 && addres.low == 0x1234560, "shl failed");
	addres = i128_shl(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "shl failed");
	addres = i128_shl(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "shl failed");
	addres = i128_shl(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERT(addres.high == 0x1234561 && addres.low == 0, "shl failed");
	printf("-- i128 Shl ok.\n");

	addres = i128_lshr(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(addres.high == 0x23 && addres.low == 0x4000000000012345, "lshr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_lshr(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "lshr failed");
	addres = i128_lshr(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "lshr failed");
	addres = i128_lshr(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(addres.high == 0 && addres.low == 0x234, "lshr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Lshr ok.\n");

	addres = i128_ashr(i128(0x234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(addres.high == 0x23 && addres.low == 0x4000000000012345, "ashr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_ashr(i128(0xF000000000000234, 0x123456), i128(0, 0x4));
	TEST_ASSERTF(addres.high == 0xFF00000000000023 && addres.low == 0x4000000000012345, "ashr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_ashr(i128(0x234, 0x1234561), i128(0, 128));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "ashr failed");
	addres = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(0, 128));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "ashr failed");
	addres = i128_ashr(i128(0x234, 0x1234561), i128(1, 1));
	TEST_ASSERT(addres.high == 0 && addres.low == 0, "ashr failed");
	addres = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(1, 1));
	TEST_ASSERT(addres.high == UINT64_MAX && addres.low == UINT64_MAX, "ashr failed");
	addres = i128_ashr(i128(0x234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(addres.high == 0 && addres.low == 0x234, "ashr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	addres = i128_ashr(i128(0xF000000000000234, 0x1234561), i128(0, 64));
	TEST_ASSERTF(addres.high == UINT64_MAX && addres.low == 0xF000000000000234, "ashr failed %llx, %llx", (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Ashr ok.\n");

	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("123"), i128_from_str("123")), i128_from_str("1")) == CMP_EQ, "Div failed");
	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("123"), i128_from_str("124")), i128_from_str("0")) == CMP_EQ, "Div failed");
	TEST_ASSERT(i128_ucomp(i128_udiv(i128_from_str("245"), i128_from_str("123")), i128_from_str("1")) == CMP_EQ, "Div failed");
	addres = i128_udiv(i128(0x12345, UINT64_MAX), i128(1, 0));
	TEST_ASSERT(addres.low == 0x12345 && addres.high == 0, "Div failed");
	addres = i128_sdiv(i128(0x12345, UINT64_MAX), i128(1, 0));
	TEST_ASSERT(addres.low == 0x12345 && addres.high == 0, "Div failed");
	addres = i128_udiv(i128(UINT64_MAX, 0), i128(1, 0));
	TEST_ASSERT(addres.low == UINT64_MAX && addres.high == 0, "Div failed");
	addres = i128_sdiv(i128(UINT64_MAX - 1, UINT64_MAX - 1), i128(1, 0));
	TEST_ASSERTF(addres.low == UINT64_MAX && addres.high == UINT64_MAX, "Div failed %s", i128_to_string(addres, 10, true));
	addres = i128_sdiv(i128(2, 0), i128(UINT64_MAX - 1, UINT64_MAX - 1));
	printf("-- i128 Div okfefe %x.\n", (unsigned)-2);
	TEST_ASSERTF(addres.low == UINT64_MAX && addres.high == UINT64_MAX, "Div failed: %s %llx, %llx", i128_to_string(addres, 10, true), (unsigned long long)addres.high, (unsigned long long)addres.low);
	printf("-- i128 Div ok.\n");



}
void compiler_tests(void)
{
	symtab_init(0x100000);

	test_file();
	test128();
	run_arena_allocator_tests();

	exit_compiler(COMPILER_SUCCESS_EXIT);
}