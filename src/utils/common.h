#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

#define NO_ARENA 0
#define MAX_VECTOR_WIDTH 65536
#define MAX_ARRAY_SIZE INT64_MAX
#define MAX_SOURCE_LOCATION_LEN 255
#define PROJECT_JSON "project.json"

#if defined( _WIN32 ) || defined( __WIN32__ ) || defined( _WIN64 )
#define PLATFORM_WINDOWS 1
#define PLATFORM_POSIX 0
#else
#define PLATFORM_WINDOWS 0
#define PLATFORM_POSIX 1
#endif

#ifndef USE_PTHREAD
#if PLATFORM_POSIX
#define USE_PTHREAD 1
#else
#define USE_PTHREAD 0
#endif
#endif

#if CURL_FOUND || PLATFORM_WINDOWS
#define FETCH_AVAILABLE 1
#else
#define FETCH_AVAILABLE 0
#endif

#define IS_GCC 0
#define IS_CLANG 0
#ifdef __GNUC__
#ifdef __clang__
#undef IS_CLANG
#define IS_CLANG 1
#else
#undef IS_GCC
#define IS_GCC 1
#endif
#endif

#ifndef __printflike
#define __printflike(x, y)
#endif

#ifndef __unused
#define __unused
#endif


#if (defined(__GNUC__) && __GNUC__ >= 7) || defined(__clang__)
#define PACK( __Declaration__ ) __Declaration__ __attribute__((__packed__))
#define FALLTHROUGH __attribute__ ((fallthrough))
#define UNUSED __attribute__((unused))
#define NORETURN __attribute__((noreturn))
#define INLINE __attribute__((always_inline)) static inline
#elif defined(_MSC_VER)
#define FALLTHROUGH ((void)0)
#define INLINE static __forceinline
#define NORETURN __declspec(noreturn)
#define UNUSED
#define PACK( __Declaration__ ) __pragma( pack(push, 1) ) __Declaration__ __pragma( pack(pop))
#else
#define PACK(__Declaration__) __Declaration__
#define INLINE static inline
#define FALLTHROUGH ((void)0)
#define UNUSED
#define NORETURN
#endif

#define INFO_LOG(_string, ...) \
  do {                          \
	if (!debug_log) break; \
	printf("-- INFO: "); printf(_string, ##__VA_ARGS__); printf("\n"); \
  } while (0)
#ifdef NDEBUG
#define REMINDER(_string, ...) do {} while (0)
#define DEBUG_LOG(_string, ...) do {} while(0)
#else
#define REMINDER(_string, ...) do { if (!debug_log) break; printf("TODO: %s -> in %s @ %s:%d\n", _string, __func__, __FILE__, __LINE__ , ##__VA_ARGS__); } while(0)
#define DEBUG_LOG(_string, ...) \
  do {                          \
	if (!debug_log) break; \
	printf("-- DEBUG: "); printf(_string, ##__VA_ARGS__); printf("\n"); \
  } while (0)
#endif

#define FATAL_ERROR(_string, ...) do { error_exit("FATAL ERROR %s -> in %s @ in %s:%d ", _string, __func__, __FILE__, __LINE__, ##__VA_ARGS__); } while(0)

#define ASSERT(_condition, _string, ...) while (!(_condition)) { FATAL_ERROR(_string, ##__VA_ARGS__); }
#define WARNING(_string, ...) do { eprintf("WARNING: "); eprintf(_string, ##__VA_ARGS__); eprintf("\n"); } while(0)
#define UNREACHABLE FATAL_ERROR("Should be unreachable");

#define TODO FATAL_ERROR("TODO reached");
#define UNSUPPORTED do { error_exit("Unsupported functionality"); } while (0)

#define TEST_ASSERT(condition_, string_) while (!(condition_)) { FATAL_ERROR(string_); }
#define TEST_ASSERTF(condition_, string_, ...) while (!(condition_)) { char* str_ = str_printf(string_, __VA_ARGS__); FATAL_ERROR(str_); }

#define EXPECT(_string, _value, _expected) \
 do { long long __tempval1 = _value; long long __tempval2 = _expected; \
	TEST_ASSERT(__tempval1 == __tempval2, "Checking " _string ": expected %lld but was %lld.", __tempval2, __tempval1); } while(0)

void evprintf(const char *format, va_list list);
void eprintf(const char *format, ...);
NORETURN void error_exit(const char *format, ...) ;
