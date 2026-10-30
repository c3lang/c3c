// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "utils/common.h"
#include "benchmark.h"
#include <time.h>

BenchTime begin;

#if USE_PTHREAD


void bench_begin(void)
{
	clock_gettime(CLOCK_MONOTONIC, &begin);
}


double bench_mark(void)
{
	return benchmark(begin);
}

BenchTime benchstart(void)
{
	BenchTime res;
	clock_gettime(CLOCK_MONOTONIC, &res);
	return res;
}

double benchmark(BenchTime start)
{
	BenchTime res;
	clock_gettime(CLOCK_MONOTONIC, &res);
	double elapsed = (res.tv_sec - start.tv_sec);
	elapsed += (res.tv_nsec - start.tv_nsec) / 1000000000.0;
	return elapsed;
}


#else

#if PLATFORM_WINDOWS
#include <windows.h>
// A monotonic wall clock, clock() would be the process CPU time on some platforms.
static BenchTime bench_now(void)
{
	LARGE_INTEGER counter;
	QueryPerformanceCounter(&counter);
	return counter.QuadPart;
}

static double bench_frequency(void)
{
	LARGE_INTEGER frequency;
	QueryPerformanceFrequency(&frequency);
	return (double)frequency.QuadPart;
}
#else
static BenchTime bench_now(void)
{
	return (BenchTime)clock();
}

static double bench_frequency(void)
{
	return (double)CLOCKS_PER_SEC;
}
#endif

void bench_begin(void)
{
	begin = bench_now();
}
double bench_mark(void)
{
	return (double)(bench_now() - begin) / bench_frequency();
}

BenchTime benchstart(void)
{
	return bench_now();
}

double benchmark(BenchTime start)
{
	return (double)(bench_now() - start) / bench_frequency();
}

#endif

