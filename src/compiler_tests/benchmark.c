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

void bench_begin(void)
{
	begin = clock();
}
double bench_mark(void)
{
	return (clock() - begin) / (double)CLOCKS_PER_SEC;
}

BenchTime benchstart(void)
{
	return clock();
}

double benchmark(BenchTime start)
{
	return (clock() - start) / (double)CLOCKS_PER_SEC;
}

#endif

