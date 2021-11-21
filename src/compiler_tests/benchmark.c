// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "benchmark.h"
#include <time.h>

static int begin = 0;

void bench_begin(void)
{
	begin = (int)clock();
}
double bench_mark(void)
{
	return (double)(clock() - (unsigned long)begin) / (double)CLOCKS_PER_SEC;
}
