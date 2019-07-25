// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "benchmark.h"
#include <time.h>

static int begin = 0;

void bench_begin(void)
{
	begin = clock();
}
double bench_mark(void)
{
	return (clock() - begin) / (double)CLOCKS_PER_SEC;
}