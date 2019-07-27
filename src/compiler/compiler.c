// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler.h"
#include "symtab.h"
#include "../build/build_options.h"
#include "../utils/lib.h"

void compiler_init(void)
{
	uint32_t symtab_size = nextHighestPowerOf2(build_options.symtab_size);
	symtab_init(symtab_size);

}