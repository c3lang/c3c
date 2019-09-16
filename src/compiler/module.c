// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol)
{
	return stable_get(&module->symbols, symbol);
}

