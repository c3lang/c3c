#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_common.h"
#include "tokens.h"

typedef enum _Precedence
{
	PREC_NONE,
	PREC_ASSIGNMENT,        // =, *=, /=, %=, ...
	PREC_CONDITIONAL,       // ?:
	PREC_LOGICAL,           // && ||
	PREC_RELATIONAL,        // < > <= >= == !=
	PREC_ADDITIVE,          // + -
	PREC_BIT,               // ^ | &
	PREC_SHIFT,             // << >> >>>
	PREC_MULTIPLICATIVE,    // * / %
	PREC_UNARY,             // ! - + ~ * & prefix ++/--
	PREC_CALL,              // . () [] postfix ++/--
} Precedence;

void parse_file(File *file);