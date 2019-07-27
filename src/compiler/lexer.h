#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "tokens.h"
#include "compiler_common.h"

Token scan_token(void);

TokenType identifier_type(const char* restrict start, int len);
TokenType ident_type_fnv1(const char *restrict start, int len);

Token scan_ident_test(const char* scan);

void lexer_test_setup(const char* text);
