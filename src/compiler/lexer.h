#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include "compiler_common.h"

Token lexer_scan_token(void);

Token lexer_scan_ident_test(const char *scan);

void lexer_test_setup(const char *text, size_t len);
void lexer_add_file_for_lexing(File *file);
File* lexer_current_file(void);
void lexer_check_init(void);