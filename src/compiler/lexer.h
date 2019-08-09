#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include "compiler_common.h"

extern Token next_tok;
extern Token tok;

Token lexer_scan_token(void);
Token lexer_scan_ident_test(const char *scan);
void lexer_test_setup(const char *text, size_t len);
void lexer_add_file_for_lexing(File *file);
File* lexer_current_file(void);
void lexer_check_init(void);
void lexer_store_state(void);
void lexer_restore_state(void);

static inline void advance(void)
{
	tok = next_tok;
	while (1)
	{
		next_tok = lexer_scan_token();
		// printf(">>> %.*s => %s\n", tok.length, tok.start, token_type_to_string(tok.type));
		if (next_tok.type != TOKEN_INVALID_TOKEN) break;
	}
}

static inline void advance_and_verify(TokenType token_type)
{
	assert(tok.type == token_type);
	advance();
}


