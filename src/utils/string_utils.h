#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include <stdbool.h>

static inline bool is_lower(char c)
{
	return c >= 'a' && c <= 'z';
}

static inline bool is_upper(char c)
{
	return c >= 'A' && c <= 'Z';
}

static inline bool is_alphanum_(char c)
{
	return (c >= 'a' && c <= 'z')
		|| (c >= 'A' && c <= 'Z')
		|| (c >= '0' && c <= '9')
		|| c == '_';
}