#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_common.h"

void diag_reset(void);
void diag_error_range(SourceRange span, const char *message, ...);
void diag_verror_range(SourceRange span, const char *message, va_list args);
void sema_error_at(SourceLoc loc, const char *message, ...);
void sema_error_range(SourceRange range, const char *message, ...);
void sema_verror_at(SourceLoc loc, const char *message, va_list args);
void sema_verror_range(SourceRange range, const char *message, va_list args);
void sema_error(const char *message, ...);
void sema_prev_at_range(SourceRange span, const char *message, ...);
void sema_prev_at(SourceLoc loc, const char *message, ...);

#define SEMA_ERROR(_tok, ...) sema_error_range(_tok.span, __VA_ARGS__)

/*


typedef struct _Array Array;

void diagnostics_init(void);
void diagnostics_reset(void);
void diagnostics_update_severity(DiagnosticsSeverity severity, DiagnosticsType type);
bool diagnostics_silence_warnings(Array *warnings);
void diagnostics_use_color(bool use_color);
void verror_at(SourceRange span, const char *message, va_list args);
void sema_error_range(SourceRange token, const char *message, ...);
void sema_error_at(SourceLoc loc, const char *message, ...);
void prev_at_range(SourceRange span, const char *message, ...);
void prev_at(SourceLoc loc, const char *message, ...);
void sema_warn_at(DiagnosticsType type, SourceLoc loc, const char *message, ...);
void sema_warn_range(DiagnosticsType type, SourceRange span, const char *message, ...);
bool in_panic_mode(void);
unsigned errors();
void reset_panic_mode(void);
bool error_found(void);
*/