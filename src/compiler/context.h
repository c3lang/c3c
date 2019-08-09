#pragma once
// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_common.h"
#include "ast.h"

typedef struct _Context
{
    Token module_name;
    Token* module_parameters;
    File * file;
    Decl** imports;
    Module *module;
    STable local_symbols;
    Decl **declarations;
    Decl **ct_ifs;
    Decl *active_function_for_analysis;
} Context;

extern Context *current_context;

Context *context_create(File *file);
void context_push(Context *context);

void context_register_global_decl(Context *context, Decl *decl);
bool context_add_import(Context *context, Token module_name, Token alias, ImportType import_type, Expr** generic_parameters);
bool context_set_module_from_filename(Context *context);
bool context_set_module(Context *context, Token module_name, Token *generic_parameters);
void context_print_ast(Context *context, FILE *file);

