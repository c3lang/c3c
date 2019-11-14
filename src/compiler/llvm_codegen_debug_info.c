// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

void gencontext_set_debug_location(GenContext *context, SourceRange source_range)
{
	if (source_range.loc == INVALID_LOC) return;

	context->debug.current_range = source_range;
#ifdef TODOLATER
	CurLoc = CGM.getContext().getSourceManager().getExpansionLoc(Loc);
#endif
	// If we've changed files in the middle of a lexical scope go ahead
	// and create a new lexical scope with file node if it's different
	// from the one in the scope.
	if (!vec_size(context->debug.lexical_block_stack)) return;

#ifdef TODOLATE
	if (auto *LBF = dyn_cast<llvm::DILexicalBlockFile>(Scope)) {
		LexicalBlockStack.pop_back();
		LexicalBlockStack.emplace_back(DBuilder.createLexicalBlockFile(
				LBF->getScope(), getOrCreateFile(CurLoc)));
	} else if (isa<llvm::DILexicalBlock>(Scope) ||
	           isa<llvm::DISubprogram>(Scope)) {
		LexicalBlockStack.pop_back();
		LexicalBlockStack.emplace_back(
				DBuilder.createLexicalBlockFile(Scope, getOrCreateFile(CurLoc)));
	}
#endif
}

void gencontext_emit_debug_location(GenContext *context, SourceRange location)
{
	gencontext_set_debug_location(context, location);

	if (context->debug.current_range.loc == INVALID_LOC || vec_size(context->debug.lexical_block_stack) == 0) return;

	LLVMMetadataRef scope = VECLAST(context->debug.lexical_block_stack);
	LLVMMetadataRef debug_location = LLVMDIBuilderCreateDebugLocation(context->context, 320, 12, scope, context->debug.inlined_at);
	LLVMSetCurrentDebugLocation2(context->builder, debug_location);
}

static LLVMMetadataRef gencontext_simple_debug_type(GenContext *context, Type *type, int dwarf_code)
{
	return LLVMDIBuilderCreateBasicType(context->debug.builder,
	                                    type->name_loc.string,
	                                    type->name_loc.span.length,
	                                    type->builtin.bitsize,
	                                    dwarf_code, 0);

}

LLVMMetadataRef gencontext_create_builtin_debug_type(GenContext *context, Type *builtin_type)
{
	assert(builtin_type->canonical == builtin_type);
	// Consider special handling of UTF8 arrays.
	switch (builtin_type->type_kind)
	{
		case TYPE_BOOL:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_boolean);
		case TYPE_I8:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_signed_char); // DW_ATE_UTF?
		case TYPE_U8:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_unsigned_char);
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_signed);
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_unsigned);
		case TYPE_F32:
		case TYPE_F64:
			return gencontext_simple_debug_type(context, builtin_type, DW_ATE_float);
		case TYPE_VOID:
			return NULL;
		default:
			UNREACHABLE
	}
}
