// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static inline LLVMMetadataRef gencontext_create_debug_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMMetadataRef debug_params[512];
	switch (decl->decl_kind)
	{
		case DECL_ATTRIBUTE:
		case DECL_ENUM_CONSTANT:
		case DECL_POISONED:
		case DECL_GENERIC:
		case DECL_MACRO:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_VAR:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MEMBER:
		case DECL_LABEL:
			UNREACHABLE;
		case DECL_FUNC:
		{
			VECEACH(decl->func.function_signature.params, i)
			{
				Type *param_type = decl->func.function_signature.params[i]->type;
				debug_params[i + 1] = gencontext_get_debug_type(context, param_type);
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			debug_params[0] = decl->func.function_signature.rtype->type->backend_debug_type;
			return LLVMDIBuilderCreateSubroutineType(context->debug.builder,
			                                                                  context->debug.file,
			                                                                  debug_params, param_size + 1,
					/** TODO **/ 0);
		}
		case DECL_TYPEDEF:
			TODO
		case DECL_STRUCT:
			TODO
		case DECL_UNION:
			TODO
		case DECL_ENUM:
			TODO
		case DECL_ERR:
			TODO
	}
	UNREACHABLE
}

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
	return type->backend_debug_type = LLVMDIBuilderCreateBasicType(context->debug.builder,
	                                                               type->name,
	                                                               strlen(type->name),
	                                                               type->builtin.bitsize,
	                                                               dwarf_code, 0);

}

LLVMMetadataRef gencontext_get_debug_type(GenContext *context, Type *type)
{
	if (type->backend_debug_type) return type->backend_debug_type;
	// Consider special handling of UTF8 arrays.
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEID:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_BOOL:
			return gencontext_simple_debug_type(context, type, DW_ATE_boolean);
		case TYPE_I8:
			return gencontext_simple_debug_type(context, type, DW_ATE_signed_char); // DW_ATE_UTF?
		case TYPE_U8:
			return gencontext_simple_debug_type(context, type, DW_ATE_unsigned_char);
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return gencontext_simple_debug_type(context, type, DW_ATE_signed);
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return gencontext_simple_debug_type(context, type, DW_ATE_unsigned);
		case TYPE_F32:
		case TYPE_F64:
			return gencontext_simple_debug_type(context, type, DW_ATE_float);
		case TYPE_VOID:
			return NULL;
		case TYPE_POINTER:
			return type->backend_debug_type = LLVMDIBuilderCreatePointerType(context->debug.builder, type->pointer->backend_debug_type, type_size(type->canonical->pointer), 0, 0, type->name, strlen(type->name));
		case TYPE_ENUM:
			TODO
		case TYPE_ERRTYPE:
			TODO
		case TYPE_FUNC:
			// TODO
			return NULL;
		case TYPE_STRUCT:
//			LLVMDIBuilderCreateStructType(context->debug.builder, NULL, type->decl->name, strlen(type->decl->name), type->decl->module->)
			TODO
		case TYPE_UNION:
			TODO
		case TYPE_TYPEDEF:
			TODO
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
			{
				LLVMMetadataRef *ranges = NULL;
				Type *current_type = type;
				while (current_type->canonical->type_kind == TYPE_ARRAY)
				{
					VECADD(ranges, LLVMDIBuilderGetOrCreateSubrange(context->debug.builder, 0, current_type->canonical->array.len));
					current_type = current_type->canonical->array.base;
				}
				return type->backend_debug_type = LLVMDIBuilderCreateArrayType(
						context->debug.builder,
						type->array.len,
						0 /* ALIGN */,
						type->array.base->backend_debug_type,
						ranges, vec_size(ranges));
			}
		case TYPE_VARARRAY:
			TODO
		case TYPE_SUBARRAY:
			TODO
		case TYPE_ERR_UNION:
			TODO
	}
	UNREACHABLE
}
