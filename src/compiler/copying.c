#include "compiler_internal.h"

#define SCOPE_FIXUP_START do { CopyFixup *current = c->current_fixup;
#define SCOPE_FIXUP_END c->current_fixup = current; } while (0)

static inline void copy_const_initializer(CopyStruct *c, ConstInitializer **initializer_ref);
static inline void copy_reg_ref(CopyStruct *c, void *original, void *result);
static inline void *fixup(CopyStruct *c, void *original);
INLINE void fixup_decl(CopyStruct *c, Decl **decl_ref);
INLINE void fixup_declid(CopyStruct *c, DeclId *declid_ref);
INLINE ConstInitializer **copy_const_initializer_list(CopyStruct *c, ConstInitializer **initializer_list);
INLINE ConstInitializer **copy_const_initializer_array(CopyStruct *c, ConstInitializer **initializer_list, unsigned len);

static Expr **copy_expr_list(CopyStruct *c, Expr **expr_list);
static Expr *copy_expr(CopyStruct *c, Expr *source_expr);
static Ast *ast_copy_deep(CopyStruct *c, Ast *source);
static Ast **copy_ast_list(CopyStruct *c, Ast **to_copy);
static Decl *copy_decl(CopyStruct *c, Decl *decl);
static Decl **copy_decl_list(CopyStruct *c, Decl **decl_list);
static TypeInfo *copy_type_info(CopyStruct *c, TypeInfo *source);

static inline void copy_reg_ref(CopyStruct *c, void *original, void *result)
{
	c->current_fixup->new_ptr = result;
	c->current_fixup->original = original;
	c->current_fixup++;
	if (c->current_fixup == &c->fixups[MAX_FIXUPS])
	{
		error_exit("Too many fix-ups for macros.");
	}
}

static inline void *fixup(CopyStruct *c, void *original)
{
	CopyFixup *fixup_entry = c->current_fixup;
	CopyFixup *first = &c->fixups[0];
	while (fixup_entry != first)
	{
		fixup_entry--;
		if (original == fixup_entry->original)
		{
			return fixup_entry->new_ptr;
		}
	}
	return NULL;
}

INLINE void fixup_decl(CopyStruct *c, Decl **decl_ref)
{
	Decl *new_decl = fixup(c, *decl_ref);
	if (new_decl) *decl_ref = new_decl;
}

INLINE void fixup_declid(CopyStruct *c, DeclId *declid_ref)
{
	DeclId id = *declid_ref;
	if (!id) return;
	Decl *decl = declptr(id);
	decl = fixup(c, decl);
	if (decl) *declid_ref = declid(decl);
}

INLINE void fixup_astid(CopyStruct *c, AstId *astid_ref)
{
	AstId id = *astid_ref;
	if (!id) return;
	Ast *ast = astptr(id);
	ast = fixup(c, ast);
	if (ast) *astid_ref = astid(ast);
}

Expr **copy_expr_list(CopyStruct *c, Expr **expr_list)
{
	Expr **result = NULL;
	VECEACH(expr_list, i)
	{
		vec_add(result, copy_expr(c, expr_list[i]));
	}
	return result;
}

static inline DeclId decl_copy_label_from_macro(CopyStruct *c, DeclId to_copy, Ast *ast)
{
	if (!to_copy) return (DeclId)0;
	Decl *copy = copy_decl(c, declptr(to_copy));
	copy->label.parent = astid(ast);
	return declid(copy);
}


static inline void copy_flow(CopyStruct *c, Ast *ast)
{
	ast->flow.label = decl_copy_label_from_macro(c,ast->flow.label, ast);
}

static TypeInfo** type_info_copy_list_from_macro(CopyStruct *c, TypeInfo **to_copy)
{
	TypeInfo **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, copy_type_info(c, to_copy[i]));
	}
	return result;
}

INLINE TypeInfoId type_info_id_copy_deep(CopyStruct *c, TypeInfoId source)
{
	if (!source) return 0;
	return type_infoid(copy_type_info(c, type_infoptr(source)));
}

INLINE AstId astid_copy_deep(CopyStruct *c, AstId source)
{
	if (!source) return 0;
	return astid(ast_copy_deep(c, astptr(source)));
}

INLINE ExprId exprid_copy_deep(CopyStruct *c, ExprId source)
{
	if (!source) return 0;
	return exprid(copy_expr(c, exprptr(source)));
}

INLINE DeclId declid_copy_deep(CopyStruct *c, DeclId source)
{
	if (!source) return 0;
	return declid(copy_decl(c, declptr(source)));
}


static DesignatorElement **macro_copy_designator_list(CopyStruct *c, DesignatorElement **list)
{
	DesignatorElement **result = NULL;
	VECEACH(list, i)
	{
		DesignatorElement *element = MALLOCS(DesignatorElement);
		DesignatorElement *to_copy = list[i];
		*element = *to_copy;
		switch (to_copy->kind)
		{
			case DESIGNATOR_FIELD:
				MACRO_COPY_EXPR(element->field_expr);
				// Nothing needed
				break;
			case DESIGNATOR_RANGE:
				MACRO_COPY_EXPR(element->index_end_expr);
				FALLTHROUGH;
			case DESIGNATOR_ARRAY:
				MACRO_COPY_EXPR(element->index_expr);
				break;
			default:
				UNREACHABLE
		}
		vec_add(result, element);
	}
	return result;
}

static CopyStruct copy_struct;

Ast *copy_ast_single(Ast *source_ast)
{
	copy_begin();
	Ast *ast = ast_copy_deep(&copy_struct, source_ast);
	copy_end();
	return ast;
}

TypeInfo *copy_type_info_single(TypeInfo *type_info)
{
	copy_begin();
	TypeInfo *type_info_copy = copy_type_info(&copy_struct, type_info);
	copy_end();
	return type_info_copy;
}

Ast *copy_ast_macro(Ast *source_ast)
{
	assert(copy_struct.copy_in_use);
	return ast_copy_deep(&copy_struct, source_ast);
}

Ast *copy_ast_defer(Ast *source_ast)
{
	copy_begin();
	copy_struct.single_static = true;
	Ast *ast = copy_ast_macro(source_ast);
	copy_end();
	return ast;
}

Expr *copy_expr_single(Expr *source_expr)
{
	copy_begin();
	Expr *expr = copy_expr(&copy_struct, source_expr);
	copy_end();
	return expr;
}

void copy_range(CopyStruct *c, Range *range)
{
	MACRO_COPY_EXPRID(range->start);
	MACRO_COPY_EXPRID(range->end);
}

INLINE ConstInitializer **copy_const_initializer_list(CopyStruct *c, ConstInitializer **initializer_list)
{
	ConstInitializer **initializer = NULL;
	FOREACH_BEGIN(ConstInitializer *element, initializer_list)
		copy_const_initializer(c, &element);
		vec_add(initializer, element);
	FOREACH_END();
	return initializer;
}

INLINE ConstInitializer **copy_const_initializer_array(CopyStruct *c, ConstInitializer **initializer_list, unsigned len)
{
	ConstInitializer **initializer = MALLOC(sizeof(ConstInitializer*) * len);
	for (unsigned i = 0; i < len; i++)
	{
		ConstInitializer *element = initializer_list[i];
		copy_const_initializer(c, &element);
		initializer[i] = element;
	}
	return initializer;
}

static inline void copy_const_initializer(CopyStruct *c, ConstInitializer **initializer_ref)
{
	ConstInitializer *copy = MALLOCS(ConstInitializer);
	*copy = **initializer_ref;
	*initializer_ref = copy;

	switch (copy->kind)
	{
		case CONST_INIT_ZERO:
			return;
		case CONST_INIT_STRUCT:
			copy->init_struct = copy_const_initializer_array(c, copy->init_struct, vec_size(type_flatten(copy->type)->decl->strukt.members));
			return;
		case CONST_INIT_UNION:
			copy_const_initializer(c, &copy->init_union.element);
			return;
		case CONST_INIT_VALUE:
			copy->init_value = copy_expr(c, copy->init_value);
			return;
		case CONST_INIT_ARRAY:
			copy->init_array.elements = copy_const_initializer_list(c, copy->init_array.elements);
			return;
		case CONST_INIT_ARRAY_FULL:
			copy->init_array_full = copy_const_initializer_list(c, copy->init_array_full);
			return;
		case CONST_INIT_ARRAY_VALUE:
			copy_const_initializer(c, &copy->init_array_value.element);
			return;
	}
	UNREACHABLE
}

INLINE Expr *copy_const_expr(CopyStruct *c, Expr *expr)
{
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
			break;
		case CONST_ENUM:
		case CONST_ERR:
			fixup_decl(c, &expr->const_expr.enum_err_val);
			break;
		case CONST_BYTES:
		case CONST_STRING:
			// Assume this is never modified.
			break;
		case CONST_POINTER:
		case CONST_TYPEID:
			break;
		case CONST_INITIALIZER:
			copy_const_initializer(c, &expr->const_expr.initializer);
			break;
		case CONST_UNTYPED_LIST:
			expr->const_expr.untyped_list = copy_expr_list(c, expr->const_expr.untyped_list);
			break;
		case CONST_MEMBER:
			fixup_decl(c, &expr->const_expr.member.decl);
			break;
	}
	return expr;
}
Expr *copy_expr(CopyStruct *c, Expr *source_expr)
{
	if (!source_expr) return NULL;
	Expr *expr = expr_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_ANYSWITCH:
			UNREACHABLE
		case EXPR_OTHER_CONTEXT:
			MACRO_COPY_EXPR(expr->expr_other_context.inner);
			return expr;
		case EXPR_EMBED:
			MACRO_COPY_EXPR(expr->embed_expr.len);
			MACRO_COPY_EXPR(expr->embed_expr.filename);
			return expr;
		case EXPR_GENERIC_IDENT:
			MACRO_COPY_EXPRID(expr->generic_ident_expr.parent);
			MACRO_COPY_EXPR_LIST(expr->generic_ident_expr.parmeters);
			return expr;
		case EXPR_MACRO_BODY_EXPANSION:
			MACRO_COPY_EXPR_LIST(expr->body_expansion_expr.values);
			MACRO_COPY_DECL_LIST(expr->body_expansion_expr.declarations);
			MACRO_COPY_ASTID(expr->body_expansion_expr.first_stmt);
			return expr;
		case EXPR_MACRO_BODY:
			MACRO_COPY_AST(expr->macro_body_expr.body);
			MACRO_COPY_DECL_LIST(expr->macro_body_expr.body_arguments);
			return expr;
		case EXPR_LAMBDA:
			if (copy_struct.is_template)
			{
				MACRO_COPY_DECL(expr->lambda_expr);
			}
			return expr;
		case EXPR_SWIZZLE:
			MACRO_COPY_EXPRID(expr->swizzle_expr.parent);
			return expr;
		case EXPR_NOP:
		case EXPR_BUILTIN:
		case EXPR_RETVAL:
		case EXPR_OPERATOR_CHARS:
			return expr;
		case EXPR_VASPLAT:
			copy_range(c, &expr->vasplat_expr);
			return expr;
		case EXPR_CT_ARG:
			MACRO_COPY_EXPRID(expr->ct_arg_expr.arg);
			return expr;
		case EXPR_POINTER_OFFSET:
			MACRO_COPY_EXPRID(expr->pointer_offset_expr.offset);
			MACRO_COPY_EXPRID(expr->pointer_offset_expr.ptr);
			return expr;
		case EXPR_BUILTIN_ACCESS:
			MACRO_COPY_EXPRID(expr->builtin_access_expr.inner);
			return expr;
		case EXPR_DECL:
			MACRO_COPY_DECL(expr->decl_expr);
			return expr;
		case EXPR_CT_CALL:
			MACRO_COPY_EXPR(expr->ct_call_expr.main_var);
			return expr;
		case EXPR_TRY_UNWRAP:
			if (expr->resolve_status != RESOLVE_DONE)
			{
				MACRO_COPY_EXPR(expr->try_unwrap_expr.init);
				MACRO_COPY_TYPE(expr->try_unwrap_expr.type);
			}
			else
			{
				MACRO_COPY_EXPR(expr->try_unwrap_expr.optional);
				if (expr->try_unwrap_expr.assign_existing)
				{
					MACRO_COPY_EXPR(expr->try_unwrap_expr.lhs);
				}
				else
				{
					MACRO_COPY_DECL(expr->try_unwrap_expr.decl);
				}
			}
			return expr;
		case EXPR_TRY_UNWRAP_CHAIN:
			MACRO_COPY_EXPR_LIST(expr->try_unwrap_chain_expr);
			return expr;
		case EXPR_CATCH_UNWRAP:
			MACRO_COPY_EXPR_LIST(expr->catch_unwrap_expr.exprs);
			MACRO_COPY_TYPE(expr->catch_unwrap_expr.type);
			return expr;
		case EXPR_IDENTIFIER:
			if (expr->resolve_status == RESOLVE_DONE)
			{
				fixup_decl(c, &expr->identifier_expr.decl);
			}
			return expr;
		case EXPR_CT_IDENT:
		case EXPR_HASH_IDENT:
			assert(expr->resolve_status != RESOLVE_DONE);
			return expr;
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		case EXPR_COMPILER_CONST:
			return expr;
		case EXPR_DESIGNATOR:
			expr->designator_expr.path = macro_copy_designator_list(c, expr->designator_expr.path);
			MACRO_COPY_EXPR(expr->designator_expr.value);
			return expr;
		case EXPR_TYPEINFO:
			MACRO_COPY_TYPE(expr->type_expr);
			return expr;
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
			MACRO_COPY_EXPRID(expr->slice_assign_expr.left);
			MACRO_COPY_EXPRID(expr->slice_assign_expr.right);
			return expr;
		case EXPR_SUBSCRIPT_ASSIGN:
			UNREACHABLE
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			MACRO_COPY_EXPRID(expr->subscript_expr.expr);
			copy_range(c, &expr->subscript_expr.range);
			return expr;
		case EXPR_ASM:
			switch (expr->expr_asm_arg.kind)
			{
				case ASM_ARG_REG:
				case ASM_ARG_ADDROF:
				case ASM_ARG_REGVAR:
				case ASM_ARG_INT:
				case ASM_ARG_MEMVAR:
					return expr;
				case ASM_ARG_VALUE:
				case ASM_ARG_ADDR:
					MACRO_COPY_EXPRID(expr->expr_asm_arg.expr_id);
					return expr;
			}
			UNREACHABLE
		case EXPR_CT_CASTABLE:
			MACRO_COPY_EXPRID(expr->castable_expr.expr);
			MACRO_COPY_TYPEID(expr->castable_expr.type);
			return expr;
		case EXPR_CT_AND_OR:
			MACRO_COPY_EXPR_LIST(expr->ct_and_or_expr.args);
			return expr;
		case EXPR_FORCE_UNWRAP:
		case EXPR_OPTIONAL:
		case EXPR_GROUP:
		case EXPR_STRINGIFY:
		case EXPR_CT_EVAL:
		case EXPR_CT_IS_CONST:
			MACRO_COPY_EXPR(expr->inner_expr);
			return expr;
		case EXPR_CT_DEFINED:
			MACRO_COPY_EXPR_LIST(expr->expression_list);
			return expr;
		case EXPR_TYPEID_INFO:
			MACRO_COPY_EXPRID(expr->typeid_info_expr.parent);
			return expr;
		case EXPR_COND:
			MACRO_COPY_EXPR_LIST(expr->cond_expr);
			return expr;
		case EXPR_MACRO_BLOCK:
			MACRO_COPY_DECL_LIST(expr->macro_block.params);
			MACRO_COPY_ASTID(expr->macro_block.first_stmt);
			MACRO_COPY_DECL(expr->macro_block.macro);
			return expr;
		case EXPR_COMPOUND_LITERAL:
			MACRO_COPY_EXPR(expr->expr_compound_literal.initializer);
			MACRO_COPY_TYPE(expr->expr_compound_literal.type_info);
			return expr;
		case EXPR_EXPR_BLOCK:
			MACRO_COPY_ASTID(expr->expr_block.first_stmt);
			return expr;
		case EXPR_POISONED:
			return source_expr;
		case EXPR_RETHROW:
			MACRO_COPY_EXPR(expr->rethrow_expr.inner);
			return expr;
		case EXPR_CONST:
			return copy_const_expr(c, expr);
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
			MACRO_COPY_EXPRID(expr->binary_expr.left);
			MACRO_COPY_EXPRID(expr->binary_expr.right);
			return expr;
		case EXPR_TERNARY:
			MACRO_COPY_EXPRID(expr->ternary_expr.cond);
			MACRO_COPY_EXPRID(expr->ternary_expr.then_expr);
			MACRO_COPY_EXPRID(expr->ternary_expr.else_expr);
			return expr;
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			MACRO_COPY_EXPR(expr->unary_expr.expr);
			return expr;
		case EXPR_TYPEID:
			MACRO_COPY_TYPE(expr->typeid_expr);
			return expr;
		case EXPR_CALL:
			if (expr->call_expr.is_func_ref)
			{
				fixup_declid(c, &expr->call_expr.func_ref);
			}
			else
			{

				MACRO_COPY_EXPRID(expr->call_expr.function);
			}
			MACRO_COPY_EXPRID(expr->call_expr.macro_body);
			MACRO_COPY_EXPR_LIST(expr->call_expr.arguments);
			if (expr->call_expr.varargs)
			{
				if (expr->call_expr.splat_vararg)
				{
					MACRO_COPY_EXPR(expr->call_expr.splat);
				}
				else
				{
					MACRO_COPY_EXPR_LIST(expr->call_expr.varargs);
				}
			}
			return expr;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			MACRO_COPY_EXPR(expr->access_expr.parent);
			if (expr->resolve_status == RESOLVE_DONE)
			{
				fixup_decl(c, &expr->access_expr.ref);
			}
			else
			{
				MACRO_COPY_EXPR(expr->access_expr.child);
			}
			return expr;
		case EXPR_INITIALIZER_LIST:
			MACRO_COPY_EXPR_LIST(expr->initializer_list);
			return expr;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			MACRO_COPY_EXPR_LIST(expr->designated_init_list);
			return expr;
		case EXPR_EXPRESSION_LIST:
			MACRO_COPY_EXPR_LIST(expr->expression_list);
			return expr;
		case EXPR_CAST:
			MACRO_COPY_EXPRID(expr->cast_expr.expr);
			MACRO_COPY_TYPEID(expr->cast_expr.type_info);
			return expr;
	}
	UNREACHABLE
}

void doc_ast_copy(CopyStruct *c, AstContractStmt *doc)
{
	switch (doc->kind)
	{
		case CONTRACT_REQUIRE:
		case CONTRACT_ENSURE:
			MACRO_COPY_EXPR(doc->contract.decl_exprs);
			break;
		case CONTRACT_OPTIONALS:
			MACRO_COPY_AST_LIST(doc->faults);
			break;
		case CONTRACT_PARAM:
		case CONTRACT_PURE:
		case CONTRACT_UNKNOWN:
			break;
	}
}

Ast *ast_copy_deep(CopyStruct *c, Ast *source)
{
	if (!source) return NULL;
	Ast *ast = ast_copy(source);
	Ast *first_return = ast;
	AstId *assign_to = NULL;
RETRY:
	if (assign_to)
	{
		ast = ast_copy(source);
		*assign_to = astid(ast);
	}
	switch (source->ast_kind)
	{
		case AST_POISONED:
			break;
		case AST_DECLS_STMT:
			MACRO_COPY_DECL_LIST(ast->decls_stmt);
			break;
		case AST_CONTRACT_FAULT:
			if (ast->contract_fault.resolved)
			{
				MACRO_COPY_DECL(ast->contract_fault.decl);
			}
			else
			{
				MACRO_COPY_TYPE(ast->contract_fault.type);
			}
			break;
		case AST_CONTRACT:
			doc_ast_copy(c, &source->contract_stmt);
			break;
		case AST_ASM_BLOCK_STMT:
			if (ast->asm_block_stmt.is_string)
			{
				MACRO_COPY_EXPRID(ast->asm_block_stmt.asm_string);
			}
			else
			{
				AsmInlineBlock *block = MALLOCS(AsmInlineBlock);
				*block = *ast->asm_block_stmt.block;
				ast->asm_block_stmt.block = block;
				MACRO_COPY_ASTID(block->asm_stmt);
			}
			break;
		case AST_ASM_STMT:
			MACRO_COPY_EXPR_LIST(ast->asm_stmt.args);
			break;
		case AST_ASSERT_STMT:
		case AST_CT_ASSERT:
			MACRO_COPY_EXPRID(ast->assert_stmt.expr);
			MACRO_COPY_EXPRID(ast->assert_stmt.message);
			MACRO_COPY_EXPR_LIST(ast->assert_stmt.args);
			break;
		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			if (ast->contbreak_stmt.is_resolved)
			{
				fixup_astid(c, &ast->contbreak_stmt.ast);
			}
			break;
		case AST_CASE_STMT:
			copy_reg_ref(c, source, ast);
			MACRO_COPY_AST(ast->case_stmt.body);
			MACRO_COPY_EXPRID(ast->case_stmt.expr);
			MACRO_COPY_EXPRID(ast->case_stmt.to_expr);
			break;
		case AST_COMPOUND_STMT:
			MACRO_COPY_ASTID(ast->compound_stmt.first_stmt);
			break;
		case AST_CT_IF_STMT:
			MACRO_COPY_EXPR(ast->ct_if_stmt.expr);
			MACRO_COPY_ASTID(ast->ct_if_stmt.elif);
			MACRO_COPY_ASTID(ast->ct_if_stmt.then);
			break;
		case AST_CT_ELSE_STMT:
			MACRO_COPY_ASTID(ast->ct_else_stmt);
			break;
		case AST_CT_FOREACH_STMT:
			MACRO_COPY_DECLID(ast->ct_foreach_stmt.index);
			MACRO_COPY_DECLID(ast->ct_foreach_stmt.value);
			MACRO_COPY_ASTID(ast->ct_foreach_stmt.body);
			MACRO_COPY_EXPRID(ast->ct_foreach_stmt.expr);
			break;
		case AST_CT_SWITCH_STMT:
			MACRO_COPY_EXPRID(ast->ct_switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->ct_switch_stmt.body);
			break;
		case AST_DECLARE_STMT:
			MACRO_COPY_DECL(ast->declare_stmt);
			break;
		case AST_DEFAULT_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			break;
		case AST_DEFER_STMT:
			MACRO_COPY_ASTID(ast->defer_stmt.body);
			copy_reg_ref(c, source, ast);
			fixup_astid(c, &ast->defer_stmt.prev_defer);
			break;
		case AST_CT_ECHO_STMT:
		case AST_EXPR_STMT:
			MACRO_COPY_EXPR(ast->expr_stmt);
			break;
		case AST_FOR_STMT:
		case AST_CT_FOR_STMT:
			copy_reg_ref(c, source, ast);
			SCOPE_FIXUP_START
				copy_flow(c, ast);
				MACRO_COPY_EXPRID(ast->for_stmt.init);
				MACRO_COPY_EXPRID(ast->for_stmt.cond);
				MACRO_COPY_ASTID(ast->for_stmt.body);
				MACRO_COPY_EXPRID(ast->for_stmt.incr);
			SCOPE_FIXUP_END;
			break;
		case AST_FOREACH_STMT:
			copy_reg_ref(c, source, ast);
			SCOPE_FIXUP_START
				copy_flow(c, ast);
				MACRO_COPY_EXPRID(ast->foreach_stmt.enumeration);
				MACRO_COPY_DECLID(ast->foreach_stmt.index);
				MACRO_COPY_DECLID(ast->foreach_stmt.variable);
				MACRO_COPY_ASTID(ast->foreach_stmt.body);
			SCOPE_FIXUP_END;
			break;
		case AST_IF_STMT:
			// We don't scope "if" for the simple reason that it might introduce a wrapper alias.
			copy_reg_ref(c, source, ast);
			copy_flow(c, ast);
			MACRO_COPY_EXPRID(ast->if_stmt.cond);
			MACRO_COPY_ASTID(ast->if_stmt.else_body);
			MACRO_COPY_ASTID(ast->if_stmt.then_body);
			break;
		case AST_NEXTCASE_STMT:
			MACRO_COPY_EXPRID(ast->nextcase_stmt.expr);
			break;
		case AST_NOP_STMT:
			break;
		case AST_BLOCK_EXIT_STMT:
		case AST_RETURN_STMT:
			MACRO_COPY_EXPR(ast->return_stmt.expr);
			MACRO_COPY_ASTID(ast->return_stmt.cleanup);
			MACRO_COPY_ASTID(ast->return_stmt.cleanup_fail);
			break;
		case AST_SWITCH_STMT:
		case AST_IF_CATCH_SWITCH_STMT:
			copy_reg_ref(c, source, ast);
			SCOPE_FIXUP_START
				copy_flow(c, ast);
				MACRO_COPY_EXPRID(ast->switch_stmt.cond);
				MACRO_COPY_AST_LIST(ast->switch_stmt.cases);
			SCOPE_FIXUP_END;
			break;
	}
	assign_to = &ast->next;
	AstId next = *assign_to;
	if (!next) return first_return;
	source = astptr(next);
	goto RETRY;
}


Ast **copy_ast_list(CopyStruct *c, Ast **to_copy)
{
	Ast **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, ast_copy_deep(c, to_copy[i]));
	}
	return result;
}

void copy_begin(void)
{
	copy_struct.current_fixup = copy_struct.fixups;
	assert(!copy_struct.copy_in_use);
	copy_struct.copy_in_use = true;
	copy_struct.single_static = false;
}

void copy_end(void)
{
	assert(copy_struct.copy_in_use);
	copy_struct.copy_in_use = false;
}

Decl **copy_decl_list_macro(Decl **decl_list)
{
	assert(copy_struct.copy_in_use);
	return copy_decl_list(&copy_struct, decl_list);
}

Decl **copy_decl_list_single(Decl **decl_list)
{
	copy_begin();
	Decl **result = copy_decl_list_macro(decl_list);
	copy_end();
	return result;
}

static Attr **copy_attributes(CopyStruct *c, Attr** attr_list)
{
	if (!attr_list) return attr_list;
	Attr** list = NULL;
	VECEACH(attr_list, i)
	{
		Attr *attribute = attr_list[i];
		Attr *copy = MALLOCS(Attr);
		*copy = *attribute;
		MACRO_COPY_EXPR_LIST(copy->exprs);
		vec_add(list, copy);
	}
	return list;
}

Attr **copy_attributes_single(Attr** attr_list)
{
	copy_begin();
	Attr **attrs = copy_attributes(&copy_struct, attr_list);
	copy_end();
	return attrs;
}

Decl **copy_decl_list_single_for_unit(Decl **decl_list)
{
	bool old_is_template = copy_struct.is_template;
	copy_struct.is_template = true;
	copy_begin();
	Decl **result = copy_decl_list_macro(decl_list);
	copy_end();
	copy_struct.is_template = old_is_template;
	return result;
}

Decl *copy_lambda_deep(Decl *decl)
{
	bool old_is_template = copy_struct.is_template;
	copy_struct.is_template = true;
	copy_begin();
	Decl *result = copy_decl(&copy_struct, decl);
	copy_end();
	copy_struct.is_template = old_is_template;
	return result;
}

Decl **copy_decl_list(CopyStruct *c, Decl **decl_list)
{
	Decl **result = NULL;
	VECEACH(decl_list, i)
	{
		vec_add(result, copy_decl(c, decl_list[i]));
	}
	return result;
}

TypeInfo *copy_type_info(CopyStruct *c, TypeInfo *source)
{
	if (!source) return NULL;
	TypeInfo *copy = type_info_copy(source);
	if (source->resolve_status == RESOLVE_DONE) return copy;
	switch (source->kind)
	{
		case TYPE_INFO_POISON:
			return copy;
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
			return copy;
		case TYPE_INFO_GENERIC:
			copy->generic.base = copy_type_info(c, copy->generic.base);
			copy->generic.params = copy_expr_list(c, copy->generic.params);
			return copy;
		case TYPE_INFO_TYPEFROM:
		case TYPE_INFO_EVALTYPE:
		case TYPE_INFO_TYPEOF:
		case TYPE_INFO_VATYPE:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->unresolved_type_expr = copy_expr(c, source->unresolved_type_expr);
			return copy;
		case TYPE_INFO_VECTOR:
		case TYPE_INFO_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.len = copy_expr(c, source->array.len);
			copy->array.base = copy_type_info(c, source->array.base);
			return copy;
		case TYPE_INFO_INFERRED_ARRAY:
		case TYPE_INFO_SUBARRAY:
		case TYPE_INFO_INFERRED_VECTOR:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = copy_type_info(c, source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			copy->pointer = copy_type_info(c, source->pointer);
			return copy;
	}
	UNREACHABLE
}

static void copy_signature_deep(CopyStruct *c, Signature *signature)
{
	MACRO_COPY_DECL_LIST(signature->params);
	MACRO_COPY_TYPEID(signature->rtype);
}
void copy_decl_type(Decl *decl)
{
	Type *type = decl->type;
	if (!type) return;
	Type *copy = type_new(type->type_kind, type->name);
	*copy = *type;
	copy->type_cache = NULL;
	copy->decl = decl;
	copy->canonical = copy;
	decl->type = copy;
}


static inline bool decl_is_resolved_static_var(Decl *decl)
{
	if (decl->resolve_status != RESOLVE_DONE) return false;
	if (decl->decl_kind != DECL_VAR) return false;
	if (decl->var.kind != VARDECL_LOCAL) return false;
	return decl->var.is_static;
}

Decl *copy_decl(CopyStruct *c, Decl *decl)
{
	if (!decl) return NULL;
	if (c->single_static && decl_is_resolved_static_var(decl)) return decl;
	Decl *copy = decl_copy(decl);
	copy_reg_ref(c, decl, copy);
	copy->attributes = copy_attributes(c, copy->attributes);
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			break;
		case DECL_ERASED:
			break;
		case DECL_INTERFACE:
			MACRO_COPY_TYPE_LIST(copy->interfaces);
			MACRO_COPY_DECL_LIST(copy->methods);
			MACRO_COPY_DECL_LIST(copy->interface_methods);
			break;
		case DECL_CT_EXEC:
			MACRO_COPY_EXPR(copy->exec_decl.filename);
			MACRO_COPY_EXPR_LIST(copy->exec_decl.args);
			break;
		case DECL_CT_INCLUDE:
			MACRO_COPY_EXPR(copy->include.filename);
			break;
		case DECL_BODYPARAM:
			MACRO_COPY_DECL_LIST(copy->body_params);
			break;
		case DECL_UNION:
		case DECL_STRUCT:
			copy_decl_type(copy);
			MACRO_COPY_TYPE_LIST(copy->interfaces);
			MACRO_COPY_DECL_LIST(copy->strukt.members);
			MACRO_COPY_DECL_LIST(copy->methods);
			break;
		case DECL_DECLARRAY:
		case DECL_GLOBALS:
			UNREACHABLE
		case DECL_BITSTRUCT:
			copy_decl_type(copy);
			MACRO_COPY_TYPE_LIST(copy->interfaces);
			MACRO_COPY_DECL_LIST(copy->bitstruct.members);
			MACRO_COPY_TYPE(copy->bitstruct.base_type);
			MACRO_COPY_DECL_LIST(copy->methods);
			break;
		case DECL_ENUM:
		case DECL_FAULT:
			copy_decl_type(copy);
			MACRO_COPY_TYPE_LIST(copy->interfaces);
			MACRO_COPY_DECL_LIST(copy->methods);
			MACRO_COPY_DECL_LIST(copy->enums.parameters);
			MACRO_COPY_TYPE(copy->enums.type_info);
			MACRO_COPY_DECL_LIST(copy->enums.values);
			break;
		case DECL_FNTYPE:
			copy_signature_deep(c, &copy->fntype_decl);
			break;
		case DECL_FUNC:
			copy_decl_type(copy);
			MACRO_COPY_TYPEID(copy->func_decl.type_parent);
			MACRO_COPY_ASTID(copy->func_decl.docs);
			copy_signature_deep(c, &copy->func_decl.signature);
			MACRO_COPY_ASTID(copy->func_decl.body);
			break;
		case DECL_VAR:
			MACRO_COPY_TYPEID(copy->var.type_info);
			switch (copy->var.kind)
			{
				case VARDECL_UNWRAPPED:
					MACRO_COPY_DECL(copy->var.alias);
					break;
				case VARDECL_BITMEMBER:
					if (copy->var.bit_is_expr)
					{
						MACRO_COPY_EXPR(copy->var.start);
						MACRO_COPY_EXPR(copy->var.end);
					}
					break;
				default:
					MACRO_COPY_EXPR(copy->var.init_expr);
					break;
			}
			break;
		case DECL_LABEL:
			// Note that the ast id should be patched by the parent.
			return copy;
		case DECL_ENUM_CONSTANT:
			fixup_declid(c, &copy->enum_constant.parent);
			MACRO_COPY_EXPR_LIST(copy->enum_constant.args);
			break;
		case DECL_FAULTVALUE:
			fixup_declid(c, &copy->enum_constant.parent);
			break;
		case DECL_TYPEDEF:
			copy_decl_type(copy);
			if (copy->typedef_decl.is_func)
			{
				MACRO_COPY_DECL(copy->typedef_decl.decl);
				break;
			}
			MACRO_COPY_TYPE(copy->typedef_decl.type_info);
			break;
		case DECL_DISTINCT:
			copy_decl_type(copy);
			MACRO_COPY_TYPE_LIST(copy->interfaces);
			MACRO_COPY_DECL_LIST(copy->methods);
			MACRO_COPY_TYPE(copy->distinct);
			break;
		case DECL_CT_ECHO:
			MACRO_COPY_AST(decl->ct_echo_decl);
			break;
		case DECL_CT_ASSERT:
			MACRO_COPY_AST(decl->ct_assert_decl);
			break;
		case DECL_IMPORT:
			break;
		case DECL_MACRO:
			MACRO_COPY_ASTID(copy->func_decl.docs);
			MACRO_COPY_TYPEID(decl->func_decl.type_parent);
			copy_signature_deep(c, &copy->func_decl.signature);
			MACRO_COPY_ASTID(decl->func_decl.body);
			MACRO_COPY_DECLID(decl->func_decl.body_param);
			break;
		case DECL_ATTRIBUTE:
			MACRO_COPY_DECL_LIST(decl->attr_decl.params);
			decl->attr_decl.attrs = copy_attributes(c, decl->attr_decl.attrs);
			break;
		case DECL_DEFINE:
			switch (decl->define_decl.define_kind)
			{
				case DEFINE_IDENT_GENERIC:
					MACRO_COPY_EXPR_LIST(decl->define_decl.generic_params);
					break;
				case DEFINE_IDENT_ALIAS:
					break;
			}
			break;
	}
	return copy;
}

