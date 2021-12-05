#include "compiler_internal.h"

Expr **copy_expr_list(Expr **expr_list)
{
	Expr **result = NULL;
	VECEACH(expr_list, i)
	{
		vec_add(result, copy_expr(expr_list[i]));
	}
	return result;
}

static inline Decl *decl_copy_label_from_macro(Decl *to_copy, Ast *ast)
{
	if (!to_copy) return NULL;
	to_copy = copy_decl(to_copy);
	to_copy->label.parent = astid(ast);
	return to_copy;
}


static inline void copy_flow(Ast *ast)
{
	ast->flow.label = decl_copy_label_from_macro(ast->flow.label, ast);
}

static TypeInfo** type_info_copy_list_from_macro(TypeInfo **to_copy)
{
	TypeInfo **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, copy_type_info(to_copy[i]));
	}
	return result;
}


static DesignatorElement **macro_copy_designator_list(DesignatorElement **list)
{
	DesignatorElement **result = NULL;
	VECEACH(list, i)
	{
		DesignatorElement *element = MALLOC(sizeof(DesignatorElement));
		DesignatorElement *to_copy = list[i];
		*element = *to_copy;
		switch (to_copy->kind)
		{
			case DESIGNATOR_FIELD:
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


Expr *copy_expr(Expr *source_expr)
{
	if (!source_expr) return NULL;
	Expr *expr = expr_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
			UNREACHABLE
		case EXPR_FLATPATH:
		case EXPR_UNDEF:
		case EXPR_NOP:
		case EXPR_BUILTIN:
			return expr;
		case EXPR_DECL:
			MACRO_COPY_DECL(expr->decl_expr);
			return expr;
		case EXPR_CT_CALL:
			MACRO_COPY_EXPR(expr->ct_call_expr.main_var);
			return expr;
		case EXPR_TRY_UNWRAP:
			MACRO_COPY_EXPR(expr->try_unwrap_expr.init);
			MACRO_COPY_TYPE(expr->try_unwrap_expr.type);
			return expr;
		case EXPR_TRY_UNWRAP_CHAIN:
			MACRO_COPY_EXPR_LIST(expr->try_unwrap_chain_expr);
			return expr;
		case EXPR_CATCH_UNWRAP:
			MACRO_COPY_EXPR_LIST(expr->catch_unwrap_expr.exprs);
			MACRO_COPY_TYPE(expr->catch_unwrap_expr.type);
			return expr;
		case EXPR_PLACEHOLDER:
		case EXPR_CONST_IDENTIFIER:
		case EXPR_CT_IDENT:
		case EXPR_IDENTIFIER:
		case EXPR_HASH_IDENT:
			return expr;
		case EXPR_MACRO_EXPANSION:
			MACRO_COPY_EXPR(expr->macro_expansion_expr.inner);
			return expr;
		case EXPR_DESIGNATOR:
			expr->designator_expr.path = macro_copy_designator_list(expr->designator_expr.path);
			MACRO_COPY_EXPR(expr->designator_expr.value);
			return expr;
		case EXPR_TYPEINFO:
			MACRO_COPY_TYPE(expr->type_expr);
			return expr;
		case EXPR_SLICE_ASSIGN:
			MACRO_COPY_EXPR(expr->slice_assign_expr.left);
			MACRO_COPY_EXPR(expr->slice_assign_expr.right);
			return expr;
		case EXPR_SLICE:
			MACRO_COPY_EXPR(expr->slice_expr.expr);
			MACRO_COPY_EXPR(expr->slice_expr.start);
			MACRO_COPY_EXPR(expr->slice_expr.end);
			return expr;
		case EXPR_LEN:
			MACRO_COPY_EXPR(expr->len_expr.inner);
			return expr;
		case EXPR_FORCE_UNWRAP:
		case EXPR_TRY:
		case EXPR_CATCH:
		case EXPR_FAILABLE:
		case EXPR_GROUP:
		case EXPR_TYPEOFANY:
			MACRO_COPY_EXPR(expr->inner_expr);
			return expr;
		case EXPR_COND:
			MACRO_COPY_EXPR_LIST(expr->cond_expr);
			return expr;
		case EXPR_OR_ERROR:
			MACRO_COPY_EXPR(expr->or_error_expr.expr);
			if (expr->or_error_expr.is_jump)
			{
				MACRO_COPY_EXPR(expr->or_error_expr.or_error_expr);
			}
			else
			{
				MACRO_COPY_AST(expr->or_error_expr.or_error_stmt);
			}
			return expr;
		case EXPR_MACRO_BLOCK:
			UNREACHABLE
		case EXPR_COMPOUND_LITERAL:
			MACRO_COPY_EXPR(expr->expr_compound_literal.initializer);
			MACRO_COPY_TYPE(expr->expr_compound_literal.type_info);
			return expr;
		case EXPR_EXPR_BLOCK:
			MACRO_COPY_AST_LIST(expr->expr_block.stmts);
			return expr;
		case EXPR_POISONED:
			return source_expr;
		case EXPR_RETHROW:
			MACRO_COPY_EXPR(expr->rethrow_expr.inner);
			return expr;
		case EXPR_CONST:
			return expr;
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
			MACRO_COPY_EXPR(expr->binary_expr.left);
			MACRO_COPY_EXPR(expr->binary_expr.right);
			return expr;
		case EXPR_TERNARY:
			MACRO_COPY_EXPR(expr->ternary_expr.cond);
			MACRO_COPY_EXPR(expr->ternary_expr.then_expr);
			MACRO_COPY_EXPR(expr->ternary_expr.else_expr);
			return expr;
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			MACRO_COPY_EXPR(expr->unary_expr.expr);
			return expr;
		case EXPR_TYPEID:
			MACRO_COPY_TYPE(expr->typeid_expr);
			return expr;
		case EXPR_CALL:
			if (expr->resolve_status != RESOLVE_DONE || expr->call_expr.is_pointer_call)
			{
				MACRO_COPY_EXPR(expr->call_expr.function);
			}
			MACRO_COPY_EXPR_LIST(expr->call_expr.arguments);
			return expr;
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			MACRO_COPY_EXPR(expr->subscript_expr.expr);
			MACRO_COPY_EXPR(expr->subscript_expr.index);
			return expr;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			MACRO_COPY_EXPR(expr->access_expr.parent);
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
			MACRO_COPY_EXPR(expr->cast_expr.expr);
			MACRO_COPY_TYPE(expr->cast_expr.type_info);
			return expr;
		case EXPR_SCOPED_EXPR:
			MACRO_COPY_EXPR(expr->expr_scope.expr);
			return expr;
	}
	UNREACHABLE
}

Ast *copy_ast(Ast *source)
{
	if (!source) return NULL;
	Ast *ast = ast_copy(source);
	switch (source->ast_kind)
	{
		case AST_SCOPING_STMT:
			MACRO_COPY_EXPR(ast->scoping_stmt.scoped);
			MACRO_COPY_AST(ast->scoping_stmt.stmt);
			return ast;
		case AST_DOCS:
			MACRO_COPY_AST_LIST(ast->directives);
			return ast;
		case AST_DOC_DIRECTIVE:
			switch (ast->doc_directive.kind)
			{
				case DOC_DIRECTIVE_REQUIRE:
				case DOC_DIRECTIVE_ENSURE:
					MACRO_COPY_EXPR(ast->doc_directive.contract.decl_exprs);
					MACRO_COPY_EXPR(ast->doc_directive.contract.comment);
					break;
				case DOC_DIRECTIVE_PARAM:
				case DOC_DIRECTIVE_ERRORS:
				case DOC_DIRECTIVE_PURE:
				case DOC_DIRECTIVE_UNKNOWN:
					break;
			}
			return ast;
		case AST_POISONED:
			return ast;
		case AST_ASM_STMT:
			MACRO_COPY_EXPR(ast->asm_stmt.body);
			return ast;
		case AST_ASSERT_STMT:
			MACRO_COPY_EXPR(ast->ct_assert_stmt.expr);
			MACRO_COPY_EXPR(ast->ct_assert_stmt.message);
			return ast;
		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			return ast;
		case AST_CASE_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			MACRO_COPY_EXPR(ast->case_stmt.expr);
			MACRO_COPY_EXPR(ast->case_stmt.to_expr);
			return ast;
		case AST_COMPOUND_STMT:
			MACRO_COPY_AST_LIST(ast->compound_stmt.stmts);
			return ast;
		case AST_CT_COMPOUND_STMT:
			MACRO_COPY_AST_LIST(ast->ct_compound_stmt);
			return ast;
		case AST_CT_ASSERT:
			MACRO_COPY_EXPR(ast->ct_assert_stmt.message);
			MACRO_COPY_EXPR(ast->ct_assert_stmt.expr);
			return ast;
		case AST_CT_IF_STMT:
			MACRO_COPY_EXPR(ast->ct_if_stmt.expr);
			MACRO_COPY_AST(ast->ct_if_stmt.elif);
			MACRO_COPY_AST(ast->ct_if_stmt.then);
			return ast;
		case AST_CT_ELIF_STMT:
			MACRO_COPY_EXPR(ast->ct_elif_stmt.expr);
			MACRO_COPY_AST(ast->ct_elif_stmt.then);
			MACRO_COPY_AST(ast->ct_elif_stmt.elif);
			return ast;
		case AST_CT_ELSE_STMT:
			MACRO_COPY_AST(ast->ct_else_stmt);
			return ast;
		case AST_CT_FOR_STMT:
			MACRO_COPY_AST(ast->ct_for_stmt.body);
			MACRO_COPY_EXPR(ast->ct_for_stmt.expr);
			return ast;
		case AST_CT_SWITCH_STMT:
			MACRO_COPY_EXPR(ast->ct_switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->ct_switch_stmt.body);
			return ast;
		case AST_DECLARE_STMT:
			MACRO_COPY_DECL(ast->declare_stmt);
			return ast;
		case AST_DEFAULT_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			return ast;
		case AST_VAR_STMT:
			ast->var_stmt = copy_decl(ast->var_stmt);
			return ast;
		case AST_DEFER_STMT:
			assert(!ast->defer_stmt.prev_defer);
			MACRO_COPY_AST(ast->defer_stmt.body);
			return ast;
		case AST_DO_STMT:
			copy_flow(ast);
			MACRO_COPY_AST(ast->do_stmt.body);
			MACRO_COPY_EXPR(ast->do_stmt.expr);
			return ast;
		case AST_EXPR_STMT:
			MACRO_COPY_EXPR(ast->expr_stmt);
			return ast;
		case AST_FOR_STMT:
			copy_flow(ast);
			MACRO_COPY_EXPR(ast->for_stmt.cond);
			MACRO_COPY_EXPR(ast->for_stmt.incr);
			MACRO_COPY_AST(ast->for_stmt.body);
			MACRO_COPY_EXPR(ast->for_stmt.init);
			return ast;
		case AST_FOREACH_STMT:
			copy_flow(ast);
			MACRO_COPY_DECL(ast->foreach_stmt.index);
			MACRO_COPY_DECL(ast->foreach_stmt.variable);
			MACRO_COPY_EXPR(ast->foreach_stmt.enumeration);
			MACRO_COPY_AST(ast->foreach_stmt.body);
			return ast;
		case AST_IF_STMT:
			copy_flow(ast);
			MACRO_COPY_EXPR(ast->if_stmt.cond);
			MACRO_COPY_AST(ast->if_stmt.else_body);
			MACRO_COPY_AST(ast->if_stmt.then_body);
			return ast;
		case AST_NEXT_STMT:
			if (ast->next_stmt.is_type)
			{
				MACRO_COPY_TYPE(ast->next_stmt.expr_or_type_info);
			}
			else
			{
				MACRO_COPY_EXPR(ast->next_stmt.expr_or_type_info);
			}
			return ast;
		case AST_NOP_STMT:
			return ast;
		case AST_RETURN_STMT:
			MACRO_COPY_EXPR(ast->return_stmt.expr);
			return ast;
		case AST_SCOPED_STMT:
			MACRO_COPY_AST(ast->scoped_stmt.stmt);
			return ast;
		case AST_SWITCH_STMT:
		case AST_IF_CATCH_SWITCH_STMT:
			copy_flow(ast);
			MACRO_COPY_EXPR(ast->switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->switch_stmt.cases);
			return ast;
		case AST_UNREACHABLE_STMT:
			return ast;
		case AST_WHILE_STMT:
			copy_flow(ast);
			MACRO_COPY_EXPR(ast->while_stmt.cond);
			MACRO_COPY_AST(ast->while_stmt.body);
			return ast;
	}
	UNREACHABLE;
}


Ast **copy_ast_list(Ast **to_copy)
{
	Ast **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, copy_ast(to_copy[i]));
	}
	return result;
}

Decl **copy_decl_list(Decl **decl_list)
{
	Decl **result = NULL;
	VECEACH(decl_list, i)
	{
		vec_add(result, copy_decl(decl_list[i]));
	}
	return result;
}

Decl *decl_copy_local_from_macro(Decl *to_copy)
{
	if (!to_copy) return NULL;
	assert(to_copy->decl_kind == DECL_VAR);
	Decl *copy = decl_copy(to_copy);
	MACRO_COPY_TYPE(copy->var.type_info);
	MACRO_COPY_EXPR(copy->var.init_expr);
	return copy;
}

TypeInfo *copy_type_info(TypeInfo *source)
{
	if (!source) return NULL;
	TypeInfo *copy = type_info_copy(source);
	if (source->resolve_status == RESOLVE_DONE) return copy;
	switch (source->kind)
	{
		case TYPE_INFO_POISON:
			return copy;
		case TYPE_INFO_IDENTIFIER:
			return copy;
		case TYPE_INFO_EXPRESSION:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->unresolved_type_expr = copy_expr(source->unresolved_type_expr);
			return copy;
		case TYPE_INFO_VECTOR:
		case TYPE_INFO_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.len = copy_expr(source->array.len);
			copy->array.base = copy_type_info(source->array.base);
			return copy;
		case TYPE_INFO_INFERRED_ARRAY:
		case TYPE_INFO_SUBARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = copy_type_info(source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			copy->pointer = copy_type_info(source->pointer);
			return copy;
	}
	UNREACHABLE
}

static void copy_function_signature_deep(FunctionSignature *signature)
{
	MACRO_COPY_DECL_LIST(signature->params);
	MACRO_COPY_TYPE(signature->rtype);
	assert(!signature->failable_abi_info);
	assert(!signature->ret_abi_info);
}
void copy_decl_type(Decl *decl)
{
	Type *type = decl->type;
	Type *copy = type_new(type->type_kind, type->name);
	*copy = *type;
	copy->type_cache = NULL;
	copy->decl = decl;
	copy->canonical = copy;
	decl->type = copy;
}

static Attr **copy_attributes(Attr** attr_list)
{
	if (!attr_list) return attr_list;
	Attr** list = NULL;
	VECEACH(attr_list, i)
	{
		Attr *attribute = attr_list[i];
		Attr *copy = MALLOC(sizeof(Attr));
		*copy = *attribute;
		MACRO_COPY_EXPR(copy->expr);
		vec_add(list, copy);
	}
	return list;
}
Decl *copy_decl(Decl *decl)
{
	if (!decl) return NULL;
	Decl *copy = decl_copy(decl);
	MACRO_COPY_AST(copy->docs);
	copy->attributes = copy_attributes(copy->attributes);
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			break;
		case DECL_UNION:
		case DECL_STRUCT:
			copy_decl_type(copy);
			MACRO_COPY_DECL_LIST(copy->strukt.members);
			MACRO_COPY_DECL_LIST(copy->methods);
			break;
		case DECL_BITSTRUCT:
			UNREACHABLE
		case DECL_ENUM:
			case DECL_ERRTYPE:
			copy_decl_type(copy);
			MACRO_COPY_DECL_LIST(copy->methods);
			MACRO_COPY_DECL_LIST(copy->enums.parameters);
			MACRO_COPY_TYPE(copy->enums.type_info);
			MACRO_COPY_DECL_LIST(copy->enums.values);
			break;
		case DECL_FUNC:
			MACRO_COPY_TYPE(copy->func_decl.type_parent);
			copy->func_decl.annotations = NULL;
			copy_function_signature_deep(&copy->func_decl.function_signature);
			MACRO_COPY_AST(copy->func_decl.body);
			break;
		case DECL_VAR:
			MACRO_COPY_TYPE(copy->var.type_info);
			if (copy->var.kind == VARDECL_UNWRAPPED)
			{
				MACRO_COPY_DECL(copy->var.alias);
			}
			else
			{
				MACRO_COPY_EXPR(copy->var.init_expr);
			}
			break;
		case DECL_LABEL:
			// Note that the ast id should be patched by the parent.
			return copy;
		case DECL_ENUM_CONSTANT:
			MACRO_COPY_EXPR(copy->enum_constant.expr);
			MACRO_COPY_EXPR_LIST(copy->enum_constant.args);
			break;
		case DECL_ERRVALUE:
			MACRO_COPY_EXPR(copy->enum_constant.expr);
			MACRO_COPY_EXPR_LIST(copy->enum_constant.args);
			break;
		case DECL_TYPEDEF:
			if (copy->typedef_decl.is_func)
			{
				copy_function_signature_deep(&copy->typedef_decl.function_signature);
				break;
			}
			MACRO_COPY_TYPE(copy->typedef_decl.type_info);
			break;
		case DECL_DISTINCT:
			MACRO_COPY_DECL_LIST(copy->methods);
			if (copy->distinct_decl.typedef_decl.is_func)
			{
				copy_function_signature_deep(&copy->distinct_decl.typedef_decl.function_signature);
				break;
			}
			MACRO_COPY_TYPE(copy->distinct_decl.typedef_decl.type_info);
			break;
		case DECL_CT_IF:
			MACRO_COPY_EXPR(decl->ct_if_decl.expr);
			MACRO_COPY_DECL(decl->ct_if_decl.elif);
			MACRO_COPY_DECL_LIST(decl->ct_if_decl.then);
			break;
		case DECL_CT_ASSERT:
			MACRO_COPY_AST(decl->ct_assert_decl);
			break;
		case DECL_CT_ELSE:
			MACRO_COPY_DECL_LIST(decl->ct_else_decl);
			break;
		case DECL_CT_ELIF:
			MACRO_COPY_EXPR(decl->ct_elif_decl.expr);
			MACRO_COPY_DECL(decl->ct_elif_decl.elif);
			MACRO_COPY_DECL_LIST(decl->ct_elif_decl.then);
			break;
		case DECL_IMPORT:
			break;
		case DECL_GENERIC:
		case DECL_MACRO:
			MACRO_COPY_TYPE(decl->macro_decl.type_parent);
			MACRO_COPY_DECL_LIST(decl->macro_decl.parameters);
			MACRO_COPY_AST(decl->macro_decl.body);
			MACRO_COPY_TYPE(decl->macro_decl.rtype);
			break;
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_ATTRIBUTE:
			TODO
		case DECL_DEFINE:
			switch (decl->define_decl.define_kind)
			{
				case DEFINE_TYPE_GENERIC:
				case DEFINE_IDENT_GENERIC:
					MACRO_COPY_TYPE_LIST(decl->define_decl.generic_params);
					break;
				case DEFINE_IDENT_ALIAS:
					break;
				case DEFINE_ATTRIBUTE:
					decl->define_decl.attributes.attrs = copy_attributes(decl->define_decl.attributes.attrs);
					MACRO_COPY_DECL_LIST(decl->define_decl.attributes.params);
					break;
			}
			break;
	}
	return copy;
}

