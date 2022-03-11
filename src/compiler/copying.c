#include "compiler_internal.h"

#define SCOPE_FIXUP_START do { CopyFixup *current = c->current_fixup;
#define SCOPE_FIXUP_END c->current_fixup = current; } while (0)



static inline void copy_reg_ref(CopyStruct *c, void *original, void *result)
{
	c->current_fixup->new_ptr = result;
	c->current_fixup->original = original;
	c->current_fixup++;
	if (c->current_fixup == &c->fixups[MAX_FIXUPS])
	{
		error_exit("Too many fixups for macros.");
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

static inline Decl *decl_copy_label_from_macro(CopyStruct *c, Decl *to_copy, Ast *ast)
{
	if (!to_copy) return NULL;
	to_copy = copy_decl(c, to_copy);
	to_copy->label.parent = astid(ast);
	return to_copy;
}


static inline void copy_flow(CopyStruct *c, Ast *ast)
{
	ast->flow.label = decl_copy_label_from_macro(c, ast->flow.label, ast);
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

Ast *ast_macro_copy(Ast *source_ast)
{
	copy_struct.current_fixup = copy_struct.fixups;
	return ast_copy_deep(&copy_struct, source_ast);
}

Expr *expr_macro_copy(Expr *source_expr)
{
	copy_struct.current_fixup = copy_struct.fixups;
	return copy_expr(&copy_struct, source_expr);
}

Expr *copy_expr(CopyStruct *c, Expr *source_expr)
{
	if (!source_expr) return NULL;
	Expr *expr = expr_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_VARIANTSWITCH:
		case EXPR_ARGV_TO_SUBARRAY:
			UNREACHABLE
		case EXPR_FLATPATH:
		case EXPR_NOP:
		case EXPR_BUILTIN:
		case EXPR_RETVAL:
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
		case EXPR_COMPILER_CONST:
			return expr;
		case EXPR_MACRO_EXPANSION:
			SCOPE_FIXUP_START
				MACRO_COPY_EXPR(expr->macro_expansion_expr.inner);
			SCOPE_FIXUP_END;
			return expr;
		case EXPR_DESIGNATOR:
			expr->designator_expr.path = macro_copy_designator_list(c, expr->designator_expr.path);
			MACRO_COPY_EXPR(expr->designator_expr.value);
			return expr;
		case EXPR_TYPEINFO:
			MACRO_COPY_TYPE(expr->type_expr);
			return expr;
		case EXPR_SLICE_ASSIGN:
			MACRO_COPY_EXPRID(expr->slice_assign_expr.left);
			MACRO_COPY_EXPRID(expr->slice_assign_expr.right);
			return expr;
		case EXPR_SLICE:
			MACRO_COPY_EXPRID(expr->slice_expr.expr);
			MACRO_COPY_EXPRID(expr->slice_expr.start);
			MACRO_COPY_EXPRID(expr->slice_expr.end);
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
		case EXPR_PTR:
		case EXPR_STRINGIFY:
		case EXPR_CT_EVAL:
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
			MACRO_COPY_ASTID(expr->expr_block.first_stmt);
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
			MACRO_COPY_ASTID(expr->call_expr.body);
			MACRO_COPY_DECL_LIST(expr->call_expr.body_arguments);
			MACRO_COPY_EXPR_LIST(expr->call_expr.arguments);
			return expr;
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			MACRO_COPY_EXPRID(expr->subscript_expr.expr);
			MACRO_COPY_EXPRID(expr->subscript_expr.index);
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
			MACRO_COPY_EXPRID(expr->cast_expr.expr);
			MACRO_COPY_TYPEID(expr->cast_expr.type_info);
			return expr;
		case EXPR_SCOPED_EXPR:
			SCOPE_FIXUP_START
				MACRO_COPY_EXPR(expr->expr_scope.expr);
			SCOPE_FIXUP_END;
			return expr;
	}
	UNREACHABLE
}

void doc_ast_copy(CopyStruct *c, AstDocStmt *doc)
{
	switch (doc->kind)
	{
		case DOC_DIRECTIVE_REQUIRE:
		case DOC_DIRECTIVE_ENSURE:
		case DOC_DIRECTIVE_CHECKED:
			MACRO_COPY_EXPR(doc->contract.decl_exprs);
			break;
		case DOC_DIRECTIVE_PARAM:
		case DOC_DIRECTIVE_ERRORS:
		case DOC_DIRECTIVE_PURE:
		case DOC_DIRECTIVE_UNKNOWN:
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
		case AST_DOC_STMT:
			doc_ast_copy(c, &source->doc_stmt);
			break;
		case AST_ASM_STMT:
			MACRO_COPY_EXPR(ast->asm_stmt.body);
			break;
		case AST_ASSERT_STMT:
		case AST_CT_ASSERT:
			MACRO_COPY_EXPRID(ast->assert_stmt.expr);
			MACRO_COPY_EXPRID(ast->assert_stmt.message);
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
			MACRO_COPY_EXPR(ast->case_stmt.expr);
			MACRO_COPY_EXPR(ast->case_stmt.to_expr);
			break;
		case AST_COMPOUND_STMT:
			MACRO_COPY_ASTID(ast->compound_stmt.first_stmt);
			break;
		case AST_CT_IF_STMT:
			MACRO_COPY_EXPR(ast->ct_if_stmt.expr);
			MACRO_COPY_AST(ast->ct_if_stmt.elif);
			MACRO_COPY_ASTID(ast->ct_if_stmt.then);
			break;
		case AST_CT_ELSE_STMT:
			MACRO_COPY_ASTID(ast->ct_else_stmt);
			break;
		case AST_CT_FOREACH_STMT:
			MACRO_COPY_ASTID(ast->ct_foreach_stmt.body);
			MACRO_COPY_EXPRID(ast->ct_foreach_stmt.expr);
			break;
		case AST_CT_SWITCH_STMT:
			MACRO_COPY_EXPR(ast->ct_switch_stmt.cond);
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
		case AST_NEXT_STMT:
			MACRO_COPY_EXPR(ast->nextcase_stmt.expr);
			break;
		case AST_NOP_STMT:
			break;
		case AST_BLOCK_EXIT_STMT:
		case AST_RETURN_STMT:
			MACRO_COPY_EXPR(ast->return_stmt.expr);
			MACRO_COPY_ASTID(ast->return_stmt.cleanup);
			break;
		case AST_SWITCH_STMT:
		case AST_IF_CATCH_SWITCH_STMT:
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

Decl **decl_copy_list(Decl **decl_list)
{
	copy_struct.current_fixup = copy_struct.fixups;
	Decl **result = NULL;
	VECEACH(decl_list, i)
	{
		vec_add(result, copy_decl(&copy_struct, decl_list[i]));
	}
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
		case TYPE_INFO_EVALTYPE:
		case TYPE_INFO_EXPRESSION:
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
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = copy_type_info(c, source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			copy->pointer = copy_type_info(c, source->pointer);
			return copy;
	}
	UNREACHABLE
}

static void copy_function_signature_deep(CopyStruct *c, FunctionSignature *signature)
{
	MACRO_COPY_DECL_LIST(signature->params);
	MACRO_COPY_TYPEID(signature->returntype);
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

static Attr **copy_attributes(CopyStruct *c, Attr** attr_list)
{
	if (!attr_list) return attr_list;
	Attr** list = NULL;
	VECEACH(attr_list, i)
	{
		Attr *attribute = attr_list[i];
		Attr *copy = MALLOCS(Attr);
		*copy = *attribute;
		MACRO_COPY_EXPR(copy->expr);
		vec_add(list, copy);
	}
	return list;
}
Decl *copy_decl(CopyStruct *c, Decl *decl)
{
	if (!decl) return NULL;
	Decl *copy = decl_copy(decl);
	copy_reg_ref(c, decl, copy);
	copy->attributes = copy_attributes(c, copy->attributes);
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			break;
		case DECL_BODYPARAM:
			MACRO_COPY_DECL_LIST(copy->body_params);
			break;
		case DECL_UNION:
		case DECL_STRUCT:
			copy_decl_type(copy);
			MACRO_COPY_DECL_LIST(copy->strukt.members);
			MACRO_COPY_DECL_LIST(copy->methods);
			break;
		case DECL_DECLARRAY:
		case DECL_BITSTRUCT:
			UNREACHABLE
		case DECL_ENUM:
			case DECL_OPTENUM:
			copy_decl_type(copy);
			MACRO_COPY_DECL_LIST(copy->methods);
			MACRO_COPY_DECL_LIST(copy->enums.parameters);
			MACRO_COPY_TYPE(copy->enums.type_info);
			MACRO_COPY_DECL_LIST(copy->enums.values);
			break;
		case DECL_FUNC:
			MACRO_COPY_TYPEID(copy->func_decl.type_parent);
			MACRO_COPY_ASTID(copy->func_decl.docs);
			copy_function_signature_deep(c, &copy->func_decl.function_signature);
			MACRO_COPY_ASTID(copy->func_decl.body);
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
		case DECL_OPTVALUE:
			MACRO_COPY_EXPR(copy->enum_constant.expr);
			MACRO_COPY_EXPR_LIST(copy->enum_constant.args);
			break;
		case DECL_TYPEDEF:
			if (copy->typedef_decl.is_func)
			{
				copy_function_signature_deep(c, &copy->typedef_decl.function_signature);
				break;
			}
			MACRO_COPY_TYPE(copy->typedef_decl.type_info);
			break;
		case DECL_DISTINCT:
			MACRO_COPY_DECL_LIST(copy->methods);
			if (copy->distinct_decl.typedef_decl.is_func)
			{
				copy_function_signature_deep(c, &copy->distinct_decl.typedef_decl.function_signature);
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
			MACRO_COPY_ASTID(copy->macro_decl.docs);
			MACRO_COPY_TYPEID(decl->macro_decl.type_parent);
			MACRO_COPY_DECL_LIST(decl->macro_decl.parameters);
			MACRO_COPY_ASTID(decl->macro_decl.body);
			MACRO_COPY_TYPEID(decl->macro_decl.rtype);
			MACRO_COPY_DECLID(decl->macro_decl.body_param);
			break;
		case DECL_CT_SWITCH:
			MACRO_COPY_DECL_LIST(decl->ct_switch_decl.cases);
			MACRO_COPY_EXPR(decl->ct_switch_decl.expr);
			break;
		case DECL_CT_CASE:
			MACRO_COPY_EXPR(decl->ct_case_decl.expr);
			MACRO_COPY_EXPR(decl->ct_case_decl.to_expr);
			MACRO_COPY_DECL_LIST(decl->ct_case_decl.body);
			break;
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
					decl->define_decl.attributes.attrs = copy_attributes(c, decl->define_decl.attributes.attrs);
					MACRO_COPY_DECL_LIST(decl->define_decl.attributes.params);
					break;
			}
			break;
	}
	return copy;
}

