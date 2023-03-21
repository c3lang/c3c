// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

static bool context_next_is_path_prefix_start(ParseContext *c);
static Decl *parse_const_declaration(ParseContext *c, bool is_global);
static inline Decl *parse_func_definition(ParseContext *c, AstId docs, bool is_interface);
static inline bool parse_bitstruct_body(ParseContext *c, Decl *decl);
static inline Decl *parse_static_top_level(ParseContext *c);
static Decl *parse_include(ParseContext *c);
static bool parse_attributes_for_global(ParseContext *c, Decl *decl);
INLINE bool parse_decl_initializer(ParseContext *c, Decl *decl);
INLINE Decl *decl_new_var_current(ParseContext *c, TypeInfo *type, VarDeclKind kind);


INLINE Decl *decl_new_var_current(ParseContext *c, TypeInfo *type, VarDeclKind kind)
{
	return decl_new_var(symstr(c), c->span, type, kind);
}

static bool context_next_is_path_prefix_start(ParseContext *c)
{
	return tok_is(c, TOKEN_IDENT) && peek(c) == TOKEN_SCOPE;
}

/**
 * Walk until we find the first top level construct, the current heuristic is this:
 * public, typedef, struct, import, union, extern, enum, generic, attribute, define
 * are *always* sync points.
 *
 * func, any type, CT_IDENT, CT_TYPE_IDENT, $if, $for, $switch, generic,
 * doc comment start, asm, typeof, TYPE_IDENT, const, IDENT
 * - are sync points only if they appear in the first column.
 */
void recover_top_level(ParseContext *c)
{
	advance(c);
	while (!tok_is(c, TOKEN_EOF))
	{
		switch (c->tok)
		{
			case TOKEN_PRIVATE:
			case TOKEN_IMPORT:
			case TOKEN_EXTERN:
			case TOKEN_ENUM:
			case TOKEN_DEFINE:
			case TOKEN_TYPEDEF:
			case TOKEN_FAULT:
				return;
			case TOKEN_IDENT: // Incr arrays only
			case TOKEN_CONST:
			case TOKEN_ASM:
			case TOKEN_CT_ASSERT:
			case TOKEN_DOCS_START:
			case TOKEN_CT_IDENT:
			case TOKEN_CT_IF:
			case TOKEN_CT_FOR:
			case TOKEN_CT_SWITCH:
			case TOKEN_FN:
			case TOKEN_STRUCT:
			case TOKEN_UNION:
			case TOKEN_BITSTRUCT:
			case TOKEN_STATIC:
			case TYPELIKE_TOKENS:
				// Only recover if this is in the first col.
				if (c->span.col == 1) return;
				advance(c);
				break;
			default:
				advance(c);
				break;
		}
	}
}

INLINE bool parse_decl_initializer(ParseContext *c, Decl *decl)
{
	ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), false);
	return true;
}

// --- Parse CT conditional code

/**
 * A general CT block contains 0 - n number of declarations, and may be terminated
 * by 1-3 different tokens. We don't accept imports or modules inside.
 */
static inline bool parse_top_level_block(ParseContext *c, Decl ***decls, TokenType end1, TokenType end2, TokenType end3)
{
	consume_deprecated_symbol(c, TOKEN_COLON); // TODO remove

	// Check whether we reached a terminating token or EOF
	while (!tok_is(c, end1) && !tok_is(c, end2) && !tok_is(c, end3) && !tok_is(c, TOKEN_EOF))
	{
		// Otherwise, try to parse it, passing in NULL ensures modules/imports are prohibited.
		Decl *decl = parse_top_level_statement(c, NULL);
		// Decl may be null only on import/module, which we prohibit
		assert(decl && "Should never happen.");
		// Bad decl?
		if (!decl_ok(decl)) return false;
		// Otherwise add it to the list.
		add_decl_to_list(decls, decl);
	}
	return true;
}

/**
 * ct_if_top_level ::= CT_IF const_paren_expr top_level_block (CT_ELSE top_level_block)? CT_ENDIF
 * @return the declaration if successfully parsed, poisoned_decl otherwise.
 */
static inline Decl *parse_ct_if_top_level(ParseContext *c)
{
	Decl *ct = decl_new_ct(DECL_CT_IF, c->span);
	advance_and_verify(c, TOKEN_CT_IF);
	ASSIGN_EXPR_OR_RET(ct->ct_if_decl.expr, parse_const_paren_expr(c), poisoned_decl);

	if (!parse_top_level_block(c, &ct->ct_if_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;

	// Chain elif TODO remove
	while (tok_is(c, TOKEN_CT_ELIF))
	{
		sema_warning_at(c->span, "$elif is deprecated, use $switch instead.");
		Decl *ct_elif = decl_new_ct(DECL_CT_IF, c->span);
		advance_and_verify(c, TOKEN_CT_ELIF);
		ASSIGN_EXPR_OR_RET(ct_elif->ct_elif_decl.expr, parse_const_paren_expr(c), poisoned_decl);
		if (!parse_top_level_block(c, &ct_elif->ct_elif_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	// <- end

	// final else
	if (tok_is(c, TOKEN_CT_ELSE))
	{
		Decl *ct_else = decl_new_ct(DECL_CT_ELSE, c->span);
		advance_and_verify(c, TOKEN_CT_ELSE);
		ct_if_decl->elif = ct_else;
		if (!parse_top_level_block(c, &ct_else->ct_else_decl, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF)) return poisoned_decl;
	}
	CONSUME_OR_RET(TOKEN_CT_ENDIF, poisoned_decl);
	consume_deprecated_symbol(c, TOKEN_EOS); // TODO remove
	return ct;
}

/**
 * ct_case ::= (CT_DEFAULT | CT_CASE constant_expr) ':' top_level_statement*
 *
 * @return poisoned decl if parsing fails.
 */
static inline Decl *parse_ct_case(ParseContext *c)
{
	Decl *decl;
	// Parse the $case expr / $default
	switch (c->tok)
	{
		case TOKEN_CT_DEFAULT:
			decl = decl_new_ct(DECL_CT_CASE, c->span);
			advance(c);
			break;
		case TOKEN_CT_CASE:
			decl = decl_new_ct(DECL_CT_CASE, c->span);
			advance(c);
			ASSIGN_EXPR_OR_RET(decl->ct_case_decl.expr, parse_constant_expr(c), poisoned_decl);
			break;
		default:
			SEMA_ERROR_HERE("Expected a $case or $default statement here.");
			return poisoned_decl;
	}
	// Parse the body
	if (!try_consume(c, TOKEN_COLON))
	{
		sema_error_at_after(c->prev_span, "Expected a ':' here.");
		return poisoned_decl;
	}
	if (!parse_top_level_block(c, &decl->ct_case_decl.body, TOKEN_CT_DEFAULT, TOKEN_CT_CASE, TOKEN_CT_ENDSWITCH)) return poisoned_decl;
	return decl;
}

/**
 * ct_switch_top_level ::= CT_SWITCH const_paren_expr? ct_case* CT_ENDSWITCH
 * @return the declaration if successfully parsed, NULL otherwise.
 */
static inline Decl *parse_ct_switch_top_level(ParseContext *c)
{
	Decl *ct = decl_new_ct(DECL_CT_SWITCH, c->span);
	advance_and_verify(c, TOKEN_CT_SWITCH);
	if (!tok_is(c, TOKEN_CT_CASE) && !tok_is(c, TOKEN_CT_DEFAULT) && !tok_is(c, TOKEN_CT_ENDSWITCH))
	{
		ASSIGN_EXPR_OR_RET(ct->ct_switch_decl.expr, parse_const_paren_expr(c), poisoned_decl);
		consume_deprecated_symbol(c, TOKEN_COLON); // TODO remove
	}
	while (!try_consume(c, TOKEN_CT_ENDSWITCH))
	{
		ASSIGN_DECL_OR_RET(Decl *result, parse_ct_case(c), poisoned_decl);
		vec_add(ct->ct_switch_decl.cases, result);
	}
	consume_deprecated_symbol(c, TOKEN_EOS); // TODO remove
	return ct;
}


// --- Parse paths

/**
 * module_path ::= IDENT (SCOPE IDENT)*
 *
 * @param c
 * @return path or null if parsing failed.
 */
static inline Path *parse_module_path(ParseContext *c)
{
	assert(tok_is(c, TOKEN_IDENT));
	scratch_buffer_clear();
	SourceSpan span = c->span;
	while (1)
	{
		const char *string = symstr(c);
		if (!try_consume(c, TOKEN_IDENT))
		{
			if (token_is_keyword(c->tok))
			{
				SEMA_ERROR_HERE("The module path cannot contain a reserved keyword, try another name.");
				return NULL;
			}
			if (token_is_some_ident(c->tok))
			{
				SEMA_ERROR_HERE("The elements of a module path must consist of only lower case letters, 0-9 and '_'.");
				return NULL;
			}
			SEMA_ERROR_HERE("Each '::' must be followed by a regular lower case sub module name.");
			return NULL;
		}
		scratch_buffer_append(string);
		if (!try_consume(c, TOKEN_SCOPE))
		{
			span = extend_span_with_token(span, c->prev_span);
			break;
		}
		scratch_buffer_append("::");
	}
	return path_create_from_string(scratch_buffer_to_string(), scratch_buffer.len, span);
}


// --- Parse import and module

/**
 *
 * module_param
 * 		: TYPE_IDENT
 *		| CONST_IDENT
 *		;
 *
 * module_params
 * 		: module_param
 * 		| module_params ',' module_param
 *		;
 */
static inline bool parse_optional_module_params(ParseContext *c, const char ***tokens_ref)
{

	*tokens_ref = NULL;

	if (!try_consume(c, TOKEN_LESS)) return true;

	if (try_consume(c, TOKEN_GREATER)) RETURN_SEMA_ERROR_HERE("Generic parameter list cannot be empty.");

	// No params
	while (1)
	{
		switch (c->tok)
		{
			case TOKEN_TYPE_IDENT:
			case TOKEN_CONST_IDENT:
				break;
			case TOKEN_COMMA:
				RETURN_SEMA_ERROR_HERE("Unexpected ','");
			case TOKEN_IDENT:
				RETURN_SEMA_ERROR_HERE("The module parameter must be a type or a constant.");
			case TOKEN_CT_IDENT:
			case TOKEN_CT_TYPE_IDENT:
				RETURN_SEMA_ERROR_HERE("The module parameter cannot be a $-prefixed name.");
			default:
				RETURN_SEMA_ERROR_HERE("Only generic parameters are allowed here as parameters to the module.");
		}
		vec_add(*tokens_ref, symstr(c));
		advance(c);
		if (!try_consume(c, TOKEN_COMMA))
		{
			return consume(c, TOKEN_GREATER, "Expected '>'.");
		}
	}

}
/**
 * module ::= MODULE module_path ('<' module_params '>')? (@public|@private|@local|@test|@export|@extern) EOS
 */
bool parse_module(ParseContext *c, AstId contracts)
{
	if (tok_is(c, TOKEN_STRING))
	{
		RETURN_SEMA_ERROR_HERE("'module' should be followed by a plain identifier, not a string. Did you accidentally put the module name between \"\"?");
	}

	if (!tok_is(c, TOKEN_IDENT))
	{
		if (token_is_keyword(c->tok))
		{
			RETURN_SEMA_ERROR_HERE("The module name cannot contain a reserved keyword, try another name.");
		}
		if (token_is_some_ident(c->tok))
		{
			RETURN_SEMA_ERROR_HERE("The module name must consist of only lower case letters, 0-9 and '_'.");
		}
		RETURN_SEMA_ERROR_HERE("'module' should be followed by a module name.");
	}

	Path *path = parse_module_path(c);

	// Expect the module name
	if (!path)
	{
		path = CALLOCS(Path);
		path->len = (unsigned)strlen("#invalid");
		path->module = "#invalid";
		path->span = INVALID_SPAN;
		context_set_module(c, path, NULL);
		recover_top_level(c);
		return false;
	}

	// Is this a generic module?
	const char **generic_parameters = NULL;
	if (!parse_optional_module_params(c, &generic_parameters))
	{
		if (!context_set_module(c, path, NULL)) return false;
		recover_top_level(c);
		if (contracts) RETURN_SEMA_ERROR(astptr(contracts), "Contracts cannot be use with non-generic modules.");
		return true;
	}
	if (!context_set_module(c, path, generic_parameters)) return false;
	if (contracts)
	{
		AstId old_contracts = c->unit->module->contracts;
		if (old_contracts)
		{
			Ast *last = ast_last(astptr(old_contracts));
			last->next = contracts;
		}
		else
		{
			c->unit->module->contracts = contracts;
		}
		while (contracts)
		{
			Ast *current = astptr(contracts);
			contracts = current->next;
			assert(current->ast_kind == AST_CONTRACT);
			switch (current->contract.kind)
			{
				case CONTRACT_UNKNOWN:
				case CONTRACT_PURE:
				case CONTRACT_PARAM:
				case CONTRACT_ERRORS:
				case CONTRACT_ENSURE:
					break;
				case CONTRACT_REQUIRE:
				case CONTRACT_CHECKED:
					continue;
			}
			RETURN_SEMA_ERROR(current, "Invalid constraint - only '@require' and '@checked' are valid for modules.");
		}
	}
	Visibility visibility = VISIBLE_PUBLIC;
	Attr** attrs = NULL;
	if (!parse_attributes(c, &attrs, &visibility)) return false;
	FOREACH_BEGIN(Attr *attr, attrs)
		if (attr->is_custom) RETURN_SEMA_ERROR(attr, "Custom attributes cannot be used with 'module'.");
		switch (attr->attr_kind)
		{
			case ATTRIBUTE_TEST:
				c->unit->test_by_default = true;
				continue;
			case ATTRIBUTE_EXPORT:
				if (attr->exprs) RETURN_SEMA_ERROR(attr, "Expected no arguments to '@export'");
				if (c->unit->export_by_default) RETURN_SEMA_ERROR(attr, "'@export' appeared more than once.");
				c->unit->export_by_default = true;
				continue;
			case ATTRIBUTE_EXTERN:
			{
				if (vec_size(attr->exprs) != 1)
				{
					RETURN_SEMA_ERROR(attr, "Expected 1 argument to '@extern(..), not %d'.", vec_size(attr->exprs));
				}
				Expr *expr = attr->exprs[0];
				if (!expr_is_const_string(expr)) RETURN_SEMA_ERROR(expr, "Expected a constant string.");
				if (c->unit->module->extname)
				{
					RETURN_SEMA_ERROR(attr, "External name for the module may only be declared in one location.");
				}
				c->unit->module->extname = expr->const_expr.string.chars;
				continue;
			}
			default:
				break;
		}
		RETURN_SEMA_ERROR(attr, "'%s' cannot be used after a module declaration.", attr->name);
	FOREACH_END();
	c->unit->default_visibility = visibility;
	CONSUME_EOS_OR_RET(false);
	return true;
}


static bool consume_type_name(ParseContext *c, const char* type)
{
	if (tok_is(c, TOKEN_IDENT) || token_is_keyword(c->tok))
	{
		RETURN_SEMA_ERROR_HERE("Names of %ss must start with an uppercase letter.", type);
	}
	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		RETURN_SEMA_ERROR_HERE("Names of %ss cannot be all uppercase.", type);
	}
	return consume(c, TOKEN_TYPE_IDENT, "'%s' should be followed by the name of the %s.", type, type);
}

bool consume_const_name(ParseContext *c, const char* type)
{
	if (tok_is(c, TOKEN_IDENT) || tok_is(c, TOKEN_TYPE_IDENT) || token_is_keyword(c->tok))
	{
		RETURN_SEMA_ERROR_HERE("Names of %ss must be all uppercase.", type);
	}
	return consume(c, TOKEN_CONST_IDENT, "A constant name was expected here, did you forget it?");
}

/**
 * Parse an optional foo::bar::
 */
bool parse_path_prefix(ParseContext *c, Path** path_ref)
{
	*path_ref = NULL;
	if (!tok_is(c, TOKEN_IDENT) || peek(c) != TOKEN_SCOPE) return true;

	char *scratch_ptr = scratch_buffer.str;
	uint32_t offset = 0;

	Path *path = CALLOCS(Path);
	path->span = c->span;
	unsigned len = (unsigned)strlen(symstr(c));
	memcpy(scratch_ptr, symstr(c), len);
	offset += len;
	SourceSpan last_loc = c->span;
	advance(c);
	advance(c);
	while (tok_is(c, TOKEN_IDENT) && peek(c) == TOKEN_SCOPE)
	{
		last_loc = c->span;
		scratch_ptr[offset++] = ':';
		scratch_ptr[offset++] = ':';
		len = c->data.lex_len;
		memcpy(scratch_ptr + offset, symstr(c), len);
		offset += len;
		advance(c); advance(c);
	}

	TokenType type = TOKEN_IDENT;
	path->span = extend_span_with_token(path->span, last_loc);
	path->module = symtab_add(scratch_ptr, offset, fnv1a(scratch_ptr, offset), &type);
	if (type != TOKEN_IDENT)
	{
		RETURN_SEMA_ERROR(path, "A module name was expected here.");
	}
	path->len = offset;
	*path_ref = path;
	return true;
}

// --- Type parsing

/**
 * base_type
 *		: VOID
 *		| BOOL
 *		| CHAR
 *		| BYTE
 *		| SHORT
 *		| USHORT
 *		| INT
 *		| UINT
 *		| LONG
 *		| ULONG
 *		| FLOAT
 *		| DOUBLE
 *		| TYPE_IDENT
 *		| ident_scope TYPE_IDENT
 *		| CT_TYPE_IDENT
 *		| VIRTUAL (ident_scope TYPE_IDENT)?
 *		| CT_TYPEOF '(' expr ')'
 *		| CT_EVALTYPE '(' expr ')'
 *		;
 *
 * Assume prev_token is the type.
 * @return TypeInfo (poisoned if fails)
 */
static inline TypeInfo *parse_base_type(ParseContext *c)
{
	if (try_consume(c, TOKEN_CT_TYPEFROM))
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_TYPEFROM, c->prev_span);
		CONSUME_OR_RET(TOKEN_LPAREN, poisoned_type_info);
		ASSIGN_EXPR_OR_RET(type_info->unresolved_type_expr, parse_expr(c), poisoned_type_info);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_type_info);
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}
	if (try_consume(c, TOKEN_CT_TYPEOF))
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_TYPEOF, c->prev_span);
		CONSUME_OR_RET(TOKEN_LPAREN, poisoned_type_info);
		ASSIGN_EXPR_OR_RET(type_info->unresolved_type_expr, parse_expr(c), poisoned_type_info);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_type_info);
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}
	if (try_consume(c, TOKEN_CT_VATYPE))
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_VATYPE, c->prev_span);
		CONSUME_OR_RET(TOKEN_LPAREN, poisoned_type_info);
		ASSIGN_EXPR_OR_RET(type_info->unresolved_type_expr, parse_expr(c), poisoned_type_info);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_type_info);
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}
	if (try_consume(c, TOKEN_CT_EVALTYPE))
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_EVALTYPE, c->prev_span);
		CONSUME_OR_RET(TOKEN_LPAREN, poisoned_type_info);
		ASSIGN_EXPR_OR_RET(type_info->unresolved_type_expr, parse_expr(c), poisoned_type_info);
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_type_info);
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}
	SourceSpan range = c->span;
	bool had_error;
	Path *path;
	if (!parse_path_prefix(c, &path)) return poisoned_type_info;
	if (path)
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_IDENTIFIER, range);
		type_info->unresolved.path = path;
		type_info->unresolved.name = symstr(c);
		if (!consume_type_name(c, "type")) return poisoned_type_info;
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}

	TypeInfo *type_info = NULL;
	Type *type_found = NULL;
	switch (c->tok)
	{
		case TOKEN_TYPE_IDENT:
			type_info = type_info_new_curr(c, TYPE_INFO_IDENTIFIER);
			type_info->unresolved.name = symstr(c);
			break;
		case TOKEN_CT_TYPE_IDENT:
			type_info = type_info_new_curr(c, TYPE_INFO_CT_IDENTIFIER);
			type_info->unresolved.name = symstr(c);
			break;
		case TYPE_TOKENS:
			type_found = type_from_token(c->tok);
			break;
		default:
			SEMA_ERROR_HERE("A type name was expected here.");
			return poisoned_type_info;
	}
	if (type_found)
	{
		assert(!type_info);
		type_info = type_info_new_curr(c, TYPE_INFO_IDENTIFIER);
		type_info->resolve_status = RESOLVE_DONE;
		type_info->type = type_found;
	}
	assert(type_info);
	advance(c);
	RANGE_EXTEND_PREV(type_info);
	return type_info;
}

/**
 * array_type_index
 *		: '[' constant_expression ']'
 *		| '[' ']'
 *		| '[' '*' ']'
 *		;
 *
 * @param type the type to wrap, may not be poisoned.
 * @return type (poisoned if fails)
 */
static inline TypeInfo *parse_array_type_index(ParseContext *c, TypeInfo *type)
{
	assert(type_info_ok(type));

	advance_and_verify(c, TOKEN_LBRACKET);
	if (try_consume(c, TOKEN_STAR))
	{
		CONSUME_OR_RET(TOKEN_RBRACKET, poisoned_type_info);
		TypeInfo *inferred_array = type_info_new(TYPE_INFO_INFERRED_ARRAY, type->span);
		inferred_array->array.base = type;
		RANGE_EXTEND_PREV(inferred_array);
		return inferred_array;
	}
	if (try_consume(c, TOKEN_RBRACKET))
	{
		switch (type->subtype)
		{
			case TYPE_COMPRESSED_NONE:
				type->subtype = TYPE_COMPRESSED_SUB;
				break;
			case TYPE_COMPRESSED_PTR:
				type->subtype = TYPE_COMPRESSED_PTRSUB;
				break;
			case TYPE_COMPRESSED_SUB:
				type->subtype = TYPE_COMPRESSED_SUBSUB;
				break;
			default:
			{
				TypeInfo *subarray = type_info_new(TYPE_INFO_SUBARRAY, type->span);
				subarray->array.base = type;
				subarray->array.len = NULL;
				RANGE_EXTEND_PREV(subarray);
				return subarray;
			}
		}
		if (type->resolve_status == RESOLVE_DONE)
		{
			type->type = type_get_subarray(type->type);
		}
		RANGE_EXTEND_PREV(type);
		return type;
	}
	TypeInfo *array = type_info_new(TYPE_INFO_ARRAY, type->span);
	array->array.base = type;
	ASSIGN_EXPR_OR_RET(array->array.len, parse_expr(c), poisoned_type_info);
	CONSUME_OR_RET(TOKEN_RBRACKET, poisoned_type_info);
	RANGE_EXTEND_PREV(array);
	return array;
}

/**
 * vector_type_index
 *		: '[<' constant_expression '>]'
 *		;
 *
 * @param type the type to wrap, may not be poisoned.
 * @return type (poisoned if fails)
 */
static inline TypeInfo *parse_vector_type_index(ParseContext *c, TypeInfo *type)
{
	assert(type_info_ok(type));

	advance_and_verify(c, TOKEN_LVEC);
	TypeInfo *vector = type_info_new(TYPE_INFO_VECTOR, type->span);
	vector->array.base = type;
	if (try_consume(c, TOKEN_RVEC))
	{
		vector->kind = TYPE_INFO_SCALED_VECTOR;
	}
	else if (try_consume(c, TOKEN_STAR))
	{
		CONSUME_OR_RET(TOKEN_RVEC, poisoned_type_info);
		vector->kind = TYPE_INFO_INFERRED_VECTOR;
	}
	else
	{
		ASSIGN_EXPR_OR_RET(vector->array.len, parse_expr(c), poisoned_type_info);
		CONSUME_OR_RET(TOKEN_RVEC, poisoned_type_info);
	}
	RANGE_EXTEND_PREV(vector);
	return vector;
}

/**
 * type
 * 		: base_type
 *		| type '*'
 *		| type array_type_index
 *
 * Assume already stepped into.
 * @return Type, poisoned if parsing is invalid.
 */
TypeInfo *parse_type_with_base(ParseContext *c, TypeInfo *type_info)
{
	while (type_info_ok(type_info))
	{
		switch (c->tok)
		{
			case TOKEN_LVEC:
				type_info = parse_vector_type_index(c, type_info);
				break;
			case TOKEN_LBRACKET:
				type_info = parse_array_type_index(c, type_info);
				break;
			case TOKEN_STAR:
				advance(c);
				{
					switch (type_info->subtype)
					{
						case TYPE_COMPRESSED_NONE:
							type_info->subtype = TYPE_COMPRESSED_PTR;
							break;
						case TYPE_COMPRESSED_PTR:
							type_info->subtype = TYPE_COMPRESSED_PTRPTR;
							break;
						case TYPE_COMPRESSED_SUB:
							type_info->subtype = TYPE_COMPRESSED_SUBPTR;
							break;
						default:
						{
							TypeInfo *ptr_type = type_info_new(TYPE_INFO_POINTER, type_info->span);
							assert(type_info);
							ptr_type->pointer = type_info;
							type_info = ptr_type;
							RANGE_EXTEND_PREV(type_info);
							return type_info;
						}
					}
					if (type_info->resolve_status == RESOLVE_DONE)
					{
						assert(type_info->type);
						type_info->type = type_get_ptr(type_info->type);
					}
					RANGE_EXTEND_PREV(type_info);
					break;
				}
				break;
			default:
				return type_info;
		}
	}
	return type_info;
}

/**
 * type
 * 		: base_type
 *		| type '*'
 *		| type array_type_index
 *
 * Assume already stepped into.
 * @return Type, poisoned if parsing is invalid.
 */
TypeInfo *parse_type(ParseContext *c)
{
	ASSIGN_TYPE_OR_RET(TypeInfo *base, parse_base_type(c), poisoned_type_info);
	return parse_type_with_base(c, base);
}

TypeInfo *parse_optional_type(ParseContext *c)
{
	ASSIGN_TYPE_OR_RET(TypeInfo *info, parse_base_type(c), poisoned_type_info);
	ASSIGN_TYPE_OR_RET(info, parse_type_with_base(c, info), poisoned_type_info);
	if (try_consume(c, TOKEN_BANG))
	{
		assert(!info->optional);
		info->optional = true;
		if (info->resolve_status == RESOLVE_DONE)
		{
			info->type = type_get_optional(info->type);
		}
		RANGE_EXTEND_PREV(info);
	}
	return info;
}


// --- Decl parsing

/**
 * Parse ident ('=' expr)?
 * @param is_static
 * @param type
 * @return
 */
Decl *parse_local_decl_after_type(ParseContext *c, TypeInfo *type)
{
	if (tok_is(c, TOKEN_LPAREN))
	{
		SEMA_ERROR_HERE("Expected '{'.");
		return poisoned_decl;
	}

	if (tok_is(c, TOKEN_CT_IDENT))
	{
		Decl *decl = decl_new_var_current(c, type, VARDECL_LOCAL_CT);
		advance(c);
		if (try_consume(c, TOKEN_EQ))
		{
			if (!parse_decl_initializer(c, decl)) return poisoned_decl;
		}
		return decl;
	}

	EXPECT_IDENT_FOR_OR("variable name", poisoned_decl);

	Decl *decl = decl_new_var_current(c, type, VARDECL_LOCAL);
	advance(c);

	if (!parse_attributes(c, &decl->attributes, NULL)) return poisoned_decl;
	if (tok_is(c, TOKEN_EQ))
	{
		if (!decl)
		{
			SEMA_ERROR_HERE("Expected an identifier before '='.");
			return poisoned_decl;
		}
		advance_and_verify(c, TOKEN_EQ);
		if (!parse_decl_initializer(c, decl)) return poisoned_decl;
	}
	return decl;
}

/**
 * declaration ::= ('static' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
Decl *parse_local_decl(ParseContext *c)
{
	if (tok_is(c, TOKEN_CONST))
	{
		ASSIGN_DECL_OR_RET(Decl *decl, parse_const_declaration(c, false), poisoned_decl);
		decl->visibility = VISIBLE_LOCAL;
		return decl;
	}

	bool is_threadlocal = try_consume(c, TOKEN_TLOCAL);
	bool is_static = !is_threadlocal && try_consume(c, TOKEN_STATIC);

	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_optional_type(c), poisoned_decl);

	ASSIGN_DECL_OR_RET(Decl * decl, parse_local_decl_after_type(c, type), poisoned_decl);
	if (type->optional && decl->var.unwrap)
	{
		SEMA_ERROR(decl, "You cannot use unwrap with an optional variable.");
		return poisoned_decl;
	}
	decl->var.is_static = is_static || is_threadlocal;
	decl->var.is_threadlocal = is_threadlocal;
	decl->visibility = VISIBLE_LOCAL;
	return decl;
}

Expr *parse_decl_or_expr(ParseContext *c, Decl **decl_ref)
{
	if (tok_is(c, TOKEN_VAR))
	{
		ASSIGN_DECL_OR_RET(*decl_ref, parse_var_decl(c), poisoned_expr);
		return NULL;
	}
	TypeInfo *type_info;
	Expr *expr = parse_expr(c);
	if (expr->expr_kind != EXPR_TYPEINFO) return expr;
	ASSIGN_DECL_OR_RET(*decl_ref, parse_local_decl_after_type(c, expr->type_expr), poisoned_expr);
	return NULL;
}


/**
 * const_decl
 *  : 'const' type? IDENT '=' const_expr
 *  ;
 */
static Decl *parse_const_declaration(ParseContext *c, bool is_global)
{
	advance_and_verify(c, TOKEN_CONST);

	TypeInfo *type_info = NULL;

	if (c->tok != TOKEN_CONST_IDENT)
	{
		ASSIGN_TYPE_OR_RET(type_info, parse_type(c), poisoned_decl);
	}
	Decl *decl = decl_new_var(symstr(c), c->span, type_info, VARDECL_CONST);

	if (!consume_const_name(c, "const")) return poisoned_decl;

	if (is_global)
	{
		if (!parse_attributes_for_global(c, decl)) return false;
	}
	else
	{
		if (!parse_attributes(c, &decl->attributes, NULL)) return poisoned_decl;
	}
	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);

	if (!parse_decl_initializer(c, decl)) return poisoned_decl;

	RANGE_EXTEND_PREV(decl);

	return decl;
}

Decl *parse_var_decl(ParseContext *c)
{
	advance_and_verify(c, TOKEN_VAR);
	Decl *decl;
	switch (c->tok)
	{
		case TOKEN_CONST_IDENT:
			SEMA_ERROR_HERE("Constants must be declared using 'const' not 'var'.");
			return poisoned_decl;
		case TOKEN_IDENT:
			decl = decl_new_var_current(c, NULL, VARDECL_LOCAL);
			advance(c);
			if (!tok_is(c, TOKEN_EQ))
			{
				SEMA_ERROR_HERE("'var' must always have an initial value, or the type cannot be inferred.");
				return false;
			}
			advance_and_verify(c, TOKEN_EQ);
			ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), poisoned_decl);
			break;
		case TOKEN_CT_IDENT:
			decl = decl_new_var_current(c, NULL, VARDECL_LOCAL_CT);
			advance(c);
			if (try_consume(c, TOKEN_EQ))
			{
				ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), poisoned_decl);
			}
			break;
		case TOKEN_CT_TYPE_IDENT:
			decl = decl_new_var_current(c, NULL, VARDECL_LOCAL_CT_TYPE);
			advance(c);
			if (try_consume(c, TOKEN_EQ))
			{
				ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), poisoned_decl);
			}
			break;
		default:
			SEMA_ERROR_HERE("Expected a compile time variable name ('$Foo' or '$foo').");
			return poisoned_decl;
	}
	return decl;
}



// --- Parse parameters & throws & attributes

bool parse_attribute(ParseContext *c, Attr **attribute_ref)
{
	bool had_error;
	Path *path;
	if (!parse_path_prefix(c, &path)) return false;
	if (!tok_is(c, TOKEN_AT_IDENT) && !tok_is(c, TOKEN_AT_TYPE_IDENT))
	{
		if (path)
		{
			SEMA_ERROR_HERE("Expected an attribute name.");
			return false;
		}
		*attribute_ref = NULL;
		return true;
	}
	Attr *attr = CALLOCS(Attr);

	attr->name = symstr(c);
	attr->span = c->span;
	attr->path = path;
	if (tok_is(c, TOKEN_AT_IDENT))
	{
		AttributeType type = attribute_by_name(attr->name);
		if (type == ATTRIBUTE_NONE)
		{
			SEMA_ERROR_HERE("This is not a known valid attribute name.");
			return false;
		}
		attr->attr_kind = type;
	}
	else
	{
		attr->is_custom = true;
	}
	advance(c);

	Expr **list = NULL;

	if (try_consume(c, TOKEN_LPAREN))
	{
		while (1)
		{
			Expr *expr;
			switch (c->tok)
			{
				case TOKEN_AMP:
					// &[]
					expr = EXPR_NEW_TOKEN(EXPR_OPERATOR_CHARS);
					expr->resolve_status = RESOLVE_DONE;
					advance(c);
					CONSUME_OR_RET(TOKEN_LBRACKET, false);
					CONSUME_OR_RET(TOKEN_RBRACKET, false);
					expr->expr_operator_chars = OVERLOAD_ELEMENT_REF;
					RANGE_EXTEND_PREV(expr);
					break;
				case TOKEN_LBRACKET:
					// []
					expr = EXPR_NEW_TOKEN(EXPR_OPERATOR_CHARS);
					expr->resolve_status = RESOLVE_DONE;
					advance(c);
					CONSUME_OR_RET(TOKEN_RBRACKET, false);
					expr->expr_operator_chars = try_consume(c, TOKEN_EQ) ? OVERLOAD_ELEMENT_SET : OVERLOAD_ELEMENT_AT;
					RANGE_EXTEND_PREV(expr);
					break;
				default:
					expr = parse_constant_expr(c);
					if (!expr_ok(expr)) return false;
					break;
			}
			vec_add(list, expr);
			if (try_consume(c, TOKEN_RPAREN)) break;
			CONSUME_OR_RET(TOKEN_COMMA, false);
		}
	}

	attr->exprs = list;

	*attribute_ref = attr;
	return true;
}

static bool parse_attributes_for_global(ParseContext *c, Decl *decl)
{
	Visibility visibility = c->unit->default_visibility;
	if (decl->decl_kind == DECL_FUNC) decl->func_decl.attr_test = c->unit->test_by_default;
	decl->is_export = c->unit->export_by_default;
	if (!parse_attributes(c, &decl->attributes, &visibility)) return false;
	decl->visibility = visibility;
	return true;
}

/**
 * attribute_list
 *  : attribute
 *  | attribute_list attribute
 *  ;
 *
 * attribute
 *  : path AT_IDENT | AT_TYPE_IDENT ('(' constant_expression ')')?
 *
 * @return true if parsing succeeded, false if recovery is needed
 */
bool parse_attributes(ParseContext *c, Attr ***attributes_ref, Visibility *visibility_ref)
{
	Visibility visibility = -1;
	while (1)
	{
		Attr *attr;
		if (!parse_attribute(c, &attr)) return false;
		if (!attr) return true;
		Visibility parsed_visibility = -1;
		if (!attr->is_custom)
		{
			switch (attr->attr_kind)
			{
				case ATTRIBUTE_PUBLIC:
					parsed_visibility = VISIBLE_PUBLIC;
					break;
				case ATTRIBUTE_PRIVATE:
					parsed_visibility = VISIBLE_PRIVATE;
					break;
				case ATTRIBUTE_LOCAL:
					parsed_visibility = VISIBLE_LOCAL;
					break;
				default:
					break;
			}
			if (parsed_visibility != -1)
			{
				if (!visibility_ref)
				{
					SEMA_ERROR(attr, "'%s' cannot be used here.");
					return false;
				}
				if (visibility != -1)
				{
					SEMA_ERROR(attr, "Only a single visibility attribute may be added.");
					return false;
				}
				*visibility_ref = visibility = parsed_visibility;
				continue;
			}
		}
		const char *name = attr->name;
		FOREACH_BEGIN(Attr *other_attr, *attributes_ref)
			if (other_attr->name == name)
			{
				SEMA_ERROR(attr, "Repeat of attribute '%s' here.", name);
				return false;
			}
		FOREACH_END();
		vec_add(*attributes_ref, attr);
	}
	return true;
}


/**
 * global_declaration
 * 	: global? optional_type IDENT ';'
 * 	| global? optional_type IDENT '=' expression ';'
 * 	| global? optional_type func_definition
 * 	;
 *
 * @param visibility
 * @return true if parsing succeeded
 */
static inline Decl *parse_global_declaration(ParseContext *c)
{
	bool threadlocal = try_consume(c, TOKEN_TLOCAL);

	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_optional_type(c), poisoned_decl);


	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		SEMA_ERROR_HERE("This looks like a constant variable, did you forget 'const'?");
		return poisoned_decl;
	}

	Decl *decl;
	Decl **decls = NULL;
	while (true)
	{
		decl = decl_new_var_current(c, type, VARDECL_GLOBAL);
		decl->var.is_threadlocal = threadlocal;
		if (!try_consume(c, TOKEN_IDENT))
		{
			if (token_is_some_ident(c->tok))
			{
				SEMA_ERROR_HERE("I expected a variable name here, but global variables need to start with lower case.");
				return poisoned_decl;
			}
			SEMA_ERROR_HERE("The name of a global variable was expected here");
			return poisoned_decl;
		}
		if (!try_consume(c, TOKEN_COMMA)) break;
		vec_add(decls, decl);
	}
	// Add the last, or we miss it.
	if (decls) vec_add(decls, decl);

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;

	if (try_consume(c, TOKEN_EQ))
	{
		if (decls)
		{
			SEMA_ERROR_HERE("Initialization is not allowed with multiple declarations.");
			return poisoned_decl;
		}
		if (!parse_decl_initializer(c, decl)) return poisoned_decl;
	}
	else if (!decl->attributes && tok_is(c, TOKEN_LPAREN) && !threadlocal)
	{
		// Guess we forgot `fn`?
		sema_error_at(type->span, "This looks like the beginning of a function declaration but it's missing the initial `fn`. Did you forget it?");
		return poisoned_decl;
	}
	CONSUME_EOS_OR_RET(poisoned_decl);
	Attr **attributes = decl->attributes;
	if (attributes)
	{
		FOREACH_BEGIN(Decl *d, decls)
			if (d == decl) continue;
			d->attributes = copy_attributes_single(attributes);
		FOREACH_END();
	}
	if (decls)
	{
		decl = decl_calloc();
		decl->decl_kind = DECL_GLOBALS;
		decl->decls = decls;
		return decl;
	}
	return decl;
}



/**
 * param_declaration ::= type_expression '...'?) (IDENT ('=' initializer)?)?
 *  ;
 */
static inline bool parse_param_decl(ParseContext *c, Decl*** parameters, bool require_name)
{
	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_optional_type(c), false);
	if (type->optional)
	{
		SEMA_ERROR(type, "Parameters may not be optional.");
		return false;
	}
	bool vararg = try_consume(c, TOKEN_ELLIPSIS);
	Decl *param = decl_new_var_current(c, type, VARDECL_PARAM);
	param->var.vararg = vararg;
	if (!try_consume(c, TOKEN_IDENT))
	{
		param->name = NULL;
	}
	const char *name = param->name;

	if (!name && require_name)
	{
		if (!tok_is(c, TOKEN_COMMA) && !tok_is(c, TOKEN_RPAREN))
		{
			if (tok_is(c, TOKEN_CT_IDENT))
			{
				SEMA_ERROR_HERE("Compile time identifiers are only allowed as macro parameters.");
				return false;
			}
			sema_error_at_after(type->span, "Unexpected end of the parameter list, did you forget an ')'?");
			return false;
		}
		SEMA_ERROR(type, "The parameter must be named.");
		return false;
	}
	if (name && try_consume(c, TOKEN_EQ))
	{
		if (!parse_decl_initializer(c, param)) return poisoned_decl;
	}

	vec_add(*parameters, param);
	RANGE_EXTEND_PREV(param);
	return true;
}

static bool parse_next_is_typed_parameter(ParseContext *c, ParameterParseKind parse_kind)
{
	switch (c->tok)
	{
		case TOKEN_IDENT:
		{
			return peek(c) == TOKEN_SCOPE;
		}
		case TYPE_TOKENS:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_EVALTYPE:
		case TOKEN_CT_TYPEOF:
		case TOKEN_CT_TYPEFROM:
			return true;
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_CT_VATYPE:
			return parse_kind == PARAM_PARSE_LAMBDA || parse_kind == PARAM_PARSE_CALL;
		default:
			return false;
	}
}

INLINE bool is_end_of_param_list(ParseContext *c)
{
	return tok_is(c, TOKEN_EOS) || tok_is(c, TOKEN_RPAREN);
}
/**
 * parameters ::= (parameter (',' parameter)*)?
 * non_type_ident = IDENT | HASH_IDENT | CT_IDENT
 * parameter ::= type ELLIPSIS? (non_type_ident ('=' expr))?
 *             | ELLIPSIS (CT_TYPE_IDENT | non_type_ident ('=' expr)?)?
 */
bool parse_parameters(ParseContext *c, Decl ***params_ref, Decl **body_params,
                      Variadic *variadic, int *vararg_index_ref, ParameterParseKind parse_kind)
{
	Decl** params = NULL;
	bool var_arg_found = false;
	while (!is_end_of_param_list(c))
	{
		bool ellipsis = try_consume(c, TOKEN_ELLIPSIS);

		// Check for "raw" variadic arguments. This is allowed on C functions and macros.
		if (ellipsis)
		{
			// In the future maybe this
			if (!is_end_of_param_list(c) && !tok_is(c, TOKEN_COMMA))
			{
				SEMA_ERROR_HERE("Expected ')' here.");
				return false;
			}
			// Variadics might not be allowed
			if (!variadic)
			{
				SEMA_ERROR_LAST("Variadic parameters are not allowed.");
				return false;
			}
			// Check that we only have one variadic parameter.
			if (var_arg_found)
			{
				SEMA_ERROR_LAST("Only a single variadic parameter is allowed.");
				return false;
			}
			// Set the variadic type and insert a dummy argument.
			*variadic = VARIADIC_RAW;
			*vararg_index_ref = vec_size(params);
			var_arg_found = true;
			vec_add(params, NULL);
			if (!try_consume(c, TOKEN_COMMA)) break;
			continue;
		}

		// Now we have the following possibilities: "foo", "Foo foo", "Foo... foo", "foo...", "Foo"
		TypeInfo *type = NULL;
		if (parse_next_is_typed_parameter(c, parse_kind))
		{
			// Parse the type,
			ASSIGN_TYPE_OR_RET(type, parse_optional_type(c), false);
			ellipsis = try_consume(c, TOKEN_ELLIPSIS);
			// We might have Foo...
			if (ellipsis)
			{
				if (!variadic)
				{
					SEMA_ERROR_HERE("Variadic arguments are not allowed.");
					return false;
				}
				if (var_arg_found)
				{
					sema_error_at(extend_span_with_token(type->span, c->prev_span), "Only a single variadic parameter is allowed.");
					return false;
				}
				*variadic = VARIADIC_TYPED;
			}
		}

		// We have parsed the optional type, next get the optional variable name
		VarDeclKind param_kind;
		const char *name = NULL;
		SourceSpan span = c->span;
		bool no_name = false;
		switch (c->tok)
		{
			case TOKEN_CONST_IDENT:
			case TOKEN_CT_CONST_IDENT:
				// We reserve upper case constants for globals.
				SEMA_ERROR_HERE("Parameter names may not be all uppercase.");
				return false;
			case TOKEN_IDENT:
				// normal "foo"
				name = symstr(c);
				param_kind = VARDECL_PARAM;
				advance_and_verify(c, TOKEN_IDENT);
				// Check for "foo..." which defines an implicit "any" vararg
				if (try_consume(c, TOKEN_ELLIPSIS))
				{
					// Did we get Foo... foo...
					if (ellipsis)
					{
						SEMA_ERROR_HERE("Unexpected '...' following a vararg declaration.");
						return false;
					}
					ellipsis = true;
					if (!variadic)
					{
						sema_error_at(extend_span_with_token(span, c->span), "Variadic parameters are not allowed.");
						return false;
					}
					// Did we get Foo foo..., then that's an error.
					if (type)
					{
						SEMA_ERROR_HERE("For typed varargs '...', needs to appear after the type.");
						return false;
					}
					// This is "foo..."
					*variadic = VARIADIC_ANY;
					// We generate the type as type_any
					type = type_info_new_base(type_any, c->span);
				}
				break;
			case TOKEN_CT_IDENT:
				// ct_var $foo
				name = symstr(c);
				advance_and_verify(c, TOKEN_CT_IDENT);
				// This will catch Type... $foo and $foo..., neither is allowed.
				if (ellipsis || peek(c) == TOKEN_ELLIPSIS)
				{
					SEMA_ERROR_HERE("Compile time parameters may not be varargs, use untyped macro varargs '...' instead.");
					return false;
				}
				param_kind = VARDECL_PARAM_CT;
				break;
			case TOKEN_AMP:
				// reference &foo
				advance_and_verify(c, TOKEN_AMP);
				name = symstr(c);
				if (!try_consume(c, TOKEN_IDENT))
				{
					SEMA_ERROR_HERE("A regular variable name, e.g. 'foo' was expected after the '&'.");
					return false;
				}
				// This will catch Type... &foo and &foo..., neighter is allowed.
				if (ellipsis || try_consume(c, TOKEN_ELLIPSIS))
				{
					SEMA_ERROR_HERE("Reference parameters may not be varargs, use untyped macro varargs '...' instead.");
					return false;
				}
				// Span includes the "&"
				span = extend_span_with_token(span, c->span);
				param_kind = VARDECL_PARAM_REF;
				break;
			case TOKEN_HASH_TYPE_IDENT:
				// #Foo (not allowed)
				SEMA_ERROR_HERE("An unevaluated expression can never be a type, did you mean to use $Type?");
				return false;
			case TOKEN_HASH_IDENT:
				// expression #foo
				name = symstr(c);
				advance_and_verify(c, TOKEN_HASH_IDENT);
				if (ellipsis || try_consume(c, TOKEN_ELLIPSIS))
				{
					SEMA_ERROR_HERE("Expression parameters may not be varargs, use untyped macro varargs '...' instead.");
					return false;
				}
				param_kind = VARDECL_PARAM_EXPR;
				break;
				// Compile time type $Type
			case TOKEN_CT_TYPE_IDENT:
				name = symstr(c);
				advance_and_verify(c, TOKEN_CT_TYPE_IDENT);
				if (ellipsis || try_consume(c, TOKEN_ELLIPSIS))
				{
					SEMA_ERROR_HERE("Expression parameters may not be varargs, use untyped macro varargs '...' instead.");
					return false;
				}
				param_kind = VARDECL_PARAM_CT_TYPE;
				break;
			case TOKEN_COMMA:
			case TOKEN_EOS:
			case TOKEN_RPAREN:
				// Handle "Type..." and "Type"
				if (!type && !ellipsis)
				{
					sema_error_at_after(c->prev_span, "Expected a parameter.");
					return false;
				}
				no_name = true;
				span = c->prev_span;
				param_kind = VARDECL_PARAM;
				break;
			default:
				SEMA_ERROR_HERE("Expected a parameter.");
				return false;
		}
		if (type && type->optional)
		{
			SEMA_ERROR(type, "Parameters may not be optional.");
			return false;
		}
		Decl *param = decl_new_var(name, span, type, param_kind);
		param->var.type_info = type;
		if (!no_name)
		{
			if (try_consume(c, TOKEN_EQ))
			{
				if (!parse_decl_initializer(c, param)) return poisoned_decl;
			}
		}
		if (!parse_attributes(c, &param->attributes, NULL)) return false;
		if (ellipsis)
		{
			var_arg_found = true;
			param->var.vararg = ellipsis;
			*vararg_index_ref = vec_size(params);
		}
		vec_add(params, param);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	*params_ref = params;
	return true;
}


/**
 *
 * parameter_type_list ::= '(' parameters ')'
 */
static inline bool parse_fn_parameter_list(ParseContext *c, Signature *signature, bool is_interface)
{
	Decl **decls = NULL;
	CONSUME_OR_RET(TOKEN_LPAREN, false);
	Variadic variadic = VARIADIC_NONE;
	int vararg_index = -1;
	if (!parse_parameters(c, &decls, NULL, &variadic, &vararg_index, PARAM_PARSE_FUNC)) return false;
	CONSUME_OR_RET(TOKEN_RPAREN, false);
	signature->vararg_index = vararg_index < 0 ? vec_size(decls) : vararg_index;
	signature->params = decls;
	signature->variadic = variadic;

	return true;
}

// --- Parse types


/**
 * Expect pointer to after '{'
 *
 * struct_body
 *		: '{' struct_declaration_list '}'
 *		;
 *
 * struct_declaration_list
 * 		: struct_member_declaration
 * 		| struct_declaration_list struct_member_declaration
 * 		;
 *
 * struct_member_declaration
 * 		: type_expression identifier_list opt_attributes ';'
 * 		| struct_or_union IDENT opt_attributes struct_body
 *		| struct_or_union opt_attributes struct_body
 *		;
 *
 * @param parent the parent of the struct
 */
bool parse_struct_body(ParseContext *c, Decl *parent)
{
	CONSUME_OR_RET(TOKEN_LBRACE, false);

	assert(decl_is_struct_type(parent));
	MemberIndex index = 0;
	while (!tok_is(c, TOKEN_RBRACE))
	{
		TokenType token_type = c->tok;
		if (token_type == TOKEN_STRUCT || token_type == TOKEN_UNION || token_type == TOKEN_BITSTRUCT)
		{
			DeclKind decl_kind = decl_from_token(token_type);
			Decl *member;
			if (peek(c) != TOKEN_IDENT)
			{
				member = decl_new_with_type(NULL, c->span, decl_kind);
				advance(c);
			}
			else
			{
				advance(c);
				member = decl_new_with_type(symstr(c), c->span, decl_kind);
				advance_and_verify(c, TOKEN_IDENT);
			}
			if (decl_kind == DECL_BITSTRUCT)
			{
				TRY_CONSUME_OR_RET(TOKEN_COLON, "':' followed by bitstruct type (e.g. 'int') was expected here.", poisoned_decl);
				ASSIGN_TYPE_OR_RET(member->bitstruct.base_type, parse_type(c), poisoned_decl);
				if (!parse_attributes_for_global(c, member)) return decl_poison(parent);
				if (!parse_bitstruct_body(c, member)) return decl_poison(parent);
			}
			else
			{
				if (!parse_attributes(c, &member->attributes, NULL)) return false;
				if (!parse_struct_body(c, member)) return decl_poison(parent);
			}
			vec_add(parent->strukt.members, member);
			index++;
			if (index > MAX_MEMBERS)
			{
				SEMA_ERROR(member, "Can't add another member: the count would exceed maximum of %d elements.", MAX_MEMBERS);
				return false;
			}
			continue;
		}
		bool was_inline = false;
		if (token_type == TOKEN_IDENT && symstr(c) == kw_inline)
		{
			if (parent->decl_kind != DECL_STRUCT)
			{
				SEMA_ERROR_HERE("Only structs may have 'inline' elements, did you make a mistake?");
				return false;
			}
			if (index > 0)
			{
				SEMA_ERROR_LAST("Only the first element may be 'inline', did you order your fields wrong?");
				return false;
			}
			parent->is_substruct = true;
			was_inline = true;
			advance(c);
		}
		ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_type(c), false);

		while (1)
		{
			if (!tok_is(c, TOKEN_IDENT))
			{
				SEMA_ERROR_HERE("A valid member name was expected here.");
				return false;
			}
			Decl *member = decl_new_var_current(c, type, VARDECL_MEMBER);
			vec_add(parent->strukt.members, member);
			index++;
			if (index > MAX_MEMBERS)
			{
				SEMA_ERROR(member, "Can't add another member: the count would exceed maximum of %d elements.", MAX_MEMBERS);
				return false;
			}
			advance(c);
			if (!parse_attributes(c, &member->attributes, NULL)) return false;
			if (!try_consume(c, TOKEN_COMMA)) break;
			if (was_inline)
			{
				SEMA_ERROR(member, "'Inline' can only be applied to a single member, so please define it on its own line.");
				return false;
			}
		}
		CONSUME_EOS_OR_RET(false);
	}
	advance_and_verify(c, TOKEN_RBRACE);
	return true;
}



/**
 * struct_declaration
 * 		: struct_or_union TYPE_IDENT opt_attributes struct_body
 * 		;
 *
 * @param visibility
 */
static inline Decl *parse_struct_declaration(ParseContext *c)
{
	TokenType type = c->tok;

	advance(c);
	const char* type_name = type == TOKEN_STRUCT ? "struct" : "union";

	Decl *decl = decl_new_with_type(symstr(c), c->span, decl_from_token(type));

	if (!consume_type_name(c, type_name)) return poisoned_decl;

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;

	if (!parse_struct_body(c, decl))
	{
		return poisoned_decl;
	}
	DEBUG_LOG("Parsed %s %s completely.", type_name, decl->name);
	return decl;
}

/**
 * body ::= '{' (TYPE IDENT ':' expr '..' expr EOS)* '}'
 * @param c
 * @param decl
 * @return
 */
static inline bool parse_bitstruct_body(ParseContext *c, Decl *decl)
{
	CONSUME_OR_RET(TOKEN_LBRACE, false);

	bool is_consecutive = false;
	while (!try_consume(c, TOKEN_RBRACE))
	{
		ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_type(c), false);
		Decl *member_decl = decl_new_var_current(c, type, VARDECL_BITMEMBER);

		if (!try_consume(c, TOKEN_IDENT))
		{
			if (try_consume(c, TOKEN_CONST_IDENT) || try_consume(c, TOKEN_TYPE_IDENT))
			{
				SEMA_ERROR_LAST("Expected a field name with an initial lower case.");
				return false;
			}
			SEMA_ERROR_HERE("Expected a field name at this position.");
			return false;
		}
		if (is_consecutive || tok_is(c, TOKEN_EOS))
		{
			if (!is_consecutive)
			{
				if (decl->bitstruct.members)
				{
					SEMA_ERROR_HERE("Expected a ':'.");
					return false;
				}
				is_consecutive = true;
			}
			CONSUME_OR_RET(TOKEN_EOS, false);
			unsigned index = vec_size(decl->bitstruct.members);
			member_decl->var.start_bit = index;
			member_decl->var.end_bit = index;
			vec_add(decl->bitstruct.members, member_decl);
			continue;
		}
		CONSUME_OR_RET(TOKEN_COLON, false);
		ASSIGN_EXPR_OR_RET(member_decl->var.start, parse_constant_expr(c), false);
		if (try_consume(c, TOKEN_DOTDOT))
		{
			ASSIGN_EXPR_OR_RET(member_decl->var.end, parse_constant_expr(c), false);
		}
		else
		{
			member_decl->var.end = NULL;
		}
		CONSUME_EOS_OR_RET(false);
		vec_add(decl->bitstruct.members, member_decl);
	}
	decl->bitstruct.consecutive = is_consecutive;
	return true;
}
/**
 * bitstruct_declaration = 'bitstruct' IDENT ':' type bitstruct_body
 */
static inline Decl *parse_bitstruct_declaration(ParseContext *c)
{
	advance_and_verify(c, TOKEN_BITSTRUCT);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_BITSTRUCT);

	if (!consume_type_name(c, "bitstruct")) return poisoned_decl;

	TRY_CONSUME_OR_RET(TOKEN_COLON, "':' followed by bitstruct type (e.g. 'int') was expected here.", poisoned_decl);

	ASSIGN_TYPE_OR_RET(decl->bitstruct.base_type, parse_type(c), poisoned_decl);

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;

	if (!parse_bitstruct_body(c, decl)) return poisoned_decl;

	return decl;

}

static inline Decl *parse_top_level_const_declaration(ParseContext *c)
{
	ASSIGN_DECL_OR_RET(Decl * decl, parse_const_declaration(c, true), poisoned_decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}


/**
 * macro_arguments ::= '(' parameters (EOS trailing_block_parameter )? ')'
 *
 * trailing_block_parameter ::= '@' IDENT ( '(' parameters ')' )?
 */
static bool parse_macro_arguments(ParseContext *c, Decl *macro)
{
	CONSUME_OR_RET(TOKEN_LPAREN, false);

	// Parse the regular parameters.
	Variadic variadic = VARIADIC_NONE;
	int vararg_index = -1;
	Decl **params = NULL;
	if (!parse_parameters(c, &params, NULL, &variadic, &vararg_index, PARAM_PARSE_MACRO)) return false;
	macro->func_decl.signature.params = params;
	macro->func_decl.signature.vararg_index = vararg_index < 0 ? vec_size(params) : vararg_index;
	macro->func_decl.signature.variadic = variadic;

	// Do we have trailing block parameters?
	if (try_consume(c, TOKEN_EOS))
	{
		// Consume AT_IDENT
		Decl *body_param = decl_new(DECL_BODYPARAM, symstr(c), c->span);
		TRY_CONSUME_OR_RET(TOKEN_AT_IDENT, "Expected an ending ')' or a block parameter on the format '@block(...).", false);
		if (try_consume(c, TOKEN_LPAREN))
		{
			if (!parse_parameters(c, &body_param->body_params, NULL, NULL, NULL, PARAM_PARSE_BODY)) return false;
			CONSUME_OR_RET(TOKEN_RPAREN, false);
		}
		macro->func_decl.body_param = declid(body_param);
	}
	else
	{
		macro->func_decl.body_param = 0;
	}
	CONSUME_OR_RET(TOKEN_RPAREN, false);
	return true;
}

/**
 * define_parameters ::= expr (',' expr)* '>'
 *
 * @return NULL if parsing failed, otherwise a list of Type*
 */
static inline Expr **parse_generic_parameters(ParseContext *c)
{
	Expr **params = NULL;
	while (!try_consume(c, TOKEN_GREATER))
	{
		ASSIGN_EXPR_OR_RET(Expr *arg, parse_generic_parameter(c), NULL);
		vec_add(params, arg);
		TokenType tok = c->tok;
		if (tok != TOKEN_RPAREN && tok != TOKEN_GREATER)
		{
			TRY_CONSUME_OR_RET(TOKEN_COMMA, "Expected ',' after argument.", NULL);
		}
	}
	return params;
}

static inline void decl_add_type(Decl *decl, TypeKind kind)
{
	Type *type = type_new(kind, decl->name);
	type->canonical = type;
	type->decl = decl;
	decl->type = type;
}
/**
 * define_type_body ::= TYPE_IDENT '=' 'distinct'? (func_typedef | type generic_params?) ';'
 *
 * func_typedef ::= 'fn' optional_type parameter_type_list
 */
static inline Decl *parse_define_type(ParseContext *c)
{
	advance(c);

	Decl *decl = decl_new(DECL_POISONED, symstr(c), c->span);
	DEBUG_LOG("Parse define %s", decl->name);
	if (!try_consume(c, TOKEN_TYPE_IDENT))
	{
		if (token_is_any_type(c->tok))
		{
			SEMA_ERROR_HERE("'%s' is the name of a built-in type and can't be used as an alias.",
			                token_type_to_string(c->tok));
			return poisoned_decl;
		}
		if (token_is_some_ident(c->tok))
		{
			SEMA_ERROR_HERE("The type name must start with an uppercase letter followed by at least 1 lowercase letter.");
			return poisoned_decl;
		}
		SEMA_ERROR_HERE("A type name was expected here.");
		return poisoned_decl;
	}

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;

	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);
	bool distinct = false;
	bool is_inline = false;
	if (tok_is(c, TOKEN_IDENT))
	{
		if (symstr(c) == kw_inline)
		{
			SEMA_ERROR_HERE("'inline' must always follow 'distinct'.");
			return poisoned_decl;
		}
		if (symstr(c) == kw_distinct)
		{
			distinct = true;
			advance(c);
			if (tok_is(c, TOKEN_IDENT) && symstr(c) == kw_inline)
			{
				is_inline = true;
				advance(c);
			}
		}
	}

	// 1. Did we have `fn`? In that case it's a function pointer.
	if (try_consume(c, TOKEN_FN))
	{
		decl->decl_kind = DECL_TYPEDEF;
		decl_add_type(decl, TYPE_TYPEDEF);
		decl->typedef_decl.is_func = true;
		decl->typedef_decl.is_distinct = distinct;
		decl->is_substruct = is_inline;
		ASSIGN_TYPE_OR_RET(TypeInfo *type_info, parse_optional_type(c), poisoned_decl);
		decl->typedef_decl.function_signature.rtype = type_infoid(type_info);
		if (!parse_fn_parameter_list(c, &(decl->typedef_decl.function_signature), true))
		{
			return poisoned_decl;
		}
		RANGE_EXTEND_PREV(decl);
		CONSUME_EOS_OR_RET(poisoned_decl);
		return decl;
	}

	// 2. Now parse the type which we know is here.
	ASSIGN_TYPE_OR_RET(TypeInfo *type_info, parse_type(c), poisoned_decl);

	// 3. Do we have '<' if so it's a parameterized type e.g. foo::bar::Type<int, double>.
	if (try_consume(c, TOKEN_LESS))
	{
		Expr **params = parse_generic_parameters(c);
		if (!params) return poisoned_decl;
		decl->decl_kind = DECL_DEFINE;
		decl_add_type(decl, TYPE_TYPEDEF);
		decl->define_decl.define_kind = DEFINE_TYPE_GENERIC;
		decl->define_decl.type_info = type_info;
		decl->define_decl.generic_params = params;
		RANGE_EXTEND_PREV(decl);
		CONSUME_EOS_OR_RET(poisoned_decl);
		return decl;
	}
	REMINDER("Distinct fn??");
	decl->typedef_decl.type_info = type_info;
	decl->typedef_decl.is_func = false;
	if (distinct)
	{
		decl->decl_kind = DECL_DISTINCT;
		decl_add_type(decl, TYPE_DISTINCT);
		decl->is_substruct = is_inline;
		TypedefDecl typedef_decl = decl->typedef_decl; // Ensure value semantics.
		decl->distinct_decl.typedef_decl = typedef_decl;
		decl->type->type_kind = TYPE_DISTINCT;
		decl->decl_kind = DECL_DISTINCT;
	}
	else
	{
		decl->decl_kind = DECL_TYPEDEF;
		decl_add_type(decl, TYPE_TYPEDEF);
	}
	RANGE_EXTEND_PREV(decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}

/**
 * define_ident ::= 'define' (IDENT | CONST_IDENT) '=' identifier_alias generic_params?
 *
 * identifier_alias ::= path? (IDENT | CONST_IDENT)
 */
static inline Decl *parse_define_ident(ParseContext *c)
{
	// 1. Store the beginning of the "define".
	advance_and_verify(c, TOKEN_DEFINE);

	// 2. At this point we expect an ident or a const token.
	//    since the Type is handled.
	TokenType alias_type = c->tok;
	if (alias_type != TOKEN_IDENT && alias_type != TOKEN_CONST_IDENT && alias_type != TOKEN_AT_IDENT)
	{
		if (token_is_any_type(alias_type))
		{
			SEMA_ERROR_HERE("'%s' is the name of a built-in type and can't be used as an alias.",
			                token_type_to_string(alias_type));
		}
		else
		{
			SEMA_ERROR_HERE("An identifier was expected here.");
		}
		return poisoned_decl;
	}

	// 3. Set up the "define".
	Decl *decl = decl_new(DECL_DEFINE, symstr(c), c->span);
	decl->define_decl.define_kind = DEFINE_IDENT_ALIAS;

	if (decl->name == kw_main)
	{
		SEMA_ERROR(decl, "'main' is reserved and cannot be used as an alias.");
		return poisoned_decl;
	}
	// 4. Advance and consume the '='
	advance(c);
	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);

	// 5. Here we may an (optional) path, we just check if it starts
	//    with IDENT '::'
	Path *path = NULL;
	if (context_next_is_path_prefix_start(c))
	{
		if (!parse_path_prefix(c, &path)) return poisoned_decl;
	}

	decl->define_decl.path = path;

	// 6. Check that the token after the path is of the same type.
	if (c->tok != alias_type)
	{
		if (token_is_any_type(c->tok) || tok_is(c, TOKEN_TYPE_IDENT))
		{
			SEMA_ERROR(decl, "A type alias must start with an uppercase letter and contain at least one lower case letter.");
			return poisoned_decl;
		}
		if (alias_type == TOKEN_CONST_IDENT)
		{
			SEMA_ERROR_HERE("Expected a constant name here.");
			return poisoned_decl;
		}
		if (alias_type == TOKEN_IDENT && c->tok == TOKEN_AT_IDENT)
		{
			SEMA_ERROR(decl, "A name with '@' prefix cannot be aliased to a name without '@', try adding a '@' before '%s'.", decl->name);
			return poisoned_decl;
		}
		if (alias_type == TOKEN_AT_IDENT && c->tok == TOKEN_IDENT)
		{
			SEMA_ERROR(decl, "An alias cannot use '@' if the aliased identifier doesn't, please remove the '@' symbol.");
			return poisoned_decl;
		}
		SEMA_ERROR_HERE("Expected a function or variable name here.");
		return poisoned_decl;
	}

	// 7. Consume the identifier
	decl->define_decl.ident = symstr(c);
	decl->define_decl.span = c->span;
	advance(c);

	if (try_consume(c, TOKEN_LESS))
	{
		decl->define_decl.define_kind = DEFINE_IDENT_GENERIC;
		Expr **params = parse_generic_parameters(c);
		if (!params) return poisoned_decl;
		decl->define_decl.generic_params = params;
	}
	else if (!tok_is(c, TOKEN_EOS) && decl->define_decl.ident == kw_distinct)
	{
		if (token_is_any_type(c->tok))
		{
			SEMA_ERROR(decl, "A type name alias must start with an uppercase letter.");
		}
		else
		{
			sema_error_at(decl->define_decl.span, "'distinct' can only be used with types.");
		}
		return poisoned_decl;
	}
	RANGE_EXTEND_PREV(decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}

/**
 * define_attribute ::= 'define' AT_TYPE_IDENT '(' parameter_list ')' ('=' attribute_list)?
 */
static inline Decl *parse_define_attribute(ParseContext *c)
{
	// 1. Store the beginning of the "define".
	advance_and_verify(c, TOKEN_DEFINE);

	Decl *decl = decl_new(DECL_ATTRIBUTE, symstr(c), c->span);

	advance_and_verify(c, TOKEN_AT_TYPE_IDENT);

	if (try_consume(c, TOKEN_LPAREN))
	{
		if (!parse_parameters(c, &decl->attr_decl.params, NULL, NULL, NULL, PARAM_PARSE_ATTR)) return poisoned_decl;
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_decl);
	}

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;
	Attr **attributes = NULL;

	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);
	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_decl);

	if (!parse_attributes(c, &attributes, NULL)) return poisoned_decl;
	CONSUME_OR_RET(TOKEN_RBRACE, poisoned_decl);
	decl->attr_decl.attrs = attributes;
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}

/**
 * define_decl ::= DEFINE define_type_body |
 */
static inline Decl *parse_define(ParseContext *c)
{
	switch (peek(c))
	{
		case TOKEN_AT_TYPE_IDENT:
			// define @Foo = @inline, @noreturn
			return parse_define_attribute(c);
		case TOKEN_TYPE_IDENT:
			sema_warning_at(c->span, "Defining types with 'define' is deprecated. Use 'typedef'.");
			return parse_define_type(c);
		default:
			return parse_define_ident(c);
	}
}

static inline bool parse_is_macro_name(ParseContext *c)
{
	return (tok_is(c, TOKEN_IDENT) && peek(c) != TOKEN_SCOPE) || tok_is(c, TOKEN_AT_IDENT);
}

/**
 * func_header ::= type '!'? (type '.')? (IDENT | MACRO_IDENT)
 * macro_header ::= (type '!'?)? (type '.')? (IDENT | MACRO_IDENT)
 */
static inline bool parse_func_macro_header(ParseContext *c, Decl *decl)
{
	TypeInfo *rtype = NULL;
	TypeInfo *method_type = NULL;

	bool is_macro = decl->decl_kind == DECL_MACRO;

	// 1. If we have a macro and see the name, we're done.
	if (is_macro && parse_is_macro_name(c))
	{
		goto RESULT;
	}

	// 2. Now we must have a type - either that is the return type or the method type.
	ASSIGN_TYPE_OR_RET(rtype, parse_optional_type(c), false);

	// 4. We might have a type here, if so then we read it.
	if (!tok_is(c, TOKEN_DOT) && !parse_is_macro_name(c))
	{
		ASSIGN_TYPE_OR_RET(method_type, parse_type(c), false);
	}

	// 5. If we have a dot here, then we need to interpret this as method function.
	if (try_consume(c, TOKEN_DOT))
	{
		// 5a. What if we don't have a method type?
		if (!method_type)
		{
			// 5b. If the rtype is not optional or the return type was an optional, then this is an error.
			if (!is_macro || rtype->optional)
			{
				SEMA_ERROR_LAST("This looks like you are declaring a method without a return type?");
				return false;
			}
			method_type = rtype;
			rtype = NULL;
		}
	}
	else if (method_type)
	{
		// 5d. A method type but no dot is also wrong.
		SEMA_ERROR(method_type, "There is unexpectedly a type after the return type, did you forget a '.'?");
		return false;
	}
	RESULT:
	decl->name = symstr(c);
	decl->span = c->span;
	if (is_macro && c->tok != TOKEN_IDENT && c->tok != TOKEN_AT_IDENT)
	{
		sema_error_at(c->span, "Expected a macro name here, e.g. '@someName' or 'someName'.");
		return false;
	}
	else if (!is_macro && c->tok != TOKEN_IDENT)
	{
		sema_error_at(c->span, "Expected a function name here, e.g. 'someName'.");
		return false;
	}
	advance(c);
	decl->func_decl.signature.rtype = rtype ? type_infoid(rtype) : 0;
	decl->func_decl.signature.is_macro = is_macro;
	decl->func_decl.signature.is_at_macro = decl->name[0] == '@';
	decl->func_decl.type_parent = method_type ? type_infoid(method_type) : 0;
	return true;
}


/**
 * macro ::= macro_header '(' macro_params ')' compound_statement
 */
static inline Decl *parse_macro_declaration(ParseContext *c, AstId docs)
{
	advance_and_verify(c, TOKEN_MACRO);

	Decl *decl = decl_calloc();
	decl->decl_kind = DECL_MACRO;
	decl->func_decl.docs = docs;
	if (!parse_func_macro_header(c, decl)) return poisoned_decl;
	const char *block_parameter = NULL;
	if (!parse_macro_arguments(c, decl)) return poisoned_decl;

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;
	if (tok_is(c, TOKEN_IMPLIES))
	{
		ASSIGN_ASTID_OR_RET(decl->func_decl.body,
		                    parse_short_body(c, decl->func_decl.signature.rtype, true), poisoned_decl);
		return decl;
	}
	ASSIGN_ASTID_OR_RET(decl->func_decl.body, parse_stmt(c), poisoned_decl);
	return decl;
}


/**
 * error_declaration
 *		: FAULT TYPE_IDENT ';'
 *		| FAULT TYPE_IDENT '{' error_data '}'
 *		;
 */
static inline Decl *parse_fault_declaration(ParseContext *c, bool is_private)
{
	advance(c);
	// advance_and_verify(context, TOKEN_ERRTYPE);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_FAULT);
	if (is_private) decl->visibility = VISIBLE_PRIVATE;
	if (!consume_type_name(c, "fault")) return poisoned_decl;

	TypeInfo *type = NULL;

	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_decl);

	decl->enums.type_info = type_info_new_base(type_iptr->canonical, decl->span);
	uint64_t ordinal = 0;
	while (!try_consume(c, TOKEN_RBRACE))
	{
		Decl *fault_const = decl_new(DECL_FAULTVALUE, symstr(c), c->span);
		if (is_private) decl->visibility = VISIBLE_PRIVATE;
		if (!consume_const_name(c, "fault value"))
		{
			return poisoned_decl;
		}
		const char *name = fault_const->name;
		fault_const->enum_constant.parent = declid(decl);
		fault_const->enum_constant.ordinal = ordinal;
		ordinal++;
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name == name)
			{
				SEMA_ERROR(fault_const, "This fault value was declared twice.");
				SEMA_NOTE(other_constant, "The previous declaration was here.");
				decl_poison(fault_const);
				break;
			}
		}
		vec_add(decl->enums.values, fault_const);
		// Allow trailing ','
		if (!try_consume(c, TOKEN_COMMA))
		{
			EXPECT_OR_RET(TOKEN_RBRACE, poisoned_decl);
		}
	}
	return decl;
}

/**
 * enum_spec
 *  : type
 *  | type '(' opt_parameter_type_list ')'
 *  ;
 */
static inline bool parse_enum_spec(ParseContext *c, TypeInfo **type_ref, Decl*** parameters_ref)
{

	ASSIGN_TYPE_OR_RET(*type_ref, parse_optional_type(c), false);
	if ((*type_ref)->optional)
	{
		SEMA_ERROR(*type_ref, "An enum can't have an optional type.");
		return false;
	}
	if (!try_consume(c, TOKEN_LPAREN)) return true;
	while (!try_consume(c, TOKEN_RPAREN))
	{
		if (!parse_param_decl(c, parameters_ref, true)) return false;
		Decl *last_parameter = VECLAST(*parameters_ref);
		assert(last_parameter);
		if (last_parameter->var.vararg)
		{
			SEMA_ERROR_LAST("Vararg parameters are not allowed as enum parameters.");
			return false;
		}
		last_parameter->var.index = vec_size(*parameters_ref) - 1;
		if (!try_consume(c, TOKEN_COMMA))
		{
			EXPECT_OR_RET(TOKEN_RPAREN, false);
		}
	}
	return true;
}

/**
 * Expect current at enum name.
 *
 * enum
 *  : ENUM type_ident '{' enum_body '}'
 *  | ENUM type_ident ':' enum_spec '{' enum_body '}'
 *  ;
 *
 * enum_body
 *  : enum_def
 *  | enum_def ',' enum_body
 *  | enum_body ','
 *  ;
 *
 * enum_def
 *  : CAPS_IDENT
 *  | CAPS_IDENT '(' expr_list ')'
 *  ;
 *
 */
static inline Decl *parse_enum_declaration(ParseContext *c, bool is_private)
{
	advance_and_verify(c, TOKEN_ENUM);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_ENUM);
	if (is_private) decl->visibility = VISIBLE_PRIVATE;
	if (!consume_type_name(c, "enum")) return poisoned_decl;

	TypeInfo *type = NULL;
	if (try_consume(c, TOKEN_COLON))
	{
		if (!parse_enum_spec(c, &type, &decl->enums.parameters)) return poisoned_decl;
	}

	if (!parse_attributes_for_global(c, decl)) return poisoned_decl;

	Visibility visibility = decl->visibility;
	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_decl);

	decl->enums.type_info = type ? type : type_info_new_base(type_int, decl->span);

	while (!try_consume(c, TOKEN_RBRACE))
	{
		Decl *enum_const = decl_new(DECL_ENUM_CONSTANT, symstr(c), c->span);
		enum_const->visibility = visibility;
		const char *name = enum_const->name;
		if (!consume_const_name(c, "enum constant"))
		{
			return poisoned_decl;
		}
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name == name)
			{
				SEMA_ERROR(enum_const, "This enum constant is declared twice.");
				SEMA_NOTE(other_constant, "The previous declaration was here.");
				decl_poison(enum_const);
				break;
			}
		}
		if (try_consume(c, TOKEN_LPAREN))
		{
			Expr **result = NULL;
			if (!parse_arg_list(c, &result, TOKEN_RPAREN, NULL, false)) return poisoned_decl;
			enum_const->enum_constant.args = result;
			CONSUME_OR_RET(TOKEN_RPAREN, poisoned_decl);
		}
		vec_add(decl->enums.values, enum_const);
		// Allow trailing ','
		if (!try_consume(c, TOKEN_COMMA))
		{
			EXPECT_OR_RET(TOKEN_RBRACE, poisoned_decl);
		}
	}
	return decl;
}

// --- Parse function



/**
 * Starts after 'fn'
 *
 * func_name
 *		: path TYPE_IDENT '.' IDENT
 *		| TYPE_IDENT '.' IDENT
 *		| IDENT
 *		;
 *
 * func_definition
 * 		: func_declaration compound_statement
 *  	| func_declaration ';'
 *  	;
 *
 * func_declaration
 *  	: FN optional_type func_name '(' opt_parameter_type_list ')' opt_attributes
 *		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_func_definition(ParseContext *c, AstId docs, bool is_interface)
{
	advance_and_verify(c, TOKEN_FN);
	Decl *func = decl_calloc();
	func->decl_kind = DECL_FUNC;
	func->func_decl.docs = docs;
	if (!parse_func_macro_header(c, func)) return poisoned_decl;
	if (func->name[0] == '@')
	{
		SEMA_ERROR(func, "Function names may not use '@'.");
		return false;
	}
	if (!parse_fn_parameter_list(c, &(func->func_decl.signature), is_interface)) return poisoned_decl;
	if (!parse_attributes_for_global(c, func)) return poisoned_decl;

	// TODO remove
	is_interface = tok_is(c, TOKEN_EOS);

	if (is_interface)
	{
		if (tok_is(c, TOKEN_LBRACE))
		{
			advance(c);
			SEMA_ERROR_HERE("A function body is not allowed here.");
			return poisoned_decl;
		}
		TRY_CONSUME_OR_RET(TOKEN_EOS, "Expected ';' after function declaration.", poisoned_decl);
		return func;
	}

	if (tok_is(c, TOKEN_IMPLIES))
	{
		ASSIGN_ASTID_OR_RET(func->func_decl.body,
		                    parse_short_body(c, func->func_decl.signature.rtype, true), poisoned_decl);
	}
	else if (tok_is(c, TOKEN_LBRACE))
	{
		ASSIGN_ASTID_OR_RET(func->func_decl.body, parse_compound_stmt(c), poisoned_decl);
	}
	else
	{
		SEMA_ERROR_HERE("Expected the beginning of a block or a short statement.");
	}

	DEBUG_LOG("Finished parsing function %s", func->name);
	return func;
}

static inline Decl *parse_static_top_level(ParseContext *c)
{
	advance_and_verify(c, TOKEN_STATIC);
	Decl *init = decl_calloc();
	if (!tok_is(c, TOKEN_IDENT))
	{
		if (token_is_any_type(c->tok))
		{
			SEMA_ERROR_HERE("'static' can only used with local variables, to hide global variables and functions, use 'private'.");
			return poisoned_decl;
		}
		SEMA_ERROR_HERE("Expected 'static initialize' or 'static finalize'.");
		return poisoned_decl;
	}
	init->decl_kind = DECL_INITIALIZE;
	if (c->data.string == kw_finalize)
	{
		init->decl_kind = DECL_FINALIZE;
	}
	else if (c->data.string != kw_initialize)
	{
		SEMA_ERROR_HERE("Expected 'static initialize' or 'static finalize'.");
		return poisoned_decl;
	}
	advance(c);
	Attr *attr = NULL;
	if (!parse_attributes(c, &init->attributes, NULL)) return poisoned_decl;
	ASSIGN_ASTID_OR_RET(init->xxlizer.init, parse_compound_stmt(c), poisoned_decl);
	RANGE_EXTEND_PREV(init);
	return init;
}

static inline bool check_no_visibility_before(ParseContext *c, bool is_private)
{
	if (is_private)
	{
		SEMA_ERROR_HERE("Unexpected 'private' before '%s'", symstr(c));
		return false;
	}
	return true;
}



/**
 *
 * import ::= IMPORT import_path (',' import_path)* EOS
 *
 * @return true if import succeeded
 */
static inline bool parse_import(ParseContext *c)
{
	advance_and_verify(c, TOKEN_IMPORT);

	bool is_not_first = false;
	while (1)
	{
		if (!tok_is(c, TOKEN_IDENT))
		{
			if (is_not_first)
			{
				SEMA_ERROR_LAST("Another module name was expected after the comma.");
				return false;
			}
			if (tok_is(c, TOKEN_STRING))
			{
				SEMA_ERROR_HERE("An import should be followed by a plain identifier, not a string. Did you accidentally put the module name between \"\"?");
				return false;
			}
			SEMA_ERROR_HERE("Import statement should be followed by the name of the module to import.");
			return false;
		}
		is_not_first = true;
		Path *path = parse_module_path(c);
		if (!path) return false;
		bool private = false;
		if (tok_is(c, TOKEN_AT_IDENT))
		{
			if (symstr(c) != attribute_list[ATTRIBUTE_PUBLIC])
			{
				SEMA_ERROR_HERE("Only '@public' is a valid attribute here.");
				return false;
			}
			private = true;
			advance_and_verify(c, TOKEN_AT_IDENT);
		}
		unit_add_import(c->unit, path, private);
		if (tok_is(c, TOKEN_COLON) && peek(c) == TOKEN_IDENT)
		{
			SEMA_ERROR_HERE("'::' was expected here, did you make a mistake?");
			return false;
		}
		if (!try_consume(c, TOKEN_COMMA)) break;
	}

	CONSUME_EOS_OR_RET(false);
	return true;
}



/**
 * contract ::= expression_list (':'? STRING)?
 */
static inline bool parse_doc_contract(ParseContext *c, AstId **docs_ref, ContractKind kind)
{
	Ast *ast = ast_new_curr(c, AST_CONTRACT);
	ast->contract.kind = kind;
	const char *start = c->lexer.data.lex_start;
	advance(c);
	ASSIGN_EXPR_OR_RET(ast->contract.contract.decl_exprs, parse_expression_list(c, kind == CONTRACT_CHECKED), false);
	const char *end = start;
	while (*++end != '\n' && *end != '\0') end++;
	if (end > c->data.lex_start) end = c->data.lex_start;
	while (end[-1] == ' ') end--;
	scratch_buffer_clear();
	switch (kind)
	{
		case CONTRACT_CHECKED:
			scratch_buffer_append("@checked \"");
			break;
		case CONTRACT_ENSURE:
			scratch_buffer_append("@ensure \"");
			break;
		default:
			scratch_buffer_append("@require \"");
			break;
	}
	scratch_buffer_append_len(start, end - start);
	scratch_buffer_append("\" violated");
	if (try_consume(c, TOKEN_COLON))
	{
		if (!tok_is(c, TOKEN_STRING))
		{
			sema_error_at(c->prev_span, "Expected a string after ':'");
			return false;
		}
	}
	if (tok_is(c, TOKEN_STRING))
	{
		scratch_buffer_append(": '");
		scratch_buffer_append(symstr(c));
		scratch_buffer_append("'.");
		ast->contract.contract.comment = scratch_buffer_copy();
		advance(c);
	}
	else
	{
		scratch_buffer_append(".");
		ast->contract.contract.expr_string = scratch_buffer_copy();
	}
	**docs_ref = astid(ast);
	*docs_ref = &ast->next;
	return true;
}

/**
 * param_contract ::= '@param' inout_attribute? any_identifier ( (':' STRING) | STRING )?
 * inout_attribute ::= '[' '&'? ('in' | 'inout' | 'out') ']'
 */
static inline bool parse_contract_param(ParseContext *c, AstId **docs_ref)
{
	Ast *ast = ast_new_curr(c, AST_CONTRACT);
	ast->contract.kind = CONTRACT_PARAM;
	advance(c);

	// [inout] [in] [out]
	bool is_ref = false;
	InOutModifier mod = PARAM_ANY;
	if (try_consume(c, TOKEN_LBRACKET))
	{
		is_ref = try_consume(c, TOKEN_AMP);
		const char *modifier = tok_is(c, TOKEN_IDENT) ? symstr(c) : NULL;
		if (modifier) advance(c);
		if (modifier == kw_in)
		{
			mod = PARAM_IN;
		}
		else if (modifier == kw_inout)
		{
			mod = PARAM_INOUT;
		}
		else if (modifier == kw_out)
		{
			mod = PARAM_OUT;
		}
		else
		{
			RETURN_SEMA_ERROR_LAST("'in', 'out' or 'inout' were expected.");
		}
		CONSUME_OR_RET(TOKEN_RBRACKET, false);
	}

	switch (c->tok)
	{
		case TOKEN_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_CONST_IDENT:
		case TOKEN_HASH_CONST_IDENT:
		case TOKEN_HASH_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_CONST_IDENT:
		case TOKEN_HASH_IDENT:
			break;
		default:
			SEMA_ERROR_HERE("Expected a parameter name here.");
			return false;
	}
	ast->contract.param.name = symstr(c);
	ast->contract.param.span = c->span;
	ast->contract.param.modifier = mod;
	ast->contract.param.by_ref = is_ref;
	advance(c);
	if (try_consume(c, TOKEN_COLON))
	{
		CONSUME_OR_RET(TOKEN_STRING, false);
	}
	else
	{
		try_consume(c, TOKEN_STRING);
	}
	**docs_ref = astid(ast);
	*docs_ref = &ast->next;
	return true;
}

static inline bool parse_doc_errors(ParseContext *c, AstId **docs_ref)
{
	DocOptReturn *returns = NULL;
	Ast *ast = ast_new_curr(c, AST_CONTRACT);
	ast->contract.kind = CONTRACT_ERRORS;
	advance(c);
	while (1)
	{
		DocOptReturn ret = { .span = c->span };
		ASSIGN_TYPE_OR_RET(ret.type, parse_base_type(c), false);
		if (ret.type->kind != TYPE_INFO_IDENTIFIER)
		{
			SEMA_ERROR(ret.type, "Expected a fault type.");
			return false;
		}
		if (try_consume(c, TOKEN_DOT))
		{
			ret.ident = c->data.string;
			TRY_CONSUME_OR_RET(TOKEN_CONST_IDENT, "Expected a fault value.", false);
		}
		ret.span = extend_span_with_token(ret.span, c->prev_span);
		vec_add(returns, ret);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	RANGE_EXTEND_PREV(ast);
	ast->contract.optreturns = returns;
	**docs_ref = astid(ast);
	*docs_ref = &ast->next;
	return true;
}


static bool parse_contracts(ParseContext *c, AstId *contracts_ref)
{
	*contracts_ref = 0;
	if (!try_consume(c, TOKEN_DOCS_START)) return true;

	AstId *last = contracts_ref;
	uint32_t row_last_row = c->span.row;
	while (1)
	{
		uint32_t row = c->span.row;
		// Spin past the lines and line ends
		switch (c->tok)
		{
			case TOKEN_DOC_DIRECTIVE:
			{
				const char *name = symstr(c);
				if (name == kw_at_param)
				{
					if (!parse_contract_param(c, &last)) return false;
					break;
				}
				else if (name == kw_at_return)
				{
					advance(c);
					if (!consume(c, TOKEN_STRING, "Expected a string description.")) return false;
					break;
				}
				else if (name == kw_at_deprecated)
				{
					advance(c);
					(void)try_consume(c, TOKEN_STRING);
					REMINDER("Implement @deprecated tracking");
					break;
				}
				else if (name == kw_at_require)
				{
					if (!parse_doc_contract(c, &contracts_ref, CONTRACT_REQUIRE)) return false;
					break;
				}
				else if (name == kw_at_checked)
				{
					if (!parse_doc_contract(c, &contracts_ref, CONTRACT_CHECKED)) return false;
					break;
				}
				else if (name == kw_at_ensure)
				{
					if (!parse_doc_contract(c, &contracts_ref, CONTRACT_ENSURE)) return false;
					break;
				}
				else if (name == kw_at_optreturn)
				{
					if (!parse_doc_errors(c, &contracts_ref)) return false;
					break;
				}
				else if (name == kw_at_pure)
				{
					Ast *ast = ast_new_curr(c, AST_CONTRACT);
					ast->contract.kind = CONTRACT_PURE;
					*contracts_ref = astid(ast);
					contracts_ref = &ast->next;
					advance(c);
					break;
				}
				else
				{
					advance(c);
					// Ignore
					break;
				}
			}
			case TOKEN_DOCS_END:
				advance(c);
				return true;
			default:
				if (row_last_row == row)
				{
					SEMA_ERROR_HERE("Expected end of line.");
					return false;
				}
				SEMA_ERROR_HERE("Expected a directive or a comment.");
				return false;
		}
		row_last_row = row;
	}
}

static Decl *parse_include(ParseContext *c)
{
	SourceSpan loc = c->span;
	Decl *decl = decl_new(DECL_CT_INCLUDE, NULL, loc);
	advance_and_verify(c, TOKEN_CT_INCLUDE);
	CONSUME_OR_RET(TOKEN_LPAREN, poisoned_decl);
	const char *str = symstr(c);
	CONSUME_OR_RET(TOKEN_STRING, poisoned_decl);
	CONSUME_OR_RET(TOKEN_RPAREN, poisoned_decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	bool loaded;
	const char *error;
	char *path;
	char *name;
	if (file_namesplit(c->unit->file->full_path, &name, &path))
	{
		str = file_append_path(path, str);
	}
	File *file = source_file_load(str, &loaded, &error);
	if (!file)
	{
		sema_error_at(loc, "Failed to load file %s: %s", str, error);
		return poisoned_decl;
	}
	decl->include.file = file;
	if (global_context.errors_found) return poisoned_decl;

	Lexer current_lexer = c->lexer;
	File *current_file = c->unit->file;
	TokenType old_tok = c->tok;
	TokenData old_data = c->data;
	SourceSpan old_prev = c->prev_span;
	SourceSpan old_span = c->span;
	c->tok = TOKEN_INVALID_TOKEN;
	c->lexer = (Lexer){ .file = decl->include.file, .context =  c };
	lexer_init(&c->lexer);
	// Prime everything
	advance(c);
	advance(c);
	Decl **list = NULL;
	while (!tok_is(c, TOKEN_EOF))
	{
		Decl *inner = parse_top_level_statement(c, &c);
		if (!inner) continue;
		if (!decl_ok(inner))
		{
			decl_poison(inner);
			goto END;
		}
		add_decl_to_list(&list, inner);
	}
	decl->include.decls = list;
END:
	c->lexer = current_lexer;
	c->tok = old_tok;
	c->data = old_data;
	c->prev_span = old_prev;
	c->span = old_span;
	c->unit->file = current_file;
	return decl;
}

/**
 * top_level_statement ::= visibility? top_level
 *
 * top_level
 *		: struct_declaration
 *		| enum_declaration
 *		| error_declaration
 *		| const_declaration
 *		| global_declaration
 *		| macro_declaration
 *		| func_definition
 *		| generics_declaration
 *		| typedef_declaration
 *		| conditional_compilation
 *		| attribute_declaration
 *		;
 * @param visibility
 * @return Decl* or a poison value if parsing failed
 */
Decl *parse_top_level_statement(ParseContext *c, ParseContext **c_ref)
{
	AstId docs = 0;
	if (!parse_contracts(c, &docs)) return poisoned_decl;
	Decl *decl;
	TokenType tok = c->tok;
	if (tok != TOKEN_MODULE && !c->unit->module)
	{
		if (!context_set_module_from_filename(c)) return poisoned_decl;
		// Pass the docs to the next thing.
	}
	bool is_private = false;
AFTER_VISIBILITY:
	switch (tok)
	{
		case TOKEN_PRIVATE:
			if (is_private)
			{
				SEMA_ERROR_HERE("Two 'private' modifiers cannot be used in a row.");
				return poisoned_decl;
			}
			sema_warning_at(c->span, "The use of 'private' is deprecated. Use '@private' instead.");
			is_private = true;
			advance(c);
			tok = c->tok;
			goto AFTER_VISIBILITY;
		case TOKEN_EXTERN:
			if (is_private)
			{
				SEMA_ERROR_HERE("'private' cannot be combined with 'extern'.");
			}
			advance(c);
			tok = c->tok;
			switch (tok)
			{
				case TOKEN_FN:
				{
					ASSIGN_DECL_OR_RET(decl, parse_func_definition(c, docs, false), poisoned_decl);
					break;
				}
				case TOKEN_CONST:
				{
					ASSIGN_DECL_OR_RET(decl, parse_top_level_const_declaration(c), poisoned_decl);
					break;
				}
				case TOKEN_IDENT:
				case TOKEN_TLOCAL:
				case TYPELIKE_TOKENS:
				{
					ASSIGN_DECL_OR_RET(decl, parse_global_declaration(c), poisoned_decl);
					break;
				}
				default:
					SEMA_ERROR_HERE("Expected 'extern' to be followed by a function, constant or global variable.");
					return poisoned_decl;
			}
			decl->is_extern = true;
			break;
		case TOKEN_MODULE:
			if (is_private)
			{
				SEMA_ERROR_HERE("Did not expect 'private' before 'module'.");
				return poisoned_decl;
			}
			if (!c_ref)
			{
				SEMA_ERROR_HERE("'module' cannot appear inside of conditional compilation.");
				return poisoned_decl;
			}
			advance(c);
			if (c->unit->module)
			{
				ParseContext *new_context = CALLOCS(ParseContext);
				*new_context = *c;
				new_context->unit = unit_create(c->unit->file);
				*c_ref = c = new_context;
			}
			if (!parse_module(c, docs)) return poisoned_decl;
			return NULL;
		case TOKEN_DOCS_START:
			if (is_private)
			{
				SEMA_ERROR_HERE("Did not expect doc comments after 'private'.");
				return poisoned_decl;
			}
			SEMA_ERROR_HERE("There are more than one doc comment in a row, that is not allowed.");
			return poisoned_decl;
		case TOKEN_TYPEDEF:
		{
			ASSIGN_DECL_OR_RET(decl, parse_define_type(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_DEFINE:
		{
			ASSIGN_DECL_OR_RET(decl, parse_define(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_FN:
		{
			ASSIGN_DECL_OR_RET(decl, parse_func_definition(c, docs, false), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_STATIC:
		{
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			ASSIGN_DECL_OR_RET(decl, parse_static_top_level(c), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(astptr(docs), "Unexpected doc comment before 'static', did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		}
		case TOKEN_CT_ASSERT:
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			{
				ASSIGN_AST_OR_RET(Ast *ast, parse_ct_assert_stmt(c), poisoned_decl);
				decl = decl_new_ct(DECL_CT_ASSERT, ast->span);
				decl->ct_assert_decl = ast;
				if (docs)
				{
					SEMA_ERROR(astptr(docs), "Unexpected doc comment before $assert, did you mean to use a regular comment?");
					return poisoned_decl;
				}
				return decl;
			}
		case TOKEN_CT_ECHO:
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			{
				ASSIGN_AST_OR_RET(Ast *ast, parse_ct_echo_stmt(c), poisoned_decl);
				decl = decl_new_ct(DECL_CT_ECHO, ast->span);
				decl->ct_echo_decl = ast;
				if (docs)
				{
					SEMA_ERROR(astptr(docs), "Unexpected doc comment before $echo, did you mean to use a regular comment?");
					return poisoned_decl;
				}
				return decl;
			}
		case TOKEN_CT_IF:
		{
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			ASSIGN_DECL_OR_RET(decl, parse_ct_if_top_level(c), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(astptr(docs), "Unexpected doc comment before $if, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		}
		case TOKEN_IMPORT:
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			if (!c_ref)
			{
				SEMA_ERROR_HERE("'import' may not appear inside a compile time statement.");
				return poisoned_decl;
			}
			if (!parse_import(c)) return poisoned_decl;
			if (docs)
			{
				SEMA_ERROR(astptr(docs), "Unexpected doc comment before import, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			return NULL;
		case TOKEN_CT_SWITCH:
		{
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			ASSIGN_DECL_OR_RET(decl, parse_ct_switch_top_level(c), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(astptr(docs), "Unexpected doc comment before $switch, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		}
		case TOKEN_CT_INCLUDE:
			if (!check_no_visibility_before(c, is_private)) return poisoned_decl;
			if (docs)
			{
				SEMA_ERROR(astptr(docs), "Unexpected doc comment before $include, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			ASSIGN_DECL_OR_RET(decl, parse_include(c), poisoned_decl);
			break;
		case TOKEN_BITSTRUCT:
		{
			ASSIGN_DECL_OR_RET(decl, parse_bitstruct_declaration(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_CONST:
		{
			ASSIGN_DECL_OR_RET(decl, parse_top_level_const_declaration(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_STRUCT:
		case TOKEN_UNION:
		{
			ASSIGN_DECL_OR_RET(decl, parse_struct_declaration(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_MACRO:
		{
			ASSIGN_DECL_OR_RET(decl, parse_macro_declaration(c, docs), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_ENUM:
		{
			ASSIGN_DECL_OR_RET(decl, parse_enum_declaration(c, is_private), poisoned_decl);
			break;
		}
		case TOKEN_FAULT:
		{
			ASSIGN_DECL_OR_RET(decl, parse_fault_declaration(c, is_private), poisoned_decl);
			break;
		}
		case TOKEN_IDENT:
		{
			ASSIGN_DECL_OR_RET(decl, parse_global_declaration(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_EOF:
			SEMA_ERROR_LAST("Expected a top level declaration");
			return poisoned_decl;
		case TOKEN_CT_CONST_IDENT:
		{
			if (peek(c) == TOKEN_EQ)
			{
				SEMA_ERROR_HERE("Did you forget a 'const' before the name of this compile time constant?");
			}
			else
			{
				SEMA_ERROR_HERE("Compile time constant unexpectedly found.");
			}
		}
			return poisoned_decl;
		case TOKEN_TLOCAL:
		case TYPELIKE_TOKENS:
		{
			ASSIGN_DECL_OR_RET(decl, parse_global_declaration(c), poisoned_decl);
			if (is_private) decl->visibility = VISIBLE_PRIVATE;
			break;
		}
		case TOKEN_EOS:
			SEMA_ERROR_HERE("';' wasn't expected here, try removing it.");
			return poisoned_decl;
		default:
			SEMA_ERROR_HERE("Expected the start of a global declaration here.");
			return poisoned_decl;
	}
	assert(decl);
	return decl;
}

void consume_deprecated_symbol(ParseContext *c, TokenType type)
{
	if (try_consume(c, type))
	{
		sema_warning_at(c->prev_span, "'%s' is deprecated here.", token_type_to_string(type));
	}
}

