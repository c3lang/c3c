#include "compiler_internal.h"
#include "parser_internal.h"

static Decl *parse_const_declaration(Context *context, Visibility visibility);
static inline Decl *parse_func_definition(Context *context, Visibility visibility, bool is_interface);

static bool context_next_is_path_prefix_start(Context *context)
{
	return context->tok.type == TOKEN_IDENT && context->next_tok.type == TOKEN_SCOPE;
}

/**
 * Walk forward through the token stream to identify a type on the format: foo::bar::Type
 *
 * @return true if there is a type at the end.
 */
static bool context_next_is_type_with_path_prefix(Context *context)
{
	// We assume it's called after "foo::" parsing.
	if (!context_next_is_path_prefix_start(context)) return false;

	TokenId current = context->next_tok.id;
	while (1)
	{
		TokenType tok;

		// 1. Step past the '::' and any following comment (doc comments are not allowed here!)
		tok = advance_token(&current);

		// 2. Check that we have an ident, otherwise if
		// we see a type token, we're done and return true
		// on any other
		if (tok != TOKEN_IDENT) return tok == TOKEN_TYPE_IDENT;

		// 3. Now we've confirmed that there is an ident, step past it
		//    and any following comments.
		tok = advance_token(&current);

		// 4. If we don't see '::' after an ident we're done.
		// And we know it's not a type.
		if (tok != TOKEN_SCOPE) return false;

		// 5. Do another pass
	}
}

static bool context_next_is_type_and_not_ident(Context *context)
{
	if (context->tok.type == TOKEN_IDENT)
	{
		if (context->next_tok.type != TOKEN_COLON) return false;
		return context_next_is_type_with_path_prefix(context);
	}
	return token_is_any_type(context->tok.type);
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
void recover_top_level(Context *context)
{
	advance(context);
	while (!TOKEN_IS(TOKEN_EOF))
	{
		switch (context->tok.type)
		{
			case TOKEN_PRIVATE:
			case TOKEN_STRUCT:
			case TOKEN_INTERFACE:
			case TOKEN_IMPORT:
			case TOKEN_UNION:
			case TOKEN_EXTERN:
			case TOKEN_ENUM:
			case TOKEN_GENERIC:
			case TOKEN_ATTRIBUTE:
			case TOKEN_DEFINE:
				return;
			case TOKEN_IDENT: // Incr arrays only
			case TOKEN_CONST:
			case TOKEN_ASM:
			case TOKEN_TYPEOF:
			case TOKEN_CT_ASSERT:
			case TOKEN_CT_TYPE_IDENT:
			case TOKEN_DOCS_START:
			case TOKEN_TYPE_IDENT:
			case TOKEN_CT_IDENT:
			case TOKEN_CT_IF:
			case TOKEN_CT_FOR:
			case TOKEN_CT_SWITCH:
			case TOKEN_FUNC:
			case TYPE_TOKENS:
				// Only recover if this is in the first col.
				if (TOKLOC(context->tok)->col == 1) return;
				advance(context);
				break;
			default:
				advance(context);
				break;
		}
	}
}

#pragma mark --- Parse CT conditional code

static inline bool parse_top_level_block(Context *context, Decl ***decls, TokenType end1, TokenType end2, TokenType end3)
{
	CONSUME_OR(TOKEN_COLON, false);
	while (!TOKEN_IS(end1) && !TOKEN_IS(end2) && !TOKEN_IS(end3) && !TOKEN_IS(TOKEN_EOF))
	{
		Decl *decl = parse_top_level_statement(context);
		assert(decl);
		if (decl_ok(decl))
		{
			vec_add(*decls, decl);
		}
		else
		{
			return false;
		}
	}
	return true;
}

/**
 * ct_if_top_level ::= CT_IF const_paren_expr ':' top_level_block
    	(CT_ELIF const_paren_expr ':' top_level_block)*
    	(CT_ELSE top_level_block)?
    	CT_ENDIF
 * @param context
 * @return the declaration if successfully parsed, poisoned_decl otherwise.
 */
static inline Decl *parse_ct_if_top_level(Context *context)
{
	Decl *ct = DECL_NEW(DECL_CT_IF, VISIBLE_LOCAL);
	advance_and_verify(context, TOKEN_CT_IF);
	ct->ct_if_decl.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_decl);

	if (!parse_top_level_block(context, &ct->ct_if_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (TOKEN_IS(TOKEN_CT_ELIF))
	{
		advance_and_verify(context, TOKEN_CT_ELIF);
		Decl *ct_elif = DECL_NEW(DECL_CT_ELIF, VISIBLE_LOCAL);
		ct_elif->ct_elif_decl.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_decl);
		if (!parse_top_level_block(context, &ct_elif->ct_elif_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (TOKEN_IS(TOKEN_CT_ELSE))
	{
		advance_and_verify(context, TOKEN_CT_ELSE);
		Decl *ct_else = DECL_NEW(DECL_CT_ELSE, VISIBLE_LOCAL);
		ct_if_decl->elif = ct_else;
		if (!parse_top_level_block(context, &ct_else->ct_else_decl, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF)) return poisoned_decl;
	}
	CONSUME_OR(TOKEN_CT_ENDIF, poisoned_decl);
	CONSUME_OR(TOKEN_EOS, poisoned_decl);
	return ct;
}

/**
 * ct_case ::= (CT_DEFAULT | CT_CASE type) ':' top_level_statement*
 *
 * @param context
 * @return poisoned decl if parsing fails.
 */
static inline Decl *parse_ct_case(Context *context)
{
	Decl *decl;
	switch (context->tok.type)
	{
		case TOKEN_CT_DEFAULT:
			advance(context);
			decl = DECL_NEW(DECL_CT_CASE, VISIBLE_LOCAL);
			break;
		case TOKEN_CT_CASE:
			decl = DECL_NEW(DECL_CT_CASE, VISIBLE_LOCAL);
			advance(context);
			decl->ct_case_decl.type = TRY_TYPE_OR(parse_type(context), poisoned_decl);
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "Expected a $case or $default statement here.");
			return poisoned_decl;
	}
	TRY_CONSUME_OR(TOKEN_COLON, "Expected ':' here.", poisoned_decl);
	while (1)
	{
		TokenType type = context->tok.type;
		if (type == TOKEN_CT_DEFAULT || type == TOKEN_CT_CASE || type == TOKEN_LBRACE) break;
		Decl *stmt = TRY_DECL_OR(parse_top_level_statement(context), poisoned_decl);
		vec_add(decl->ct_case_decl.body, stmt);
	}
	return decl;
}

/**
 * ct_switch_top_level ::= CT_SWITCH const_paren_expr '{' ct_case* '}'
 * @param context
 * @return the declaration if successfully parsed, NULL otherwise.
 */
static inline Decl *parse_ct_switch_top_level(Context *context)
{
	Decl *ct = DECL_NEW(DECL_CT_SWITCH, VISIBLE_LOCAL);
	advance_and_verify(context, TOKEN_CT_SWITCH);
	ct->ct_switch_decl.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_decl);

	CONSUME_OR(TOKEN_LBRACE, poisoned_decl);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Decl *result = TRY_DECL_OR(parse_ct_case(context), poisoned_decl);
		vec_add(ct->ct_switch_decl.cases, result);
	}
	return ct;
}


#pragma mark --- Parse paths

/**
 * module_path ::= IDENT (SCOPE IDENT)*
 *
 * @param context
 * @return path or null if parsing failed.
 */
static inline Path *parse_module_path(Context *context)
{
	assert(TOKEN_IS(TOKEN_IDENT));
	scratch_buffer_clear();
	SourceSpan span = source_span_from_token_id(context->tok.id);
	scratch_buffer_append_len(TOKSTR(context->tok), TOKLEN(context->tok));
	TokenId last_token;
	while (1)
	{
		last_token = context->tok.id;
		if (!try_consume(context, TOKEN_IDENT))
		{
			SEMA_TOKEN_ERROR(context->tok, "Each '::' must be followed by a regular lower case sub module name.");
			return NULL;
		}
		if (!try_consume(context, TOKEN_SCOPE))
		{
			span.end_loc = last_token;
			break;
		}
		scratch_buffer_append("::");
		scratch_buffer_append_len(TOKSTR(context->tok), TOKLEN(context->tok));
	}
	return path_create_from_string(scratch_buffer_to_string(), global_context.scratch_buffer_len, span);
}


#pragma mark --- Parse import and module

/**
 *
 * module_param
 * 		: TYPE_IDENT
 *		| IDENT
 *		;
 *
 * module_params
 * 		: module_param
 * 		| module_params ',' module_param
 *		;
 */
static inline bool parse_optional_module_params(Context *context, TokenId **tokens)
{

	*tokens = NULL;

	if (!try_consume(context, TOKEN_LESS)) return true;

	if (try_consume(context, TOKEN_GREATER))
	{
		SEMA_TOKEN_ERROR(context->tok, "Generic parameter list cannot be empty.");
		return false;
	}

	// No params
	while (1)
	{
		switch (context->tok.type)
		{
			case TOKEN_TYPE_IDENT:
				break;
			case TOKEN_COMMA:
				SEMA_TOKEN_ERROR(context->tok, "Unexpected ','");
				return false;
			case TOKEN_IDENT:
				SEMA_TOKEN_ERROR(context->tok, "The module parameter must be a type.");
				return false;
			case TOKEN_CT_IDENT:
			case TOKEN_CT_TYPE_IDENT:
				SEMA_TOKEN_ERROR(context->tok, "The module parameter cannot be a $-prefixed name.");
				return false;
			default:
				SEMA_TOKEN_ERROR(context->tok, "Only generic parameters are allowed here as parameters to the module.");
				return false;
		}
		*tokens = VECADD(*tokens, context->tok.id);
		advance(context);
		if (!try_consume(context, TOKEN_COMMA))
		{
			return consume(context, TOKEN_GREATER, "Expected '>'.");
		}
	}

}
/**
 * module ::= MODULE module_path ('<' module_params '>')? EOS
 */
bool parse_module(Context *context)
{
	if (!try_consume(context, TOKEN_MODULE))
	{
		return context_set_module_from_filename(context);
	}

	bool is_private = try_consume(context, TOKEN_PRIVATE);

	if (TOKEN_IS(TOKEN_STRING))
	{
		SEMA_TOKEN_ERROR(context->tok, "'module' should be followed by a plain identifier, not a string. Did you accidentally put the module name between \"\"?");
		return false;
	}

	if (!TOKEN_IS(TOKEN_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Module statement should be followed by the name of the module.");
		return false;
	}


	Path *path = parse_module_path(context);

	// Expect the module name
	if (!path)
	{
		path = CALLOCS(Path);
		path->len = strlen("INVALID");
		path->module = "INVALID";
		path->span = INVALID_RANGE;
		context_set_module(context, path, NULL, false);
		recover_top_level(context);
		return false;
	}

	// Is this a generic module?
	TokenId *generic_parameters = NULL;
	if (!parse_optional_module_params(context, &generic_parameters))
	{
		context_set_module(context, path, NULL, is_private);
		recover_top_level(context);
		return true;
	}
	context_set_module(context, path, generic_parameters, is_private);
	TRY_CONSUME_EOS_OR(false);
	return true;
}

/**
 * specified_import ::= IDENT (AS IDENT)?
 *                    | CONST_IDENT (AS CONST_IDENT)?
 *                    | TYPE_IDENT (AS TYPE_IDENT)?
 *
 * @return true if import succeeded
 */
static inline bool parse_specified_import(Context *context, Path *path)
{
	if (!token_is_symbol(context->tok.type))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected a symbol name here, the syntax is 'import <module> : <symbol>'.");
		return false;
	}
	Token symbol = context->tok;
	advance(context);
	// Alias?
	if (!try_consume(context, TOKEN_AS))
	{
		return context_add_import(context, path, symbol, NO_TOKEN, false);
	}
	if (context->tok.type != symbol.type)
	{
		if (!token_is_symbol(context->tok.type))
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a symbol name here, the syntax is 'import <module> : <symbol> AS <alias>'.");
			return false;
		}
		SEMA_TOKEN_ERROR(context->tok, "Expected the alias be the same type of name as the symbol aliased.");
		return false;
	}
	Token alias = context->tok;
	advance(context);
	return context_add_import(context, path, symbol, alias, false);
}


static inline bool consume_ident(Context *context, const char* name)
{
	if (try_consume(context, TOKEN_IDENT)) return true;
	if (TOKEN_IS(TOKEN_TYPE_IDENT) || TOKEN_IS(TOKEN_CONST_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "A %s cannot start with a capital letter.", name);
		return false;
	}
	SEMA_TOKEN_ERROR(context->tok, "A %s was expected.", name);
	return false;
}

static bool consume_type_name(Context *context, const char* type)
{
	if (TOKEN_IS(TOKEN_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss must start with an upper case letter.", type);
		return false;
	}
	if (TOKEN_IS(TOKEN_CONST_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss cannot be all upper case.", type);
		return false;
	}
	if (!consume(context, TOKEN_TYPE_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}

bool consume_const_name(Context *context, const char* type)
{
	if (TOKEN_IS(TOKEN_IDENT) || TOKEN_IS(TOKEN_TYPE_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Names of %ss must be all upper case.", type);
		return false;
	}
	if (!consume(context, TOKEN_CONST_IDENT, "The constant name was expected here, did you forget it?")) return false;
	return true;
}


Path *parse_path_prefix(Context *context, bool *had_error)
{
	*had_error = false;
	if (!TOKEN_IS(TOKEN_IDENT) || context->next_tok.type != TOKEN_SCOPE) return NULL;

	char *scratch_ptr = global_context.scratch_buffer;
	size_t offset = 0;

	Path *path = CALLOCS(Path);
	path->span = source_span_from_token_id(context->tok.id);
	unsigned len = TOKLEN(context->tok);
	memcpy(scratch_ptr, TOKSTR(context->tok.id), len);
	offset += len;
	TokenId last_token = context->tok.id;
	advance(context);
	advance(context);
	while (TOKEN_IS(TOKEN_IDENT) && context->next_tok.type == TOKEN_SCOPE)
	{
		last_token = context->tok.id;
		scratch_ptr[offset++] = ':';
		scratch_ptr[offset++] = ':';
		len = TOKLEN(context->tok);
		memcpy(scratch_ptr + offset, TOKSTR(context->tok.id), len);
		offset += len;
		advance(context); advance(context);
	}

	TokenType type = TOKEN_IDENT;
	path->span.end_loc = last_token;
	path->module = symtab_add(scratch_ptr, offset, fnv1a(scratch_ptr, offset), &type);
	if (type != TOKEN_IDENT)
	{
		SEMA_ERROR(path, "A module name was expected here.");
		*had_error = true;
		return NULL;

	}
	path->len = offset;

	return path;
}

#pragma mark --- Type parsing

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
 *		;
 *
 * Assume prev_token is the type.
 * @return TypeInfo (poisoned if fails)
 */
static inline TypeInfo *parse_base_type(Context *context)
{
	bool virtual = try_consume(context, TOKEN_VIRTUAL);
	SourceSpan range = source_span_from_token_id(context->tok.id);
	bool had_error;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_type_info;
	if (path)
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_IDENTIFIER, range);
		type_info->unresolved.path = path;
		type_info->unresolved.name_loc = context->tok.id;
		type_info->virtual_type = virtual;
		if (!consume_type_name(context, "type")) return poisoned_type_info;
		if (virtual) TRY_CONSUME_OR(TOKEN_STAR, "Expected '*' after virtual name.", poisoned_type_info);
		RANGE_EXTEND_PREV(type_info);
		return type_info;
	}

	TypeInfo *type_info = NULL;
	Type *type_found = NULL;
	switch (context->tok.type)
	{
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
			type_info = type_info_new(TYPE_INFO_IDENTIFIER, source_span_from_token_id(context->tok.id));
			type_info->unresolved.name_loc = context->tok.id;
			break;
		case TYPE_TOKENS:
		case TOKEN_ERR:
			type_found = type_from_token(context->tok.type);
			break;
		default:
			// Special case: "virtual *"
			if (virtual && context->tok.type == TOKEN_STAR)
			{
				type_info = type_info_new(TYPE_INFO_IDENTIFIER, source_span_from_token_id(context->prev_tok));
				advance(context);
				type_info->resolve_status = RESOLVE_DONE;
				type_info->type = type_virtual;
				type_info->virtual_type = true;
				RANGE_EXTEND_PREV(type_info);
				return type_info;
			}
			SEMA_TOKEN_ERROR(context->tok, "A type name was expected here.");
			return poisoned_type_info;
	}
	if (type_found)
	{
		if (virtual)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an interface name.");
			advance(context);
			return poisoned_type_info;
		}
		assert(!type_info);
		type_info = type_info_new(TYPE_INFO_IDENTIFIER, source_span_from_token_id(context->tok.id));
		type_info->resolve_status = RESOLVE_DONE;
		type_info->type = type_found;
	}
	type_info->virtual_type = virtual;
	advance(context);
	if (virtual) TRY_CONSUME_OR(TOKEN_STAR, "Expected '*' after virtual name.", poisoned_type_info);
	RANGE_EXTEND_PREV(type_info);
	return type_info;
}

/**
 * array_type_index
 *		: '[' constant_expression ']'
 *		| '[' ']'
 *		| '[' '+' ']'
 *		| '[' '*' ']'
 *		;
 *
 * @param type the type to wrap, may not be poisoned.
 * @return type (poisoned if fails)
 */
static inline TypeInfo *parse_array_type_index(Context *context, TypeInfo *type)
{
	assert(type_info_ok(type));

	advance_and_verify(context, TOKEN_LBRACKET);
	if (try_consume(context, TOKEN_PLUS))
	{
		CONSUME_OR(TOKEN_RBRACKET, poisoned_type_info);
		TypeInfo *incr_array = type_info_new(TYPE_INFO_INC_ARRAY, type->span);
		incr_array->array.base = type;
		RANGE_EXTEND_PREV(incr_array);
		return incr_array;
	}
	if (try_consume(context, TOKEN_STAR))
	{
		CONSUME_OR(TOKEN_RBRACKET, poisoned_type_info);
		TypeInfo *inferred_array = type_info_new(TYPE_INFO_INFERRED_ARRAY, type->span);
		inferred_array->array.base = type;
		RANGE_EXTEND_PREV(inferred_array);
		return inferred_array;
	}
	if (try_consume(context, TOKEN_RBRACKET))
	{
		TypeInfo *subarray = type_info_new(TYPE_INFO_SUBARRAY, type->span);
		subarray->array.base = type;
		subarray->array.len = NULL;
		RANGE_EXTEND_PREV(subarray);
		return subarray;
	}
	TypeInfo *array = type_info_new(TYPE_INFO_ARRAY, type->span);
	array->array.base = type;
	array->array.len = TRY_EXPR_OR(parse_expr(context), poisoned_type_info);
	CONSUME_OR(TOKEN_RBRACKET, poisoned_type_info);
	RANGE_EXTEND_PREV(array);
	return array;
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
TypeInfo *parse_type_with_base(Context *context, TypeInfo *type_info)
{
	while (type_info_ok(type_info))
	{
		switch (context->tok.type)
		{
			case TOKEN_LBRACKET:
				type_info = parse_array_type_index(context, type_info);
				break;
			case TOKEN_STAR:
				advance(context);
				{
					TypeInfo *ptr_type = type_info_new(TYPE_INFO_POINTER, type_info->span);
					assert(type_info);
					ptr_type->pointer = type_info;
					type_info = ptr_type;
					RANGE_EXTEND_PREV(type_info);
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
TypeInfo *parse_type(Context *context)
{
	return parse_type_with_base(context, parse_base_type(context));
}


#pragma mark --- Decl parsing

/**
 * Parse ident ('=' expr)?
 * @param is_static
 * @param type
 * @return
 */
Decl *parse_decl_after_type(Context *context, TypeInfo *type)
{
	if (TOKEN_IS(TOKEN_LPAREN))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected '{'.");
		return poisoned_decl;
	}


	EXPECT_IDENT_FOR_OR("variable name", poisoned_decl);

	TokenId name = context->tok.id;
	advance(context);

	Decl *decl = decl_new_var(name, type, VARDECL_LOCAL, VISIBLE_LOCAL);
	if (TOKEN_IS(TOKEN_EQ))
	{
		if (!decl)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an identifier before '='.");
			return poisoned_decl;
		}
		advance_and_verify(context, TOKEN_EQ);
		decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);
	}
	return decl;
}


/**
 * declaration ::= ('static' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
Decl *parse_decl(Context *context)
{
	if (TOKEN_IS(TOKEN_CONST))
	{
		return parse_const_declaration(context, VISIBLE_LOCAL);
	}

	bool is_static = try_consume(context, TOKEN_STATIC);

	TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_decl);

	bool failable = try_consume(context, TOKEN_BANG);

	Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, type), poisoned_decl);
	if (failable && decl->var.unwrap)
	{
		SEMA_ERROR(decl, "You cannot use unwrap with a failable variable.");
		return poisoned_decl;
	}
	decl->var.failable = failable;
	decl->var.is_static = is_static;
	return decl;
}



/**
 * const_decl
 *  : 'const' type? IDENT '=' const_expr
 *  ;
 */
static Decl *parse_const_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_CONST);

	Decl *decl = DECL_NEW_VAR(NULL, VARDECL_CONST, visibility);
	decl->span.loc = context->prev_tok;

	if (parse_next_is_decl(context))
	{
		decl->var.type_info = TRY_TYPE_OR(parse_type(context), poisoned_decl);
	}
	decl->name = TOKSTR(context->tok);
	decl->name_token = context->tok.id;
	if (!consume_const_name(context, "const")) return poisoned_decl;

	CONSUME_OR(TOKEN_EQ, poisoned_decl);

	decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);

	return decl;
}

/**
 * Possibilities:
 * foo(...)
 * Bar.foo(...)
 * foo::baz::bar.foo(...)
 *
 * @return true if this is a function start.
 */
static inline bool is_function_start(Context *context)
{
	// If it's a ! then it must be function!
	if (TOKEN_IS(TOKEN_BANG)) return true;
	if (TOKEN_IS(TOKEN_IDENT))
	{
		if (context->next_tok.type == TOKEN_EQEQ || context->next_tok.type == TOKEN_EOS) return false;
		if (context->next_tok.type == TOKEN_LPAREN) return true;
	}
	TokenId current = context->tok.id;
	TokenType tok = TOKTYPE(current);
	while (1)
	{
		if (tok != TOKEN_IDENT) break;
		tok = advance_token(&current);
		if (tok != TOKEN_SCOPE) break;
		tok = advance_token(&current);
	}
	if (tok != TOKEN_TYPE_IDENT) return false;
	tok = advance_token(&current);
	if (tok != TOKEN_DOT) return false;
	tok = advance_token(&current);
	if (tok != TOKEN_IDENT) return false;
	tok = advance_token(&current);
	return tok == TOKEN_LPAREN;
}

/**
 * global_declaration
 * 	: failable_type IDENT ';'
 * 	| failable_type IDENT '=' expression ';'
 * 	| failable_type func_definition
 * 	;
 *
 * @param visibility
 * @return true if parsing succeeded
 */
static inline Decl *parse_global_declaration(Context *context, Visibility visibility)
{
	TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_decl);

	Decl *decl = decl_new_var(context->tok.id, type, VARDECL_GLOBAL, visibility);


	if (TOKEN_IS(TOKEN_CONST_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "This looks like a constant variable, did you forget 'const'?");
		return poisoned_decl;
	}
	if (!consume_ident(context, "global variable")) return poisoned_decl;

	if (try_consume(context, TOKEN_EQ))
	{
		decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);
	}
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}

static inline Decl *parse_incremental_array(Context *context)
{
	Token name = context->tok;
	advance_and_verify(context, TOKEN_IDENT);

	if (!try_consume(context, TOKEN_PLUS_ASSIGN))
	{
		SEMA_TOKEN_ERROR(name, "Did you miss a declaration before the variable name?");
		return poisoned_decl;
	}
	Decl *decl = decl_new(DECL_ARRAY_VALUE, name.id, VISIBLE_LOCAL);
	decl->incr_array_decl = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}



/**
 * decl_expr_list
 *  : expression
 *  | declaration
 *  | decl_expr_list ',' expression
 *  | decl_expr_list ',' declaration
 *  ;
 *
 * @return bool
 */
Expr *parse_decl_expr_list(Context *context)
{
	Expr *decl_expr = EXPR_NEW_TOKEN(EXPR_DECL_LIST, context->tok);
	decl_expr->dexpr_list_expr = NULL;
	while (1)
	{
		if (parse_next_is_decl(context))
		{
			Decl *decl = TRY_DECL_OR(parse_decl(context), poisoned_expr);
			Ast *stmt = AST_NEW(AST_DECLARE_STMT, decl->span);
			stmt->declare_stmt = decl;
			vec_add(decl_expr->dexpr_list_expr, stmt);
		}
		else
		{
			Expr *expr = TRY_EXPR_OR(parse_expr(context), poisoned_expr);
			Ast *stmt = AST_NEW(AST_EXPR_STMT, expr->span);
			stmt->expr_stmt = expr;
			vec_add(decl_expr->dexpr_list_expr, stmt);
		}
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
	RANGE_EXTEND_PREV(decl_expr);
	return decl_expr;
}


bool parse_next_is_decl(Context *context)
{
	TokenType next_tok = context->next_tok.type;
	switch (context->tok.type)
	{
		case TOKEN_VOID:
		case TOKEN_CHAR:
		case TOKEN_BOOL:
		case TOKEN_ICHAR:
		case TOKEN_DOUBLE:
		case TOKEN_FLOAT:
		case TOKEN_INT:
		case TOKEN_ISIZE:
		case TOKEN_LONG:
		case TOKEN_SHORT:
		case TOKEN_UINT:
		case TOKEN_ULONG:
		case TOKEN_USHORT:
		case TOKEN_USIZE:
		case TOKEN_QUAD:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_ERR:
		case TOKEN_TYPEID:
			return (next_tok == TOKEN_BANG) | (next_tok == TOKEN_STAR) | (next_tok == TOKEN_LBRACKET) | (next_tok == TOKEN_IDENT)
			       | (next_tok == TOKEN_CONST_IDENT);
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			return context_next_is_type_with_path_prefix(context);
		default:
			return false;
	}
}

bool parse_next_is_type(Context *context)
{
	TokenType next_tok = context->next_tok.type;
	switch (context->tok.type)
	{
		case TYPE_TOKENS:
		case TOKEN_VIRTUAL:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_ERR:
			return true;
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			return context_next_is_type_with_path_prefix(context);
		default:
			return false;
	}
}


bool parse_next_is_case_type(Context *context)
{
	TokenType next_tok = context->next_tok.type;
	switch (context->tok.type)
	{
		case TYPE_TOKENS:
		case TOKEN_VIRTUAL:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_ERR:
			return (next_tok == TOKEN_STAR) | (next_tok == TOKEN_LBRACKET) |  (next_tok == TOKEN_COMMA) | (next_tok == TOKEN_COLON) | (next_tok == TOKEN_EOS);
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			return context_next_is_type_with_path_prefix(context);
		default:
			return false;
	}
}



#pragma mark --- Parse parameters & throws & attributes


/**
 * attribute_list
 *  : attribute
 *  | attribute_list attribute
 *  ;
 *
 * attribute
 *  : AT IDENT
 *  | AT path IDENT
 *  | AT IDENT '(' constant_expression ')'
 *  | AT path IDENT '(' constant_expression ')'
 *  ;
 *
 * @return true if parsing succeeded, false if recovery is needed
 */
static inline bool parse_attributes(Context *context, Decl *parent_decl)
{
	parent_decl->attributes = NULL;

	while (try_consume(context, TOKEN_AT))
	{
		bool had_error;
		Path *path = parse_path_prefix(context, &had_error);
		if (had_error) return false;

		Attr *attr = CALLOCS(Attr);

		attr->name = context->tok.id;
		attr->path = path;

		TRY_CONSUME_OR(TOKEN_IDENT, "Expected an attribute", false);

		if (TOKEN_IS(TOKEN_LPAREN))
		{
			attr->expr = TRY_EXPR_OR(parse_const_paren_expr(context), false);
		}
		const char *name = TOKSTR(attr->name);
		VECEACH(parent_decl->attributes, i)
		{
			Attr *other_attr = parent_decl->attributes[i];
			if (TOKSTR(other_attr->name) == name)
			{
				SEMA_TOKID_ERROR(attr->name, "Repeat of attribute '%s' here.", name);
				return false;
			}
		}
		parent_decl->attributes = VECADD(parent_decl->attributes, attr);
	}
	return true;
}





/**
 * param_declaration ::= type_expression '...'?) (IDENT ('=' initializer)?)?
 *  ;
 */
static inline bool parse_param_decl(Context *context, Visibility parent_visibility, Decl*** parameters, bool require_name)
{
	TokenId first = context->tok.id;
	TypeInfo *type = TRY_TYPE_OR(parse_type(context), false);
	bool vararg = try_consume(context, TOKEN_ELLIPSIS);
	Decl *param = decl_new_var(context->tok.id, type, VARDECL_PARAM, parent_visibility);
	param->span = (SourceSpan) { first, context->tok.id };
	param->var.vararg = vararg;
	if (!try_consume(context, TOKEN_IDENT))
	{
		param->name = NULL;
	}
	const char *name = param->name;

	if (!name && require_name)
	{
		if (!TOKEN_IS(TOKEN_COMMA) && !TOKEN_IS(TOKEN_RPAREN))
		{
			if (TOKEN_IS(TOKEN_CT_IDENT))
			{
				SEMA_TOKEN_ERROR(context->tok, "Compile time identifiers are only allowed as macro parameters.");
				return false;
			}
			sema_error_at_prev_end(context->tok, "Unexpected end of the parameter list, did you forget an ')'?");
			return false;
		}
		SEMA_ERROR(type, "The parameter must be named.");
		return false;
	}
	if (name && try_consume(context, TOKEN_EQ))
	{
		param->var.init_expr = TRY_EXPR_OR(parse_initializer(context), false);
	}

	*parameters = VECADD(*parameters, param);
	RANGE_EXTEND_PREV(param);
	return true;
}

/**
 * parameters ::= (parameter (',' parameter)*)?
 * non_type_ident = IDENT | HASH_IDENT | CT_IDENT
 * parameter ::= type ELLIPSIS? (non_type_ident ('=' expr))?
 *             | ELLIPSIS (CT_TYPE_IDENT | non_type_ident ('=' expr)?)?
 */
bool parse_parameters(Context *context, Visibility visibility, Decl ***params_ref)
{
	Decl** params = NULL;
	bool var_arg_found = false;

	while (!TOKEN_IS(TOKEN_EOS) && !TOKEN_IS(TOKEN_RPAREN))
	{
		TypeInfo *type = NULL;

		bool ellipsis = try_consume(context, TOKEN_ELLIPSIS);

		// Special case, we might see foo($Type).
		// there is an ambiguity here, since ($Type) and ($Type x) is potentially possible
		// to evaluate. However, at the top level we never have global compile time values.
		// so consequently we need fix this and ignore CT_TYPE_IDENT
		if (!ellipsis && context_next_is_type_and_not_ident(context) && context->tok.type != TOKEN_CT_TYPE_IDENT )
		{
			type = TRY_TYPE_OR(parse_type(context), false);
			ellipsis = try_consume(context, TOKEN_ELLIPSIS);
		}

		if (ellipsis && var_arg_found)
		{
			SEMA_TOKID_ERROR(context->prev_tok, "Only a single vararg parameter is allowed.");
			return false;
		}

		VarDeclKind param_kind;
		TokenId token = context->tok.id;
		bool no_name = false;

		switch (context->tok.type)
		{
			case TOKEN_IDENT:
				// normal foo
				param_kind = VARDECL_PARAM;
				break;
			case TOKEN_CT_IDENT:
				// ct_var $foo
				param_kind = VARDECL_PARAM_CT;
				break;
			case TOKEN_AMP:
				// reference &foo
				advance(context);
				token = context->tok.id;
				if (!TOKEN_IS(TOKEN_IDENT))
				{
					SEMA_TOKEN_ERROR(context->tok, "Only normal variables may be passed by reference.");
					return false;
				}
				param_kind = VARDECL_PARAM_REF;
				break;
			case TOKEN_HASH_TYPE_IDENT:
				// #Foo (not allowed)
				SEMA_TOKEN_ERROR(context->tok, "An unevaluated expression can never be a type, did you mean to use $Type?");
				return false;
			case TOKEN_HASH_IDENT:
				// expression #foo
				param_kind = VARDECL_PARAM_EXPR;
				break;
				// Compile time type $Type
			case TOKEN_CT_TYPE_IDENT:
				param_kind = VARDECL_PARAM_CT_TYPE;
				break;
			case TOKEN_COMMA:
			case TOKEN_EOS:
			case TOKEN_RPAREN:
				if (!type && !ellipsis)
				{
					SEMA_TOKEN_ERROR(context->tok, "Expected a parameter.");
					return false;
				}
				no_name = true;
				token = context->prev_tok;
				param_kind = VARDECL_PARAM;
				break;
			default:
				SEMA_TOKEN_ERROR(context->tok, "Expected a parameter.");
				return false;
		}
		Decl *param = decl_new_var(token, type, param_kind, visibility);
		param->var.type_info = type;
		if (no_name)
		{
			param->name = NULL;
		}
		else
		{
			advance(context);
			if (try_consume(context, TOKEN_EQ))
			{
				param->var.init_expr = TRY_EXPR_OR(parse_initializer(context), false);
			}
		}
		var_arg_found |= ellipsis;
		param->var.vararg = ellipsis;
		vec_add(params, param);
		if (!try_consume(context, TOKEN_COMMA)) break;
	}
	*params_ref = params;
	return true;
}


/**
 *
 * parameter_type_list ::= '(' parameters ')'
 */
static inline bool parse_parameter_list(Context *context, Visibility parent_visibility, FunctionSignature *signature, bool is_interface)
{
	CONSUME_OR(TOKEN_LPAREN, false);
	Decl **decls;
	if (!parse_parameters(context, parent_visibility, &decls)) return false;
	if (vec_size(decls))
	{
		Decl *last = VECLAST(decls);
		if (last->var.vararg)
		{
			if (!last->var.type_info)
			{
				vec_resize(decls, vec_size(decls) - 1);
				signature->variadic = true;
			}
			else
			{
				signature->typed_variadic = true;
			}
		}
	}
	signature->params = decls;
	CONSUME_OR(TOKEN_RPAREN, false);
	return true;
}

#pragma mark --- Parse types


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
bool parse_struct_body(Context *context, Decl *parent)
{
	CONSUME_OR(TOKEN_LBRACE, false);

	assert(decl_is_struct_type(parent));
	MemberIndex index = 0;
	while (!TOKEN_IS(TOKEN_RBRACE))
	{
		TokenType token_type = context->tok.type;
		if (token_type == TOKEN_STRUCT || token_type == TOKEN_UNION)
		{
			DeclKind decl_kind = decl_from_token(token_type);
			Decl *member;
			if (context->next_tok.type != TOKEN_IDENT)
			{
				member = decl_new_with_type(NO_TOKEN_ID, decl_kind, parent->visibility);
				member->span = source_span_from_token_id(context->tok.id);
				advance(context);
			}
			else
			{
				advance(context);
				member = decl_new_with_type(context->tok.id, decl_kind, parent->visibility);
				member->span.loc = context->prev_tok;
				advance_and_verify(context, TOKEN_IDENT);
			}
			if (!parse_attributes(context, member)) return false;
			if (!parse_struct_body(context, member))
			{
				decl_poison(parent);
				return false;
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
		if (token_type == TOKEN_IDENT && TOKSTR(context->tok) == kw_inline)
		{
			if (parent->decl_kind != DECL_STRUCT)
			{
				SEMA_TOKEN_ERROR(context->tok, "Only structs may have 'inline' elements, did you make a mistake?");
				return false;
			}
			if (index > 0)
			{
				SEMA_TOKID_ERROR(context->prev_tok, "Only the first element may be 'inline', did you order your fields wrong?");
				return false;
			}
			parent->is_substruct = true;
			was_inline = true;
			advance(context);
		}
		TypeInfo *type = TRY_TYPE_OR(parse_type(context), false);
		while (1)
		{
			EXPECT_OR(TOKEN_IDENT, false);
			Decl *member = decl_new_var(context->tok.id, type, VARDECL_MEMBER, parent->visibility);
			vec_add(parent->strukt.members, member);
			index++;
			if (index > MAX_MEMBERS)
			{
				SEMA_ERROR(member, "Can't add another member: the count would exceed maximum of %d elements.", MAX_MEMBERS);
				return false;
			}
			advance(context);
			if (!parse_attributes(context, member)) return false;
			if (!try_consume(context, TOKEN_COMMA)) break;
			if (was_inline)
			{
				SEMA_ERROR(member, "'Inline' can only be applied to a single member, so please define it on its own line.");
				return false;
			}
		}
		CONSUME_OR(TOKEN_EOS, false);
	}
	advance_and_verify(context, TOKEN_RBRACE);
	return true;
}


/**
 * struct_declaration
 * 		: struct_or_union TYPE_IDENT opt_attributes struct_body
 * 		;
 *
 * @param visibility
 */
static inline Decl *parse_struct_declaration(Context *context, Visibility visibility)
{
	TokenType type = context->tok.type;

	advance(context);
	const char* type_name = struct_union_name_from_token(type);

	TokenId name = context->tok.id;

	if (!consume_type_name(context, type_name)) return poisoned_decl;
	Decl *decl = decl_new_with_type(name, decl_from_token(type), visibility);

	if (!parse_attributes(context, decl))
	{
		return poisoned_decl;
	}

	if (!parse_struct_body(context, decl))
	{
		return poisoned_decl;
	}
	DEBUG_LOG("Parsed %s %s completely.", type_name, TOKSTR(name));
	return decl;
}


static inline Decl *parse_top_level_const_declaration(Context *context, Visibility visibility)
{
	Decl *decl = TRY_DECL_OR(parse_const_declaration(context, visibility), poisoned_decl);
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}


/**
 * macro_arguments ::= '(' parameters (EOS trailing_block_parameter )? ')'
 *
 * trailing_block_parameter ::= '@' IDENT ( '(' parameters ')' )?
 */
static bool parse_macro_arguments(Context *context, Visibility visibility, Decl ***params_ref, Decl ***body_params, TokenId *block_parameter)
{
	CONSUME_OR(TOKEN_LPAREN, false);
	*params_ref = NULL;
	*body_params = NULL;
	*block_parameter = (TokenId) {};
	// Parse the regular parameters.
	if (!parse_parameters(context, visibility, params_ref)) return false;

	// Do we have trailing block parameters?
	if (try_consume(context, TOKEN_EOS))
	{
		// Consume '@' IDENT
		TRY_CONSUME_OR(TOKEN_AT, "Expected a trailing block with the format '@block(...).", false);
		*block_parameter = context->tok.id;
		if (!consume_ident(context, "variable name")) return false;
		TokenId name = context->prev_tok;
		if (try_consume(context, TOKEN_LPAREN))
		{
			if (!parse_parameters(context, visibility, body_params)) return false;
			CONSUME_OR(TOKEN_RPAREN, false);
		}
		// TODO use the body param.
	}
	TRY_CONSUME(TOKEN_RPAREN, false);
	return true;
}

/**
 * define_parameters ::= type (',' type)* '>'
 *
 * @return NULL if parsing failed, otherwise a list of Type*
 */
static inline TypeInfo **parse_generic_parameters(Context *context)
{
	TypeInfo **types = NULL;
	while (!try_consume(context, TOKEN_GREATER))
	{
		TypeInfo *expr = TRY_TYPE_OR(parse_type(context), NULL);
		vec_add(types, expr);
		if (context->tok.type != TOKEN_RPAREN && context->tok.type != TOKEN_GREATER)
		{
			TRY_CONSUME_OR(TOKEN_COMMA, "Expected ',' after argument.", NULL);
		}
	}
	return types;
}

static inline bool parse_define_optional_path(Context *context, Path **path)
{
	if (context->tok.type != TOKEN_IDENT || context->next_tok.type != TOKEN_SCOPE)
	{
		*path = NULL;
		return true;
	}
	bool error = false;
	*path = parse_path_prefix(context, &error);
	if (error) return false;
	return true;
}

/**
 * define_type_body ::= TYPE_IDENT '=' 'distinct'? (func_typedef | type generic_params?) ';'
 *
 * func_typedef ::= 'func' failable_type parameter_type_list
 */
static inline Decl *parse_define_type(Context *context, Visibility visibility)
{
	TokenId start = context->tok.id;
	advance_and_verify(context, TOKEN_DEFINE);

	TokenId alias_name = context->tok.id;
	DEBUG_LOG("Parse define %s", TOKSTR(alias_name));
	advance_and_verify(context, TOKEN_TYPE_IDENT);
	CONSUME_OR(TOKEN_EQ, poisoned_decl);
	bool distinct = false;
	if (context->tok.type == TOKEN_IDENT && TOKSTR(context->tok) == kw_distinct)
	{
		distinct = true;
		advance(context);
	}

	// 1. Did we have `func`? In that case it's a function pointer.
	if (try_consume(context, TOKEN_FUNC))
	{
		Decl *decl = decl_new_with_type(alias_name, DECL_TYPEDEF, visibility);
		decl->span.loc = start;
		decl->typedef_decl.is_func = true;
		decl->typedef_decl.is_distinct = distinct;
		TypeInfo *type_info = TRY_TYPE_OR(parse_type(context), poisoned_decl);
		decl->typedef_decl.function_signature.rtype = type_info;
		if (try_consume(context, TOKEN_BANG))
		{
			decl->typedef_decl.function_signature.failable = true;
		}
		if (!parse_parameter_list(context, decl->visibility, &(decl->typedef_decl.function_signature), true))
		{
			return poisoned_decl;
		}
		RANGE_EXTEND_PREV(decl);
		TRY_CONSUME_EOS_OR(poisoned_decl);
		return decl;
	}

	// 2. Now parse the type which we know is here.
	TypeInfo *type_info = TRY_TYPE_OR(parse_type(context), poisoned_decl);

	// 3. Do we have '<' if so it's a parameterized type e.g. foo::bar::Type<int, double>.
	if (try_consume(context, TOKEN_LESS))
	{
		TypeInfo **params = parse_generic_parameters(context);
		if (!params) return poisoned_decl;
		Decl *decl = decl_new(DECL_DEFINE, alias_name, visibility);
		decl->span.loc = start;
		decl->define_decl.define_kind = DEFINE_TYPE_GENERIC;
		decl->define_decl.type_info = type_info;
		decl->define_decl.generic_params = params;
		RANGE_EXTEND_PREV(decl);
		TRY_CONSUME_EOS_OR(poisoned_decl);
		return decl;
	}

	Decl *decl = decl_new_with_type(alias_name, distinct ? DECL_DISTINCT : DECL_TYPEDEF, visibility);
	decl->span.loc = start;
	decl->typedef_decl.type_info = type_info;
	decl->typedef_decl.is_func = false;
	if (distinct)
	{
		decl->distinct_decl.typedef_decl = decl->typedef_decl;
		decl->type->type_kind = TYPE_DISTINCT;
		decl->decl_kind = DECL_DISTINCT;
	}
	RANGE_EXTEND_PREV(decl);
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}

/**
 * define_ident ::= 'define' (IDENT | CONST_IDENT) '=' identifier_alias generic_params?
 *
 * identifier_alias ::= path? (IDENT | CONST_IDENT)
 */
static inline Decl *parse_define_ident(Context  *context, Visibility visibility)
{
	// 1. Store the beginning of the define.
	TokenId start = context->tok.id;
	advance_and_verify(context, TOKEN_DEFINE);

	// 2. At this point we expect an ident or a const token.
	//    since the Type is handled.
	TokenType alias_type = context->tok.type;
	if (alias_type != TOKEN_IDENT && alias_type != TOKEN_CONST_IDENT)
	{
		SEMA_TOKEN_ERROR(context->tok, "An identifier was expected here.");
		return poisoned_decl;
	}

	// 3. Set up the define.
	Decl *decl = decl_new(DECL_DEFINE, context->tok.id, visibility);
	decl->define_decl.define_kind = DEFINE_IDENT_ALIAS;
	decl->span.loc = start;

	// 4. Advance and consume the '='
	advance(context);
	CONSUME_OR(TOKEN_EQ, poisoned_decl);

	// 5. Here we may an (optional) path, we just check if it starts
	//    with IDENT '::'
	Path *path = NULL;
	if (context_next_is_path_prefix_start(context))
	{
		bool error;
		path = parse_path_prefix(context, &error);
		if (error) return poisoned_decl;
	}

	decl->define_decl.path = path;

	// 6. Check that the token after the path is of the same type.
	if (context->tok.type != alias_type)
	{
		if (alias_type == TOKEN_CONST_IDENT)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a constant name here.");
		}
		else
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a function or variable name here.");
		}
		return poisoned_decl;
	}

	// 7. Consume the identifier
	decl->define_decl.identifier = context->tok.id;
	advance(context);

	if (try_consume(context, TOKEN_LESS))
	{
		decl->define_decl.define_kind = DEFINE_IDENT_GENERIC;
		TypeInfo **params = parse_generic_parameters(context);
		if (!params) return poisoned_decl;
		decl->define_decl.generic_params = params;
	}
	RANGE_EXTEND_PREV(decl);
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}
/**
 * define_decl ::= DEFINE define_type_body |
 */
static inline Decl *parse_define(Context *context, Visibility visibility)
{
	if (context->next_tok.type == TOKEN_TYPE_IDENT)
	{
		return parse_define_type(context, visibility);
	}
	return parse_define_ident(context, visibility);
}


static AttributeDomain TOKEN_TO_ATTR[TOKEN_EOF + 1]  = {
		[TOKEN_FUNC] = ATTR_FUNC,
		[TOKEN_VAR] = ATTR_VAR,
		[TOKEN_ENUM] = ATTR_ENUM,
		[TOKEN_STRUCT] = ATTR_STRUCT,
		[TOKEN_INTERFACE] = ATTR_INTERFACE,
		[TOKEN_UNION] = ATTR_UNION,
		[TOKEN_CONST] = ATTR_CONST,
		[TOKEN_DEFINE] = ATTR_TYPEDEF,
		[TOKEN_ERR] = ATTR_ERROR,
};


/**
 * func_header ::= type '!'? (type '.')? IDENT
 * macro_header ::= (type '!'?)? (type '.')? IDENT
 */
static inline bool parse_func_macro_header(Context *context, bool rtype_is_optional, TypeInfo **rtype_ref, bool *failable_ref, TypeInfo **method_type_ref, TokenId *name_ref)
{
	TypeInfo *rtype = NULL;
	TypeInfo *method_type = NULL;
	bool failable = false;

	// 1. We have a macro with just a name, if so, then we set the name and we're done.
	if (rtype_is_optional && !context_next_is_type_and_not_ident(context))
	{
		goto RESULT;
	}

	// 2. Now we must have a type - either that is the return type or the method type.
	rtype = TRY_TYPE_OR(parse_type(context), false);

	// 3. We possibly have a failable return at this point.
	failable = try_consume(context, TOKEN_BANG);

	// 4. We might have a type here, if so then we read it.
	if (!TOKEN_IS(TOKEN_DOT) && context_next_is_type_and_not_ident(context))
	{
		method_type = TRY_TYPE_OR(parse_type(context), false);
	}

	// 5. If we have a dot here, then we need to interpret this as method function.
	if (try_consume(context, TOKEN_DOT))
	{
		// 5a. What if we don't have a method type?
		if (!method_type)
		{
			// 5b. If the rtype is not optional or the return type was a failable, then this is an error.
			if (!rtype_is_optional || failable)
			{
				SEMA_TOKID_ERROR(context->prev_tok,
				                 "This looks like you are declaring a method without a return type?");
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
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected a name here.", false);
	*failable_ref = failable;
	*name_ref = context->prev_tok;
	*rtype_ref = rtype;
	*method_type_ref = method_type;
	return true;
}


/**
 * macro ::= macro_header '(' macro_params ')' compound_statement
 */
static inline Decl *parse_macro_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_MACRO);

	Decl *decl = decl_new(DECL_MACRO, context->tok.id, visibility);
	TypeInfo **rtype_ref = &decl->macro_decl.rtype;
	TypeInfo **method_type_ref = &decl->macro_decl.type_parent;
	bool failable;
	TokenId name;
	if (!parse_func_macro_header(context, true, rtype_ref, &failable, method_type_ref, &name)) return poisoned_decl;

	decl->macro_decl.failable = failable;
	decl->name = TOKSTR(name);
	decl->name_token = name;

	TokenId block_parameter = {};
	if (!parse_macro_arguments(context, visibility, &decl->macro_decl.parameters, &decl->macro_decl.body_parameters, &block_parameter)) return poisoned_decl;
	decl->macro_decl.block_parameter = block_parameter;
	decl->macro_decl.body = TRY_AST_OR(parse_stmt(context), poisoned_decl);
	return decl;
}


/**
 * error_declaration
 *		: ERROR TYPE_IDENT ';'
 *		| ERROR TYPE_IDENT '{' error_data '}'
 *		;
 */
static inline Decl *parse_error_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ERR);

	Decl *err_decl = decl_new_with_type(context->tok.id, DECL_ERR, visibility);

	if (!consume_type_name(context, "error type")) return poisoned_decl;

	if (try_consume(context, TOKEN_LBRACE))
	{
		while (!try_consume(context, TOKEN_RBRACE))
		{
			TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_decl);
			if (!TOKEN_IS(TOKEN_IDENT))
			{
				SEMA_TOKEN_ERROR(context->tok, "Expected an identifier here.");
				return poisoned_decl;
			}
			Decl *member = decl_new_var(context->tok.id, type, VARDECL_MEMBER, visibility);
			advance(context);
			vec_add(err_decl->strukt.members, member);
			TRY_CONSUME_EOS_OR(poisoned_decl);
		}
		return err_decl;
	}
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return err_decl;
}

/**
 * enum_spec
 *  : type
 *  | type '(' opt_parameter_type_list ')'
 *  ;
 */
static inline bool parse_enum_spec(Context *context, TypeInfo **type_ref, Decl*** parameters_ref, Visibility parent_visibility)
{
	*type_ref = TRY_TYPE_OR(parse_type(context), false);
	if (!try_consume(context, TOKEN_LPAREN)) return true;
	while (!try_consume(context, TOKEN_RPAREN))
	{
		if (!parse_param_decl(context, parent_visibility, parameters_ref, true)) return false;
		if (VECLAST(*parameters_ref)->var.vararg)
		{
			SEMA_TOKID_ERROR(context->prev_tok, "Vararg parameters are not allowed as enum parameters.");
			return false;
		}
		if (!try_consume(context, TOKEN_COMMA))
		{
			EXPECT_OR(TOKEN_RPAREN, false);
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
 *  | CAPS_IDENT '=' const_expr
 *  | CAPS_IDENT '(' expr_list ')'
 *  | CAPS_IDENT '(' expr_list ')' '=' const_expr
 *  ;
 *
 */
static inline Decl *parse_enum_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ENUM);

	Decl *decl = DECL_NEW_WITH_TYPE(DECL_ENUM, visibility);

	if (!consume_type_name(context, "enum")) return poisoned_decl;

	TypeInfo *type = NULL;
	if (try_consume(context, TOKEN_COLON))
	{
		if (!parse_enum_spec(context, &type, &decl->enums.parameters, visibility)) return poisoned_decl;
	}

	CONSUME_OR(TOKEN_LBRACE, false);

	decl->enums.type_info = type ? type : type_info_new_base(type_int, decl->span);
	while (!try_consume(context, TOKEN_RBRACE))
	{
		Decl *enum_const = DECL_NEW(DECL_ENUM_CONSTANT, decl->visibility);
		const char *name = TOKSTR(context->tok);
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name == name)
			{
				SEMA_TOKEN_ERROR(context->tok, "This enum constant is declared twice.");
				SEMA_PREV(other_constant, "The previous declaration was here.");
				decl_poison(enum_const);
				break;
			}
		}
		if (!consume_const_name(context, "enum constant"))
		{
			return poisoned_decl;
		}
		if (try_consume(context, TOKEN_LPAREN))
		{
			Expr **result = NULL;
			if (!parse_arg_list(context, &result, TOKEN_RPAREN, NULL)) return poisoned_decl;
			enum_const->enum_constant.args = result;
			CONSUME_OR(TOKEN_RPAREN, poisoned_decl);
		}
		if (try_consume(context, TOKEN_EQ))
		{
			enum_const->enum_constant.expr = TRY_EXPR_OR(parse_expr(context), poisoned_decl);
		}
		vec_add(decl->enums.values, enum_const);
		// Allow trailing ','
		if (!try_consume(context, TOKEN_COMMA))
		{
			EXPECT_OR(TOKEN_RBRACE, poisoned_decl);
		}
	}
	return decl;
}

#pragma mark --- Parse function



/**
 * Starts after 'func'
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
 *  	: FUNC failable_type func_name '(' opt_parameter_type_list ')' opt_attributes
 *		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_func_definition(Context *context, Visibility visibility, bool is_interface)
{
	Decl *func = decl_new(DECL_FUNC, context->next_tok.id, visibility);
	advance_and_verify(context, TOKEN_FUNC);
	TypeInfo **rtype_ref = &func->func_decl.function_signature.rtype;
	TypeInfo **method_type_ref = &func->func_decl.type_parent;
	bool failable;
	TokenId name;
	if (!parse_func_macro_header(context, false, rtype_ref, &failable, method_type_ref, &name)) return poisoned_decl;
	func->func_decl.function_signature.failable = failable;
	func->name = TOKSTR(name);
	func->name_token = name;
	RANGE_EXTEND_PREV(func);
	if (!parse_parameter_list(context, visibility, &(func->func_decl.function_signature), is_interface)) return poisoned_decl;

	if (!parse_attributes(context, func)) return poisoned_decl;

	// TODO remove
	is_interface = TOKEN_IS(TOKEN_EOS);

	if (is_interface)
	{
		if (TOKEN_IS(TOKEN_LBRACE))
		{
			SEMA_TOKEN_ERROR(context->next_tok, "A function body is not allowed here.");
			return poisoned_decl;
		}
		TRY_CONSUME_OR(TOKEN_EOS, "Expected ';' after function declaration.", poisoned_decl);
		return func;
	}

	TRY_EXPECT_OR(TOKEN_LBRACE, "Expected the beginning of a block with '{'", poisoned_decl);

	func->func_decl.body = TRY_AST_OR(parse_compound_stmt(context), poisoned_decl);

	DEBUG_LOG("Finished parsing function %s", func->name);
	return func;
}


/**
 * interface_declaration ::= INTERFACE TYPE '{' func_decl* '}'
 *
 * @param visibility
 */
static inline Decl *parse_interface_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_INTERFACE);

	TokenId name = context->tok.id;

	if (!consume_type_name(context, "interface")) return poisoned_decl;
	Decl *decl = decl_new_with_type(name, DECL_INTERFACE, visibility);

	if (!parse_attributes(context, decl))
	{
		return poisoned_decl;
	}

	CONSUME_OR(TOKEN_LBRACE, poisoned_decl);

	while (!TOKEN_IS(TOKEN_RBRACE))
	{
		if (!TOKEN_IS(TOKEN_FUNC))
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected a function here.");
			return poisoned_decl;
		}
		Decl *function = TRY_DECL_OR(parse_func_definition(context, visibility, true), poisoned_decl);
		vec_add(decl->interface_decl.functions, function);
	}

	CONSUME_OR(TOKEN_RBRACE, poisoned_decl);
	DEBUG_LOG("Parsed interface %s completely.", TOKSTR(name));
	return decl;
}

static inline bool check_no_visibility_before(Context *context, Visibility visibility)
{
	switch (visibility)
	{
		case VISIBLE_MODULE:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'static' before '%.*s'.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
			return false;
		case VISIBLE_EXTERN:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'extern' before '%.*s'.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
			return false;
		default:
			return true;
	}
}



/**
 *
 * import ::= IMPORT import_path (AS MODULE)? ';'
 *
 * @return true if import succeeded
 */
static inline bool parse_import(Context *context)
{
	advance_and_verify(context, TOKEN_IMPORT);

	bool private = try_consume(context, TOKEN_PRIVATE);

	if (!TOKEN_IS(TOKEN_IDENT))
	{
		if (TOKEN_IS(TOKEN_STRING))
		{
			SEMA_TOKEN_ERROR(context->tok, "An import should be followed by a plain identifier, not a string. Did you accidentally put the module name between \"\"?");
			return false;
		}
		SEMA_TOKEN_ERROR(context->tok, "Import statement should be followed by the name of the module to import.");
		return false;
	}

	Path *path = parse_module_path(context);
	context_add_import(context, path, NO_TOKEN, NO_TOKEN, private);
	TRY_CONSUME_EOS_OR(false);
	return true;
}


/**
 * imports ::= import*
 */
void parse_imports(Context *context)
{
	while (TOKEN_IS(TOKEN_IMPORT))
	{
		if (!parse_import(context)) recover_top_level(context);
	}
}

static inline TokenId parse_doc_opt_rest_of_line(Context *context)
{
	return try_consume(context, TOKEN_DOCS_LINE) ? context->prev_tok : INVALID_TOKEN_ID;
}

static inline bool parse_doc_param(Context *context, Ast *docs)
{
	switch (context->tok.type)
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
			SEMA_TOKEN_ERROR(context->tok, "Expected a parameter name here.");
			return false;
	}
	docs->doc_directive.kind = DOC_DIRECTIVE_PARAM;
	docs->doc_directive.param.param = context->tok.id;
	advance(context);
	docs->doc_directive.param.rest_of_line = parse_doc_opt_rest_of_line(context);
	return true;
}

static inline bool parse_doc_errors(Context *context, Ast *docs)
{
	TODO
	while (1)
	{
		if (context->tok.type != TOKEN_TYPE_IDENT)
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an error type here.");
		}
	}
	switch (context->tok.type)
	{
		case TOKEN_TYPE_IDENT:
			break;
		default:
			return false;
	}
	docs->doc_directive.kind = DOC_DIRECTIVE_PARAM;
	docs->doc_directive.param.param = context->tok.id;
	advance(context);
	docs->doc_directive.param.rest_of_line = parse_doc_opt_rest_of_line(context);
	return true;
}

static inline bool parse_doc_contract(Context *context, Ast *docs)
{
	docs->doc_directive.contract.decl_exprs = TRY_EXPR_OR(parse_decl_expr_list(context), false);
	if (try_consume(context, TOKEN_COLON))
	{
		docs->doc_directive.contract.comment = TRY_EXPR_OR(parse_expr(context), false);
	}
	return true;
}

static bool parse_docs(Context *context, Ast **docs)
{
	*docs = NULL;
	if (!try_consume(context, TOKEN_DOCS_START)) return true;

	Ast *ast = new_ast(AST_DOCS, (SourceSpan) { .loc = context->prev_tok, .end_loc = context->prev_tok });
	while (!try_consume(context, TOKEN_DOCS_END))
	{
		// Spin past the lines and line ends
		if (try_consume(context, TOKEN_DOCS_EOL)) continue;
		if (try_consume(context, TOKEN_DOCS_LINE)) continue;
		CONSUME_OR(TOKEN_DOCS_DIRECTIVE, false);
		CONSUME_OR(TOKEN_IDENT, false);
		const char *directive = TOKSTR(context->prev_tok);
		SourceSpan span = { context->prev_tok, context->prev_tok };
		Ast *doc_ast = new_ast(AST_DOC_DIRECTIVE, span);
		if (directive == kw_param)
		{
			if (!parse_doc_param(context, doc_ast)) return false;
			goto LINE_END;
		}
		if (directive == kw_pure)
		{
			vec_add(ast->directives, doc_ast);
			doc_ast->doc_directive.kind = DOC_DIRECTIVE_PURE;
			doc_ast->doc_directive.pure.rest_of_line = parse_doc_opt_rest_of_line(context);
			goto LINE_END;
		}
		if (directive == kw_ensure)
		{
			doc_ast->doc_directive.kind = DOC_DIRECTIVE_ENSURE;
			if (!parse_doc_contract(context, doc_ast)) return false;
			goto LINE_END;
		}
		if (directive == kw_require)
		{
			doc_ast->doc_directive.kind = DOC_DIRECTIVE_REQUIRE;
			if (!parse_doc_contract(context, doc_ast)) return false;
			goto LINE_END;
		}
		if (directive == kw_errors)
		{
			if (!parse_doc_errors(context, doc_ast)) return false;
			goto LINE_END;
		}
		doc_ast->doc_directive.kind = DOC_DIRECTIVE_UNKNOWN;
		doc_ast->doc_directive.generic.directive_name = directive;
		doc_ast->doc_directive.generic.rest_of_line = parse_doc_opt_rest_of_line(context);

LINE_END:
		vec_add(ast->directives, doc_ast);
		if (try_consume(context, TOKEN_DOCS_EOL)) continue;
		EXPECT_OR(TOKEN_DOCS_END, false);
	}
	*docs = ast;
	return true;
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
Decl *parse_top_level_statement(Context *context)
{
	Ast *docs = NULL;
	if (!parse_docs(context, &docs)) return poisoned_decl;
	Visibility visibility = VISIBLE_PUBLIC;
	switch (context->tok.type)
	{
		case TOKEN_PRIVATE:
			visibility = VISIBLE_MODULE;
			advance(context);
			break;
		case TOKEN_EXTERN:
			visibility = VISIBLE_EXTERN;
			advance(context);
		default:
			break;
	}

	Decl *decl;
	TokenType type = context->tok.type;
	switch (context->tok.type)
	{
		case TOKEN_DOCS_START:
			if (context->docs_start.index == INVALID_TOKEN_ID.index)
			{
				SEMA_TOKEN_ERROR(context->tok, "Did not expect doc comments after visibility.");
				return poisoned_decl;
			}
			SEMA_TOKEN_ERROR(context->tok, "There are more than one doc comment in a row, that is not allowed.");
			return poisoned_decl;
		case TOKEN_DEFINE:
			decl = TRY_DECL_OR(parse_define(context, visibility), poisoned_decl);
			break;
		case TOKEN_ATTRIBUTE:
			TODO
			break;
		case TOKEN_FUNC:
			decl = TRY_DECL_OR(parse_func_definition(context, visibility, false), poisoned_decl);
			break;
		case TOKEN_CT_ASSERT:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			{
				Ast *ast = TRY_AST_OR(parse_ct_assert_stmt(context), false);
				decl = decl_new(DECL_CT_ASSERT, ast->span.loc, visibility);
				decl->ct_assert_decl = ast;
				if (docs)
				{
					SEMA_ERROR(docs, "Unexpected doc comment before $assert, did you mean to use a regular comment?");
					return poisoned_decl;
				}
				return decl;
			}
		case TOKEN_CT_IF:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			decl = TRY_DECL_OR(parse_ct_if_top_level(context), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(docs, "Unexpected doc comment before $if, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		case TOKEN_CT_SWITCH:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			decl = TRY_DECL_OR(parse_ct_switch_top_level(context), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(docs, "Unexpected doc comment before $switch, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		case TOKEN_CONST:
			decl = TRY_DECL_OR(parse_top_level_const_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_INTERFACE:
			decl = TRY_DECL_OR(parse_interface_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			decl = TRY_DECL_OR(parse_struct_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_GENERIC:
			TODO
			break;
		case TOKEN_MACRO:
			decl = TRY_DECL_OR(parse_macro_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_ENUM:
			decl = TRY_DECL_OR(parse_enum_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_ERR:
			decl = TRY_DECL_OR(parse_error_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_TYPE_IDENT:
			// All of these start type
			decl = TRY_DECL_OR(parse_global_declaration(context, visibility), poisoned_decl);
			break;
		case TOKEN_IDENT:
			if (context->next_tok.type == TOKEN_SCOPE)
			{
				decl = TRY_DECL_OR(parse_global_declaration(context, visibility), poisoned_decl);
				break;
			}
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			decl = TRY_DECL_OR(parse_incremental_array(context), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(docs,
				           "Unexpected doc comment before incremental array, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		case TOKEN_EOF:
			SEMA_TOKID_ERROR(context->prev_tok, "Expected a top level declaration");
			return poisoned_decl;
		case TOKEN_CT_CONST_IDENT:
			if (context->next_tok.type == TOKEN_EQ)
			{
				SEMA_TOKEN_ERROR(context->tok,
				                 "Did you forget a 'const' before the name of this compile time constant?");
			}
			else
			{
				SEMA_TOKEN_ERROR(context->tok, "Compile time constant unexpectedly found.");
			}
			return poisoned_decl;
		case TOKEN_IMPORT:
			SEMA_TOKEN_ERROR(context->tok, "Imports are only allowed directly after the module declaration.");
			return poisoned_decl;
		default:
			// We could have included all fundamental types above, but do it here instead.
			if (!token_is_type(context->tok.type))
			{
				SEMA_TOKEN_ERROR(context->tok, "Expected a top level declaration here.");
				return poisoned_decl;
			}
			decl = TRY_DECL_OR(parse_global_declaration(context, visibility), poisoned_decl);
			break;
	}
	assert(decl);
	decl->docs = docs;
	return decl;
}