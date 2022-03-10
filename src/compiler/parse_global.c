#include "compiler_internal.h"
#include "parser_internal.h"


static Decl *parse_const_declaration(ParseContext *c, Visibility visibility);
static inline Decl *parse_func_definition(ParseContext *c, Visibility visibility, AstDocDirective *docs, bool is_interface);
static inline bool parse_bitstruct_body(ParseContext *c, Decl *decl);

#define DECL_VAR_NEW(type__, var__, visible__) decl_new_var(symstr(c), c->span, type__, var__, visible__);


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
			case TOKEN_GENERIC:
			case TOKEN_DEFINE:
			case TOKEN_OPTENUM:
			case TOKEN_OPTNUM:
			case TOKEN_ERRNUM:
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
			case TOKEN_FUNC:
			case TOKEN_FN:
			case TOKEN_STRUCT:
			case TOKEN_UNION:
			case TOKEN_BITSTRUCT:
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

static inline bool parse_decl_initializer(ParseContext *c, Decl *decl, bool allow_void)
{
	if (try_consume(c, TOKEN_VOID))
	{
		if (!allow_void)
		{
			SEMA_ERROR_LAST("'void' is not allowed here, it's only allowed for non constant global and local variables.");
			return false;
		}
		decl->var.no_init = true;
		return true;
	}
	ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), false);
	return true;
}

// --- Parse CT conditional code

static inline bool parse_top_level_block(ParseContext *c, Decl ***decls, TokenType end1, TokenType end2, TokenType end3)
{
	CONSUME_OR_RET(TOKEN_COLON, false);
	while (!tok_is(c, end1) && !tok_is(c, end2) && !tok_is(c, end3) && !tok_is(c, TOKEN_EOF))
	{
		Decl *decl = parse_top_level_statement(c);
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
 * @param c
 * @return the declaration if successfully parsed, poisoned_decl otherwise.
 */
static inline Decl *parse_ct_if_top_level(ParseContext *c)
{
	Decl *ct = decl_new_ct(DECL_CT_IF, c->span);
	advance_and_verify(c, TOKEN_CT_IF);
	ASSIGN_EXPR_OR_RET(ct->ct_if_decl.expr, parse_const_paren_expr(c), poisoned_decl);

	if (!parse_top_level_block(c, &ct->ct_if_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (tok_is(c, TOKEN_CT_ELIF))
	{
		Decl *ct_elif = decl_new_ct(DECL_CT_ELIF, c->span);
		advance_and_verify(c, TOKEN_CT_ELIF);
		ASSIGN_EXPR_OR_RET(ct_elif->ct_elif_decl.expr, parse_const_paren_expr(c), poisoned_decl);

		if (!parse_top_level_block(c, &ct_elif->ct_elif_decl.then, TOKEN_CT_ENDIF, TOKEN_CT_ELIF, TOKEN_CT_ELSE)) return poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (tok_is(c, TOKEN_CT_ELSE))
	{
		Decl *ct_else = decl_new_ct(DECL_CT_ELSE, c->span);
		advance_and_verify(c, TOKEN_CT_ELSE);
		ct_if_decl->elif = ct_else;
		if (!parse_top_level_block(c, &ct_else->ct_else_decl, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF, TOKEN_CT_ENDIF)) return poisoned_decl;
	}
	CONSUME_OR_RET(TOKEN_CT_ENDIF, poisoned_decl);
	CONSUME_OR_RET(TOKEN_EOS, poisoned_decl);
	return ct;
}

/**
 * ct_case ::= (CT_DEFAULT | CT_CASE type) ':' top_level_statement*
 *
 * @param c
 * @return poisoned decl if parsing fails.
 */
static inline Decl *parse_ct_case(ParseContext *c)
{
	Decl *decl;
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
	TRY_CONSUME_OR_RET(TOKEN_COLON, "Expected ':' here.", poisoned_decl);
	while (1)
	{
		TokenType type = c->tok;
		if (type == TOKEN_CT_DEFAULT || type == TOKEN_CT_CASE || type == TOKEN_CT_ENDSWITCH) break;
		ASSIGN_DECL_OR_RET(Decl * stmt, parse_top_level_statement(c), poisoned_decl);
		vec_add(decl->ct_case_decl.body, stmt);
	}
	return decl;
}

/**
 * ct_switch_top_level ::= CT_SWITCH const_paren_expr '{' ct_case* '}'
 * @param c
 * @return the declaration if successfully parsed, NULL otherwise.
 */
static inline Decl *parse_ct_switch_top_level(ParseContext *c)
{
	Decl *ct = decl_new_ct(DECL_CT_SWITCH, c->span);
	advance_and_verify(c, TOKEN_CT_SWITCH);
	ASSIGN_EXPR_OR_RET(ct->ct_switch_decl.expr, parse_const_paren_expr(c), poisoned_decl);

	CONSUME_OR_RET(TOKEN_COLON, poisoned_decl);
	while (!try_consume(c, TOKEN_CT_ENDSWITCH))
	{
		ASSIGN_DECL_OR_RET(Decl * result, parse_ct_case(c), poisoned_decl);
		vec_add(ct->ct_switch_decl.cases, result);
	}
	CONSUME_OR_RET(TOKEN_EOS, poisoned_decl);
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
				return false;
			}
			if (token_is_some_ident(c->tok))
			{
				SEMA_ERROR_HERE("The elements of a module path must consist of only lower case letters, 0-9 and '_'.");
				return false;
			}
			SEMA_ERROR_HERE("Each '::' must be followed by a regular lower case sub module name.");
			return NULL;
		}
		if (string == kw_main)
		{
			SEMA_ERROR_LAST("'main' is not a valid name in a module path, please pick something else.");
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
	return path_create_from_string(scratch_buffer_to_string(), global_context.scratch_buffer_len, span);
}


// --- Parse import and module

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
static inline bool parse_optional_module_params(ParseContext *c, const char ***tokens_ref)
{

	*tokens_ref = NULL;

	if (!try_consume(c, TOKEN_LESS)) return true;

	if (try_consume(c, TOKEN_GREATER))
	{
		SEMA_ERROR_HERE("Generic parameter list cannot be empty.");
		return false;
	}

	// No params
	while (1)
	{
		switch (c->tok)
		{
			case TOKEN_TYPE_IDENT:
				break;
			case TOKEN_COMMA:
				SEMA_ERROR_HERE("Unexpected ','");
				return false;
			case TOKEN_IDENT:
				SEMA_ERROR_HERE("The module parameter must be a type.");
				return false;
			case TOKEN_CT_IDENT:
			case TOKEN_CT_TYPE_IDENT:
				SEMA_ERROR_HERE("The module parameter cannot be a $-prefixed name.");
				return false;
			default:
				SEMA_ERROR_HERE("Only generic parameters are allowed here as parameters to the module.");
				return false;
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
 * module ::= MODULE module_path ('<' module_params '>')? EOS
 */
bool parse_module(ParseContext *c)
{
	if (!try_consume(c, TOKEN_MODULE))
	{
		return context_set_module_from_filename(c);
	}

	bool is_private = try_consume(c, TOKEN_PRIVATE);

	if (tok_is(c, TOKEN_STRING))
	{
		SEMA_ERROR_HERE("'module' should be followed by a plain identifier, not a string. Did you accidentally put the module name between \"\"?");
		return false;
	}

	if (!tok_is(c, TOKEN_IDENT))
	{
		if (token_is_keyword(c->tok))
		{
			SEMA_ERROR_HERE("The module name cannot contain a reserved keyword, try another name.");
			return false;
		}
		if (token_is_some_ident(c->tok))
		{
			SEMA_ERROR_HERE("The module name must consist of only lower case letters, 0-9 and '_'.");
			return false;
		}
		SEMA_ERROR_HERE("'module' should be followed by a module name.");
		return false;
	}


	Path *path = parse_module_path(c);

	// Expect the module name
	if (!path)
	{
		path = CALLOCS(Path);
		path->len = strlen("#invalid");
		path->module = "#invalid";
		path->span = INVALID_SPAN;
		context_set_module(c, path, NULL, false);
		recover_top_level(c);
		return false;
	}

	// Is this a generic module?
	const char **generic_parameters = NULL;
	if (!parse_optional_module_params(c, &generic_parameters))
	{
		context_set_module(c, path, NULL, is_private);
		recover_top_level(c);
		return true;
	}
	context_set_module(c, path, generic_parameters, is_private);
	CONSUME_EOS_OR_RET(false);
	return true;
}


bool consume_ident(ParseContext *c, const char* name)
{
	if (try_consume(c, TOKEN_IDENT)) return true;
	if (tok_is(c, TOKEN_TYPE_IDENT) || tok_is(c, TOKEN_CONST_IDENT))
	{
		SEMA_ERROR_HERE("A %s must start with a lower case letter.", name);
		return false;
	}
	if (token_is_keyword(c->tok))
	{
		SEMA_ERROR_HERE("This is a reserved keyword, did you accidentally use it?");
		return false;
	}
	SEMA_ERROR_HERE("A %s was expected.", name);
	return false;
}

static bool consume_type_name(ParseContext *c, const char* type)
{
	if (tok_is(c, TOKEN_IDENT) || token_is_keyword(c->tok))
	{
		SEMA_ERROR_HERE("Names of %ss must start with an upper case letter.", type);
		return false;
	}
	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		SEMA_ERROR_HERE("Names of %ss cannot be all upper case.", type);
		return false;
	}
	if (!consume(c, TOKEN_TYPE_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}

bool consume_const_name(ParseContext *c, const char* type)
{
	if (tok_is(c, TOKEN_IDENT) || tok_is(c, TOKEN_TYPE_IDENT))
	{
		SEMA_ERROR_HERE("Names of %ss must be all upper case.", type);
		return false;
	}
	if (!consume(c, TOKEN_CONST_IDENT, "A constant name was expected here, did you forget it?")) return false;
	return true;
}


Path *parse_path_prefix(ParseContext *c, bool *had_error)
{
	*had_error = false;
	if (!tok_is(c, TOKEN_IDENT) || peek(c) != TOKEN_SCOPE) return NULL;

	char *scratch_ptr = global_context.scratch_buffer;
	uint32_t offset = 0;

	Path *path = CALLOCS(Path);
	path->span = c->span;
	unsigned len = strlen(symstr(c));
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
		SEMA_ERROR(path, "A module name was expected here.");
		*had_error = true;
		return NULL;

	}
	path->len = offset;

	return path;
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
	if (try_consume(c, TOKEN_CT_TYPEOF))
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_EXPRESSION, c->prev_span);
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
	Path *path = parse_path_prefix(c, &had_error);
	if (had_error) return poisoned_type_info;
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
	ASSIGN_EXPR_OR_RET(vector->array.len, parse_expr(c), poisoned_type_info);
	CONSUME_OR_RET(TOKEN_RVEC, poisoned_type_info);
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

TypeInfo *parse_failable_type(ParseContext *c)
{
	ASSIGN_TYPE_OR_RET(TypeInfo *info, parse_base_type(c), poisoned_type_info);
	ASSIGN_TYPE_OR_RET(info, parse_type_with_base(c, info), poisoned_type_info);
	if (try_consume(c, TOKEN_BANG))
	{
		assert(!info->failable);
		info->failable = true;
		if (info->resolve_status == RESOLVE_DONE)
		{
			info->type = type_get_failable(info->type);
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
Decl *parse_decl_after_type(ParseContext *c, TypeInfo *type)
{
	if (tok_is(c, TOKEN_LPAREN))
	{
		SEMA_ERROR_HERE("Expected '{'.");
		return poisoned_decl;
	}

	EXPECT_IDENT_FOR_OR("variable name", poisoned_decl);

	Decl *decl = DECL_VAR_NEW(type, VARDECL_LOCAL, VISIBLE_LOCAL);
	advance(c);

	if (!parse_attributes(c, &decl->attributes)) return poisoned_decl;
	if (tok_is(c, TOKEN_EQ))
	{
		if (!decl)
		{
			SEMA_ERROR_HERE("Expected an identifier before '='.");
			return poisoned_decl;
		}
		advance_and_verify(c, TOKEN_EQ);
		if (!parse_decl_initializer(c, decl, true)) return poisoned_decl;
	}
	return decl;
}

/**
 * declaration ::= ('static' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
Decl *parse_decl(ParseContext *c)
{
	if (tok_is(c, TOKEN_CONST))
	{
		return parse_const_declaration(c, VISIBLE_LOCAL);
	}

	bool is_threadlocal = try_consume(c, TOKEN_TLOCAL);
	bool is_static = !is_threadlocal && try_consume(c, TOKEN_STATIC);

	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_failable_type(c), poisoned_decl);

	ASSIGN_DECL_OR_RET(Decl * decl, parse_decl_after_type(c, type), poisoned_decl);
	if (type->failable && decl->var.unwrap)
	{
		SEMA_ERROR(decl, "You cannot use unwrap with a failable variable.");
		return poisoned_decl;
	}
	decl->var.is_static = is_static || is_threadlocal;
	decl->var.is_threadlocal = is_threadlocal;
	return decl;
}

Expr *parse_decl_or_expr(ParseContext *c, Decl **decl_ref)
{
	TypeInfo *type_info;
	Expr *expr = parse_expr_or_type(c, &type_info);
	if (expr) return expr;
	ASSIGN_DECL_OR_RET(*decl_ref, parse_decl_after_type(c, type_info), poisoned_expr);
	return NULL;
}


/**
 * const_decl
 *  : 'const' type? IDENT '=' const_expr
 *  ;
 */
static Decl *parse_const_declaration(ParseContext *c, Visibility visibility)
{
	advance_and_verify(c, TOKEN_CONST);

	TypeInfo *type_info = NULL;

	if (c->tok != TOKEN_CONST_IDENT)
	{
		ASSIGN_TYPE_OR_RET(type_info, parse_type(c), poisoned_decl);
	}
	Decl *decl = decl_new_var(symstr(c), c->span, type_info, VARDECL_CONST, visibility);

	if (!consume_const_name(c, "const")) return poisoned_decl;

	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);

	if (!parse_decl_initializer(c, decl, false)) return poisoned_decl;

	RANGE_EXTEND_PREV(decl);

	return decl;
}

Decl *parse_var_decl(ParseContext *c)
{
	advance_and_verify(c, TOKEN_VAR);
	Decl *decl;
	switch (c->tok)
	{
		case TOKEN_CT_IDENT:
			decl = DECL_VAR_NEW(NULL, VARDECL_LOCAL_CT, VISIBLE_LOCAL);
			advance(c);
			if (try_consume(c, TOKEN_EQ))
			{
				ASSIGN_EXPR_OR_RET(decl->var.init_expr, parse_expr(c), poisoned_decl);
			}
			break;
		case TOKEN_CT_TYPE_IDENT:
			decl = DECL_VAR_NEW(NULL, VARDECL_LOCAL_CT_TYPE, VISIBLE_LOCAL);
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
	if (!try_consume(c, TOKEN_AT))
	{
		*attribute_ref = NULL;
		return true;
	}
	bool had_error;
	Path *path = parse_path_prefix(c, &had_error);
	if (had_error) return false;

	Attr *attr = CALLOCS(Attr);

	attr->name = symstr(c);
	attr->span = c->span;
	attr->path = path;

	if (tok_is(c, TOKEN_TYPE_IDENT) || tok_is(c, TOKEN_TYPE_IDENT))
	{
		advance(c);
	}
	else
	{
		TRY_CONSUME_OR_RET(TOKEN_IDENT, "Expected an attribute", false);
	}

	if (tok_is(c, TOKEN_LPAREN))
	{
		ASSIGN_EXPR_OR_RET(attr->expr, parse_const_paren_expr(c), false);
	}

	*attribute_ref = attr;
	return true;
}
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
bool parse_attributes(ParseContext *c, Attr ***attributes_ref)
{
	*attributes_ref = NULL;

	while (1)
	{
		Attr *attr;
		if (!parse_attribute(c, &attr)) return false;
		if (!attr) return true;
		const char *name = attr->name;
		VECEACH(*attributes_ref, i)
		{
			Attr *other_attr = *attributes_ref[i];
			if (other_attr->name == name)
			{
				SEMA_ERROR(attr, "Repeat of attribute '%s' here.", name);
				return false;
			}
		}
		vec_add(*attributes_ref, attr);
	}
	return true;
}


/**
 * global_declaration
 * 	: global? failable_type IDENT ';'
 * 	| global? failable_type IDENT '=' expression ';'
 * 	| global? failable_type func_definition
 * 	;
 *
 * @param visibility
 * @return true if parsing succeeded
 */
static inline Decl *parse_global_declaration(ParseContext *c, Visibility visibility)
{
	bool threadlocal = try_consume(c, TOKEN_TLOCAL);

	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_failable_type(c), poisoned_decl);

	Decl *decl = DECL_VAR_NEW(type, VARDECL_GLOBAL, visibility);

	decl->var.is_threadlocal = threadlocal;

	if (tok_is(c, TOKEN_CONST_IDENT))
	{
		SEMA_ERROR_HERE("This looks like a constant variable, did you forget 'const'?");
		return poisoned_decl;
	}

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

	if (!parse_attributes(c, &decl->attributes)) return poisoned_decl;
	if (try_consume(c, TOKEN_EQ))
	{
		if (!parse_decl_initializer(c, decl, true)) return poisoned_decl;
	}
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}



/**
 * param_declaration ::= type_expression '...'?) (IDENT ('=' initializer)?)?
 *  ;
 */
static inline bool parse_param_decl(ParseContext *c, Visibility parent_visibility, Decl*** parameters, bool require_name)
{
	ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_type(c), false);
	bool vararg = try_consume(c, TOKEN_ELLIPSIS);
	Decl *param = DECL_VAR_NEW(type, VARDECL_PARAM, parent_visibility);
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
		if (!parse_decl_initializer(c, param, false)) return poisoned_decl;
	}

	vec_add(*parameters, param);
	RANGE_EXTEND_PREV(param);
	return true;
}

bool parse_next_is_typed_parameter(ParseContext *c)
{
	switch (c->tok)
	{
		case TOKEN_IDENT:
		{
			return peek(c) == TOKEN_SCOPE;
		}
		case TYPE_TOKENS:
		case TOKEN_TYPE_IDENT:
			return true;
		default:
			return false;
	}
}

/**
 * parameters ::= (parameter (',' parameter)*)?
 * non_type_ident = IDENT | HASH_IDENT | CT_IDENT
 * parameter ::= type ELLIPSIS? (non_type_ident ('=' expr))?
 *             | ELLIPSIS (CT_TYPE_IDENT | non_type_ident ('=' expr)?)?
 */
bool parse_parameters(ParseContext *c, Visibility visibility, Decl ***params_ref)
{
	Decl** params = NULL;
	bool var_arg_found = false;

	while (!tok_is(c, TOKEN_EOS) && !tok_is(c, TOKEN_RPAREN))
	{
		TypeInfo *type = NULL;

		bool ellipsis = try_consume(c, TOKEN_ELLIPSIS);

		if (!ellipsis && parse_next_is_typed_parameter(c))
		{
			ASSIGN_TYPE_OR_RET(type, parse_type(c), false);
			ellipsis = try_consume(c, TOKEN_ELLIPSIS);
		}

		if (ellipsis && var_arg_found)
		{
			SEMA_ERROR_LAST("Only a single vararg parameter is allowed.");
			return false;
		}

		VarDeclKind param_kind;
		const char *name = NULL;
		SourceSpan span = c->span;
		bool no_name = false;
		bool vararg_implicit = false;
		switch (c->tok)
		{
			case TOKEN_IDENT:
				// normal foo
				name = symstr(c);
				param_kind = VARDECL_PARAM;
				// Check for "foo..." which defines an implicit "any" vararg
				if (peek(c) == TOKEN_ELLIPSIS)
				{
					if (ellipsis)
					{
						SEMA_ERROR_HERE("Unexpected '...' here.");
						return false;
					}
					advance(c);
					if (type)
					{
						SEMA_ERROR_HERE("The '...' should appear after the type.");
						return false;
					}
					type = type_info_new_base(type_any, c->span);
					ellipsis = true;
					vararg_implicit = true;
				}
				break;
			case TOKEN_CT_IDENT:
				// ct_var $foo
				name = symstr(c);
				param_kind = VARDECL_PARAM_CT;
				break;
			case TOKEN_AMP:
				// reference &foo
				advance(c);
				name = symstr(c);
				span = extend_span_with_token(span, c->span);
				if (!tok_is(c, TOKEN_IDENT))
				{
					SEMA_ERROR_HERE("Only normal variables may be passed by reference.");
					return false;
				}
				param_kind = VARDECL_PARAM_REF;
				break;
			case TOKEN_HASH_TYPE_IDENT:
				// #Foo (not allowed)
				SEMA_ERROR_HERE("An unevaluated expression can never be a type, did you mean to use $Type?");
				return false;
			case TOKEN_HASH_IDENT:
				// expression #foo
				name = symstr(c);
				param_kind = VARDECL_PARAM_EXPR;
				break;
				// Compile time type $Type
			case TOKEN_CT_TYPE_IDENT:
				name = symstr(c);
				param_kind = VARDECL_PARAM_CT_TYPE;
				break;
			case TOKEN_COMMA:
			case TOKEN_EOS:
			case TOKEN_RPAREN:
				if (!type && !ellipsis)
				{
					SEMA_ERROR_HERE("Expected a parameter.");
					return false;
				}
				no_name = true;
				span = c->prev_span;
				param_kind = VARDECL_PARAM;
				break;
			default:
				if (!type && parse_next_may_be_type(c))
				{
					ASSIGN_TYPE_OR_RET(type, parse_type(c), false);
					param_kind = VARDECL_PARAM;
					no_name = true;
					span = type->span;
					break;
				}
				SEMA_ERROR_HERE("Expected a parameter.");
				return false;
		}
		Decl *param = decl_new_var(name, span, type, param_kind, visibility);
		param->var.type_info = type;
		if (!no_name)
		{
			advance(c);
			if (try_consume(c, TOKEN_EQ))
			{
				if (!parse_decl_initializer(c, param, false)) return poisoned_decl;
			}
		}
		if (!parse_attributes(c, &param->attributes)) return false;
		var_arg_found |= ellipsis;
		param->var.vararg = ellipsis;
		param->var.vararg_implicit = vararg_implicit;
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
static inline bool parse_parameter_list(ParseContext *c, Visibility parent_visibility, FunctionSignature *signature, bool is_interface)
{
	Decl **decls = NULL;
	CONSUME_OR_RET(TOKEN_LPAREN, false);
	if (!parse_parameters(c, parent_visibility, &decls)) return false;
	CONSUME_OR_RET(TOKEN_RPAREN, false);

	signature->params = decls;
	Decl *last = VECLAST(decls);

	// The last parameter may hold a vararg
	if (last && last->var.vararg)
	{
		// Is it "(foo...)" (any) or "(int... foo)" (typed)?
		if (last->var.type_info)
		{
			signature->variadic = last->var.vararg_implicit ? VARIADIC_ANY : VARIADIC_TYPED;
		}
		else
		{
			// Remove the last element if it's a raw variant, i.e. "(...)"
			vec_pop(decls);
			signature->variadic = VARIADIC_RAW;
		}
	}
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
				member = decl_new_with_type(NULL, c->span, decl_kind, parent->visibility);
				advance(c);
			}
			else
			{
				advance(c);
				member = decl_new_with_type(symstr(c), c->span, decl_kind, parent->visibility);
				advance_and_verify(c, TOKEN_IDENT);
			}
			if (decl_kind == DECL_BITSTRUCT)
			{
				TRY_CONSUME_OR_RET(TOKEN_COLON, "':' followed by bitstruct type (e.g. 'int') was expected here.", poisoned_decl);
				ASSIGN_TYPE_OR_RET(member->bitstruct.base_type, parse_type(c), poisoned_decl);
				if (!parse_bitstruct_body(c, member)) return decl_poison(parent);
			}
			else
			{
				if (!parse_attributes(c, &member->attributes)) return false;
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
			EXPECT_OR_RET(TOKEN_IDENT, false);
			Decl *member = DECL_VAR_NEW(type, VARDECL_MEMBER, parent->visibility);
			vec_add(parent->strukt.members, member);
			index++;
			if (index > MAX_MEMBERS)
			{
				SEMA_ERROR(member, "Can't add another member: the count would exceed maximum of %d elements.", MAX_MEMBERS);
				return false;
			}
			advance(c);
			if (!parse_attributes(c, &member->attributes)) return false;
			if (!try_consume(c, TOKEN_COMMA)) break;
			if (was_inline)
			{
				SEMA_ERROR(member, "'Inline' can only be applied to a single member, so please define it on its own line.");
				return false;
			}
		}
		CONSUME_OR_RET(TOKEN_EOS, false);
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
static inline Decl *parse_struct_declaration(ParseContext *c, Visibility visibility)
{
	TokenType type = c->tok;

	advance(c);
	const char* type_name = struct_union_name_from_token(type);

	Decl *decl = decl_new_with_type(symstr(c), c->span, decl_from_token(type), visibility);

	if (!consume_type_name(c, type_name)) return poisoned_decl;

	if (!parse_attributes(c, &decl->attributes))
	{
		return poisoned_decl;
	}

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

	while (!try_consume(c, TOKEN_RBRACE))
	{
		ASSIGN_TYPE_OR_RET(TypeInfo *type, parse_type(c), false);
		Decl *member_decl = DECL_VAR_NEW(type, VARDECL_BITMEMBER, VISIBLE_LOCAL);

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
		CONSUME_OR_RET(TOKEN_EOS, false);
		vec_add(decl->bitstruct.members, member_decl);
	}

	return true;
}
/**
 * bitstruct_declaration = 'bitstruct' IDENT ':' type bitstruct_body
 */
static inline Decl *parse_bitstruct_declaration(ParseContext *c, Visibility visibility)
{
	advance_and_verify(c, TOKEN_BITSTRUCT);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_BITSTRUCT, visibility);
	if (!consume_type_name(c, "bitstruct")) return poisoned_decl;

	TRY_CONSUME_OR_RET(TOKEN_COLON, "':' followed by bitstruct type (e.g. 'int') was expected here.", poisoned_decl);

	ASSIGN_TYPE_OR_RET(decl->bitstruct.base_type, parse_type(c), poisoned_decl);

	if (!parse_attributes(c, &decl->attributes))
	{
		return poisoned_decl;
	}

	if (!parse_bitstruct_body(c, decl))
	{
		return poisoned_decl;
	}

	return decl;

}

static inline Decl *parse_top_level_const_declaration(ParseContext *c, Visibility visibility)
{
	ASSIGN_DECL_OR_RET(Decl * decl, parse_const_declaration(c, visibility), poisoned_decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}


/**
 * macro_arguments ::= '(' parameters (EOS trailing_block_parameter )? ')'
 *
 * trailing_block_parameter ::= '@' IDENT ( '(' parameters ')' )?
 */
static bool parse_macro_arguments(ParseContext *c, Visibility visibility, Decl ***params_ref, DeclId *body_param_ref)
{
	CONSUME_OR_RET(TOKEN_LPAREN, false);
	*params_ref = NULL;
	*body_param_ref = 0;

	// Parse the regular parameters.
	if (!parse_parameters(c, visibility, params_ref)) return false;

	// Do we have trailing block parameters?
	if (try_consume(c, TOKEN_EOS))
	{
		// Consume '@' IDENT
		TRY_CONSUME_OR_RET(TOKEN_AT, "Expected an ending ')' or a block parameter on the format '@block(...).", false);
		Decl *body_param = decl_new(DECL_BODYPARAM, symstr(c), c->span, visibility);
		if (!consume_ident(c, "variable name")) return false;
		if (try_consume(c, TOKEN_LPAREN))
		{
			if (!parse_parameters(c, visibility, &body_param->body_params)) return false;
			CONSUME_OR_RET(TOKEN_RPAREN, false);
		}
		*body_param_ref = declid(body_param);
	}
	CONSUME_OR_RET(TOKEN_RPAREN, false);
	return true;
}

/**
 * define_parameters ::= type (',' type)* '>'
 *
 * @return NULL if parsing failed, otherwise a list of Type*
 */
static inline TypeInfo **parse_generic_parameters(ParseContext *c)
{
	TypeInfo **types = NULL;
	while (!try_consume(c, TOKEN_GREATER))
	{
		ASSIGN_TYPE_OR_RET(TypeInfo *type_info, parse_type(c), NULL);
		vec_add(types, type_info);
		TokenType tok = c->tok;
		if (tok != TOKEN_RPAREN && tok != TOKEN_GREATER)
		{
			TRY_CONSUME_OR_RET(TOKEN_COMMA, "Expected ',' after argument.", NULL);
		}
	}
	return types;
}

/**
 * define_type_body ::= TYPE_IDENT '=' 'distinct'? (func_typedef | type generic_params?) ';'
 *
 * func_typedef ::= 'func' failable_type parameter_type_list
 */
static inline Decl *parse_define_type(ParseContext *c, Visibility visibility)
{
	advance_and_verify(c, TOKEN_DEFINE);

	const char *alias_name = symstr(c);
	SourceSpan name_loc = c->span;
	DEBUG_LOG("Parse define %s", alias_name);
	advance_and_verify(c, TOKEN_TYPE_IDENT);
	CONSUME_OR_RET(TOKEN_EQ, poisoned_decl);
	bool distinct = false;
	if (tok_is(c, TOKEN_IDENT) && symstr(c) == kw_distinct)
	{
		distinct = true;
		advance(c);
	}

	// 1. Did we have `func`? In that case it's a function pointer.
	if (try_consume(c, TOKEN_FUNC) || try_consume(c, TOKEN_FN))
	{
		Decl *decl = decl_new_with_type(alias_name, name_loc, DECL_TYPEDEF, visibility);
		decl->typedef_decl.is_func = true;
		decl->typedef_decl.is_distinct = distinct;
		ASSIGN_TYPE_OR_RET(TypeInfo *type_info, parse_failable_type(c), poisoned_decl);
		decl->typedef_decl.function_signature.returntype = type_infoid(type_info);
		if (!parse_parameter_list(c, decl->visibility, &(decl->typedef_decl.function_signature), true))
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
		TypeInfo **params = parse_generic_parameters(c);
		if (!params) return poisoned_decl;
		Decl *decl = decl_new(DECL_DEFINE, alias_name, name_loc, visibility);
		decl->define_decl.define_kind = DEFINE_TYPE_GENERIC;
		decl->define_decl.type_info = type_info;
		decl->define_decl.generic_params = params;
		RANGE_EXTEND_PREV(decl);
		CONSUME_EOS_OR_RET(poisoned_decl);
		return decl;
	}

	Decl *decl = decl_new_with_type(alias_name, name_loc, distinct ? DECL_DISTINCT : DECL_TYPEDEF, visibility);
	decl->typedef_decl.type_info = type_info;
	decl->typedef_decl.is_func = false;
	if (distinct)
	{
		TypedefDecl typedef_decl = decl->typedef_decl; // Ensure value semantics.
		decl->distinct_decl.typedef_decl = typedef_decl;
		decl->type->type_kind = TYPE_DISTINCT;
		decl->decl_kind = DECL_DISTINCT;
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
static inline Decl *parse_define_ident(ParseContext *c, Visibility visibility)
{
	// 1. Store the beginning of the "define".
	advance_and_verify(c, TOKEN_DEFINE);

	// 2. At this point we expect an ident or a const token.
	//    since the Type is handled.
	TokenType alias_type = c->tok;
	if (alias_type != TOKEN_IDENT && alias_type != TOKEN_CONST_IDENT)
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
	Decl *decl = decl_new(DECL_DEFINE, symstr(c), c->span, visibility);
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
		bool error;
		path = parse_path_prefix(c, &error);
		if (error) return poisoned_decl;
	}

	decl->define_decl.path = path;

	// 6. Check that the token after the path is of the same type.
	if (c->tok != alias_type)
	{
		if (token_is_any_type(c->tok) || tok_is(c, TOKEN_TYPE_IDENT))
		{
			SEMA_ERROR(decl, "A type alias must start with an upper case letter and contain at least one lower case letter.");
			return poisoned_decl;
		}
		if (alias_type == TOKEN_CONST_IDENT)
		{
			SEMA_ERROR_HERE("Expected a constant name here.");
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
		TypeInfo **params = parse_generic_parameters(c);
		if (!params) return poisoned_decl;
		decl->define_decl.generic_params = params;
	}
	RANGE_EXTEND_PREV(decl);
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}

/**
 * define_attribute ::= 'define' '@' IDENT '(' parameter_list ')' ('=' attribute_list)?
 */
static inline Decl *parse_define_attribute(ParseContext *c, Visibility visibility)
{
	// 1. Store the beginning of the "define".
	advance_and_verify(c, TOKEN_DEFINE);

	advance_and_verify(c, TOKEN_AT);

	TokenType alias_type = c->tok;
	if (alias_type != TOKEN_TYPE_IDENT)
	{
		if (token_is_some_ident(alias_type) || token_is_keyword(alias_type))
		{
			SEMA_ERROR_HERE("A user defined attribute must start with an uppercase character, followed by at least one lower case.");
			return false;
		}
		SEMA_ERROR_HERE("The attribute name was expected here.");
		return false;
	}
	Decl *decl = decl_new(DECL_DEFINE, symstr(c), c->span, visibility);
	advance_and_verify(c, TOKEN_TYPE_IDENT);

	Decl **parameters = NULL;
	if (try_consume(c, TOKEN_LPAREN))
	{
		if (!parse_parameters(c, visibility, &parameters)) return false;
		CONSUME_OR_RET(TOKEN_RPAREN, poisoned_decl);
	}

	Attr **attributes = NULL;
	if (try_consume(c, TOKEN_EQ))
	{
		if (!parse_attributes(c, &attributes)) return false;
	}

	decl->define_decl.define_kind = DEFINE_ATTRIBUTE;
	decl->define_decl.attributes.attrs = attributes;
	decl->define_decl.attributes.params = parameters;
	CONSUME_EOS_OR_RET(poisoned_decl);
	return decl;
}

/**
 * define_decl ::= DEFINE define_type_body |
 */
static inline Decl *parse_define(ParseContext *c, Visibility visibility)
{
	switch (peek(c))
	{
		case TOKEN_AT:
			// define @foo = @inline, @noreturn
			return parse_define_attribute(c, visibility);
		case TOKEN_TYPE_IDENT:
			return parse_define_type(c, visibility);
		default:
			return parse_define_ident(c, visibility);
	}
}

static inline bool parse_is_func_name(ParseContext *c)
{
	return tok_is(c, TOKEN_IDENT) && peek(c) != TOKEN_SCOPE;
}

/**
 * func_header ::= type '!'? (type '.')? IDENT
 * macro_header ::= (type '!'?)? (type '.')? IDENT
 */
static inline bool parse_func_macro_header(ParseContext *c, bool rtype_is_optional,
                                           TypeInfoId *rtype_ref, TypeInfoId *method_type_ref,
                                           const char **name_ref, SourceSpan *name_span)
{
	TypeInfo *rtype = NULL;
	TypeInfo *method_type = NULL;

	// 1. If we have a macro and see the name, we're done.
	if (rtype_is_optional && parse_is_func_name(c))
	{
		goto RESULT;
	}

	// 2. Now we must have a type - either that is the return type or the method type.
	ASSIGN_TYPE_OR_RET(rtype, parse_failable_type(c), false);

	// 4. We might have a type here, if so then we read it.
	if (!tok_is(c, TOKEN_DOT) && !parse_is_func_name(c))
	{
		ASSIGN_TYPE_OR_RET(method_type, parse_type(c), false);
	}

	// 5. If we have a dot here, then we need to interpret this as method function.
	if (try_consume(c, TOKEN_DOT))
	{
		// 5a. What if we don't have a method type?
		if (!method_type)
		{
			// 5b. If the rtype is not optional or the return type was a failable, then this is an error.
			if (!rtype_is_optional || rtype->failable)
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
	*name_ref = symstr(c);
	*name_span = c->span;
	TRY_CONSUME_OR_RET(TOKEN_IDENT, "Expected a name here.", false);
	*rtype_ref = rtype ? type_infoid(rtype) : 0;
	*method_type_ref = method_type ? type_infoid(method_type) : 0;
	return true;
}


/**
 * macro ::= macro_header '(' macro_params ')' compound_statement
 */
static inline Decl *parse_macro_declaration(ParseContext *c, Visibility visibility)
{
	DeclKind kind = try_consume(c, TOKEN_MACRO) ? DECL_MACRO : DECL_GENERIC;
	if (kind == DECL_GENERIC) advance_and_verify(c, TOKEN_GENERIC);

	Decl *decl = decl_calloc();
	decl->decl_kind = kind;
	decl->visibility = visibility;
	TypeInfoId *rtype_ref = &decl->macro_decl.rtype;
	TypeInfoId *method_type_ref = &decl->macro_decl.type_parent;
	if (!parse_func_macro_header(c, true, rtype_ref, method_type_ref, &decl->name, &decl->span)) return poisoned_decl;

	const char *block_parameter = NULL;
	if (!parse_macro_arguments(c, visibility, &decl->macro_decl.parameters, &decl->macro_decl.body_param)) return poisoned_decl;

	if (!parse_attributes(c, &decl->attributes)) return poisoned_decl;

	ASSIGN_ASTID_OR_RET(decl->macro_decl.body, parse_stmt(c), poisoned_decl);
	return decl;
}


/**
 * error_declaration
 *		: OPTENUM TYPE_IDENT ';'
 *		| OPTENUM TYPE_IDENT '{' error_data '}'
 *		;
 */
static inline Decl *parse_optenum_declaration(ParseContext *c, Visibility visibility)
{
	advance(c);
	// advance_and_verify(context, TOKEN_ERRTYPE);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_OPTENUM, visibility);

	if (!consume_type_name(c, "optenum")) return poisoned_decl;

	TypeInfo *type = NULL;

	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_decl);

	decl->enums.type_info = type_info_new_base(type_iptr->canonical, decl->span);
	while (!try_consume(c, TOKEN_RBRACE))
	{
		Decl *opt_const = decl_new(DECL_OPTVALUE, symstr(c), c->span, decl->visibility);
		if (!consume_const_name(c, "optional value"))
		{
			return poisoned_decl;
		}
		const char *name = opt_const->name;
		VECEACH(decl->enums.values, i)
		{
			Decl *other_constant = decl->enums.values[i];
			if (other_constant->name == name)
			{
				SEMA_ERROR(opt_const, "This opt constant is declared twice.");
				SEMA_PREV(other_constant, "The previous declaration was here.");
				decl_poison(opt_const);
				break;
			}
		}
		vec_add(decl->enums.values, opt_const);
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
static inline bool parse_enum_spec(ParseContext *c, TypeInfo **type_ref, Decl*** parameters_ref, Visibility parent_visibility)
{

	ASSIGN_TYPE_OR_RET(*type_ref, parse_type(c), false);

	if (!try_consume(c, TOKEN_LPAREN)) return true;
	while (!try_consume(c, TOKEN_RPAREN))
	{
		if (!parse_param_decl(c, parent_visibility, parameters_ref, true)) return false;
		Decl *last_parameter = VECLAST(*parameters_ref);
		assert(last_parameter);
		if (last_parameter->var.vararg)
		{
			SEMA_ERROR_LAST("Vararg parameters are not allowed as enum parameters.");
			return false;
		}
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
 *  | CAPS_IDENT '=' const_expr
 *  | CAPS_IDENT '(' expr_list ')'
 *  | CAPS_IDENT '(' expr_list ')' '=' const_expr
 *  ;
 *
 */
static inline Decl *parse_enum_declaration(ParseContext *c, Visibility visibility)
{
	advance_and_verify(c, TOKEN_ENUM);

	Decl *decl = decl_new_with_type(symstr(c), c->span, DECL_ENUM, visibility);

	if (!consume_type_name(c, "enum")) return poisoned_decl;

	TypeInfo *type = NULL;
	if (try_consume(c, TOKEN_COLON))
	{
		if (!parse_enum_spec(c, &type, &decl->enums.parameters, visibility)) return poisoned_decl;
	}

	CONSUME_OR_RET(TOKEN_LBRACE, poisoned_decl);

	decl->enums.type_info = type ? type : type_info_new_base(type_int, decl->span);
	while (!try_consume(c, TOKEN_RBRACE))
	{
		Decl *enum_const = decl_new(DECL_ENUM_CONSTANT, symstr(c), c->span, decl->visibility);
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
				SEMA_PREV(other_constant, "The previous declaration was here.");
				decl_poison(enum_const);
				break;
			}
		}
		if (try_consume(c, TOKEN_LPAREN))
		{
			Expr **result = NULL;
			if (!parse_arg_list(c, &result, TOKEN_RPAREN, NULL)) return poisoned_decl;
			enum_const->enum_constant.args = result;
			CONSUME_OR_RET(TOKEN_RPAREN, poisoned_decl);
		}
		if (try_consume(c, TOKEN_EQ))
		{
			ASSIGN_EXPR_OR_RET(enum_const->enum_constant.expr, parse_expr(c), poisoned_decl);
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
static inline Decl *parse_func_definition(ParseContext *c, Visibility visibility, AstDocDirective *docs, bool is_interface)
{
	if (tok_is(c, TOKEN_FUNC))
	{
		advance_and_verify(c, TOKEN_FUNC);
	}
	else
	{
		advance_and_verify(c, TOKEN_FN);
	}
	Decl *func = decl_calloc();
	func->decl_kind = DECL_FUNC;
	func->visibility = visibility;
	func->func_decl.docs = docs;
	TypeInfoId *rtype_id_ref = &func->func_decl.function_signature.returntype;
	TypeInfoId *method_type_ref = &func->func_decl.type_parent;
	if (!parse_func_macro_header(c, false, rtype_id_ref, method_type_ref, &func->name, &func->span)) return poisoned_decl;
	if (!parse_parameter_list(c, visibility, &(func->func_decl.function_signature), is_interface)) return poisoned_decl;
	if (!parse_attributes(c, &func->attributes)) return poisoned_decl;

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

	TRY_EXPECT_OR_RET(TOKEN_LBRACE, "Expected the beginning of a block with '{'", poisoned_decl);

	ASSIGN_ASTID_OR_RET(func->func_decl.body, parse_compound_stmt(c), poisoned_decl);

	DEBUG_LOG("Finished parsing function %s", func->name);
	return func;
}


static inline bool check_no_visibility_before(ParseContext *c, Visibility visibility)
{
	switch (visibility)
	{
		case VISIBLE_MODULE:
			SEMA_ERROR_HERE("Unexpected 'static' before '%s'", symstr(c));
			return false;
		case VISIBLE_EXTERN:
			SEMA_ERROR_HERE("Unexpected 'extern' before '%s'.", symstr(c));
			return false;
		default:
			return true;
	}
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

	bool private = try_consume(c, TOKEN_PRIVATE);

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
		unit_add_import(c->unit, path, private);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}

	CONSUME_EOS_OR_RET(false);
	return true;
}


/**
 * imports ::= import*
 */
void parse_imports(ParseContext *c)
{
	while (tok_is(c, TOKEN_IMPORT))
	{
		if (!parse_import(c)) recover_top_level(c);
	}
}



static inline bool parse_doc_contract(ParseContext *c, AstDocDirective **docs_ref, DocDirectiveKind kind)
{
	AstDocDirective directive = { .span = c->span, .kind = kind };
	const char *start = c->lexer.data.lex_start;
	advance(c);
	ASSIGN_EXPR_OR_RET(directive.contract.decl_exprs, parse_expression_list(c, kind == DOC_DIRECTIVE_CHECKED), false);
	const char *end = c->data.lex_start;
	while (end[-1] == ' ') end--;
	scratch_buffer_clear();
	switch (kind)
	{
		case DOC_DIRECTIVE_CHECKED:
			scratch_buffer_append("@require \"");
			break;
		case DOC_DIRECTIVE_ENSURE:
			scratch_buffer_append("@ensure \"");
			break;
		default:
			scratch_buffer_append("@require \"");
			break;
	}
	scratch_buffer_append_len(start, end - start);
	scratch_buffer_append("\" violated");
	if (tok_is(c, TOKEN_STRING))
	{
		scratch_buffer_append(": '");
		scratch_buffer_append(symstr(c));
		scratch_buffer_append("'.");
		directive.contract.comment = scratch_buffer_copy();
		advance(c);
	}
	else
	{
		scratch_buffer_append(".");
		directive.contract.expr_string = scratch_buffer_copy();
	}
	vec_add(*docs_ref, directive);
	return true;
}

static inline bool parse_doc_param(ParseContext *c, AstDocDirective **docs_ref)
{
	AstDocDirective directive = { .span = c->span, .kind = DOC_DIRECTIVE_PARAM };
	advance(c);

	// [&inout] [in] [out]
	bool is_ref = false;
	InOutModifier mod = PARAM_ANY;
	if (try_consume(c, TOKEN_LBRACKET))
	{
		is_ref = try_consume(c, TOKEN_AMP);
		const char *modifier = symstr(c);
		if (!consume_ident(c, "Expected 'in', 'inout' or 'out'")) return false;
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
			SEMA_ERROR_LAST("'in', 'out' or 'inout' were expected.");
			return false;
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

	directive.param.name = symstr(c);
	directive.param.span = c->span;
	directive.param.modifier = mod;
	directive.param.by_ref = is_ref;
	advance(c);
	try_consume(c, TOKEN_STRING);
	vec_add(*docs_ref, directive);
	return true;
}

static inline bool parse_doc_errors(ParseContext *c, AstDocDirective **docs)
{

	DocOptReturn *returns = NULL;
	AstDocDirective directive = { .span = c->span, .kind = DOC_DIRECTIVE_ERRORS };
	advance(c);
	while (1)
	{
		DocOptReturn ret = { .span = c->span };
		ASSIGN_TYPE_OR_RET(ret.type, parse_base_type(c), false);
		if (ret.type->kind != TYPE_INFO_IDENTIFIER)
		{
			SEMA_ERROR(ret.type, "Expected an optenum type.");
			return false;
		}
		if (try_consume(c, TOKEN_DOT))
		{
			ret.ident = c->data.string;
			TRY_CONSUME_OR_RET(TOKEN_CONST_IDENT, "Expected an optenum value.", false);
		}
		ret.span = extend_span_with_token(ret.span, c->prev_span);
		vec_add(returns, ret);
		if (!try_consume(c, TOKEN_COMMA)) break;
	}
	directive.span = extend_span_with_token(directive.span, c->prev_span);
	directive.optreturns = returns;
	vec_add(*docs, directive);
	return true;
}


static bool parse_docs(ParseContext *c, AstDocDirective **docs_ref)
{
	*docs_ref = NULL;
	if (!try_consume(c, TOKEN_DOCS_START)) return true;

	uint32_t row_last_row = c->span.row;
	while (1)
	{
		uint32_t row = c->span.row;
		// Spin past the lines and line ends
		switch (c->tok)
		{
			case TOKEN_DOCS_PARAM:
				if (!parse_doc_param(c, docs_ref)) return false;
				break;
			case TOKEN_DOCS_RETURN:
				advance(c);
				if (!consume(c, TOKEN_STRING, "Expected a string description.")) return false;
				break;
			case TOKEN_DOCS_REQUIRE:
				if (!parse_doc_contract(c, docs_ref, DOC_DIRECTIVE_REQUIRE)) return false;
				break;
			case TOKEN_DOCS_CHECKED:
				if (!parse_doc_contract(c, docs_ref, DOC_DIRECTIVE_CHECKED)) return false;
				break;
			case TOKEN_DOCS_ENSURE:
				if (!parse_doc_contract(c, docs_ref, DOC_DIRECTIVE_ENSURE)) return false;
				break;
			case TOKEN_DOCS_OPTRETURN:
				if (!parse_doc_errors(c, docs_ref)) return false;
				break;
			case TOKEN_DOCS_PURE:
			{
				AstDocDirective directive = { .span = c->span, .kind = DOC_DIRECTIVE_PURE };
				vec_add(*docs_ref, directive);
				advance(c);
				break;
			}
			case TOKEN_DOC_DIRECTIVE:
				advance(c);
				// Ignore
				break;
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
Decl *parse_top_level_statement(ParseContext *c)
{
	AstDocDirective *docs = NULL;
	if (!parse_docs(c, &docs)) return poisoned_decl;
	Visibility visibility = VISIBLE_PUBLIC;
	switch (c->tok)
	{
		case TOKEN_PRIVATE:
			visibility = VISIBLE_MODULE;
			advance(c);
			break;
		case TOKEN_EXTERN:
			visibility = VISIBLE_EXTERN;
			advance(c);
		default:
			break;
	}

	Decl *decl;
	TokenType tok = c->tok;
	switch (tok)
	{
		case TOKEN_DOCS_START:
			if (visibility != VISIBLE_PUBLIC)
			{
				SEMA_ERROR_HERE("Did not expect doc comments after visibility.");
				return poisoned_decl;
			}
			SEMA_ERROR_HERE("There are more than one doc comment in a row, that is not allowed.");
			return poisoned_decl;
		case TOKEN_DEFINE:
		{
			ASSIGN_DECL_OR_RET(decl, parse_define(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_FUNC:
		case TOKEN_FN:
		{
			ASSIGN_DECL_OR_RET(decl, parse_func_definition(c, visibility, docs, false), poisoned_decl);
			break;
		}
		case TOKEN_CT_ASSERT:
			if (!check_no_visibility_before(c, visibility)) return poisoned_decl;
			{
				ASSIGN_AST_OR_RET(Ast *ast, parse_ct_assert_stmt(c), poisoned_decl);
				decl = decl_new_ct(DECL_CT_ASSERT, ast->span);
				decl->ct_assert_decl = ast;
				if (docs)
				{
					SEMA_ERROR(docs, "Unexpected doc comment before $assert, did you mean to use a regular comment?");
					return poisoned_decl;
				}
				return decl;
			}
		case TOKEN_CT_IF:
		{
			if (!check_no_visibility_before(c, visibility)) return poisoned_decl;
			ASSIGN_DECL_OR_RET(decl, parse_ct_if_top_level(c), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(docs, "Unexpected doc comment before $if, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		}
		case TOKEN_CT_SWITCH:
		{
			if (!check_no_visibility_before(c, visibility)) return poisoned_decl;
			ASSIGN_DECL_OR_RET(decl, parse_ct_switch_top_level(c), poisoned_decl);
			if (docs)
			{
				SEMA_ERROR(docs, "Unexpected doc comment before $switch, did you mean to use a regular comment?");
				return poisoned_decl;
			}
			break;
		}
		case TOKEN_BITSTRUCT:
		{
			ASSIGN_DECL_OR_RET(decl, parse_bitstruct_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_CONST:
		{
			ASSIGN_DECL_OR_RET(decl, parse_top_level_const_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_STRUCT:
		case TOKEN_UNION:
		{
			ASSIGN_DECL_OR_RET(decl, parse_struct_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_GENERIC:
		case TOKEN_MACRO:
		{
			ASSIGN_DECL_OR_RET(decl, parse_macro_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_ENUM:
		{
			ASSIGN_DECL_OR_RET(decl, parse_enum_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_OPTENUM:
		case TOKEN_OPTNUM:
		case TOKEN_ERRNUM:
		{
			ASSIGN_DECL_OR_RET(decl, parse_optenum_declaration(c, visibility), poisoned_decl);
			break;
		}
		case TOKEN_IDENT:
			return parse_global_declaration(c, visibility);
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
		case TOKEN_IMPORT:
			SEMA_ERROR_HERE("Imports are only allowed directly after the module declaration.");
			return poisoned_decl;
		case TOKEN_TLOCAL:
		case TYPELIKE_TOKENS:
		{
			ASSIGN_DECL_OR_RET(decl, parse_global_declaration(c, visibility), poisoned_decl);
			break;
		}
		default:
			SEMA_ERROR_HERE("Expected a top level declaration here.");
			return poisoned_decl;
			break;
	}
	assert(decl);
	return decl;
}

