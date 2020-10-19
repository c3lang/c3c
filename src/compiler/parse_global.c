#include "compiler_internal.h"
#include "parser_internal.h"

static Decl *parse_const_declaration(Context *context, Visibility visibility);

static bool context_next_up_is_type_with_path(Context *context)
{
	TokenId current = context->next_tok.id;
	TokenType tok = context->next_tok.type;
	while (1)
	{
		while (1)
		{
			current.index += 1;
			tok = TOKITYPE(current);
			if (tok == TOKEN_COMMENT || tok == TOKEN_DOC_COMMENT) continue;
			if (tok != TOKEN_IDENT) goto DONE_CHECK;
			break;
		}
		while (1)
		{
			current.index += 1;
			tok = TOKITYPE(current);
			if (tok == TOKEN_COMMENT || tok == TOKEN_DOC_COMMENT) continue;
			if (tok != TOKEN_SCOPE) goto DONE_CHECK;
			break;
		}
	}
	DONE_CHECK:
	return tok == TOKEN_TYPE_IDENT;
}


/**
 * Walk until we find the first top level construct.
 * (Note that this is the slow path, so no need to inline)
 */
void recover_top_level(Context *context)
{
	advance(context);
	while (!TOKEN_IS(TOKEN_EOF))
	{
		switch (context->tok.type)
		{
			case TOKEN_PUBLIC:
			case TOKEN_FUNC:
			case TOKEN_CONST:
			case TOKEN_TYPEDEF:
			case TOKEN_STRUCT:
			case TOKEN_IMPORT:
			case TOKEN_UNION:
			case TOKEN_MACRO:
			case TOKEN_EXTERN:
			case TOKEN_ENUM:
			case TOKEN_ERR:
				return;
			default:
				advance(context);
				break;
		}
	}
}

#pragma mark --- Parse CT conditional code

static inline bool parse_top_level_block(Context *context, Decl ***decls)
{
	CONSUME_OR(TOKEN_LBRACE, false);
	while (!TOKEN_IS(TOKEN_RBRACE) && !TOKEN_IS(TOKEN_EOF))
	{
		Decl *decl = parse_top_level_statement(context);
		if (decl == NULL) continue;
		if (decl_ok(decl))
		{
			vec_add(*decls, decl);
		}
		else
		{
			recover_top_level(context);
		}
	}
	CONSUME_OR(TOKEN_RBRACE, false);
	return true;
}

/**
 * ct_if_top_level ::= CT_IF const_paren_expr top_level_block
    	(CT_ELIF const_paren_expr top_level_block)*
    	(CT_ELSE top_level_block)?
 * @param context
 * @return the declaration if successfully parsed, poisoned_decl otherwise.
 */
static inline Decl *parse_ct_if_top_level(Context *context)
{
	Decl *ct = DECL_NEW(DECL_CT_IF, VISIBLE_LOCAL);
	advance_and_verify(context, TOKEN_CT_IF);
	ct->ct_if_decl.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_decl);

	if (!parse_top_level_block(context, &ct->ct_if_decl.then)) return poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (TOKEN_IS(TOKEN_CT_ELIF))
	{
		advance_and_verify(context, TOKEN_CT_ELIF);
		Decl *ct_elif = DECL_NEW(DECL_CT_ELIF, VISIBLE_LOCAL);
		ct_elif->ct_elif_decl.expr = TRY_EXPR_OR(parse_const_paren_expr(context), poisoned_decl);
		if (!parse_top_level_block(context, &ct_elif->ct_elif_decl.then)) return poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (TOKEN_IS(TOKEN_CT_ELSE))
	{
		advance_and_verify(context, TOKEN_CT_ELSE);
		Decl *ct_else = DECL_NEW(DECL_CT_ELSE, VISIBLE_LOCAL);
		ct_if_decl->elif = ct_else;
		if (!parse_top_level_block(context, &ct_else->ct_else_decl)) return poisoned_decl;
	}
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
	char *scratch_ptr = context->path_scratch;
	size_t offset = 0;
	SourceSpan span = source_span_from_token_id(context->tok.id);
	unsigned len = TOKLEN(context->tok);
	memcpy(scratch_ptr, TOKSTR(context->tok), len);
	offset += len;
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
		scratch_ptr[offset++] = ':';
		scratch_ptr[offset++] = ':';
		len = TOKLEN(context->tok);
		memcpy(scratch_ptr + offset, TOKSTR(context->tok), len);
		offset += len;
	}
	scratch_ptr[offset] = '\0';
	return path_create_from_string(context, scratch_ptr, offset, span);
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

	if (!try_consume(context, TOKEN_LPAREN)) return true;

	if (try_consume(context, TOKEN_RPAREN))
	{
		SEMA_TOKEN_ERROR(context->tok, "Generic parameter list cannot be empty.");
		return false;
	}

	// No params
	while (1)
	{
		switch (context->tok.type)
		{
			case TOKEN_IDENT:
			case TOKEN_TYPE_IDENT:
				return false;
			case TOKEN_COMMA:
				SEMA_TOKEN_ERROR(context->tok, "Unexpected ','");
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
			return consume(context, TOKEN_RPAREN, "Expected ')'.");
		}
	}
}
/**
 * module ::= MODULE module_path ('(' module_params ')')? EOS
 */
bool parse_module(Context *context)
{
	if (!try_consume(context, TOKEN_MODULE))
	{
		return context_set_module_from_filename(context);
	}

	if (!TOKEN_IS(TOKEN_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Module statement should be followed by the name of the module to import.");
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
		context_set_module(context, path, NULL);
		recover_top_level(context);
		return false;
	}

	// Is this a generic module?
	TokenId *generic_parameters = NULL;
	if (!parse_optional_module_params(context, &generic_parameters))
	{
		context_set_module(context, path, generic_parameters);
		recover_top_level(context);
		return true;
	}
	context_set_module(context, path, generic_parameters);
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
		return context_add_import(context, path, symbol, NO_TOKEN);
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
	return context_add_import(context, path, symbol, alias);
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

	char *scratch_ptr = context->path_scratch;
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
 *		;
 *
 * Assume prev_token is the type.
 * @return TypeInfo (poisoned if fails)
 */
static inline TypeInfo *parse_base_type(Context *context)
{
	SourceSpan range = source_span_from_token_id(context->tok.id);
	bool had_error;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_type_info;
	if (path)
	{
		TypeInfo *type_info = type_info_new(TYPE_INFO_IDENTIFIER, range);
		type_info->unresolved.path = path;
		type_info->unresolved.name_loc = context->tok.id;
		if (!consume_type_name(context, "types")) return poisoned_type_info;
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
		case TOKEN_ERR:
			type_found = type_error;
			break;
		case TOKEN_VOID:
			type_found = type_void;
			break;
		case TOKEN_BOOL:
			type_found = type_bool;
			break;
		case TOKEN_BYTE:
			type_found = type_byte;
			break;
		case TOKEN_CHAR:
			type_found = type_char;
			break;
		case TOKEN_DOUBLE:
			type_found = type_double;
			break;
		case TOKEN_FLOAT:
			type_found = type_float;
			break;
		case TOKEN_INT:
			type_found = type_int;
			break;
		case TOKEN_ISIZE:
			type_found = type_isize;
			break;
		case TOKEN_LONG:
			type_found = type_long;
			break;
		case TOKEN_SHORT:
			type_found = type_short;
			break;
		case TOKEN_UINT:
			type_found = type_uint;
			break;
		case TOKEN_ULONG:
			type_found = type_ulong;
			break;
		case TOKEN_USHORT:
			type_found = type_ushort;
			break;
		case TOKEN_USIZE:
			type_found = type_usize;
			break;
		case TOKEN_C_SHORT:
			type_found = type_c_short;
			break;
		case TOKEN_C_INT:
			type_found = type_c_int;
			break;
		case TOKEN_C_LONG:
			type_found = type_c_long;
			break;
		case TOKEN_C_LONGLONG:
			type_found = type_c_longlong;
			break;
		case TOKEN_C_USHORT:
			type_found = type_c_ushort;
			break;
		case TOKEN_C_UINT:
			type_found = type_c_uint;
			break;
		case TOKEN_C_ULONG:
			type_found = type_c_ulong;
			break;
		case TOKEN_C_ULONGLONG:
			type_found = type_c_ulonglong;
			break;
		case TOKEN_TYPEID:
			type_found = type_typeid;
			break;
		default:
			SEMA_TOKEN_ERROR(context->tok, "A type name was expected here.");
			return poisoned_type_info;
	}
	if (type_found)
	{
		assert(!type_info);
		type_info = type_info_new(TYPE_INFO_IDENTIFIER, source_span_from_token_id(context->tok.id));
		type_info->resolve_status = RESOLVE_DONE;
		type_info->type = type_found;
	}
	advance(context);
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
		TypeInfo *vararray = type_info_new(TYPE_INFO_VARARRAY, type->span);
		vararray->array.base = type;
		vararray->array.len = NULL;
		RANGE_EXTEND_PREV(vararray);
		return vararray;
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
 * @param local
 * @param type
 * @return
 */
Decl *parse_decl_after_type(Context *context, bool local, TypeInfo *type)
{
	if (TOKEN_IS(TOKEN_LPAREN))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected '{'.");
		return poisoned_decl;
	}


	EXPECT_IDENT_FOR_OR("variable name", poisoned_decl);

	TokenId name = context->tok.id;
	advance(context);

	Visibility visibility = local ? VISIBLE_LOCAL : VISIBLE_MODULE;
	Decl *decl = decl_new_var(name, type, VARDECL_LOCAL, visibility);
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
 * declaration ::= ('local' | 'const')? type variable ('=' expr)?
 *
 * @return Decl* (poisoned on error)
 */
Decl *parse_decl(Context *context)
{
	bool local = try_consume(context, TOKEN_LOCAL);

	if (TOKEN_IS(TOKEN_CONST))
	{
		if (local)
		{
			SEMA_TOKID_ERROR(context->prev_tok, "A 'local' variable cannot also be declared 'constant'.");
			return poisoned_decl;
		}
		return parse_const_declaration(context, VISIBLE_LOCAL);
	}

	TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_decl);

	bool failable = try_consume(context, TOKEN_BANG);

	Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, local, type), poisoned_decl);
	if (failable && decl->var.unwrap)
	{
		SEMA_ERROR(decl, "You cannot use unwrap with a failable variable.");
		return poisoned_decl;
	}
	decl->var.failable = failable;

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
	decl->name = TOKKSTR(context->tok);
	decl->name_token = context->tok.id;
	if (!consume_const_name(context, "const")) return poisoned_decl;

	CONSUME_OR(TOKEN_EQ, poisoned_decl);

	decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);

	return decl;
}

/**
 * global_declaration
 * 	: failable_type IDENT ';'
 * 	| failable_type IDENT '=' expression ';'
 * 	;
 *
 * @param visibility
 * @return true if parsing succeeded
 */
static inline Decl *parse_global_declaration(Context *context, Visibility visibility)
{
	TypeInfo *type = TRY_TYPE_OR(parse_type(context), poisoned_decl);

	Decl *decl = decl_new_var(context->tok.id, type, VARDECL_GLOBAL, visibility);

	if (TOKEN_IS(TOKEN_FUNC))
	{
		SEMA_TOKEN_ERROR(context->tok, "'func' can't appear here, maybe you intended to put 'func' the type?");
		advance(context);
		return poisoned_decl;
	}

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
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_CHAR:
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
		case TOKEN_C_SHORT:
		case TOKEN_C_INT:
		case TOKEN_C_LONG:
		case TOKEN_C_LONGLONG:
		case TOKEN_C_USHORT:
		case TOKEN_C_UINT:
		case TOKEN_C_ULONG:
		case TOKEN_C_ULONGLONG:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_ERR:
		case TOKEN_TYPEID:
			return (next_tok == TOKEN_BANG) | (next_tok == TOKEN_STAR) | (next_tok == TOKEN_LBRACKET) | (next_tok == TOKEN_IDENT)
			       | (next_tok == TOKEN_CONST_IDENT);
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			return context_next_up_is_type_with_path(context);
		default:
			return false;
	}
}



bool parse_next_is_case_type(Context *context)
{
	TokenType next_tok = context->next_tok.type;
	switch (context->tok.type)
	{
		case TOKEN_VOID:
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_CHAR:
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
		case TOKEN_C_SHORT:
		case TOKEN_C_INT:
		case TOKEN_C_LONG:
		case TOKEN_C_LONGLONG:
		case TOKEN_C_USHORT:
		case TOKEN_C_UINT:
		case TOKEN_C_ULONG:
		case TOKEN_C_ULONGLONG:
		case TOKEN_TYPE_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_ERR:
		case TOKEN_TYPEID:
			return (next_tok == TOKEN_STAR) | (next_tok == TOKEN_LBRACKET) | (next_tok == TOKEN_COLON) | (next_tok == TOKEN_EOS);
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			return context_next_up_is_type_with_path(context);
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
 * param_declaration
 *  : type_expression
 *  | type_expression IDENT
 *  | type_expression IDENT '=' initializer
 *  ;
 */
static inline bool parse_param_decl(Context *context, Visibility parent_visibility, Decl*** parameters, bool type_only)
{
	TypeInfo *type = TRY_TYPE_OR(parse_type(context), false);
	Decl *param = decl_new_var(context->tok.id, type, VARDECL_PARAM, parent_visibility);
	param->span = type->span;
	if (!try_consume(context, TOKEN_IDENT))
	{
		param->name = NULL;
	}

	const char *name = param->name;

	if (!name && !type_only)
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
		SEMA_ERROR(type, "The function parameter must be named.");
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
 *
 * parameter_type_list
 *  : parameter_list
 *  | parameter_list ',' ELLIPSIS
 *  | parameter_list ',' type_expression ELLIPSIS
 *  ;
 *
 * opt_parameter_type_list
 *  : '(' ')'
 *  | '(' parameter_type_list ')'
 *  ;
 *
 * parameter_list
 *  : param_declaration
 *  | parameter_list ',' param_declaration
 *  ;
 *
 */
static inline bool parse_opt_parameter_type_list(Context *context, Visibility parent_visibility, FunctionSignature *signature, bool is_interface)
{
	CONSUME_OR(TOKEN_LPAREN, false);
	while (!try_consume(context, TOKEN_RPAREN))
	{
		if (signature->variadic)
		{
			SEMA_TOKEN_ERROR(context->tok, "Variadic arguments should be the last in a parameter list.");
			return false;
		}
		if (try_consume(context, TOKEN_ELLIPSIS))
		{
			signature->variadic = true;
		}
		else
		{
			if (!parse_param_decl(context, parent_visibility, &(signature->params), is_interface)) return false;
		}
		if (!try_consume(context, TOKEN_COMMA))
		{
			EXPECT_OR(TOKEN_RPAREN, false);
		}
	}
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
			continue;
		}
		TypeInfo *type = TRY_TYPE_OR(parse_type(context), false);
		while (1)
		{
			EXPECT_OR(TOKEN_IDENT, false);
			Decl *member = decl_new_var(context->tok.id, type, VARDECL_MEMBER, parent->visibility);
			vec_add(parent->strukt.members, member);
			advance(context);
			if (!try_consume(context, TOKEN_COMMA)) break;
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
 * Parse statements up to the next '}', 'case' or 'default'
 */
static inline Ast *parse_generics_statements(Context *context)
{
	Ast *ast = AST_NEW_TOKEN(AST_COMPOUND_STMT, context->tok);
	while (!TOKEN_IS(TOKEN_RBRACE) && !TOKEN_IS(TOKEN_CASE) && !TOKEN_IS(TOKEN_DEFAULT))
	{
		Ast *stmt = TRY_AST_OR(parse_stmt(context), poisoned_ast);
		ast->compound_stmt.stmts = VECADD(ast->compound_stmt.stmts, stmt);
	}
	return ast;
}


/**
 * generics_declaration
 *	: GENERIC opt_path IDENT '(' macro_argument_list ')' '{' generics_body '}'
 *	| GENERIC failable_type opt_path IDENT '(' macro_argument_list ')' '{' generics_body '}'
 *	;
 *
 * opt_path
 *	:
 *	| path
 *	;
 *
 * @param visibility
 * @return
 */
static inline Decl *parse_generics_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_GENERIC);
	TypeInfo *rtype = NULL;
	if (!TOKEN_IS(TOKEN_IDENT))
	{
		rtype = TRY_TYPE_OR(parse_type(context), poisoned_decl);
	}
	bool had_error;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_decl;
	Decl *decl = decl_new(DECL_GENERIC, context->tok.id, visibility);
	decl->generic_decl.path = path;
	if (!consume_ident(context, "generic function name")) return poisoned_decl;
	decl->generic_decl.rtype = rtype;
	TokenId *parameters = NULL;
	CONSUME_OR(TOKEN_LPAREN, poisoned_decl);
	while (!try_consume(context, TOKEN_RPAREN))
	{
		if (!TOKEN_IS(TOKEN_IDENT))
		{
			SEMA_TOKEN_ERROR(context->tok, "Expected an identifier.");
			return false;
		}
		parameters = VECADD(parameters, context->tok.id);
		advance(context);
		COMMA_RPAREN_OR(poisoned_decl);
	}
	CONSUME_OR(TOKEN_LBRACE, poisoned_decl);
	Ast **cases = NULL;
	if (!parse_switch_body(context, &cases, TOKEN_CASE, TOKEN_DEFAULT)) return poisoned_decl;
	decl->generic_decl.cases = cases;
	decl->generic_decl.parameters = parameters;
	return decl;
}

static inline Decl *parse_define(Context *context, Visibility visibility)
{
	if (context->next_tok.type == TOKEN_CT_TYPE_IDENT || context->next_tok.type == TOKEN_CT_IDENT)
	{
		SEMA_TOKEN_ERROR(context->next_tok, "Compile time variables cannot be defined at the global level.");
		return poisoned_decl;
	}
	advance_and_verify(context, TOKEN_DEFINE);
	bool had_error = false;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_decl;
	Decl *decl = decl_new(DECL_DEFINE, context->tok.id, visibility);
	if (!token_is_symbol(context->tok.type))
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected an identifier here.");
		return poisoned_decl;
	}
	advance(context);
	CONSUME_OR(TOKEN_LPAREN, poisoned_decl);
	Expr **exprs = NULL;
	while (!try_consume(context, TOKEN_RPAREN))
	{
		Expr *expr = TRY_EXPR_OR(parse_constant_expr(context), poisoned_decl);
		vec_add(exprs, expr);
		if (context->tok.type != TOKEN_RPAREN)
		{
			TRY_CONSUME_OR(TOKEN_COMMA, "Expected ',' after argument.", poisoned_decl);
		}
	}
	decl->define_decl.path = path;
	decl->define_decl.params = exprs;
	TRY_CONSUME_OR(TOKEN_AS, "Expected 'as' after generic declaration.", poisoned_decl);
	if (!token_is_symbol(context->tok.type))
	{
		SEMA_TOKEN_ERROR(context->tok, "The aliased name must follow after 'as'.");
		return poisoned_decl;
	}
	decl->define_decl.alias = context->tok;
	advance(context);
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}


static AttributeDomain TOKEN_TO_ATTR[TOKEN_EOF + 1]  = {
		[TOKEN_FUNC] = ATTR_FUNC,
		[TOKEN_VAR] = ATTR_VAR,
		[TOKEN_ENUM] = ATTR_ENUM,
		[TOKEN_STRUCT] = ATTR_STRUCT,
		[TOKEN_UNION] = ATTR_UNION,
		[TOKEN_CONST] = ATTR_CONST,
		[TOKEN_TYPEDEF] = ATTR_TYPEDEF,
		[TOKEN_ERR] = ATTR_ERROR,
};

/**
 * attribute_declaration
 * 		: ATTRIBUTE attribute_domains IDENT ';'
 * 		| ATTRIBUTE attribute_domains IDENT '(' parameter_type_list ')' ';'
 * 		;
 *
 * attribute_domains
 * 		: attribute_domain
 * 		| attribute_domains ',' attribute_domain
 * 		;
 *
 * attribute_domain
 * 		: FUNC
 * 		| VAR
 * 		| ENUM
 * 		| STRUCT
 * 		| UNION
 * 		| TYPEDEF
 * 		| CONST
 * 		| ERROR
 * 		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_attribute_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_ATTRIBUTE);
	AttributeDomain domains = 0;
	AttributeDomain last_domain;
	last_domain = TOKEN_TO_ATTR[context->tok.type];
	while (last_domain)
	{
		advance(context);
		if ((domains & last_domain) != 0)
		{
			SEMA_TOKEN_ERROR(context->tok, "'%s' appeared more than once.", TOKSTR(context->tok.id));
			continue;
		}
		domains |= last_domain;
		if (!try_consume(context, TOKEN_COMMA)) break;
		last_domain = TOKEN_TO_ATTR[context->tok.type];
	}
	Decl *decl = decl_new(DECL_ATTRIBUTE, context->tok.id, visibility);
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected an attribute name.", poisoned_decl);
	if (last_domain == 0)
	{
		SEMA_TOKEN_ERROR(context->tok, "Expected at least one domain for attribute '%s'.", decl->name);
		return poisoned_decl;
	}
	if (!parse_opt_parameter_type_list(context, visibility, &decl->attr.attr_signature, false)) return poisoned_decl;
	TRY_CONSUME_EOS_OR(poisoned_decl);
	return decl;
}

/**
 *
 */
/**
 * func_typedef
 *  : FUNC failable_type opt_parameter_type_list
 *  | FUNC failable_type opt_parameter_type_list throw_declaration
 *  ;
 */
static inline bool parse_func_typedef(Context *context, Decl *decl, Visibility visibility)
{
	decl->typedef_decl.is_func = true;
	advance_and_verify(context, TOKEN_FUNC);
	TypeInfo *type_info = TRY_TYPE_OR(parse_type(context), false);
	if (try_consume(context, TOKEN_BANG))
	{
		decl->typedef_decl.function_signature.failable = true;
	}
	decl->typedef_decl.function_signature.rtype = type_info;
	if (!parse_opt_parameter_type_list(context, visibility, &(decl->typedef_decl.function_signature), true))
	{
		return false;
	}
	return true;

}

static inline Decl *parse_typedef_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_TYPEDEF);
	Decl *decl = decl_new_with_type(context->tok.id, DECL_TYPEDEF, visibility);
	if (TOKEN_IS(TOKEN_FUNC))
	{
		if (!parse_func_typedef(context, decl, visibility)) return poisoned_decl;
	}
	else
	{
		decl->typedef_decl.type_info = TRY_TYPE_OR(parse_type(context), poisoned_decl);
		decl->typedef_decl.is_func = false;
	}
	CONSUME_OR(TOKEN_AS, poisoned_decl);
	decl->name = TOKSTR(context->tok);
	decl->type->name = TOKSTR(context->tok);
	decl->name_token = context->tok.id;
	if (!consume_type_name(context, "typedef")) return poisoned_decl;
	CONSUME_OR(TOKEN_EOS, poisoned_decl);
	return decl;
}

/**
 * macro ::= MACRO type? identifier '(' macro_params ')' compound_statement
 */
static inline Decl *parse_macro_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_MACRO);

	TypeInfo *rtype = NULL;
	bool failable = false;
	if (!TOKEN_IS(TOKEN_IDENT))
	{
		rtype = TRY_TYPE_OR(parse_type(context), poisoned_decl);
		failable = try_consume(context, TOKEN_BANG);
	}
	Decl *decl = decl_new(DECL_MACRO, context->tok.id, visibility);
	decl->macro_decl.rtype = rtype;
	decl->macro_decl.failable = failable;
	TRY_CONSUME_OR(TOKEN_IDENT, "Expected a macro name here", poisoned_decl);

	CONSUME_OR(TOKEN_LPAREN, poisoned_decl);
	Decl **params = NULL;
	while (!try_consume(context, TOKEN_RPAREN))
	{
		TypeInfo *parm_type = NULL;
		VarDeclKind param_kind;
		TEST_TYPE:
		switch (context->tok.type)
		{
			// normal foo
			case TOKEN_IDENT:
				param_kind = VARDECL_PARAM;
				break;
			// ct_var $foo
			case TOKEN_CT_IDENT:
				param_kind = VARDECL_PARAM_CT;
				break;
			// reference &foo
			case TOKEN_AMP:
				advance(context);
				if (!TOKEN_IS(TOKEN_IDENT))
				{
					SEMA_TOKEN_ERROR(context->tok, "Only normal variables may be passed by reference.");
					return poisoned_decl;
				}
				param_kind = VARDECL_PARAM_REF;
				break;
			// #Foo (not allowed)
			case TOKEN_HASH_TYPE_IDENT:
				SEMA_TOKEN_ERROR(context->tok, "An unevaluated expression can never be a type, did you mean to use $Type?");
				return poisoned_decl;
			// expression #foo
			case TOKEN_HASH_IDENT:
				// Note that the HASH_TYPE_IDENT will be an error later on.
				param_kind = VARDECL_PARAM_EXPR;
				break;
			// Compile time type $Type
			case TOKEN_CT_TYPE_IDENT:
				param_kind = VARDECL_PARAM_CT_TYPE;
				break;
			case TOKEN_ELLIPSIS:
				// varargs
				TODO
			default:
				if (parm_type)
				{
					SEMA_TOKEN_ERROR(context->tok, "Expected a macro parameter");
					return poisoned_decl;
				}
				parm_type = TRY_TYPE_OR(parse_type(context), poisoned_decl);
				goto TEST_TYPE;
		}
		Decl *param = decl_new_var(context->tok.id, parm_type, param_kind, visibility);
		advance(context);
		params = VECADD(params, param);
		COMMA_RPAREN_OR(poisoned_decl);
	}
	decl->macro_decl.parameters = params;
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
		if (!parse_param_decl(context, parent_visibility, parameters_ref, false)) return false;
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
			if (!parse_param_list(context, &result, true, TOKEN_RPAREN)) return poisoned_decl;
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
 *		| FUNC failable_type func_name '(' opt_parameter_type_list ')' throw_declaration opt_attributes
 *		;
 *
 * @param visibility
 * @return Decl*
 */
static inline Decl *parse_func_definition(Context *context, Visibility visibility, bool is_interface)
{
	advance_and_verify(context, TOKEN_FUNC);

	TypeInfo *return_type = TRY_TYPE_OR(parse_type(context), poisoned_decl);
	Decl *func = DECL_NEW(DECL_FUNC, visibility);
	func->func.function_signature.rtype = return_type;
	func->func.function_signature.failable = try_consume(context, TOKEN_BANG);
	SourceSpan start = source_span_from_token_id(context->tok.id);
	bool had_error;
	Path *path = parse_path_prefix(context, &had_error);
	if (had_error) return poisoned_decl;
	if (path || TOKEN_IS(TOKEN_TYPE_IDENT))
	{
		// Special case, actually an extension
		TRY_EXPECT_OR(TOKEN_TYPE_IDENT, "A type was expected after '::'.", poisoned_decl);
		// TODO span is incorrect
		TypeInfo *type = type_info_new(TYPE_INFO_IDENTIFIER, start);
		type->unresolved.path = path;
		type->unresolved.name_loc = context->tok.id;
		func->func.type_parent = type;
		advance_and_verify(context, TOKEN_TYPE_IDENT);

		TRY_CONSUME_OR(TOKEN_DOT, "Expected '.' after the type in a method declaration.", poisoned_decl);
	}

	EXPECT_IDENT_FOR_OR("function name", poisoned_decl);
	func->name = TOKSTR(context->tok);
	func->name_token = context->tok.id;
	advance_and_verify(context, TOKEN_IDENT);
	RANGE_EXTEND_PREV(func);
	if (!parse_opt_parameter_type_list(context, visibility, &(func->func.function_signature), is_interface)) return poisoned_decl;

	if (!parse_attributes(context, func)) return poisoned_decl;

	// TODO remove
	is_interface = TOKEN_IS(TOKEN_EOS);

	if (is_interface)
	{
		if (TOKEN_IS(TOKEN_LBRACE))
		{
			SEMA_TOKEN_ERROR(context->next_tok, "Functions bodies are not allowed in interface files.");
			return poisoned_decl;
		}
		TRY_CONSUME_OR(TOKEN_EOS, "Expected ';' after function declaration.", poisoned_decl);
		return func;
	}

	TRY_EXPECT_OR(TOKEN_LBRACE, "Expected the beginning of a block with '{'", poisoned_decl);

	func->func.body = TRY_AST_OR(parse_compound_stmt(context), poisoned_decl);

	DEBUG_LOG("Finished parsing function %s", func->name);
	return func;
}


static inline bool check_no_visibility_before(Context *context, Visibility visibility)
{
	switch (visibility)
	{
		case VISIBLE_PUBLIC:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'public' before '%.*s'.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
			return false;
		case VISIBLE_LOCAL:
			SEMA_TOKEN_ERROR(context->tok, "Unexpected 'local' before '%.*s'.", TOKLEN(context->tok.id), TOKSTR(context->tok.id));
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
 * import ::= IMPORT import_path (':' specified_import_list)? ';'
 *
 * specified_import_list ::= specified_import (',' specified_import)*
 *
 * @return true if import succeeded
 */
static inline bool parse_import(Context *context)
{
	advance_and_verify(context, TOKEN_IMPORT);

	if (!TOKEN_IS(TOKEN_IDENT))
	{
		SEMA_TOKEN_ERROR(context->tok, "Import statement should be followed by the name of the module to import.");
		return false;
	}

	Path *path = parse_module_path(context);
	if (TOKEN_IS(TOKEN_COLON))
	{
		while (1)
		{
			if (!parse_specified_import(context, path)) return false;
			if (!try_consume(context, TOKEN_COMMA)) break;
		}
	}
	else
	{
		context_add_import(context, path, NO_TOKEN, NO_TOKEN);
	}
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
	Visibility visibility = VISIBLE_MODULE;
	switch (context->tok.type)
	{
		case TOKEN_PUBLIC:
			visibility = VISIBLE_PUBLIC;
			advance(context);
			break;
		case TOKEN_LOCAL:
			visibility = VISIBLE_LOCAL;
			advance(context);
			break;
		case TOKEN_EXTERN:
			visibility = VISIBLE_EXTERN;
			advance(context);
		default:
			break;
	}

	switch (context->tok.type)
	{
		case TOKEN_DEFINE:
			return parse_define(context, visibility);
		case TOKEN_ATTRIBUTE:
			return parse_attribute_declaration(context, visibility);
		case TOKEN_FUNC:
			return parse_func_definition(context, visibility, false);
		case TOKEN_CT_ASSERT:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			{
				Ast *ast = TRY_AST_OR(parse_ct_assert_stmt(context), false);
				vec_add(context->ct_asserts, ast);
				return poisoned_decl;
			}
		case TOKEN_CT_IF:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			return parse_ct_if_top_level(context);
		case TOKEN_CT_SWITCH:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			return parse_ct_switch_top_level(context);
		case TOKEN_CONST:
			return parse_top_level_const_declaration(context, visibility);
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			return parse_struct_declaration(context, visibility);
		case TOKEN_GENERIC:
			return parse_generics_declaration(context, visibility);
		case TOKEN_MACRO:
			return parse_macro_declaration(context, visibility);
		case TOKEN_ENUM:
			return parse_enum_declaration(context, visibility);
		case TOKEN_ERR:
			return parse_error_declaration(context, visibility);
		case TOKEN_TYPEDEF:
			return parse_typedef_declaration(context, visibility);
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_TYPE_IDENT:
			// All of these start type
			return parse_global_declaration(context, visibility);
		case TOKEN_IDENT:
			if (!check_no_visibility_before(context, visibility)) return poisoned_decl;
			return parse_incremental_array(context);
		case TOKEN_EOF:
			assert(visibility != VISIBLE_MODULE);
			TODO
			//sema_error_at(context->token->span.loc - 1, "Expected a top level declaration'.");
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
		default:
			// We could have included all fundamental types above, but do it here instead.
			if (token_is_type(context->tok.type))
			{
				return parse_global_declaration(context, visibility);
			}
			SEMA_TOKEN_ERROR(context->tok, "Expected a top level declaration here.");
			return poisoned_decl;
	}
}