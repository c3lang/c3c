// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "parser_internal.h"

static Decl *parse_top_level(Context *context);


#pragma mark --- Context storage

static void context_store_lexer_state(Context *context)
{
	assert(!context->stored.in_lookahead && "Nested lexer store is forbidden");
	context->stored.in_lookahead = true;
	context->stored.current = context->lexer.current;
	context->stored.start = context->lexer.lexing_start;
	context->stored.tok = context->tok;
	context->stored.next_tok = context->next_tok;
	context->stored.lead_comment = context->lead_comment;
	context->stored.trailing_comment = context->trailing_comment;
	context->stored.next_lead_comment = context->next_lead_comment;
	context->stored.lexer_index = context->lexer.lexer_index;
}

static void context_restore_lexer_state(Context *context)
{
	assert(context->stored.in_lookahead && "Tried to restore missing stored state.");
	context->stored.in_lookahead = false;
	context->lexer.current = context->stored.current;
	context->lexer.lexing_start = context->stored.start;
	context->tok = context->stored.tok;
	context->next_tok = context->stored.next_tok;
	context->lead_comment = context->stored.lead_comment;
	context->next_lead_comment = context->stored.next_lead_comment;
	context->trailing_comment = context->stored.trailing_comment;
	context->lexer.lexer_index = context->stored.lexer_index;
}

#pragma mark --- Parser base methods

/**
 * Advance to the next non-comment token.
 *
 * @param context the current context.
 */
inline void advance(Context *context)
{
	context->lead_comment = context->next_lead_comment;
	context->trailing_comment = NULL;
	context->next_lead_comment = NULL;
	context->tok = context->next_tok;
	while(1)
	{
		if (context->tok.type == TOKEN_EOF)
		{
			context->next_tok = context->tok;
		}
		else
		{
			context->next_tok = lexer_advance(&context->lexer);
		}

		if (context->next_tok.type == TOKEN_INVALID_TOKEN) continue;

		// Skip comments during lookahead
		if (context->stored.in_lookahead && (context->next_tok.type == TOKEN_COMMENT
			|| context->next_tok.type == TOKEN_DOC_COMMENT))
		{
			continue;
		}

		// Walk through any regular comments
		if (context->next_tok.type == TOKEN_COMMENT)
		{
			vec_add(context->comments, context->next_tok);
			continue;
		}

		// Handle doc comments
		if (context->next_tok.type == TOKEN_DOC_COMMENT)
		{
			SourceLocation *curr = TOKKLOC(context->tok);
			SourceLocation *next = TOKKLOC(context->next_tok);
			vec_add(context->comments, context->next_tok);

			if (curr->line == next->line)
			{
				if (context->trailing_comment)
				{
					SEMA_TOKEN_ERROR(context->next_tok, "You have multiple trailing doc-style comments, should the second one go on the next line?");
				}
				else
				{
					context->trailing_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			else
			{
				if (context->lead_comment)
				{
					SEMA_TOKEN_ERROR(context->next_tok, "You have multiple doc-style comments in a row, are all of them really meant to document the code that follows?");
				}
				else
				{
					context->lead_comment = context->comments + vec_size(context->comments) - 1;
				}
			}
			continue;
		}
		break;
	}

}

bool try_consume(Context *context, TokenType type)
{
	if (context->tok.type == type)
	{
		advance(context);
		return true;
	}
	return false;
}

bool consume(Context *context, TokenType type, const char *message, ...)
{
	if (try_consume(context, type))
	{
		return true;
	}

	va_list args;
	va_start(args, message);
	sema_verror_range(TOKKLOC(context->tok), message, args);
	va_end(args);
	return false;
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
	if (!consume(context, TOKEN_CONST_IDENT, "'%s' should be followed by the name of the %s.", type, type)) return false;
	return true;
}
/**
 * Walk until we find the first top level construct.
 * (Note that this is the slow path, so no need to inline)
 */
static void recover_top_level(Context *context)
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
				return;
			default:
				advance(context);
				break;
		}
	}
}


#pragma mark --- Parse paths

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
TypeInfo *parse_type(Context *context)
{
	TypeInfo *type_info = parse_base_type(context);
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

	EXPECT_IDENT_FOR_OR("variable_name", poisoned_decl);

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
	bool local = TOKEN_IS(TOKEN_LOCAL);
	bool constant = TOKEN_IS(TOKEN_CONST);
	if (local || constant) advance(context);

	TypeInfo *type_info = parse_type(context);
	TypeInfo *type = TRY_TYPE_OR(type_info, poisoned_decl);

	bool failable = try_consume(context, TOKEN_BANG);
	Decl *decl = TRY_DECL_OR(parse_decl_after_type(context, local, type), poisoned_decl);
	if (failable && decl->var.unwrap)
	{
		SEMA_ERROR(decl, "You cannot use unwrap with a failable variable.");
		return poisoned_decl;
	}
	decl->var.failable = failable;
	if (constant) decl->var.kind = VARDECL_CONST;

	return decl;
}


/**
 * const_decl
 *  : 'const' CT_IDENT '=' const_expr ';'
 *  | 'const' type IDENT '=' const_expr ';'
 *  ;
 */
static inline Decl *parse_const_declaration(Context *context, Visibility visibility)
{
	advance_and_verify(context, TOKEN_CONST);

	Decl *decl = DECL_NEW_VAR(NULL, VARDECL_CONST, visibility);
	// Parse the compile time constant.
	if (TOKEN_IS(TOKEN_CT_IDENT))
	{
		if (!is_all_upper(decl->name))
		{
			SEMA_TOKEN_ERROR(context->tok, "Compile time constants must be all upper characters.");
			return poisoned_decl;
		}
	}
	else
	{
		if (token_is_any_type(context->tok.type))
		{
			decl->var.type_info = TRY_TYPE_OR(parse_type(context), poisoned_decl);
		}
		if (!consume_const_name(context, "constant")) return poisoned_decl;
	}

	CONSUME_OR(TOKEN_EQ, poisoned_decl);

	decl->var.init_expr = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);

	CONSUME_OR(TOKEN_EOS, poisoned_decl);
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
		return false;
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
	CONSUME_OR(TOKEN_PLUS_ASSIGN, poisoned_decl);
	Decl *decl = decl_new(DECL_ARRAY_VALUE, name.id, VISIBLE_LOCAL);
	decl->incr_array_decl = TRY_EXPR_OR(parse_initializer(context), poisoned_decl);
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
			return (next_tok == TOKEN_BANG) | (next_tok == TOKEN_STAR) | (next_tok == TOKEN_LBRACKET) | (next_tok == TOKEN_IDENT);
		case TOKEN_IDENT:
			if (next_tok != TOKEN_SCOPE) return false;
			// We need a little lookahead to see if this is type or expression.
			context_store_lexer_state(context);
			do
			{
				advance(context); advance(context);
			} while (TOKEN_IS(TOKEN_IDENT) && context->next_tok.type == TOKEN_SCOPE);
			bool is_type = TOKEN_IS(TOKEN_TYPE_IDENT);
			context_restore_lexer_state(context);
			return is_type;
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
			// We need a little lookahead to see if this is type or expression.
			context_store_lexer_state(context);
			do
			{
				advance(context); advance(context);
			} while (TOKEN_IS(TOKEN_IDENT) && context->next_tok.type == TOKEN_SCOPE);
			bool is_type = TOKEN_IS(TOKEN_TYPE_IDENT);
			context_restore_lexer_state(context);
			return is_type;
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
			attr->expr = TRY_EXPR_OR(parse_paren_expr(context), false);
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

void add_struct_member(Decl *parent, Decl *parent_struct, Decl *member, TypeInfo *type)
{
	unsigned index = vec_size(parent_struct->strukt.members);
	vec_add(parent_struct->strukt.members, member);
	member->member_decl.index = index;
	member->member_decl.reference_type = type_new(TYPE_MEMBER, member->name);
	member->member_decl.reference_type->canonical = member->member_decl.reference_type;
	member->member_decl.reference_type->decl = member;
	member->member_decl.type_info = type;
	member->member_decl.parent = parent;
}

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
 * @param parent the parent if this is the body of member
 * @param struct_parent the struct this is the body of
 * @param visible_parent the visible struct parent for checking duplicates.
 */
bool parse_struct_body(Context *context, Decl *parent, Decl *parent_struct, Decl *visible_parent)
{

	CONSUME_OR(TOKEN_LBRACE, false);

	assert(decl_is_struct_type(parent_struct));
	while (!TOKEN_IS(TOKEN_RBRACE))
	{
		TokenType token_type = context->tok.type;
		if (token_type == TOKEN_STRUCT || token_type == TOKEN_UNION)
		{
			DeclKind decl_kind = decl_from_token(token_type);
			Decl *member;
			const char *name = TOKSTR(context->tok);
			Decl *strukt_type = decl_new_with_type(context->tok.id, decl_kind, visible_parent->visibility);
			if (context->next_tok.type != TOKEN_IDENT)
			{
				member = decl_new(DECL_MEMBER, NO_TOKEN_ID, visible_parent->visibility);
				advance(context);
			}
			else
            {
			    advance(context);
	            member = decl_new(DECL_MEMBER, context->tok.id, visible_parent->visibility);
				Decl *other = struct_find_name(visible_parent, TOKSTR(context->tok));
				if (other)
				{
					SEMA_TOKEN_ERROR(context->tok, "Duplicate member '%s' found.", TOKSTR(context->tok.id));
					SEMA_PREV(other, "Previous declaration with the same name was here.");
					decl_poison(visible_parent);
					decl_poison(other);
					decl_poison(member);
					return false;
				}
				advance_and_verify(context, TOKEN_IDENT);
			}
			if (!parse_attributes(context, strukt_type)) return false;
			if (!parse_struct_body(context, member, strukt_type, TOKEN_IS(TOKEN_IDENT) ? strukt_type : visible_parent))
			{
				decl_poison(visible_parent);
				return false;
			}
			VECADD(context->types, strukt_type);
			add_struct_member(parent, parent_struct, member, type_info_new_base(strukt_type->type, strukt_type->span));
			continue;
		}
		TypeInfo *type = TRY_TYPE_OR(parse_type(context), false);
		while (1)
        {
            EXPECT_OR(TOKEN_IDENT, false);
            Decl *member = decl_new(DECL_MEMBER, context->tok.id, visible_parent->visibility);
            Decl *other = struct_find_name(visible_parent, member->name);
            if (other)
            {
                SEMA_ERROR(member, "Duplicate member '%s' found.", member->name);
                SEMA_PREV(other, "Previous declaration with the same name was here.");
                decl_poison(visible_parent);
                decl_poison(other);
                decl_poison(member);
            }
            add_struct_member(parent, parent_struct, member, type);
            advance(context);
            if (!TOKEN_IS(TOKEN_COMMA)) break;
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

	if (!parse_struct_body(context, decl, decl, decl))
	{
		return poisoned_decl;
	}
	DEBUG_LOG("Parsed %s %s completely.", type_name, TOKSTR(name));
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
	advance_and_verify(context, TOKEN_DEFINE);
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
		return false;
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
        TEST_TYPE:
        switch (context->tok.type)
        {
            case TOKEN_IDENT:
            case TOKEN_CT_IDENT:
                break;
            default:
                if (parm_type)
                {
                    SEMA_TOKEN_ERROR(context->tok, "Expected a macro parameter");
                    return poisoned_decl;
                }
                parm_type = TRY_TYPE_OR(parse_type(context), poisoned_decl);
                goto TEST_TYPE;
        }
        Decl *param = decl_new_var(context->tok.id, parm_type, VARDECL_PARAM, visibility);
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
		    Decl *member = decl_new(DECL_MEMBER, context->tok.id, visibility);
    		advance(context);
    		add_struct_member(err_decl, err_decl, member, type);
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
	*type_ref = TRY_TYPE_OR(parse_base_type(context), false);
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


#pragma mark --- Parse CT conditional code

static inline bool parse_conditional_top_level(Context *context, Decl ***decls)
{
	CONSUME_OR(TOKEN_LBRACE, false);
	while (!TOKEN_IS(TOKEN_RBRACE) && !TOKEN_IS(TOKEN_EOF))
	{
		Decl *decl = parse_top_level(context);
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

static inline Decl *parse_ct_if_top_level(Context *context)
{
	Decl *ct = DECL_NEW(DECL_CT_IF, VISIBLE_LOCAL);
	advance_and_verify(context, TOKEN_CT_IF);
	ct->ct_if_decl.expr = TRY_EXPR_OR(parse_paren_expr(context), poisoned_decl);

	if (!parse_conditional_top_level(context, &ct->ct_if_decl.then)) return poisoned_decl;

	CtIfDecl *ct_if_decl = &ct->ct_if_decl;
	while (TOKEN_IS(TOKEN_CT_ELIF))
	{
		advance_and_verify(context, TOKEN_CT_ELIF);
		Decl *ct_elif = DECL_NEW(DECL_CT_ELIF, VISIBLE_LOCAL);
		ct_elif->ct_elif_decl.expr = TRY_EXPR_OR(parse_paren_expr(context), poisoned_decl);
		if (!parse_conditional_top_level(context, &ct_elif->ct_elif_decl.then)) return poisoned_decl;
		ct_if_decl->elif = ct_elif;
		ct_if_decl = &ct_elif->ct_elif_decl;
	}
	if (TOKEN_IS(TOKEN_CT_ELSE))
	{
		advance_and_verify(context, TOKEN_CT_ELSE);
		Decl *ct_else = DECL_NEW(DECL_CT_ELSE, VISIBLE_LOCAL);
		ct_if_decl->elif = ct_else;
		if (!parse_conditional_top_level(context, &ct_else->ct_else_decl)) return poisoned_decl;
	}
	return ct;
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
 * @return true if parsing worked
 */
static inline Decl *parse_top_level(Context *context)
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
		case TOKEN_CT_IF:
			if (!check_no_visibility_before(context, visibility)) return false;
			return parse_ct_if_top_level(context);
		case TOKEN_CONST:
			return parse_const_declaration(context, visibility);
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
			if (!check_no_visibility_before(context, visibility)) return false;
			return parse_incremental_array(context);
		case TOKEN_EOF:
			assert(visibility != VISIBLE_MODULE);
			TODO
			//sema_error_at(context->token->span.loc - 1, "Expected a top level declaration'.");
			return poisoned_decl;
		default:
			// We could have included all fundamental types above, but do it here instead.
			if (token_is_type(context->tok.type))
			{
				return parse_global_declaration(context, visibility);
			}
			SEMA_TOKEN_ERROR(context->tok, "Unexpected symbol found");
			return poisoned_decl;
	}
}

#pragma mark --- Parse import and module


/**
 *
 * module_param
 * 		: CT_IDENT
 *		| HASH_IDENT
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
				SEMA_TOKEN_ERROR(context->tok, "The module parameter must be a $ or #-prefixed name, did you forgot the '$'?");
				return false;
			case TOKEN_COMMA:
				SEMA_TOKEN_ERROR(context->tok, "Unexpected ','");
				return false;
			case TOKEN_CT_IDENT:
			case TOKEN_TYPE_IDENT:
				break;
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
 * module
 * 		: MODULE path ';'
 * 		| MODULE path '(' module_params ')' ';'
 */
static inline void parse_module(Context *context)
{

	if (!try_consume(context, TOKEN_MODULE))
	{
		context_set_module_from_filename(context);
		return;
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
		return;
	}

	// Is this a generic module?
	TokenId *generic_parameters = NULL;
	if (!parse_optional_module_params(context, &generic_parameters))
	{
		context_set_module(context, path, generic_parameters);
		recover_top_level(context);
		return;
	}
	context_set_module(context, path, generic_parameters);
	TRY_CONSUME_EOS_OR();
}

/**
 * import_selective
 * 		: import_spec
 * 		| import_spec AS import_spec
 * 		;
 *
 * import_spec
 * 		: IDENT
 * 		| TYPE
 * 		| MACRO
 * 		| CONST_IDENT
 * 		;
 *
 * @return true if import succeeded
 */
static inline bool parse_import_selective(Context *context, Path *path)
{
	// TODO constident, @ etc
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

/**
 *
 * import
 * 		: IMPORT PATH ';'
 * 		| IMPORT PATH ':' import_selective ';'
 * 		| IMPORT IDENT AS IDENT LOCAL ';'
 * 		| IMPORT IDENT LOCAL ';'
 *
 * import_list
 * 		: import_selective
 * 		| import_list ',' import_selective
 * 		;
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
			if (!parse_import_selective(context, path)) return false;
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
 * imports
 * 		: import_decl
 *      | imports import_decl
 *      ;
 */
static inline void parse_imports(Context *context)
{

	while (TOKEN_IS(TOKEN_IMPORT))
	{
		if (!parse_import(context)) recover_top_level(context);
	}
}



#pragma mark --- Extern functions

static inline void parse_current(Context *context)
{
	// Prime everything
	advance(context); advance(context);
	parse_module(context);
	parse_imports(context);
	while (!TOKEN_IS(TOKEN_EOF))
	{
		Decl *decl = parse_top_level(context);
		if (decl_ok(decl))
		{
			context_register_global_decl(context, decl);
		}
		else
		{
			recover_top_level(context);
		}
	}
}

void parse_file(Context *context)
{
	lexer_init_with_file(&context->lexer, context->file);
	parse_current(context);
}






