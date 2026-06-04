#include "c_codegen_internal.h"

#include <inttypes.h>

typedef struct OptionalCatch_
{
	const char* fault;
	int block;
} OptionalCatch;


#define PUSH_CATCH() OptionalCatch _old_catch = c->catch
#define PUSH_CATCH_VAR_BLOCK(fault_, block_) OptionalCatch _old_catch = c->catch; c->catch = (OptionalCatch) { fault_, block_ }
#define PUSH_CLEAR_CATCH() OptionalCatch _old_catch = c->catch; c->catch = (OptionalCatch) { NULL, 0 }
#define POP_CATCH() c->catch = _old_catch

typedef int VariableId;

typedef enum
{
	CV_VALUE,
	CV_ADDRESS,
	CV_OPTIONAL_ADDRESS,
} CValueType;

typedef struct CValue
{
	Type *type;
	VariableId optional;
	VariableId var;
	CValueType kind;
} CValue;

typedef struct
{
	HTable gen_decl;
	HTable gen_def;
	FILE *file;
	OptionalCatch catch;
	int x;
	int typename;
	int id_gen;
} GenContext;

#define PRINTF(x, ...) fprintf(c->file, x, ## __VA_ARGS__) /* NOLINT */
#define PRINT(x, ...) fputs(x, c->file) /* NOLINT */

static void c_emit_stmt(GenContext *c, Ast *stmt);

static const char *c_type_name(GenContext *c, Type *type)
{
	type = type_lowering(type);
	switch (type->type_kind)
	{
		case TYPE_VOID:
			return "void";
		case TYPE_BOOL:
			return "bool";
		case TYPE_POINTER:
			return "void*";
		case TYPE_I8:
			return "int8_t";
		case TYPE_I16:
			return "int16_t";
		case TYPE_I32:
			return "int32_t";
		case TYPE_I64:
			return "int64_t";
		case TYPE_I128:
			return "__c3_int128";
		case TYPE_U8:
			return "uint8_t";
		case TYPE_U16:
			return "uint16_t";
		case TYPE_U32:
			return "uint32_t";
		case TYPE_U64:
			return "uint64_t";
		case TYPE_U128:
			return "__c3_uint128";
		case TYPE_F16:
			error_exit("float16 not supported in the C backend.");
		case TYPE_BF16:
			error_exit("bfloat16 not supported in the C backend.");
		case TYPE_F32:
			return "float";
		case TYPE_F64:
			return "double";
		case TYPE_F128:
			error_exit("float128 not supported in the C backend.");
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_INTERFACE:
		case TYPE_TYPEDEF:
		case TYPE_FUNC_RAW:
		case TYPE_ALIAS:
		case TYPE_ENUM:
		case TYPE_CONSTDEF:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_OPTIONAL:
		case SPECIAL_TYPES:
		case TYPE_WILDCARD:
		case TYPE_POISONED:
		case TYPE_UNTYPEDLIST:
			UNREACHABLE
		case TYPE_ANY:
			return "__c3_any__";
		case TYPE_FUNC_PTR:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (!prev) return "NOT_REGISTERED";
			return prev;
		}
	}
	UNREACHABLE
}
static bool c_emit_type_decl(GenContext *c, Type *type)
{
	type = type_lowering(type);
	if (type == type_u128 || type == type_i128)
	{
		void *prev = htable_get(&c->gen_decl, type);
		if (prev) return false;
		PRINT("typedef struct { uint64_t hi; uint64_t lo; } ");
		PRINT(type == type_u128 ? "__c3_uint128;\n" : "__c3_int128;\n");
		htable_set(&c->gen_decl, type, type == type_u128 ? "__c3_uint128" : "__c3_int128");
		return true;
	}
	switch (type->type_kind)
	{
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_POINTER:
			return false;
		case TYPE_TYPEDEF:
		case TYPE_FUNC_RAW:
		case TYPE_ALIAS:
		case TYPE_ENUM:
		case CT_TYPES:
		case TYPE_UNTYPEDLIST:
		case TYPE_CONSTDEF:
		case TYPE_OPTIONAL:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_INTERFACE:
			UNREACHABLE
		case TYPE_ANY:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			PRINTF("typedef struct { void* ptr; void* typeid; } __c3_any__;\n");
			htable_set(&c->gen_decl, type, "__c3_any__");
			return true;
		}
		case TYPE_FUNC_PTR:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			Type *base = type;
			type = type->pointer;
			FunctionPrototype* prototype = type->function.prototype;
			int id = ++c->typename;

			c_emit_type_decl(c, prototype->return_result);

			for (size_t i = 0; i < prototype->param_count; i++)
			{
				c_emit_type_decl(c, prototype->abi_args[i]->original_type);
			}

			scratch_buffer_clear();
			const char* typename = c_type_name(c, prototype->return_result);
			scratch_buffer_printf("%s(*__c3_func_ptr%d)(", typename, id);
			for (size_t i = 0; i < prototype->param_count; i++)
			{
				if (i > 0) scratch_buffer_printf(", ");
				scratch_buffer_printf("%s", c_type_name(c, prototype->abi_args[i]->original_type));
			}
			if (prototype->param_vacount > 0)
			{
				if (prototype->param_count > 0) scratch_buffer_printf(", ");
				scratch_buffer_printf("va_list");
			}
			scratch_buffer_printf(")");

			PRINTF("%s;\n", scratch_buffer_copy());
			htable_set(&c->gen_decl, base, scratch_buffer_copy());
			
			return true;
		}
		case TYPE_STRUCT:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			Decl *d = type->decl;
			FOREACH(Decl *, m, d->strukt.members)
			{
				c_emit_type_decl(c, m->type);
			}
			PRINTF("typedef struct { ");
			int id = ++c->typename;
			FOREACH_IDX(i, Decl *, m, d->strukt.members)
			{
				PRINTF("%s m%d; ", c_type_name(c, m->type), i);
			}
			PRINTF("} __c3_struct_%d;\n", id);
			scratch_buffer_clear();
			scratch_buffer_printf("__c3_struct_%d", id);
			htable_set(&c->gen_decl, type, scratch_buffer_copy());
			return true;
		}
		case TYPE_UNION:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			Decl *d = type->decl;
			FOREACH(Decl *, m, d->strukt.members)
			{
				c_emit_type_decl(c, m->type);
			}
			PRINTF("typedef union { ");
			int id = ++c->typename;
			FOREACH_IDX(i, Decl *, m, d->strukt.members)
			{
				PRINTF("%s m%d; ", c_type_name(c, m->type), i);
			}
			PRINTF("} __c3_union_%d;\n", id);
			scratch_buffer_clear();
			scratch_buffer_printf("__c3_union_%d", id);
			htable_set(&c->gen_decl, type, scratch_buffer_copy());
			return true;
		}
		case TYPE_BITSTRUCT:
			TODO
			break;
		case TYPE_SLICE:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			c_emit_type_decl(c, type->array.base);
			int id = ++c->typename;
			PRINTF("typedef struct { %s* ptr; size_t size; } __c3_slice%d;\n", c_type_name(c, type->array.base), id);
			scratch_buffer_clear();
			scratch_buffer_printf(" __c3_slice%d", id);
			htable_set(&c->gen_decl, type, scratch_buffer_copy());
			return true;
		}
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		{
			void *prev = htable_get(&c->gen_decl, type);
			if (prev) return false;
			c_emit_type_decl(c, type->array.base);
			int id = ++c->typename;
			PRINTF("typedef struct { %s ptr[%llu]; } __c3_array%d;\n", c_type_name(c, type->array.base), (unsigned long long)type->array.len, id);
			scratch_buffer_clear();
			scratch_buffer_printf(" __c3_array%d", id);
			htable_set(&c->gen_decl, type, scratch_buffer_copy());
			return true;
		}
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
			error_exit("Vectors are not supported in the C backend yet.");
	}
	UNREACHABLE
}
static bool c_emit_function_decl(GenContext *c, Decl *fn)
{
	Signature *sig = &fn->func_decl.signature;
	(void)c_emit_type_decl(c, typeget(sig->rtype));
	FOREACH(Decl *, d, sig->params)
	{
		c_emit_type_decl(c, d->type);
	}

	if (fn->is_extern)
	{
		PRINTF("extern ");
	}
	else if (!fn->is_external_visible)
	{
		PRINTF("static ");
	}

	PRINTF("%s ", c_type_name(c, typeget(sig->rtype)));

	fn->is_export = true;
	scratch_buffer_set_extern_decl_name(fn, true);
	PRINTF("%s(", scratch_buffer_to_string());
	FOREACH_IDX(i, Decl *, d, sig->params)
	{
		if (i != 0) PRINT(",");
		PRINT(c_type_name(c, d->type));
	}
	PRINT(");\n");
	return !fn->is_extern;
}

static void c_emit_stmt_chain(GenContext *c, AstId current)
{
	while (current)
	{
		c_emit_stmt(c, ast_next(&current));
	}
}

static int c_create_label(GenContext *c)
{
	return ++c->id_gen;
}

static int c_create_variable(GenContext *c)
{
	return ++c->id_gen;
}
static void c_emit_label(GenContext *c, int label)
{
	PRINTF("__C3_LABEL_%d:;\n", label);
}

static void c_emit_goto_label(GenContext *c, int label)
{
	PRINTF("goto __C3_LABEL_%d;\n", label);
}

static VariableId c_emit_temp(GenContext *c, CValue *value, Type *type)
{
	*value = (CValue) { .var = c_create_variable(c), .kind = CV_VALUE, .type = type_lowering(type) };
	return c->id_gen;
}

static void c_emit_expr(GenContext *c, CValue *value, Expr *expr);
static void c_emit_ignored_expr(GenContext *c, Expr *expr);
static void c_emit_local_decl(GenContext *c, Decl *decl, CValue *value);

static void c_emit_call_expr(GenContext *c, CValue *value, Expr *expr)
{
	ExprCall *call = &expr->call_expr;

	Expr **args = call->arguments;
	Expr **varargs = call->varargs;
	size_t num_args = vec_size(args);
	size_t num_varargs = vec_size(varargs);
	size_t total_num_args = num_args + num_varargs;

	CValue *c_args = (CValue*)malloc(sizeof(CValue) * total_num_args);

	for (size_t i = 0; i < num_args; i++)
	{
		c_emit_expr(c, &c_args[i], args[i]);
	}
	for (size_t i = 0; i < num_varargs; i++)
	{
		c_emit_expr(c, &c_args[i + num_args], varargs[i]);
	}

	if (!call->is_func_ref)
	{
		Expr *function = exprptr(call->function);
		c_emit_expr(c, value, function);
	}
	else
	{
		Decl *function_decl = declptr(call->func_ref);
		if (!call->no_return)
		{
			Type *return_type = typeget(function_decl->type->function.signature->rtype);
			PRINTF("%s ___var_%d = ", c_type_name(c, return_type), c_emit_temp(c, value, return_type));
		}
		PRINT(function_decl->name);
	}

	PRINT("(");
	for (size_t i = 0; i < total_num_args; i++)
	{
		if (i != 0) PRINT(", ");
		PRINTF("___var_%d", c_args[i].var);
	}
	PRINT(");\n");
	free(c_args);
}

static void c_emit_const_expr(GenContext *c, CValue *value, Expr *expr)
{
	Type *t = type_lowering(expr->type);
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
			PRINTF("%s ___var_%d = %20.20g;\n", c_type_name(c, t), c_emit_temp(c, value, t), expr->const_expr.fxx.f);
			return;
		case CONST_INTEGER:
			if (t == type_u128 || t == type_i128)
			{
				TODO
			}
			if (type_is_unsigned(t))
			{
				PRINTF("%s ___var_%d = %" PRIu64 ";\n", c_type_name(c, t), c_emit_temp(c, value, t), expr->const_expr.ixx.i.low);
			}
			else
			{
				PRINTF("%s ___var_%d = %" PRId64 ";\n", c_type_name(c, t), c_emit_temp(c, value, t), (int64_t)expr->const_expr.ixx.i.low);
			}
			return;
		case CONST_BOOL:
			PRINTF("bool ___var_%d = %s;\n", c_emit_temp(c, value, t), expr->const_expr.b ? "true" : "false");
			return;
		case CONST_STRING:
			PRINTF("%s ___var_%d = ", c_type_name(c, t), c_emit_temp(c, value, t));
			if (t->type_kind == TYPE_SLICE)
			{
				PRINT("{ ");
			}
			PRINT("\"");
			for (ArraySize i = 0; i < expr->const_expr.bytes.len; i++)
			{
				char b = expr->const_expr.bytes.ptr[i];
				if (b >= ' ' && b < 127)
				{
					PRINTF("%c", b);
					continue;
				}
				PRINTF("\\%d%d%d", b / 64, (b % 64) / 8, b % 8);
			}
			PRINT("\"");
			if (t->type_kind == TYPE_SLICE)
			{
				PRINTF(", %llu }", (unsigned long long)expr->const_expr.bytes.len);
			}
			PRINT(";\n");
			return;
		case CONST_FAULT:
		case CONST_ENUM:
		case CONST_BYTES:
			break;
		case CONST_POINTER:
			break;
		case CONST_TYPEID:
			break;
		case CONST_SLICE:
			break;
		case CONST_INITIALIZER:
			break;
		case CONST_REF:
			break;
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
		case CONST_REFLECTION:
			UNREACHABLE_VOID
	}
	PRINT("/* CONST EXPR */\n");
}
static void c_emit_cond_expr(GenContext *c, CValue *value, Expr *expr)
{
	Expr **list = expr->cond_expr;
	unsigned size = vec_size(list);
	assert(size);
	unsigned last = size - 1;
	for (unsigned i = 0; i < last; i++)
	{
		c_emit_ignored_expr(c, list[i]);
	}
	c_emit_expr(c, value, list[last]);
}
static void c_emit_expression_list_expr(GenContext *c, CValue *value, Expr *expr)
{
	Expr **list = expr->expression_list;
	unsigned size = vec_size(list);
	assert(size);
	unsigned last = size - 1;
	for (unsigned i = 0; i < last; i++)
	{
		c_emit_ignored_expr(c, list[i]);
		// In the llvm backend, there is a possibility of an early return here
		// in the event the builder is not the global builder.
		// This is not ever the case here, so this function acts the same as
		// c_emit_cond_expr
	}
	c_emit_expr(c, value, list[last]);
}
static void c_emit_ptr_access_expr(GenContext *c, CValue *value, Expr *expr)
{
	CValue inner_value;
	c_emit_expr(c, &inner_value, expr->inner_expr);
	value->var = c_create_variable(c);
	const char *type_name = c_type_name(c, expr->type);
	PRINTF("%s ___var_%d = *(%s*)&___var_%d;\n", type_name, value->var, type_name, inner_value.var);
}
static void c_emit_identifier_expr(GenContext *c, CValue *value, Expr *expr)
{
	Decl *decl = expr->ident_expr;
	value->var = c_create_variable(c);
	PRINTF("%s ___var_%d = ___var_%d;\n", c_type_name(c, decl->type), value->var, decl->backend_id);
}

static void c_emit_binary_expr(GenContext *c, CValue *value, Expr *expr)
{
	ExprBinary *binary = &expr->binary_expr;
	CValue left_value, right_value;
	c_emit_expr(c, &left_value, exprptr(binary->left));
	c_emit_expr(c, &right_value, exprptr(binary->right));

	const char *operator_string = NULL;
	
	assert(expr->type);
	const char *type_string = c_type_name(c, expr->type);

	value->var = c_create_variable(c);

	switch(binary->operator){
		case BINARYOP_ERROR: UNREACHABLE_VOID;
		case BINARYOP_MULT: operator_string = "*"; break;
		case BINARYOP_SUB: operator_string = "-"; break;
		case BINARYOP_ADD: operator_string = "+"; break;
		case BINARYOP_DIV: operator_string = "/"; break;
		case BINARYOP_MOD: operator_string = "%"; break;
		case BINARYOP_SHR: operator_string = ">>"; break;
		case BINARYOP_SHL: operator_string = "<<"; break;
		case BINARYOP_BIT_OR: operator_string = "|"; break;
		case BINARYOP_BIT_XOR: operator_string = "^"; break;
		case BINARYOP_BIT_AND: operator_string = "&"; break;
		case BINARYOP_AND: operator_string = "&&"; break;
		case BINARYOP_OR: operator_string = "||"; break;
		case BINARYOP_ELSE: TODO // Its the ?? operator for optionals
		case BINARYOP_CT_AND:
		case BINARYOP_CT_OR:
		case BINARYOP_CT_CONCAT:
		case BINARYOP_CT_CONCAT_ASSIGN:
			// Handled elsewhere.
			UNREACHABLE_VOID
		case BINARYOP_GT: operator_string = ">"; break;
		case BINARYOP_GE: operator_string = ">="; break;
		case BINARYOP_LT: operator_string = "<"; break;
		case BINARYOP_LE: operator_string = "<="; break;
		case BINARYOP_NE: operator_string = "!="; break;
		case BINARYOP_EQ: operator_string = "=="; break;
		case BINARYOP_VEC_GT:
		case BINARYOP_VEC_GE:
		case BINARYOP_VEC_LT:
		case BINARYOP_VEC_LE:
		case BINARYOP_VEC_NE:
		case BINARYOP_VEC_EQ:
			// These will probably be special functions that
			// the backend calls
			TODO
		case BINARYOP_ASSIGN: operator_string = "="; break;
		case BINARYOP_ADD_ASSIGN: operator_string = "+="; break;
		case BINARYOP_BIT_AND_ASSIGN: operator_string = "&="; break;
		case BINARYOP_BIT_OR_ASSIGN: operator_string = "|="; break;
		case BINARYOP_BIT_XOR_ASSIGN: operator_string = ""; break;
		case BINARYOP_DIV_ASSIGN: operator_string = "^="; break;
		case BINARYOP_MOD_ASSIGN: operator_string = "%="; break;
		case BINARYOP_MULT_ASSIGN: operator_string = "*="; break;
		case BINARYOP_SHR_ASSIGN: operator_string = ">>="; break;
		case BINARYOP_SHL_ASSIGN: operator_string = "<<="; break;
		case BINARYOP_SUB_ASSIGN: operator_string = "-="; break;
	};

	assert(operator_string);
	PRINTF("%s ___var_%d = ___var_%d %s ___var_%d;\n", type_string, value->var, left_value.var, operator_string, right_value.var);
}
static void c_emit_expr(GenContext *c, CValue *value, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_ENUM_FROM_ORD:
			break;
		case EXPR_PTR_ACCESS:
			c_emit_ptr_access_expr(c, value, expr);
			return;
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_DISCARD:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_EXT_TRUNC:
		case EXPR_MAKE_ANY:
		case EXPR_MAKE_SLICE:
		case EXPR_INT_TO_BOOL:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_TWO:
			break;
		case NON_RUNTIME_EXPR:
		case UNRESOLVED_EXPRS:
			UNREACHABLE_VOID
		case EXPR_ACCESS_RESOLVED:
			break;
		case EXPR_ASM:
			break;
		case EXPR_BENCHMARK_HOOK:
			break;
		case EXPR_BINARY:
			c_emit_binary_expr(c, value, expr);
			return;
		case EXPR_BITACCESS:
			break;
		case EXPR_BITASSIGN:
			break;
		case EXPR_BUILTIN:
			break;
		case EXPR_BUILTIN_ACCESS:
			break;
		case EXPR_CALL:
			c_emit_call_expr(c, value, expr);
			return;
		case EXPR_CATCH:
			break;
		case EXPR_COND:
			c_emit_cond_expr(c, value, expr);
			return;
			break;
		case EXPR_CONST:
			c_emit_const_expr(c, value, expr);
			return;
		case EXPR_DECL:
			c_emit_local_decl(c, expr->decl_expr, value);
			return;
		case EXPR_DEFAULT_ARG:
			break;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			break;
		case EXPR_DESIGNATOR:
			break;
		case EXPR_EXPRESSION_LIST:
			c_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_FORCE_UNWRAP:
			break;
		case EXPR_IDENTIFIER:
			c_emit_identifier_expr(c, value, expr);
			return;
		case EXPR_INITIALIZER_LIST:
			break;
		case EXPR_LAMBDA:
			break;
		case EXPR_LAST_FAULT:
			break;
		case EXPR_MACRO_BLOCK:
			break;
		case EXPR_MACRO_BODY_EXPANSION:
			break;
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
			break;
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NAMED_EVAL_ARGUMENT:
			break;
		case EXPR_NOP:
			break;
		case EXPR_OPERATOR_CHARS:
			break;
		case EXPR_OPTIONAL:
			break;
		case EXPR_POINTER_OFFSET:
			break;
		case EXPR_POST_UNARY:
			break;
		case EXPR_RETHROW:
			break;
		case EXPR_RETVAL:
			break;
		case EXPR_SLICE:
			break;
		case EXPR_SLICE_ASSIGN:
			break;
		case EXPR_SLICE_COPY:
			break;
		case EXPR_SUBSCRIPT:
			break;
		case EXPR_SUBSCRIPT_ADDR:
			break;
		case EXPR_SUBSCRIPT_ASSIGN:
			break;
		case EXPR_SWIZZLE:
			break;
		case EXPR_TERNARY:
			break;
		case EXPR_TEST_HOOK:
			break;
		case EXPR_TRY:
			break;
		case EXPR_TRY_UNWRAP_CHAIN:
			break;
		case EXPR_TYPEID_INFO:
			break;
		case EXPR_UNARY:
			break;
	}
	PRINT("/* TODO EXPR */\n");
}

static void c_emit_jump_to_optional_exit(GenContext *c, int value)
{
	TODO
}
static int c_emit_load(GenContext *c, VariableId id)
{
	TODO
	UNREACHABLE
}

static void c_value_fold_optional(GenContext *c, CValue *value)
{
	if (value->kind == CV_OPTIONAL_ADDRESS)
	{
		c_emit_jump_to_optional_exit(c, c_emit_load(c, value->optional));
		value->kind = CV_ADDRESS;
	}
}

static void c_emit_ignored_expr(GenContext *c, Expr *expr)
{
	CValue value;

	// For a standalone catch, we can ignore storing the value.
	if (IS_OPTIONAL(expr))
	{
		int discard_fail = c_create_label(c);
		PUSH_CATCH_VAR_BLOCK(NULL, discard_fail);
		c_emit_expr(c, &value, expr);
		c_value_fold_optional(c, &value);
		c_emit_goto_label(c, discard_fail);
		c_emit_label(c, discard_fail);
		POP_CATCH();
		return;
	}
	c_emit_expr(c, &value, expr);
}

static void c_emit_expr_stmt(GenContext *c, Ast *ast)
{
	PRINT("/*EXPR*/\n");
	c_emit_ignored_expr(c, ast->expr_stmt);
}

static void c_emit_local_decl(GenContext *c, Decl *decl, CValue *value)
{
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			PRINT("/* LOCAL DECL */\n");
//			llvm_emit_local_static(c, decl, value);
			return;
		case VARDECL_LOCAL:
			if (decl->var.is_static)
			{
				PRINT("/* LOCAL DECL */\n");
//				llvm_emit_local_static(c, decl, value);
				return;
			}
			break;
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_EXPR:
		case VARDECL_GLOBAL:
		case VARDECL_MEMBER:
		case VARDECL_BITMEMBER:
			UNREACHABLE_VOID;
		case VARDECL_PARAM:
		case VARDECL_UNWRAPPED:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
			return;
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			UNREACHABLE_VOID
	}

	// Get the declaration and the LLVM type.
	Type *var_type = type_lowering(decl->type);

	*value = (CValue) { .var = c_create_variable(c), .kind = CV_VALUE, .type = var_type };
	decl->backend_id = value->var;
	PRINTF("%s ___var_%d;\n", c_type_name(c, var_type), value->var);
	// Create optional storage
	bool is_optional = IS_OPTIONAL(decl);
	if (is_optional)
	{
		decl->var.optional_id = value->optional = c_create_variable(c);
		PRINTF("void* ___var_f_%d;\n", value->optional);
	}

	// Grab the init expression
	Expr *init = decl->var.init_expr;
	if (init)
	{
		CValue out;
		c_emit_expr(c, &out, decl->var.init_expr);
		if (out.kind == CV_OPTIONAL_ADDRESS)
		{
			PRINTF("___var_f_%d = __var_%d;\n", value->optional, out.optional);
		}
		else if (value->optional)
		{
			PRINTF("___var_f_%d = NULL;\n", value->optional);
		}
		PRINTF("___var_%d = ___var_%d;\n", value->var, out.var);
		return;
	}

	// If the variable has a no-init, then skip
	if (decl->var.no_init)
	{
		return;
	}

	if (is_optional)
	{
		PRINTF("___var_f_%d = NULL;\n", value->optional);
		value->kind = CV_VALUE;
	}

	switch (var_type->type_kind)
	{
		case TYPE_BOOL:
			PRINTF("___var_%d = false;\n", value->var);
			break;
		case ALL_INTS:
		case ALL_FLOATS:
			PRINTF("___var_%d = 0;\n", value->var);
			break;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_TYPEDEF:
		case TYPE_CONSTDEF:
		case TYPE_FUNC_RAW:
		case TYPE_BITSTRUCT:
		case TYPE_ALIAS:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_OPTIONAL:
		case TYPE_WILDCARD:
		case TYPE_UNTYPEDLIST:
		case SPECIAL_TYPES:
			UNREACHABLE_VOID
		case TYPE_ANY:
		case TYPE_INTERFACE:
			PRINTF("___var_%d = (c3_any_t){ NULL, NULL };\n", value->var);
			break;
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
		case TYPE_ENUM:
			PRINTF("___var_%d = 0;\n", value->var);
			break;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
			PRINT("/* TODO ZERO INIT */\n");

	}
}

static void c_emit_return(GenContext *c, Ast *stmt)
{
	Expr *expr = stmt->return_stmt.expr;
	if (expr && expr->expr_kind == EXPR_OPTIONAL)
	{
		PRINT("/* RETURN */\n");
		/*
		BEValue be_value;
		llvm_emit_expr(c, &be_value, expr->inner_expr);
		if (ast->return_stmt.cleanup_fail)
		{
			llvm_value_rvalue(c, &be_value);
			LLVMValueRef error_out = llvm_emit_alloca_aligned(c, type_fault, "reterr");
			llvm_store_to_ptr(c, error_out, &be_value);
			PUSH_DEFER_ERROR(error_out);
			llvm_emit_statement_chain(c, ast->return_stmt.cleanup_fail);
			POP_DEFER_ERROR();
		}
		llvm_emit_return_abi(c, NULL, &be_value);
		 */
		return;
	}

	PUSH_CATCH();

	int error_return_block = 0;
	int error_out = 0;

	if (!stmt->return_stmt.expr)
	{
		PRINT("return;\n");
		return;
	}
	PRINT("/* RETURN */\n");
	return;
	/*
	if (c->cur_func.prototype && type_is_optional(c->cur_func.prototype->rtype))
	{
		error_return_block = llvm_basic_block_new(c, "err_retblock");
		error_out = llvm_emit_alloca_aligned(c, type_fault, "reterr");
		c->catch = (OptionalCatch) { error_out, error_return_block };
	}

	bool has_return_value = ast->return_stmt.expr != NULL;
	BEValue return_value = { 0 };
	if (has_return_value)
	{
		llvm_emit_expr(c, &return_value, ast->return_stmt.expr);
		llvm_value_fold_optional(c, &return_value);
		c->retval = return_value;
	}

	POP_CATCH();

	if (ast->return_stmt.cleanup || ast->return_stmt.cleanup_fail)
	{
		if (has_return_value)
		{
			if (llvm_temp_as_address(c, return_value.type))
			{
				LLVMValueRef temp = llvm_emit_alloca_aligned(c, return_value.type, "ret$temp");
				llvm_store_to_ptr(c, temp, &return_value);
				llvm_value_set_address_abi_aligned(&return_value, temp, return_value.type);
			}
			else
			{
				llvm_value_rvalue(c, &return_value);
			}
		}
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup);
	}

	if (llvm_get_current_block_if_in_use(c))
	{
		// Are we in an expression block?
		if (!has_return_value)
		{
			llvm_emit_return_implicit(c);
		}
		else
		{
			llvm_emit_return_abi(c, &return_value, NULL);
		}
	}
	if (error_return_block && LLVMGetFirstUse(LLVMBasicBlockAsValue(error_return_block)))
	{
		llvm_emit_block(c, error_return_block);
		PUSH_DEFER_ERROR(error_out);
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup_fail);
		POP_DEFER_ERROR();
		BEValue value;
		llvm_value_set_address_abi_aligned(&value, error_out, type_fault);
		llvm_emit_return_abi(c, NULL, &value);
	}*/
}

/*
	TODO:
	When implementing continue/break and the labeled varients, some data
	will need to be pushed about what the labels will be called
	These labels will then need to be emitted here
*/
static void c_emit_for_stmt(GenContext *c, Ast *stmt)
{
	// We emit a while loop because thats more flexible
	AstForStmt *for_stmt = &stmt->for_stmt;
	CValue initializer_expr, condition_expr;
	if (for_stmt->init) c_emit_expr(c, &initializer_expr, exprptr(for_stmt->init));
	if (for_stmt->cond) c_emit_expr(c, &condition_expr, exprptr(for_stmt->cond));

	PRINTF("while (___var_%d)\n{\n", condition_expr.var);
	c_emit_stmt(c, astptrzero(for_stmt->body));
	if (for_stmt->incr)
	{
		CValue in_loop_incr;
		c_emit_expr(c, &in_loop_incr, exprptr(for_stmt->incr));
		PRINTF("___var_%d = ___var_%d;\n", initializer_expr.var, in_loop_incr.var);
	}
	if (for_stmt->cond)
	{
		CValue in_loop_cond;
		c_emit_expr(c, &in_loop_cond, exprptr(for_stmt->cond));
		PRINTF("___var_%d = ___var_%d;\n", condition_expr.var, in_loop_cond.var);
	}

	PRINT("}\n");

}

static void c_emit_if_stmt(GenContext *c, Ast *stmt)
{
	AstIfStmt *if_stmt = &stmt->if_stmt;
	CValue condition;
	c_emit_expr(c, &condition, exprptr(if_stmt->cond));

	PRINTF("if (___var_%d)\n", condition.var);
	c_emit_stmt(c, astptrzero(if_stmt->then_body));
	if (if_stmt->else_body)
	{
		PRINT("else");
		c_emit_stmt(c, astptrzero(if_stmt->else_body));
	}
}

static void c_emit_stmt(GenContext *c, Ast *stmt)
{
	if (!stmt) return;
	switch (stmt->ast_kind)
	{
		case AST_POISONED:
			UNREACHABLE_VOID
		case AST_ASM_STMT:
			break;
		case AST_ASM_BLOCK_STMT:
			break;
		case AST_ASM_LABEL:
			break;
		case AST_ASSERT_STMT:
			break;
		case AST_BREAK_STMT:
			break;
		case AST_CASE_STMT:
			break;
		case AST_COMPOUND_STMT:
			PRINT("{\n");
			c_emit_stmt_chain(c, stmt->compound_stmt.first_stmt);
			PRINT("}\n");
			return;
		case AST_CONTINUE_STMT:
			break;
		case AST_CT_ASSERT:
			break;
		case AST_CT_ECHO_STMT:
		case AST_CT_EXPAND_STMT:
			break;
		case AST_CT_COMPOUND_STMT:
		case AST_CT_ELSE_STMT:
			break;
		case AST_CT_FOREACH_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_IF_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CT_TYPE_ASSIGN_STMT:
			UNREACHABLE_VOID
		case AST_DECLARE_STMT:
		{
			CValue value;
			c_emit_local_decl(c, stmt->declare_stmt, &value);
			return;
		}
		case AST_DECLS_STMT:
			break;
		case AST_DEFAULT_STMT:
			break;
		case AST_DEFER_STMT:
			break;
		case AST_EXPR_STMT:
			c_emit_expr_stmt(c, stmt);
			return;
		case AST_FOR_STMT:
			c_emit_for_stmt(c, stmt);
			return;
		case AST_FOREACH_STMT:
			break;
		case AST_IF_STMT:
			c_emit_if_stmt(c, stmt);
			return;
		case AST_NOP_STMT:
			PRINT(";\n");
			return;
		case AST_RETURN_STMT:
			c_emit_return(c, stmt);
			return;
		case AST_BLOCK_EXIT_STMT:
			break;
		case AST_SWITCH_STMT:
			break;
		case AST_NEXTCASE_STMT:
			break;
	}
	PRINT("/* TODO */\n");
}
static void c_emit_function(GenContext *c, Decl *fn)
{
	Signature *sig = &fn->func_decl.signature;
	if (!fn->is_external_visible)
	{
		PRINTF("static ");
	}

	PRINTF("%s ", c_type_name(c, typeget(sig->rtype)));

	fn->is_export = true;
	scratch_buffer_set_extern_decl_name(fn, true);
	PRINTF("%s(", scratch_buffer_to_string());
	FOREACH_IDX(i, Decl *, d, sig->params)
	{
		if (i != 0) PRINT(",");
		PRINTF("%s ___arg_%d__", c_type_name(c, d->type), i);
	}
	PRINT(") {\n");
	c_emit_stmt(c, astptrzero(fn->func_decl.body));
	PRINT("}\n");
}

static GenContext *c_gen_module(Module *module, int num)
{
	if (!vec_size(module->units)) return NULL;
	// if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(module)) return NULL;

	scratch_buffer_clear();
	scratch_buffer_printf("temp%d.c", num);
	FILE* f = fopen(scratch_buffer_to_string(), "wb");
	if (!f) error_exit("Failed to output module.");
	fputs("#include <stdint.h>\n", f);
	fputs("#include <stddef.h>\n", f);
	fputs("#include <stdbool.h>\n", f);

	bool has_elements = false;
	GenContext *c = cmalloc(sizeof(GenContext));
	*c = (GenContext) { .file = f };
	htable_init(&c->gen_decl, 1024);
	htable_init(&c->gen_decl, 1024);

	bool only_used = strip_unused();
	FOREACH(CompilationUnit *, unit, module->units)
	{
		FOREACH(Decl *, method, unit->methods)
		{
			if (only_used && !method->is_live) continue;
			has_elements |= c_emit_function_decl(c, method);
		}
		FOREACH(Decl *, func, unit->functions)
		{
			if (only_used && !func->is_live) continue;
			has_elements |= c_emit_function_decl(c, func);
		}

		FOREACH(Decl *, type_decl, unit->types)
		{
			if (only_used && !type_decl->is_live) continue;
			c_emit_type_decl(c, type_decl->type);
		}
	}
	FOREACH(CompilationUnit *, unit, module->units)
	{
		FOREACH(Decl *, method, unit->methods)
		{
			if (only_used && !method->is_live) continue;
			has_elements = true;
			c_emit_function(c, method);
		}
		FOREACH(Decl *, func, unit->functions)
		{
			if (only_used && !func->is_live) continue;
			has_elements = true;
			c_emit_function(c, func);
		}
	}
/*

		FOREACH(Decl *, enum_decl, unit->enums)
		{
			if (only_used && !enum_decl->is_live) continue;
			llvm_emit_type_decls(gen_context, enum_decl);
		}

		FOREACH(Decl *, func, unit->functions)
		{
			if (only_used && !func->is_live) continue;
			if (func->func_decl.attr_test)
			{
				if (!compiler.build.testing) continue;
				vec_add(module->tests, func);
			}
			if (func->func_decl.attr_benchmark)
			{
				if (!compiler.build.benchmarking) continue;
				vec_add(module->benchmarks, func);
			}
			llvm_emit_function_decl(gen_context, func);
		}


		FOREACH(Decl *, func, unit->lambdas)
		{
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_decl(gen_context, func);
		}

		if (compiler.build.type != TARGET_TYPE_TEST && compiler.build.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_decl(gen_context, unit->main_function);
		}
*/

/*
	FOREACH(CompilationUnit *, unit, module->units)
	{
		gen_context->debug.compile_unit = unit->llvm.debug_compile_unit;
		gen_context->debug.file = (DebugFile){
				.debug_file = unit->llvm.debug_file,
				.file_id = unit->file->file_id };

		FOREACH(Decl *, var, unit->vars)
		{
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_get_ref(gen_context, var);
		}

		FOREACH(Decl *, var, unit->vars)
		{
			if (only_used && !var->is_live) continue;
			has_elements = true;
			llvm_emit_global_variable_init(gen_context, var);
		}

		FOREACH(Decl *, decl, unit->functions)
		{
			if (decl->func_decl.attr_test && !compiler.build.testing) continue;
			if (decl->func_decl.attr_benchmark && !compiler.build.benchmarking) continue;
			if (only_used && !decl->is_live) continue;
			if (decl->func_decl.body)
			{
				has_elements = true;
				llvm_emit_function_body(gen_context, decl);
			}
		}

		FOREACH(Decl *, func, unit->lambdas)
		{
			if (only_used && !func->is_live) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, func);
		}

		if (compiler.build.type != TARGET_TYPE_TEST && compiler.build.type != TARGET_TYPE_BENCHMARK && unit->main_function && unit->main_function->is_synthetic)
		{
			has_elements = true;
			llvm_emit_function_body(gen_context, unit->main_function);
		}

		FOREACH(Decl *, decl, unit->methods)
		{
			if (only_used && !decl->is_live) continue;
			if (!decl->func_decl.body) continue;
			has_elements = true;
			llvm_emit_function_body(gen_context, decl);
		}

		gencontext_end_file_emit(gen_context, unit);

	}

	llvm_emit_dynamic_functions(gen_context, gen_context->dynamic_functions);

	llvm_emit_constructors_and_destructors(gen_context);

	if (llvm_use_debug(gen_context))
	{
		LLVMDIBuilderFinalize(gen_context->debug.builder);
		LLVMDisposeDIBuilder(gen_context->debug.builder);
	}

	// If it's in test or benchmark, then we want to serialize the IR before it is optimized.
	if (compiler.build.test_output || compiler.build.benchmark_output)
	{
		gencontext_print_llvm_ir(gen_context);
		gencontext_verify_ir(gen_context);
	}
	if (!has_elements) return NULL;
	return gen_context;
*/
	fclose(f);
	if (!has_elements)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("temp%d.c", num);
		file_delete_file(scratch_buffer_to_string());
	}
	return c;
}

void **c_gen(Module** modules, unsigned module_count)
{
	if (!module_count) return NULL;
	void **gen_contexts = NULL;
	for (unsigned i = 0; i < module_count; i++)
	{
		c_gen_module(modules[i], i);
	}
	return NULL;
}
