#include "c_codegen_internal.h"

typedef struct
{
	BuiltinFunction kind;
	const char *c_fn;
} BuiltinMap;

static const BuiltinMap simple_builtins_table[] = {
    {BUILTIN_SQRT, "__builtin_sqrt"},
    {BUILTIN_SIN, "__builtin_sin"},
    {BUILTIN_COS, "__builtin_cos"},
    {BUILTIN_TAN, "__builtin_tan"},
    {BUILTIN_ASIN, "__builtin_asin"},
    {BUILTIN_ACOS, "__builtin_acos"},
    {BUILTIN_ATAN, "__builtin_atan"},
    {BUILTIN_SINH, "__builtin_sinh"},
    {BUILTIN_COSH, "__builtin_cosh"},
    {BUILTIN_TANH, "__builtin_tanh"},
    {BUILTIN_EXP, "__builtin_exp"},
    {BUILTIN_EXP2, "__builtin_exp2"},
    {BUILTIN_EXP10, "__builtin_exp10"},
    {BUILTIN_LOG, "__builtin_log"},
    {BUILTIN_LOG2, "__builtin_log2"},
    {BUILTIN_LOG10, "__builtin_log10"},
    {BUILTIN_FLOOR, "__builtin_floor"},
    {BUILTIN_CEIL, "__builtin_ceil"},
    {BUILTIN_ROUND, "__builtin_round"},
    {BUILTIN_ROUNDEVEN, "__builtin_round"},
    {BUILTIN_TRUNC, "__builtin_trunc"},
    {BUILTIN_RINT, "__builtin_rint"},
    {BUILTIN_NEARBYINT, "__builtin_nearbyint"},
    {BUILTIN_LRINT, "__builtin_lrint"},
    {BUILTIN_LROUND, "__builtin_lround"},
    {BUILTIN_POW, "__builtin_pow"},
    {BUILTIN_POW_INT, "__builtin_pow"},
    {BUILTIN_FMA, "__builtin_fma"},
    {BUILTIN_FMULADD, "__builtin_fma"},
    {BUILTIN_COPYSIGN, "__builtin_copysign"},
};

static bool c_try_emit_table_builtin(GenContext *c, CValue *value, Type *return_type, BuiltinFunction bfn, CValue *c_args, int arg_idx, bool has_return)
{
	for (size_t i = 0; i < sizeof(simple_builtins_table) / sizeof(simple_builtins_table[0]); i++)
	{
		if (simple_builtins_table[i].kind == bfn)
		{
			const char *builtin_fn     = simple_builtins_table[i].c_fn;
			compiler.linking.link_math = true;
			if (has_return)
			{
				c_emit_type_forward_decl(c, return_type);
				Type *flat_ret = return_type ? type_flatten(return_type) : NULL;
				bool is_vec    = flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY);
				if (is_vec)
				{
					int len        = c_type_aggregate_len(flat_ret);
					int temp       = c_emit_temp_var(c, value, return_type);
					const char *tn = c_type_name(c, return_type);
					PRINTF("%s ___var_%d;\n", tn, temp);
					for (int k = 0; k < len; k++)
					{
						PRINTF("___var_%d.ptr[%d] = %s(", temp, k, builtin_fn);
						for (int j = 0; j < arg_idx; j++)
						{
							if (j != 0)
							{
								PRINT(", ");
							}
							Type *arg_flat  = c_args[j].type ? type_flatten(c_args[j].type) : NULL;
							bool arg_is_ptr = false;
							if (arg_flat && arg_flat->type_kind == TYPE_POINTER && arg_flat->pointer)
							{
								arg_is_ptr = true;
								arg_flat   = type_flatten(arg_flat->pointer);
							}
							if (c_args[j].kind == CV_ADDRESS)
							{
								arg_is_ptr = true;
							}
							bool arg_is_vec = arg_flat && (arg_flat->type_kind == TYPE_VECTOR || arg_flat->type_kind == TYPE_SIMD_VECTOR || arg_flat->type_kind == TYPE_ARRAY);
							if (arg_is_vec)
							{
								PRINTF("___var_%d%sptr[%d]", c_args[j].var, c_arrow(&c_args[j]), k);
							}
							else if (arg_is_ptr)
							{
								PRINTF("*(%s*)___var_%d", c_type_name(c, arg_flat), c_args[j].var);
							}
							else
							{
								PRINTF("___var_%d", c_args[j].var);
							}
						}
						PRINT(");\n");
					}
					return true;
				}
				PRINTF("%s ___var_%d = %s(", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), builtin_fn);
			}
			else
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
				PRINTF("%s(", builtin_fn);
			}
			for (int j = 0; j < arg_idx; j++)
			{
				if (j != 0)
				{
					PRINT(", ");
				}
				c_value_rvalue(c, &c_args[j]);
				PRINTF("___var_%d", c_args[j].var);
			}
			PRINT(");\n");
			return true;
		}
	}
	return false;
}

static void c_emit_builtin_fshl(GenContext *c, CValue *value, Type *rtype, CValue *a, CValue *b, CValue *s)
{
	c_value_rvalue(c, a);
	c_value_rvalue(c, b);
	c_value_rvalue(c, s);
	int temp       = c_emit_temp_var(c, value, rtype);
	const char *tn = c_type_name(c, rtype);
	PRINTF("%s ___var_%d;\n", tn, temp);
	PRINTF("{\n\tsize_t _w = sizeof(%s) * 8;\n", tn);
	PRINTF("\tsize_t _s = ___var_%d & (_w - 1);\n", s->var);
	PRINTF("\t___var_%d = _s ? ((___var_%d << _s) | (___var_%d >> (_w - _s))) : ___var_%d;\n}\n",
	       temp, a->var, b->var, a->var);
}

static void c_emit_builtin_fshr(GenContext *c, CValue *value, Type *rtype, CValue *a, CValue *b, CValue *s)
{
	c_value_rvalue(c, a);
	c_value_rvalue(c, b);
	c_value_rvalue(c, s);
	int temp       = c_emit_temp_var(c, value, rtype);
	const char *tn = c_type_name(c, rtype);
	PRINTF("%s ___var_%d;\n", tn, temp);
	PRINTF("{\n\tsize_t _w = sizeof(%s) * 8;\n", tn);
	PRINTF("\tsize_t _s = ___var_%d & (_w - 1);\n", s->var);
	PRINTF("\t___var_%d = _s ? ((___var_%d << (_w - _s)) | (___var_%d >> _s)) : ___var_%d;\n}\n",
	       temp, a->var, b->var, b->var);
}

static void c_emit_builtin_sat(GenContext *c, CValue *value, Type *rtype, BuiltinFunction op, CValue *a, CValue *b)
{
	c_value_rvalue(c, a);
	c_value_rvalue(c, b);
	int temp          = c_emit_temp_var(c, value, rtype);
	const char *tn    = c_type_name(c, rtype);
	const char *bname = (op == BUILTIN_SAT_SUB) ? "__builtin_sub_overflow" : (op == BUILTIN_SAT_MUL) ? "__builtin_mul_overflow"
	                                                                                                 : "__builtin_add_overflow";
	PRINTF("%s ___var_%d = 0;\n", tn, temp);
	PRINTF("if (%s(___var_%d, ___var_%d, &___var_%d)) {\n", bname, a->var, b->var, temp);
	int bytes = (int)type_size(rtype);
	if (type_is_signed(rtype))
	{
		if (bytes == 16)
		{
			if (op == BUILTIN_SAT_SUB)
			{
				PRINTF("\t___var_%d = (___var_%d < 0) ? (__c3_int128)(~((__c3_uint128)1 << 127)) : (__c3_int128)((__c3_uint128)1 << 127);\n",
				       temp, b->var);
			}
			else
			{
				PRINTF("\t___var_%d = (___var_%d >= 0) ? (__c3_int128)(~((__c3_uint128)1 << 127)) : (__c3_int128)((__c3_uint128)1 << 127);\n",
				       temp, b->var);
			}
		}
		else
		{
			if (op == BUILTIN_SAT_SUB)
			{
				PRINTF("\t___var_%d = (___var_%d < 0) ? (%s)(((uint64_t)1 << (sizeof(%s)*8-1)) - 1) : (%s)(~(((uint64_t)1 << (sizeof(%s)*8-1)) - 1));\n",
				       temp, b->var, tn, tn, tn, tn);
			}
			else
			{
				PRINTF("\t___var_%d = (___var_%d >= 0) ? (%s)(((uint64_t)1 << (sizeof(%s)*8-1)) - 1) : (%s)(~(((uint64_t)1 << (sizeof(%s)*8-1)) - 1));\n",
				       temp, b->var, tn, tn, tn, tn);
			}
		}
	}
	else
	{
		if (op == BUILTIN_SAT_SUB)
		{
			PRINTF("\t___var_%d = 0;\n", temp);
		}
		else
		{
			if (bytes == 16)
			{
				PRINTF("\t___var_%d = (__c3_uint128)~(__c3_uint128)0;\n", temp);
			}
			else
			{
				PRINTF("\t___var_%d = (%s)(~0ULL);\n", temp, tn);
			}
		}
	}
	PRINT("}\n");
}

static void c_emit_builtin_reduce(GenContext *c, CValue *value, Type *rtype, BuiltinFunction op, CValue *c_args, int num_args)
{
	int temp       = c_emit_temp_var(c, value, rtype);
	const char *tn = c_type_name(c, rtype);

	CValue *vec_arg = &c_args[0];
	CValue *acc_arg = NULL;
	if (num_args >= 2)
	{
		Type *t0       = c_args[0].type ? type_flatten(c_args[0].type) : NULL;
		Type *t1       = c_args[1].type ? type_flatten(c_args[1].type) : NULL;
		bool t0_is_vec = t0 && (t0->type_kind == TYPE_ARRAY || t0->type_kind == TYPE_VECTOR || t0->type_kind == TYPE_SIMD_VECTOR);
		bool t1_is_vec = t1 && (t1->type_kind == TYPE_ARRAY || t1->type_kind == TYPE_VECTOR || t1->type_kind == TYPE_SIMD_VECTOR);
		if (t1_is_vec)
		{
			acc_arg = &c_args[0];
			vec_arg = &c_args[1];
		}
		else if (t0_is_vec)
		{
			vec_arg = &c_args[0];
			acc_arg = &c_args[1];
		}
		else
		{
			acc_arg = &c_args[0];
			vec_arg = &c_args[1];
		}
	}

	Type *vt    = (vec_arg->type && is_valid_type_ptr(vec_arg->type)) ? type_flatten(vec_arg->type) : rtype;
	bool is_ptr = false;
	if (vt && vt->type_kind == TYPE_POINTER && vt->pointer)
	{
		is_ptr = true;
		vt     = type_flatten(vt->pointer);
	}
	if (vec_arg->kind == CV_ADDRESS)
	{
		is_ptr = true;
	}

	bool is_vec = vt && (vt->type_kind == TYPE_ARRAY || vt->type_kind == TYPE_VECTOR || vt->type_kind == TYPE_SIMD_VECTOR);
	int len     = is_vec ? c_type_aggregate_len(vt) : 1;

	if (!is_vec)
	{
		if (acc_arg)
		{
			c_value_rvalue(c, acc_arg);
			c_value_rvalue(c, vec_arg);
			PRINTF("%s ___var_%d = (%s)(___var_%d + ___var_%d);\n", tn, temp, tn, acc_arg->var, vec_arg->var);
		}
		else if (is_ptr)
		{
			PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", tn, temp, tn, vec_arg->var);
		}
		else
		{
			c_value_rvalue(c, vec_arg);
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tn, temp, tn, vec_arg->var);
		}
		return;
	}

	const char *arrow = c_arrow(vec_arg);

	if (op == BUILTIN_REDUCE_MIN || op == BUILTIN_REDUCE_MAX)
	{
		const char *comp_op = (op == BUILTIN_REDUCE_MIN) ? "<" : ">";
		int start_k         = 1;
		if (acc_arg)
		{
			c_value_rvalue(c, acc_arg);
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tn, temp, tn, acc_arg->var);
			start_k = 0;
		}
		else
		{
			PRINTF("%s ___var_%d = ___var_%d%sptr[0];\n", tn, temp, vec_arg->var, arrow);
		}
		for (int k = start_k; k < len; k++)
		{
			PRINTF("if (___var_%d%sptr[%d] %s ___var_%d) ___var_%d = ___var_%d%sptr[%d];\n",
			       vec_arg->var, arrow, k, comp_op, temp, temp, vec_arg->var, arrow, k);
		}
		return;
	}

	const char *bin_op = "+";
	if (op == BUILTIN_REDUCE_MUL || op == BUILTIN_REDUCE_FMUL)
	{
		bin_op = "*";
	}
	else if (op == BUILTIN_REDUCE_AND)
	{
		bin_op = "&";
	}
	else if (op == BUILTIN_REDUCE_OR)
	{
		bin_op = "|";
	}
	else if (op == BUILTIN_REDUCE_XOR)
	{
		bin_op = "^";
	}

	int start_k = 1;
	if (acc_arg)
	{
		c_value_rvalue(c, acc_arg);
		PRINTF("%s ___var_%d = (%s)___var_%d;\n", tn, temp, tn, acc_arg->var);
		start_k = 0;
	}
	else
	{
		PRINTF("%s ___var_%d = ___var_%d%sptr[0];\n", tn, temp, vec_arg->var, arrow);
	}
	for (int k = start_k; k < len; k++)
	{
		PRINTF("___var_%d %s= ___var_%d%sptr[%d];\n", temp, bin_op, vec_arg->var, arrow, k);
	}
}

void c_emit_builtin_call(GenContext *c, CValue *value, Expr *expr)
{
	Expr *bfn               = expr->call_expr.function ? exprptrzero(expr->call_expr.function) : NULL;
	BuiltinFunction builtin = bfn && bfn->expr_kind == EXPR_BUILTIN ? bfn->builtin_expr.builtin : BUILTIN_TRAP;

	Expr **args    = expr->call_expr.arguments;
	int num_args   = vec_size(args);
	CValue *c_args = (CValue *)malloc(sizeof(CValue) * (num_args > 0 ? num_args : 1));
	for (int i = 0; i < num_args; i++)
	{
		c_args[i] = (CValue){0};
		if (args[i])
		{
			c_emit_expr(c, &c_args[i], args[i]);
			c_ensure_cvalue_var(c, &c_args[i], args[i]->type);
		}
	}

	Type *return_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
	bool has_return   = (return_type->type_kind != TYPE_VOID && !expr->call_expr.no_return);

	if (c_try_emit_table_builtin(c, value, return_type, builtin, c_args, num_args, has_return))
	{
		free(c_args);
		return;
	}

	switch (builtin)
	{
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMCOPY_INLINE:
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[2]);
				PRINTF("__c3_memcpy((void*)___var_%d, (void*)___var_%d, (size_t)___var_%d);\n", c_args[0].var, c_args[1].var, c_args[2].var);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_MEMMOVE:
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[2]);
				PRINTF("__c3_memmove((void*)___var_%d, (void*)___var_%d, (size_t)___var_%d);\n", c_args[0].var, c_args[1].var, c_args[2].var);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_MEMSET:
		case BUILTIN_MEMSET_INLINE:
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				PRINTF("__c3_memset((void*)___var_%d, (int)___var_%d, (size_t)___var_%d);\n", c_args[0].var, c_args[1].var, c_args[2].var);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_SYSCALL:
		{
			for (int i = 0; i < num_args; i++)
			{
				c_value_rvalue(c, &c_args[i]);
			}
			int temp = c_emit_temp_var(c, value, return_type);
			PRINTF("%s ___var_%d = (%s)syscall(", c_type_name(c, return_type), temp, c_type_name(c, return_type));
			for (int i = 0; i < num_args; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				PRINTF("(long)___var_%d", c_args[i].var);
			}
			PRINT(");\n");
			break;
		}
		case BUILTIN_ANY_MAKE:
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[1]);
				int temp = c_emit_temp_var(c, value, type_any);
				PRINTF("__c3_any__ ___var_%d = (__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)___var_%d };\n",
				       temp, c_args[0].var, c_args[1].var);
			}
			else
			{
				PRINTF("__c3_any__ ___var_%d = {0};\n", c_emit_temp_var(c, value, type_any));
			}
			break;
		case BUILTIN_MATRIX_MUL:
		{
			int M = 4, K = 4, N = 4;
			if (num_args >= 5)
			{
				if (args[2]->expr_kind == EXPR_CONST && args[2]->const_expr.const_kind == CONST_INTEGER)
				{
					M = (int)args[2]->const_expr.ixx.i.low;
				}
				if (args[3]->expr_kind == EXPR_CONST && args[3]->const_expr.const_kind == CONST_INTEGER)
				{
					K = (int)args[3]->const_expr.ixx.i.low;
				}
				if (args[4]->expr_kind == EXPR_CONST && args[4]->const_expr.const_kind == CONST_INTEGER)
				{
					N = (int)args[4]->const_expr.ixx.i.low;
				}
			}
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			PRINTF("%s ___var_%d = {0};\n", tn, temp);
			const char *arrow0 = c_arrow(&c_args[0]);
			const char *arrow1 = c_arrow(&c_args[1]);

			for (int col = 0; col < N; col++)
			{
				for (int row = 0; row < M; row++)
				{
					int out_idx = col * M + row;
					PRINTF("___var_%d.ptr[%d] = ", temp, out_idx);
					for (int k = 0; k < K; k++)
					{
						if (k > 0)
						{
							PRINT(" + ");
						}
						PRINTF("(___var_%d%sptr[%d] * ___var_%d%sptr[%d])",
						       c_args[0].var, arrow0, k * M + row,
						       c_args[1].var, arrow1, col * K + k);
					}
					PRINT(";\n");
				}
			}
			break;
		}
		case BUILTIN_MATRIX_TRANSPOSE:
		{
			int M = 4, N = 4;
			if (num_args >= 3)
			{
				if (args[1]->expr_kind == EXPR_CONST && args[1]->const_expr.const_kind == CONST_INTEGER)
				{
					M = (int)args[1]->const_expr.ixx.i.low;
				}
				if (args[2]->expr_kind == EXPR_CONST && args[2]->const_expr.const_kind == CONST_INTEGER)
				{
					N = (int)args[2]->const_expr.ixx.i.low;
				}
			}
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			PRINTF("%s ___var_%d = {0};\n", tn, temp);
			const char *arrow0 = c_arrow(&c_args[0]);
			for (int r = 0; r < M; r++)
			{
				for (int c_idx = 0; c_idx < N; c_idx++)
				{
					PRINTF("___var_%d.ptr[%d] = ___var_%d%sptr[%d];\n",
					       temp, r * N + c_idx, c_args[0].var, arrow0, c_idx * M + r);
				}
			}
			break;
		}
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
		case BUILTIN_BREAKPOINT:
			PRINT("__c3_abort();\n");
			c->current_block_live = false;
			value->var            = 0;
			value->type           = type_void;
			value->kind           = CV_VALUE;
			break;
		case BUILTIN_MIN:
		case BUILTIN_MAX:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			const char *op = (builtin == BUILTIN_MIN) ? "<" : ">";
			if (num_args >= 2)
			{
				Type *flat_ret = return_type ? type_flatten(return_type) : NULL;
				bool is_vec    = flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY);
				if (is_vec)
				{
					int len = c_type_aggregate_len(flat_ret);
					PRINTF("%s ___var_%d;\n", tn, temp);
					for (int k = 0; k < len; k++)
					{
						Type *t0  = c_args[0].type ? type_flatten(c_args[0].type) : NULL;
						bool ptr0 = (t0 && t0->type_kind == TYPE_POINTER) || (c_args[0].kind == CV_ADDRESS);
						if (ptr0 && t0 && t0->pointer)
						{
							t0 = type_flatten(t0->pointer);
						}
						bool vec0      = t0 && (t0->type_kind == TYPE_VECTOR || t0->type_kind == TYPE_SIMD_VECTOR || t0->type_kind == TYPE_ARRAY);
						const char *a0 = vec0 ? str_printf("___var_%d%sptr[%d]", c_args[0].var, c_arrow(&c_args[0]), k)
						                      : (ptr0 ? str_printf("*(%s*)___var_%d", c_type_name(c, t0), c_args[0].var) : str_printf("___var_%d", c_args[0].var));

						PRINTF("___var_%d.ptr[%d] = %s;\n", temp, k, a0);
						for (int a = 1; a < num_args; a++)
						{
							Type *ta  = c_args[a].type ? type_flatten(c_args[a].type) : NULL;
							bool ptra = (ta && ta->type_kind == TYPE_POINTER) || (c_args[a].kind == CV_ADDRESS);
							if (ptra && ta && ta->pointer)
							{
								ta = type_flatten(ta->pointer);
							}
							bool veca        = ta && (ta->type_kind == TYPE_VECTOR || ta->type_kind == TYPE_SIMD_VECTOR || ta->type_kind == TYPE_ARRAY);
							const char *next = veca ? str_printf("___var_%d%sptr[%d]", c_args[a].var, c_arrow(&c_args[a]), k)
							                        : (ptra ? str_printf("*(%s*)___var_%d", c_type_name(c, ta), c_args[a].var) : str_printf("___var_%d", c_args[a].var));
							PRINTF("if (%s %s ___var_%d.ptr[%d]) ___var_%d.ptr[%d] = %s;\n",
							       next, op, temp, k, temp, k, next);
						}
					}
				}
				else
				{
					c_value_rvalue(c, &c_args[0]);
					PRINTF("%s ___var_%d = ___var_%d;\n", tn, temp, c_args[0].var);
					for (int a = 1; a < num_args; a++)
					{
						c_value_rvalue(c, &c_args[a]);
						PRINTF("if (___var_%d %s ___var_%d) ___var_%d = ___var_%d;\n",
						       c_args[a].var, op, temp, temp, c_args[a].var);
					}
				}
			}
			else if (num_args == 1)
			{
				c_value_rvalue(c, &c_args[0]);
				PRINTF("%s ___var_%d = ___var_%d;\n", tn, temp, c_args[0].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_SELECT:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				if (c_type_is_aggregate(return_type))
				{
					int len          = c_type_aggregate_len(return_type);
					bool cond_is_agg = (c_args[0].type && c_type_is_aggregate(c_args[0].type));
					bool arg1_is_agg = (c_args[1].type && c_type_is_aggregate(c_args[1].type));
					bool arg2_is_agg = (c_args[2].type && c_type_is_aggregate(c_args[2].type));
					PRINTF("%s ___var_%d;\n", tn, temp);
					for (int i = 0; i < len; i++)
					{
						const char *c_str = cond_is_agg ? str_printf("___var_%d.ptr[%d]", c_args[0].var, i) : str_printf("___var_%d", c_args[0].var);
						const char *t_str = arg1_is_agg ? str_printf("___var_%d.ptr[%d]", c_args[1].var, i) : str_printf("___var_%d", c_args[1].var);
						const char *e_str = arg2_is_agg ? str_printf("___var_%d.ptr[%d]", c_args[2].var, i) : str_printf("___var_%d", c_args[2].var);
						PRINTF("___var_%d.ptr[%d] = %s ? %s : %s;\n", temp, i, c_str, t_str, e_str);
					}
				}
				else
				{
					PRINTF("%s ___var_%d = ___var_%d ? ___var_%d : ___var_%d;\n", tn, temp, c_args[0].var, c_args[1].var, c_args[2].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_GATHER:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				Type *flat_ret      = type_flatten(return_type);
				int len             = (flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY)) ? (int)flat_ret->array.len : 0;
				Type *elem_type     = flat_ret->array.base;
				const char *elem_tn = c_type_name(c, elem_type);
				PRINTF("%s ___var_%d;\n", tn, temp);
				for (int i = 0; i < len; i++)
				{
					PRINTF("if (___var_%d.ptr[%d]) {\n", c_args[1].var, i);
					PRINTF("\t___var_%d.ptr[%d] = *(%s*)___var_%d.ptr[%d];\n", temp, i, elem_tn, c_args[0].var, i);
					PRINTF("} else {\n");
					PRINTF("\t___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", temp, i, c_args[2].var, i);
					PRINTF("}\n");
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_SCATTER:
		{
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				Type *flat_val      = type_flatten(args[1]->type);
				int len             = (flat_val && (flat_val->type_kind == TYPE_VECTOR || flat_val->type_kind == TYPE_SIMD_VECTOR || flat_val->type_kind == TYPE_ARRAY)) ? (int)flat_val->array.len : 0;
				Type *elem_type     = flat_val->array.base;
				const char *elem_tn = c_type_name(c, elem_type);
				for (int i = 0; i < len; i++)
				{
					PRINTF("if (___var_%d.ptr[%d]) {\n", c_args[2].var, i);
					PRINTF("\t*(%s*)___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", elem_tn, c_args[0].var, i, c_args[1].var, i);
					PRINTF("}\n");
				}
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		}
		case BUILTIN_MASKED_LOAD:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				Type *flat_ret      = type_flatten(return_type);
				int len             = (flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY)) ? (int)flat_ret->array.len : 0;
				Type *elem_type     = flat_ret->array.base;
				const char *elem_tn = c_type_name(c, elem_type);
				PRINTF("%s ___var_%d;\n", tn, temp);
				for (int i = 0; i < len; i++)
				{
					PRINTF("if (___var_%d.ptr[%d]) {\n", c_args[1].var, i);
					PRINTF("\t___var_%d.ptr[%d] = ((%s*)___var_%d)[%d];\n", temp, i, elem_tn, c_args[0].var, i);
					PRINTF("} else {\n");
					PRINTF("\t___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", temp, i, c_args[2].var, i);
					PRINTF("}\n");
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_MASKED_STORE:
		{
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				Type *flat_val      = type_flatten(args[1]->type);
				int len             = (flat_val && (flat_val->type_kind == TYPE_VECTOR || flat_val->type_kind == TYPE_SIMD_VECTOR || flat_val->type_kind == TYPE_ARRAY)) ? (int)flat_val->array.len : 0;
				Type *elem_type     = flat_val->array.base;
				const char *elem_tn = c_type_name(c, elem_type);
				for (int i = 0; i < len; i++)
				{
					PRINTF("if (___var_%d.ptr[%d]) {\n", c_args[2].var, i);
					PRINTF("\t((%s*)___var_%d)[%d] = ___var_%d.ptr[%d];\n", elem_tn, c_args[0].var, i, c_args[1].var, i);
					PRINTF("}\n");
				}
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		}
		case BUILTIN_MASK_TO_INT:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				Type *vec          = type_flatten(args[0]->type);
				int len            = (vec && (vec->type_kind == TYPE_VECTOR || vec->type_kind == TYPE_SIMD_VECTOR || vec->type_kind == TYPE_ARRAY)) ? (int)vec->array.len : 0;
				const char *one_tn = (type_bit_size(return_type) > 64) ? "__c3_uint128" : "uint64_t";
				PRINTF("%s ___var_%d = 0;\n", tn, temp);
				for (int i = 0; i < len; i++)
				{
					PRINTF("if (___var_%d.ptr[%d]) ___var_%d |= (%s)((%s)1 << %d);\n",
					       c_args[0].var, i, temp, tn, one_tn, i);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = 0;\n", tn, temp);
			}
			break;
		}
		case BUILTIN_INT_TO_MASK:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				Type *flat_ret     = type_flatten(return_type);
				int len            = (flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY)) ? (int)flat_ret->array.len : 0;
				Type *arg0_type    = type_flatten(args[0]->type);
				BitSize bits       = type_bit_size(arg0_type);
				Type *u_type       = type_int_unsigned_by_bitsize(bits);
				const char *u_tn   = c_type_name(c, u_type);
				const char *one_tn = (bits > 64) ? "__c3_uint128" : "uint64_t";
				PRINTF("%s ___var_%d;\n", tn, temp);
				for (int i = 0; i < len; i++)
				{
					PRINTF("___var_%d.ptr[%d] = (((%s)___var_%d & ((%s)1 << %d)) != 0);\n",
					       temp, i, u_tn, c_args[0].var, one_tn, i);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_SWIZZLE:
		case BUILTIN_SWIZZLE2:
		{
			bool is_swizzle2 = (builtin == BUILTIN_SWIZZLE2);
			int temp         = c_emit_temp_var(c, value, return_type);
			const char *tn   = c_type_name(c, return_type);
			int first_mask   = is_swizzle2 ? 2 : 1;
			if (num_args >= first_mask)
			{
				c_value_rvalue(c, &c_args[0]);
				if (is_swizzle2)
				{
					c_value_rvalue(c, &c_args[1]);
				}
				Type *flat     = type_flatten(args[0]->type);
				int components = (int)flat->array.len;
				PRINTF("%s ___var_%d;\n", tn, temp);
				int out_len = num_args - first_mask;
				for (int i = 0; i < out_len; i++)
				{
					int mask_idx = first_mask + i;
					int idx      = (int)args[mask_idx]->const_expr.ixx.i.low;
					if (is_swizzle2 && idx >= components)
					{
						PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", temp, i, c_args[1].var, idx - components);
					}
					else
					{
						PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", temp, i, c_args[0].var, idx);
					}
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MOD:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			const char *op = (builtin == BUILTIN_EXACT_ADD) ? "+" : (builtin == BUILTIN_EXACT_SUB) ? "-"
			                                                    : (builtin == BUILTIN_EXACT_MUL)   ? "*"
			                                                    : (builtin == BUILTIN_EXACT_DIV)   ? "/"
			                                                                                       : "%";
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				PRINTF("%s ___var_%d = ___var_%d %s ___var_%d;\n", tn, temp, c_args[0].var, op, c_args[1].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_EXACT_NEG:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				PRINTF("%s ___var_%d = -___var_%d;\n", tn, temp, c_args[0].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_SUB:
		case BUILTIN_OVERFLOW_MUL:
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				int temp          = c_emit_temp_var(c, value, type_bool);
				const char *bname = (builtin == BUILTIN_OVERFLOW_SUB) ? "__builtin_sub_overflow" : (builtin == BUILTIN_OVERFLOW_MUL) ? "__builtin_mul_overflow"
				                                                                                                                     : "__builtin_add_overflow";
				Type *op_type     = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : type_int;
				PRINTF("bool ___var_%d = %s(___var_%d, ___var_%d, (%s*)___var_%d);\n",
				       temp, bname, c_args[0].var, c_args[1].var, c_type_name(c, op_type), c_args[2].var);
			}
			else
			{
				PRINTF("bool ___var_%d = false;\n", c_emit_temp_var(c, value, type_bool));
			}
			break;
		case BUILTIN_SAT_ADD:
		case BUILTIN_SAT_SUB:
		case BUILTIN_SAT_MUL:
			if (num_args >= 2)
			{
				c_emit_builtin_sat(c, value, return_type, builtin, &c_args[0], &c_args[1]);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_SAT_SHL:
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				PRINTF("%s ___var_%d = (___var_%d >= (sizeof(%s)*8)) ? 0 : (___var_%d << ___var_%d);\n",
				       tn, temp, c_args[1].var, tn, c_args[0].var, c_args[1].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_CTLZ:
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp        = c_emit_temp_var(c, value, return_type);
				const char *tn  = c_type_name(c, return_type);
				Type *st        = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : return_type;
				int bytes       = (int)type_size(st);
				const char *stn = c_type_name(c, st);
				if (bytes == 16)
				{
					PRINTF("%s ___var_%d = ((uint64_t)((__c3_uint128)___var_%d >> 64)) ? (%s)__builtin_clzll((uint64_t)((__c3_uint128)___var_%d >> 64)) : (((uint64_t)___var_%d) ? (%s)(64 + __builtin_clzll((uint64_t)___var_%d)) : (%s)128);\n",
					       tn, temp, c_args[0].var, tn, c_args[0].var, c_args[0].var, tn, c_args[0].var, tn);
				}
				else
				{
					PRINTF("%s ___var_%d = (___var_%d == 0) ? (%s)(sizeof(%s)*8) : (%s)(__builtin_clzll((uint64_t)___var_%d) - (64 - (int)sizeof(%s)*8));\n",
					       tn, temp, c_args[0].var, tn, stn, tn, c_args[0].var, stn);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_CTTZ:
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp        = c_emit_temp_var(c, value, return_type);
				const char *tn  = c_type_name(c, return_type);
				Type *st        = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : return_type;
				int bytes       = (int)type_size(st);
				const char *stn = c_type_name(c, st);
				if (bytes == 16)
				{
					PRINTF("%s ___var_%d = ((uint64_t)___var_%d) ? (%s)__builtin_ctzll((uint64_t)___var_%d) : (((uint64_t)((__c3_uint128)___var_%d >> 64)) ? (%s)(64 + __builtin_ctzll((uint64_t)((__c3_uint128)___var_%d >> 64))) : (%s)128);\n",
					       tn, temp, c_args[0].var, tn, c_args[0].var, c_args[0].var, tn, c_args[0].var, tn);
				}
				else
				{
					PRINTF("%s ___var_%d = (___var_%d == 0) ? (%s)(sizeof(%s)*8) : (%s)__builtin_ctzll((uint64_t)___var_%d);\n",
					       tn, temp, c_args[0].var, tn, stn, tn, c_args[0].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_POPCOUNT:
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				Type *st       = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : return_type;
				int bytes      = (int)type_size(st);
				if (bytes == 16)
				{
					PRINTF("%s ___var_%d = (%s)(__builtin_popcountll((uint64_t)___var_%d) + __builtin_popcountll((uint64_t)((__c3_uint128)___var_%d >> 64)));\n",
					       tn, temp, tn, c_args[0].var, c_args[0].var);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)__builtin_popcountll((uint64_t)___var_%d);\n", tn, temp, tn, c_args[0].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_BSWAP:
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				Type *st       = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : return_type;
				int bytes      = (int)type_size(st);
				if (bytes == 2)
				{
					PRINTF("%s ___var_%d = (%s)__builtin_bswap16((uint16_t)___var_%d);\n", tn, temp, tn, c_args[0].var);
				}
				else if (bytes == 4)
				{
					PRINTF("%s ___var_%d = (%s)__builtin_bswap32((uint32_t)___var_%d);\n", tn, temp, tn, c_args[0].var);
				}
				else if (bytes == 8)
				{
					PRINTF("%s ___var_%d = (%s)__builtin_bswap64((uint64_t)___var_%d);\n", tn, temp, tn, c_args[0].var);
				}
				else if (bytes == 1)
				{
					PRINTF("%s ___var_%d = ___var_%d;\n", tn, temp, c_args[0].var);
				}
				else if (bytes == 16)
				{
					PRINTF("%s ___var_%d = (%s)((((__c3_uint128)__builtin_bswap64((uint64_t)___var_%d)) << 64) | ((__c3_uint128)__builtin_bswap64((uint64_t)((__c3_uint128)___var_%d >> 64))));\n",
					       tn, temp, tn, c_args[0].var, c_args[0].var);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)__builtin_bswap64((uint64_t)___var_%d);\n", tn, temp, tn, c_args[0].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_BITREVERSE:
			if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp        = c_emit_temp_var(c, value, return_type);
				const char *tn  = c_type_name(c, return_type);
				Type *st        = (c_args[0].type && is_valid_type_ptr(c_args[0].type)) ? c_safe_type_lower(c_args[0].type) : return_type;
				const char *stn = c_type_name(c, st);
				int bytes       = (int)type_size(st);
				if (bytes == 16)
				{
					PRINTF("%s ___var_%d = 0;\n", tn, temp);
					PRINTF("for (size_t ___i = 0; ___i < 128; ___i++) { if ((((__c3_uint128)___var_%d) >> ___i) & 1) ___var_%d |= ((__c3_uint128)1 << (127 - ___i)); }\n",
					       c_args[0].var, temp);
				}
				else
				{
					PRINTF("%s ___var_%d = 0;\n", tn, temp);
					PRINTF("for (size_t ___i = 0; ___i < sizeof(%s)*8; ___i++) { if (((uint64_t)___var_%d >> ___i) & 1) ___var_%d |= ((%s)1 << (sizeof(%s)*8 - 1 - ___i)); }\n",
					       stn, c_args[0].var, temp, tn, stn);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_FSHL:
			if (num_args >= 3)
			{
				c_emit_builtin_fshl(c, value, return_type, &c_args[0], &c_args[1], &c_args[2]);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_FSHR:
			if (num_args >= 3)
			{
				c_emit_builtin_fshr(c, value, return_type, &c_args[0], &c_args[1], &c_args[2]);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_ABS:
			if (num_args >= 1)
			{
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				Type *flat_ret = return_type ? type_flatten(return_type) : NULL;
				bool is_vec    = flat_ret && (flat_ret->type_kind == TYPE_VECTOR || flat_ret->type_kind == TYPE_SIMD_VECTOR || flat_ret->type_kind == TYPE_ARRAY);
				if (is_vec)
				{
					int len       = c_type_aggregate_len(flat_ret);
					Type *elem_t  = flat_ret->array.base ? type_flatten(flat_ret->array.base) : return_type;
					bool is_float = type_is_float(elem_t);

					const char *arrow = c_arrow(&c_args[0]);
					PRINTF("%s ___var_%d;\n", tn, temp);
					for (int k = 0; k < len; k++)
					{
						if (is_float)
						{
							PRINTF("___var_%d.ptr[%d] = __builtin_fabs(___var_%d%sptr[%d]);\n",
							       temp, k, c_args[0].var, arrow, k);
						}
						else
						{
							PRINTF("___var_%d.ptr[%d] = (___var_%d%sptr[%d] < 0 ? -___var_%d%sptr[%d] : ___var_%d%sptr[%d]);\n",
							       temp, k, c_args[0].var, arrow, k, c_args[0].var, arrow, k, c_args[0].var, arrow, k);
						}
					}
				}
				else
				{
					c_value_rvalue(c, &c_args[0]);
					if (type_is_float(return_type))
					{
						PRINTF("%s ___var_%d = (__builtin_fabs(___var_%d));\n", tn, temp, c_args[0].var);
					}
					else
					{
						PRINTF("%s ___var_%d = (___var_%d < 0 ? -___var_%d : ___var_%d);\n", tn, temp, c_args[0].var, c_args[0].var, c_args[0].var);
					}
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_EXPECT:
		case BUILTIN_EXPECT_WITH_PROBABILITY:
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[0]);
				c_value_rvalue(c, &c_args[1]);
				int temp = c_emit_temp_var(c, value, return_type);
				PRINTF("%s ___var_%d = __builtin_expect(___var_%d, ___var_%d);\n", c_type_name(c, return_type), temp, c_args[0].var, c_args[1].var);
			}
			else if (num_args >= 1)
			{
				c_value_rvalue(c, &c_args[0]);
				int temp = c_emit_temp_var(c, value, return_type);
				PRINTF("%s ___var_%d = ___var_%d;\n", c_type_name(c, return_type), temp, c_args[0].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_PREFETCH:
			if (num_args >= 1)
			{
				PRINTF("__builtin_prefetch((void*)___var_%d);\n", c_args[0].var);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_FRAMEADDRESS:
			PRINTF("%s ___var_%d = __builtin_frame_address(0);\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type));
			break;
		case BUILTIN_RETURNADDRESS:
			PRINTF("%s ___var_%d = __builtin_return_address(0);\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type));
			break;
		case BUILTIN_VOLATILE_LOAD:
			if (num_args >= 1)
			{
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				PRINTF("%s ___var_%d = *(volatile %s*)___var_%d;\n", tn, temp, tn, c_args[0].var);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_VOLATILE_STORE:
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[1]);
				Type *vt = (c_args[1].type && is_valid_type_ptr(c_args[1].type)) ? c_safe_type_lower(c_args[1].type) : type_void;
				PRINTF("*(volatile %s*)___var_%d = ___var_%d;\n", c_type_name(c, vt), c_args[0].var, c_args[1].var);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_UNALIGNED_LOAD:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			PRINTF("%s ___var_%d;\n", tn, temp);
			if (num_args >= 1)
			{
				PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", temp, c_args[0].var, tn);
			}
			break;
		}
		case BUILTIN_UNALIGNED_STORE:
			if (num_args >= 2)
			{
				Type *vt = (c_args[1].type && is_valid_type_ptr(c_args[1].type)) ? c_safe_type_lower(c_args[1].type) : type_void;
				if (c_args[1].kind == CV_ADDRESS)
				{
					PRINTF("__c3_memcpy((void*)___var_%d, (void*)___var_%d, sizeof(%s));\n", c_args[0].var, c_args[1].var, c_type_name(c, vt));
				}
				else
				{
					PRINTF("__c3_memcpy((void*)___var_%d, &___var_%d, sizeof(%s));\n", c_args[0].var, c_args[1].var, c_type_name(c, vt));
				}
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_ATOMIC_LOAD:
			if (num_args >= 1)
			{
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				if (type_is_float(return_type))
				{
					const char *u_int = (type_size(return_type) == 8) ? "uint64_t" : "uint32_t";
					PRINTF("%s ___var_%d;\n", tn, temp);
					PRINT("{\n");
					PRINTF("\t%s _raw = __atomic_load_n((%s*)___var_%d, __ATOMIC_SEQ_CST);\n", u_int, u_int, c_args[0].var);
					PRINTF("\t__c3_memcpy(&___var_%d, &_raw, sizeof(%s));\n", temp, tn);
					PRINT("}\n");
				}
				else if (return_type->type_kind == TYPE_BOOL)
				{
					PRINTF("bool ___var_%d = (bool)__atomic_load_n((uint8_t*)___var_%d, __ATOMIC_SEQ_CST);\n", temp, c_args[0].var);
				}
				else
				{
					PRINTF("%s ___var_%d = __atomic_load_n((%s*)___var_%d, __ATOMIC_SEQ_CST);\n", tn, temp, tn, c_args[0].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_ATOMIC_STORE:
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[1]);
				Type *vt = (c_args[1].type && is_valid_type_ptr(c_args[1].type)) ? c_safe_type_lower(c_args[1].type) : type_void;
				if (type_is_float(vt))
				{
					const char *u_int = (type_size(vt) == 8) ? "uint64_t" : "uint32_t";
					PRINT("{\n");
					PRINTF("\t%s _raw;\n", u_int);
					PRINTF("\t__c3_memcpy(&_raw, &___var_%d, sizeof(%s));\n", c_args[1].var, u_int);
					PRINTF("\t__atomic_store_n((%s*)___var_%d, _raw, __ATOMIC_SEQ_CST);\n", u_int, c_args[0].var);
					PRINT("}\n");
				}
				else if (vt->type_kind == TYPE_BOOL)
				{
					PRINTF("__atomic_store_n((uint8_t*)___var_%d, (uint8_t)___var_%d, __ATOMIC_SEQ_CST);\n", c_args[0].var, c_args[1].var);
				}
				else
				{
					PRINTF("__atomic_store_n((%s*)___var_%d, ___var_%d, __ATOMIC_SEQ_CST);\n", c_type_name(c, vt), c_args[0].var, c_args[1].var);
				}
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_ATOMIC_FETCH_ADD:
		case BUILTIN_ATOMIC_FETCH_SUB:
		case BUILTIN_ATOMIC_FETCH_AND:
		case BUILTIN_ATOMIC_FETCH_OR:
		case BUILTIN_ATOMIC_FETCH_XOR:
		case BUILTIN_ATOMIC_FETCH_NAND:
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
		{
			int temp          = c_emit_temp_var(c, value, return_type);
			const char *tn    = c_type_name(c, return_type);
			const char *aname = (builtin == BUILTIN_ATOMIC_FETCH_SUB) ? "__atomic_fetch_sub" : (builtin == BUILTIN_ATOMIC_FETCH_AND)    ? "__atomic_fetch_and"
			                                                                               : (builtin == BUILTIN_ATOMIC_FETCH_OR)       ? "__atomic_fetch_or"
			                                                                               : (builtin == BUILTIN_ATOMIC_FETCH_XOR)      ? "__atomic_fetch_xor"
			                                                                               : (builtin == BUILTIN_ATOMIC_FETCH_NAND)     ? "__atomic_fetch_nand"
			                                                                               : (builtin == BUILTIN_ATOMIC_FETCH_EXCHANGE) ? "__atomic_exchange_n"
			                                                                                                                            : "__atomic_fetch_add";
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[1]);
				if (type_is_float(return_type) && (builtin == BUILTIN_ATOMIC_FETCH_ADD || builtin == BUILTIN_ATOMIC_FETCH_SUB))
				{
					const char *u_int = (type_size(return_type) == 8) ? "uint64_t" : "uint32_t";
					const char *op    = (builtin == BUILTIN_ATOMIC_FETCH_SUB) ? "-" : "+";
					PRINTF("%s ___var_%d;\n", tn, temp);
					PRINT("{\n");
					PRINTF("\tunion { %s f; %s i; } _old, _new;\n", tn, u_int);
					PRINTF("\t_old.f = *(volatile %s*)___var_%d;\n", tn, c_args[0].var);
					PRINTF("\tdo {\n");
					PRINTF("\t\t_new.f = _old.f %s ___var_%d;\n", op, c_args[1].var);
					PRINTF("\t} while (!__atomic_compare_exchange_n((%s*)___var_%d, &_old.i, _new.i, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));\n", u_int, c_args[0].var);
					PRINTF("\t___var_%d = _old.f;\n", temp);
					PRINT("}\n");
				}
				else if (return_type->type_kind == TYPE_BOOL)
				{
					PRINTF("bool ___var_%d = (bool)%s((uint8_t*)___var_%d, (uint8_t)___var_%d, __ATOMIC_SEQ_CST);\n",
					       temp, aname, c_args[0].var, c_args[1].var);
				}
				else
				{
					PRINTF("%s ___var_%d = %s((%s*)___var_%d, ___var_%d, __ATOMIC_SEQ_CST);\n", tn, temp, aname, tn, c_args[0].var, c_args[1].var);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_ATOMIC_FETCH_MAX:
		case BUILTIN_ATOMIC_FETCH_MIN:
		{
			int temp       = c_emit_temp_var(c, value, return_type);
			const char *tn = c_type_name(c, return_type);
			const char *op = (builtin == BUILTIN_ATOMIC_FETCH_MIN) ? "<" : ">";
			if (num_args >= 2)
			{
				c_value_rvalue(c, &c_args[1]);
				if (type_is_float(return_type))
				{
					const char *u_int = (type_size(return_type) == 8) ? "uint64_t" : "uint32_t";
					PRINTF("%s ___var_%d;\n", tn, temp);
					PRINT("{\n");
					PRINTF("\tunion { %s f; %s i; } _old, _new;\n", tn, u_int);
					PRINTF("\t_old.f = *(volatile %s*)___var_%d;\n", tn, c_args[0].var);
					PRINTF("\tdo {\n");
					PRINTF("\t\t_new.f = (_old.f %s ___var_%d) ? _old.f : ___var_%d;\n", op, c_args[1].var, c_args[1].var);
					PRINTF("\t} while (!__atomic_compare_exchange_n((%s*)___var_%d, &_old.i, _new.i, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));\n", u_int, c_args[0].var);
					PRINTF("\t___var_%d = _old.f;\n", temp);
					PRINT("}\n");
				}
				else if (return_type->type_kind == TYPE_BOOL)
				{
					PRINTF("bool ___var_%d;\n", temp);
					PRINT("{\n");
					PRINTF("\tuint8_t _old = *(volatile uint8_t*)___var_%d;\n", c_args[0].var);
					PRINTF("\tuint8_t _new;\n");
					PRINTF("\tdo {\n");
					PRINTF("\t\t_new = (_old %s (uint8_t)___var_%d) ? _old : (uint8_t)___var_%d;\n", op, c_args[1].var, c_args[1].var);
					PRINTF("\t} while (!__atomic_compare_exchange_n((uint8_t*)___var_%d, &_old, _new, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));\n", c_args[0].var);
					PRINTF("\t___var_%d = (bool)_old;\n", temp);
					PRINT("}\n");
				}
				else
				{
					PRINTF("%s ___var_%d;\n", tn, temp);
					PRINT("{\n");
					PRINTF("\t%s _old = *(volatile %s*)___var_%d;\n", tn, tn, c_args[0].var);
					PRINTF("\t%s _new;\n", tn);
					PRINTF("\tdo {\n");
					PRINTF("\t\t_new = (_old %s ___var_%d) ? _old : ___var_%d;\n", op, c_args[1].var, c_args[1].var);
					PRINTF("\t} while (!__atomic_compare_exchange_n((%s*)___var_%d, &_old, _new, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));\n", tn, c_args[0].var);
					PRINTF("\t___var_%d = _old;\n", temp);
					PRINT("}\n");
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", tn, temp, c_type_zero_literal(return_type));
			}
			break;
		}
		case BUILTIN_COMPARE_EXCHANGE:
			if (num_args >= 3)
			{
				c_value_rvalue(c, &c_args[1]);
				c_value_rvalue(c, &c_args[2]);
				int temp       = c_emit_temp_var(c, value, return_type);
				const char *tn = c_type_name(c, return_type);
				if (type_is_float(return_type))
				{
					const char *u_int = (type_size(return_type) == 8) ? "uint64_t" : "uint32_t";
					PRINTF("%s ___var_%d;\n", tn, temp);
					PRINT("{\n");
					PRINTF("\t%s _exp, _des;\n", u_int);
					PRINTF("\t__c3_memcpy(&_exp, &___var_%d, sizeof(%s));\n", c_args[1].var, u_int);
					PRINTF("\t__c3_memcpy(&_des, &___var_%d, sizeof(%s));\n", c_args[2].var, u_int);
					PRINTF("\t__atomic_compare_exchange_n((%s*)___var_%d, &_exp, _des, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);\n", u_int, c_args[0].var);
					PRINTF("\t__c3_memcpy(&___var_%d, &_exp, sizeof(%s));\n", temp, tn);
					PRINT("}\n");
				}
				else if (return_type->type_kind == TYPE_BOOL)
				{
					PRINTF("uint8_t ___var_%d_expected = (uint8_t)___var_%d;\n", temp, c_args[1].var);
					PRINTF("__atomic_compare_exchange_n((uint8_t*)___var_%d, &___var_%d_expected, (uint8_t)___var_%d, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);\n",
					       c_args[0].var, temp, c_args[2].var);
					PRINTF("bool ___var_%d = (bool)___var_%d_expected;\n", temp, temp);
				}
				else
				{
					PRINTF("%s ___var_%d_expected = ___var_%d;\n", tn, temp, c_args[1].var);
					PRINTF("__atomic_compare_exchange_n((%s*)___var_%d, &___var_%d_expected, ___var_%d, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);\n",
					       tn, c_args[0].var, temp, c_args[2].var);
					PRINTF("%s ___var_%d = ___var_%d_expected;\n", tn, temp, temp);
				}
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		case BUILTIN_FENCE:
			PRINT("__atomic_thread_fence(__ATOMIC_SEQ_CST);\n");
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			break;
		case BUILTIN_SYSCLOCK:
			PRINTF("%s ___var_%d = 0;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type));
			break;
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_FADD:
		case BUILTIN_REDUCE_MUL:
		case BUILTIN_REDUCE_FMUL:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MIN:
		case BUILTIN_REDUCE_MAX:
			if (num_args >= 1)
			{
				c_emit_builtin_reduce(c, value, return_type, builtin, c_args, num_args);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			break;
		default:
			if (has_return)
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type), c_type_zero_literal(return_type));
			}
			else
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
			}
			break;
	}
	free(c_args);
}
