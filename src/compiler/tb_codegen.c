#include "tilde_internal.h"

#if TB_BACKEND

static void tinybackend_emit_expr(TbContext *c, TBEValue *value, Expr *expr);
static inline void tilde_emit_block(TbContext *c, TB_Label label);
static inline void tinybackend_emit_exprid(TbContext *c, TBEValue *value, ExprId id)
{
	tinybackend_emit_expr(c, value, exprptr(id));
}

static TB_Register tilde_value_rvalue_get(TbContext *c, TBEValue *value);
static void TBE_VALUE_set_reg(TBEValue *value, TB_Register reg, Type *type);
// Per instance i think?
void tinybackend_codegen_setup()
{

}

static inline bool tinybackend_value_is_addr(TBEValue *value)
{ return value->kind == TBE_ADDRESS || value->kind == TBE_ADDRESS_FAILABLE; }


static TB_CallingConv tilde_call_convention(CallABI abi)
{
	switch (abi)
	{
		case CALL_C:
			return TB_CDECL;
		case CALL_X86_STD:
			return TB_STDCALL;
		default:
			FATAL_ERROR("Unsupported call convention for TildeBE");
	}
}

TB_DataType tbtype(Type *type)
{
	type = type_lowering(type);
	uint8_t elements = 1;
	if (type->type_kind == TYPE_VECTOR)
	{
		elements = (uint8_t)type->array.len;
		switch (type->array.base->type_kind)
		{
			case TYPE_U8:
			case TYPE_I8:
				return tb_vector_type(TB_I8, elements);
			case TYPE_U16:
			case TYPE_I16:
				return tb_vector_type(TB_I16, elements);
			case TYPE_U32:
			case TYPE_I32:
				return tb_vector_type(TB_I32, elements);
			case TYPE_U64:
			case TYPE_I64:
				return tb_vector_type(TB_I64, elements);
			case TYPE_I128:
			case TYPE_U128:
				FATAL_ERROR("Unsupported int128");
			case TYPE_F32:
				return tb_vector_type(TB_F32, elements);
			case TYPE_F64:
				return tb_vector_type(TB_F64, elements);
			case TYPE_F16:
				FATAL_ERROR("Unsupported f16");
			case TYPE_F128:
				FATAL_ERROR("Unsupported f128");
			default:
				UNREACHABLE
		}
	}
	switch (type->type_kind)
	{
		case TYPE_U8:
		case TYPE_I8:
			return TB_TYPE_I8;
		case TYPE_U16:
		case TYPE_I16:
			return TB_TYPE_I16;
		case TYPE_U32:
		case TYPE_I32:
			return TB_TYPE_I32;
		case TYPE_U64:
		case TYPE_I64:
			return TB_TYPE_I64;
		case TYPE_POINTER:
			return TB_TYPE_PTR;
		case TYPE_I128:
		case TYPE_U128:
			FATAL_ERROR("Unsupported int128");
		case TYPE_BOOL:
			return TB_TYPE_BOOL;
		case TYPE_F64:
			return TB_TYPE_F64;
		case TYPE_F32:
			return TB_TYPE_F32;
		case TYPE_VOID:
			return TB_TYPE_VOID;
		case TYPE_F16:
			FATAL_ERROR("Unsupported f16");
		case TYPE_F128:
			FATAL_ERROR("Unsupported f128");
		case TYPE_VECTOR:
			UNREACHABLE
		default:
			// Structs? Unions?
			TODO
	}
}

static TB_DataType tilde_get_abi_type(AbiType type)
{
	if (abi_type_is_type(type)) return tbtype(type.type);
	TODO
}

TB_DataType tilde_get_int_type_of_bytesize(int byte_size)
{
	switch (byte_size)
	{
		case 1:
			return TB_TYPE_I8;
		case 2:
			return TB_TYPE_I16;
		case 4:
			return TB_TYPE_I32;
		case 8:
			return TB_TYPE_I64;
		default:
			FATAL_ERROR("Unsupported size");
	}

}
static void param_expand(TB_DataType **params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (ArraySize i = type->array.len; i > 0; i--)
			{
				param_expand(params_ref, type->array.base);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				param_expand(params_ref, members[i]->type);
			}
			return;
		}
		case TYPE_ENUM:
		case TYPE_ANYERR:
		case TYPE_FAULTTYPE:
			param_expand(params_ref, type_lowering(type));
			return;
		case TYPE_UNION:
		{
			ByteSize largest = 0;
			Type *largest_type = NULL;
			Decl **members = type->decl->strukt.members;
			// Clang: Unions can be here only in degenerative cases - all the fields are same
			// after flattening. Thus we have to use the "largest" field.
			VECEACH(members, i)
			{
				if (type_size(type) > largest)
				{
					largest = type_size(type);
					type = type->canonical;
				}
			}
			if (!largest) return;
			param_expand(params_ref, largest_type);
			return;
		}
		default:
			vec_add(*params_ref, tbtype(type));
			return;
	}

}

static inline void add_func_type_param(Type *param_type, ABIArgInfo *arg_info, TB_DataType **params)
{
	arg_info->param_index_start = (MemberIndex)vec_size(*params);
	switch (arg_info->kind)
	{
		case ABI_ARG_IGNORE:
			break;
		case ABI_ARG_INDIRECT:
			vec_add(*params, TB_TYPE_PTR);
			break;
		case ABI_ARG_EXPAND_COERCE:
			vec_add(*params, tilde_get_abi_type(arg_info->coerce_expand.lo));
			if (abi_type_is_valid(arg_info->coerce_expand.hi))
			{
				vec_add(*params, tilde_get_abi_type(arg_info->coerce_expand.hi));
			}
			break;
		case ABI_ARG_EXPAND:
			// Expanding a structs
			param_expand(params, param_type->canonical);
			// If we have padding, add it here.
			if (arg_info->expand.padding_type)
			{
				vec_add(*params, tbtype(arg_info->expand.padding_type));
			}
			break;
		case ABI_ARG_DIRECT:
			vec_add(*params, tbtype(param_type));
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			// Normal direct.
			TB_DataType coerce_type = tbtype(arg_info->direct_struct_expand.type);
			for (unsigned idx = 0; idx < arg_info->direct_struct_expand.elements; idx++)
			{
				vec_add(*params, coerce_type);
			}
			break;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
			vec_add(*params, tilde_get_int_type_of_bytesize(type_size(param_type)));
			break;
		case ABI_ARG_DIRECT_COERCE:
		{
			// Normal direct.
			TB_DataType coerce_type = tbtype(arg_info->direct_coerce_type);
			vec_add(*params, coerce_type);
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
			// Pairs are passed by param.
			vec_add(*params, tilde_get_abi_type(arg_info->direct_pair.lo));
			vec_add(*params, tilde_get_abi_type(arg_info->direct_pair.hi));
			break;
	}
	arg_info->param_index_end = (MemberIndex)vec_size(*params);
}

static TB_FunctionPrototype *tilde_get_function_type(TB_Module *module, FunctionPrototype *prototype)
{
	if (prototype->tb_prototype) return prototype->tb_prototype;

	TB_DataType *params = NULL;
	TB_DataType return_type = TB_TYPE_VOID;

	Type *call_return_type = prototype->abi_ret_type;
	ABIArgInfo *ret_arg_info = prototype->ret_abi_info;

	ret_arg_info->param_index_end = 0;
	ret_arg_info->param_index_start = 0;

	switch (ret_arg_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE;
		case ABI_ARG_INDIRECT:
			vec_add(params, TB_TYPE_PTR);
			return_type = TB_TYPE_VOID;
			break;
		case ABI_ARG_EXPAND_COERCE:
		{
			TB_DataType lo = tilde_get_abi_type(ret_arg_info->direct_pair.lo);
			if (!abi_type_is_valid(ret_arg_info->direct_pair.hi))
			{
				return_type = lo;
				break;
			}
			TB_DataType hi = tilde_get_abi_type(ret_arg_info->direct_pair.hi);
			TODO
			// return_type = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_IGNORE:
			return_type = TB_TYPE_VOID;
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			TB_DataType lo = tilde_get_abi_type(ret_arg_info->direct_pair.lo);
			TB_DataType hi = tilde_get_abi_type(ret_arg_info->direct_pair.hi);
			TODO // return_type = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_DIRECT:
			return_type = tbtype(call_return_type);
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			UNREACHABLE
		case ABI_ARG_DIRECT_COERCE_INT:
			return_type = tilde_get_int_type_of_bytesize(type_size(call_return_type));
			break;
		case ABI_ARG_DIRECT_COERCE:
			return_type = tbtype(ret_arg_info->direct_coerce_type);
			break;
	}

	// If it's failable and it's not void (meaning ret_abi_info will be NULL)
	if (prototype->ret_by_ref)
	{
		add_func_type_param(type_get_ptr(type_lowering(prototype->ret_by_ref_type)),
		                    prototype->ret_by_ref_abi_info,
		                    &params);
	}

	// Add in all of the required arguments.
	VECEACH(prototype->params, i)
	{
		add_func_type_param(prototype->params[i], prototype->abi_args[i], &params);
	}

	unsigned param_count = vec_size(params);
	TB_FunctionPrototype *tb_proto = tb_prototype_create(module,
														 tilde_call_convention(prototype->call_abi),
														 return_type, param_count, prototype->variadic == VARIADIC_RAW);
	tb_prototype_add_params(tb_proto, param_count, params);
	prototype->tb_prototype = tb_proto;
	return tb_proto;
}

static inline TB_Register decl_ref(Decl *decl)
{
	if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED) return decl_ref(decl->var.alias);
	return decl->tb_register;
}

TB_Register tilde_load_aligned(TbContext *c, TB_DataType dt, TB_Register pointer, AlignSize alignment)
{
	TB_Register value = tb_inst_load(c->f, dt, pointer, alignment);
	return value;
}

void tilde_store_self_aligned(TbContext *c, TB_Register pointer, TB_Register value, Type *type)
{
	tb_inst_store(c->f, tbtype(type), pointer, value, type_abi_alignment(type));
}



void tinybackend_store_value(TbContext *c, TBEValue *destination, TBEValue *value)
{
	assert(tinybackend_value_is_addr(destination));
	TB_DataType type = tbtype(destination->type);
	if (value->kind == TBE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->reg = tilde_load_aligned(c, type, value->reg, value->alignment);
		value->kind = TBE_VALUE;
	}

	AlignSize alignment = destination->alignment;
	assert(alignment);
	switch (value->kind)
	{
		case TBE_VALUE:
			tb_inst_store(c->f, type, destination->reg, value->reg, alignment);
			return;
		case TBE_ADDRESS:
		{
			// Here we do an optimized(?) memcopy.
			ByteSize size = type_size(value->type);
			TB_Register copy_size = tb_inst_uint(c->f,
			                                     size <= UINT32_MAX ? TB_TYPE_I32 : TB_TYPE_I64,
			                                     size);

			alignment = type_min_alignment(destination->alignment, value->alignment);
			tb_inst_memcpy(c->f, destination->reg, value->reg, copy_size, alignment);
			return;
		}
		default:
			UNREACHABLE
	}
}



void TBE_VALUE_set(TBEValue *value, TB_Register reg, Type *type)
{
	type = type_lowering(type);
	assert(reg || type == type_void);
	value->reg = reg;
	value->alignment = type_abi_alignment(type);
	value->kind = TBE_VALUE;
	value->type = type;
}

static void tinybackend_emit_const_expr(TbContext *c, TBEValue *value, Expr *expr)
{
	Type *type = type_flatten(expr->type);
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
			TBE_VALUE_set(value, tb_inst_float(c->f, tbtype(type), expr->const_expr.fxx.f), type);
			return;
		case CONST_INTEGER:
		{
			TB_Register reg;
			Int128 i = expr->const_expr.ixx.i;

			TB_DataType dt = tbtype(type);
			printf("Const int type: %d\n", dt.type);

			switch (expr->const_expr.ixx.type)
			{

				case TYPE_I128:
				case TYPE_U128:
					FATAL_ERROR("TB does not yet support int128");
					//reg = tb_inst_iconst128(c->function, dt, (TB_Int128){ .lo = i.low, .hi = i.high });
					break;
				default:
					reg = type_kind_is_signed(expr->const_expr.ixx.type) ? tb_inst_sint(c->f, dt, i.low) : tb_inst_uint(c->f, dt, i.low);
					break;
			}
			TBE_VALUE_set(value, reg, type);
			return;
		}
		default:
			TODO
	}
}

static int find_member_index(Decl *parent, Decl *member)
{
	VECEACH(parent->strukt.members, i)
	{
		Decl *maybe_member = parent->strukt.members[i];
		if (member == maybe_member)
		{
			return (int)i;
		}
		if (!maybe_member->name)
		{
			if (find_member_index(maybe_member, member) != -1) return (int)i;
		}
	}
	return -1;
}

static void tinybackend_emit_member_addr(TbContext *c, TBEValue *value, Decl *parent, Decl *member)
{
	assert(member->resolve_status == RESOLVE_DONE);

	Decl *found = NULL;
	do
	{
		int index = find_member_index(parent, member);
		assert(index > -1);
		found = parent->strukt.members[index];

		// Unlike LLVM, TinyBackend doesn't handle aggregates for you
		// so we need offsetof to perform member accesses
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
			{
				TODO
			}
				break;
			case TYPE_STRUCT:
			{
				TB_Register ref = tb_inst_member_access(c->f, value->reg, (int32_t)found->offset);

				value_set_address_abi_aligned(value, ref, member->type);
				value->alignment = found->alignment;
			}
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}

void tinybackend_emit_access_addr(TbContext *c, TBEValue *TBE_VALUE, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	tinybackend_emit_expr(c, TBE_VALUE, parent);
	Decl *member = expr->access_expr.ref;

	tinybackend_emit_member_addr(c, TBE_VALUE, type_lowering(parent->type)->decl, member);
}

static inline TB_ArithmaticBehavior tinybackend_get_arith_behavior(Type *type)
{
	if (active_target.feature.trap_on_wrap)
	{
		return type_is_unsigned(type) ? TB_UNSIGNED_TRAP_ON_WRAP : TB_SIGNED_TRAP_ON_WRAP;
	}

	return TB_CAN_WRAP;
}




static inline void tilde_emit_block(TbContext *c, TB_Label label)
{
	tb_inst_label(c->f, label);
}

TB_Register tilde_value_rvalue_get(TbContext *c, TBEValue *value)
{
	if (value->kind == TBE_VALUE) return value->reg;
	//llvm_value_fold_failable(c, value);
	return tilde_load_aligned(c,
	                          tbtype(value->type),
	                          value->reg,
	                          value->alignment ? value->alignment : type_abi_alignment(value->type));

}

void tinybackend_emit_binary(TbContext *c, TBEValue *TBE_VALUE, Expr *expr, TBEValue *lhs_loaded, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		//TbContext_emit_logical_and_or(c, TBE_VALUE, expr, binary_op);
		//return;
		TODO
	}
	TBEValue lhs;
	if (lhs_loaded)
	{
		lhs = *lhs_loaded;
	}
	else
	{
		tinybackend_emit_exprid(c, &lhs, expr->binary_expr.left);
	}
	value_rvalue(c, &lhs);

	TBEValue rhs;
	tinybackend_emit_exprid(c, &rhs, expr->binary_expr.right);
	value_rvalue(c, &rhs);

	/*EMIT_LOC(c, expr);
	if (binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_emit_comparison(c, TBE_VALUE, &lhs, &rhs, binary_op);
		return;
	}*/

	Type *lhs_type = lhs.type;
	Type *rhs_type = rhs.type;
	Type *vector_type = lhs_type->type_kind == TYPE_VECTOR ? lhs_type->array.base : NULL;
	bool is_float = type_is_float(lhs_type) || (vector_type && type_is_float(vector_type));

	TB_Register val = TB_NULL_REG;
	TB_Register lhs_value = lhs.reg;
	TB_Register rhs_value = rhs.reg;

	TB_DataType dt = tbtype(lhs_type);
	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			if (is_float)
			{
				val = tb_inst_fmul(c->f, lhs_value, rhs_value);
				break;
			}

			// TODO(NeGate): review this later, maybe it shouldn't be NO_WRAP
			val = tb_inst_mul(c->f, lhs_value, rhs_value, TB_ASSUME_NUW);
			break;
		case BINARYOP_SUB:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				/*bool is_constant = LLVMIsConstant(lhs_value) && LLVMIsConstant(rhs_value);
				if (lhs_type == rhs_type)
				{
					LLVMTypeRef int_type = llvm_get_type(c, type_iptrdiff);
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_type, ""),
									   LLVMBuildPtrToInt(c->builder, rhs_value, int_type, ""), "");
					val = LLVMBuildExactSDiv(c->builder, val, llvm_const_int(c, type_iptrdiff, type_abi_alignment(lhs_type->pointer)), "");
					break;
				}
				rhs_value = LLVMConstNeg(rhs_value);
				val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);*/
				TODO
			}
			if (is_float)
			{
				val = tb_inst_fsub(c->f, lhs_value, rhs_value);
				break;
			}
			val = tb_inst_mul(c->f, lhs_value, rhs_value, tinybackend_get_arith_behavior(lhs_type));
			break;
		case BINARYOP_ADD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				TODO
			}
			if (is_float)
			{
				val = tb_inst_fadd(c->f, lhs_value, rhs_value);
				break;
			}
			val = tb_inst_add(c->f, lhs_value, rhs_value, tinybackend_get_arith_behavior(lhs_type));
			break;
		case BINARYOP_DIV:
			//llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero", TOKLOC(expr->span.loc));
			if (is_float)
			{
				val = tb_inst_fdiv(c->f, lhs_value, rhs_value);
				break;
			}

			val = tb_inst_div(c->f, lhs_value, rhs_value, !type_is_unsigned(lhs_type));
			break;
		case BINARYOP_MOD:
		{
			TODO
		}
		case BINARYOP_SHR:
			if (type_is_unsigned(lhs_type))
			{
				val = tb_inst_shr(c->f, lhs_value, rhs_value);
				return;
			}

			val = tb_inst_sar(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_SHL:
			val = tb_inst_shl(c->f, lhs_value, rhs_value, TB_ASSUME_NUW);
			break;
		case BINARYOP_BIT_AND:
			val = tb_inst_and(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_BIT_OR:
			val = tb_inst_or(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_BIT_XOR:
			TODO
			break;
		case BINARYOP_EQ:
		case BINARYOP_NE:
		case BINARYOP_GE:
		case BINARYOP_GT:
		case BINARYOP_LE:
		case BINARYOP_LT:
			UNREACHABLE
		case BINARYOP_AND:
		case BINARYOP_OR:
		case BINARYOP_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			UNREACHABLE
		case BINARYOP_OR_ERR:
			TODO
			break;
	}
	assert(val);
	TBE_VALUE_set(TBE_VALUE, val, expr->type);
}


static void tinybackend_emit_binary_expr(TbContext *c, TBEValue *TBE_VALUE, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	/*if (binary_op >= BINARYOP_ASSIGN && expr_is_vector_index(expr->binary_expr.left))
	{
		tinybackend_emit_vector_assign_expr(c, TBE_VALUE, expr);
		return;
	}*/
	printf("A\n");
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		TBEValue addr;
		tinybackend_emit_exprid(c, &addr, expr->binary_expr.left);
		value_rvalue(c, &addr);
		tinybackend_emit_binary(c, TBE_VALUE, expr, &addr, base_op);
		tinybackend_store_value(c, &addr, TBE_VALUE);
		return;
	}
	printf("B\n");
	if (binary_op == BINARYOP_ASSIGN)
	{
		tinybackend_emit_exprid(c, TBE_VALUE, expr->binary_expr.left);
		assert(tinybackend_value_is_addr(TBE_VALUE));

		*TBE_VALUE = tilde_emit_assign_expr(c, TBE_VALUE, exprptr(expr->binary_expr.right), TB_NULL_REG /* failable_ref */);
		return;
	}

	tinybackend_emit_binary(c, TBE_VALUE, expr, NULL, binary_op);
}

static void tinybackend_emit_expr(TbContext *c, TBEValue *value, Expr *expr)
{
	printf("expr->expr_kind = %d\n", expr->expr_kind);

	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			tinybackend_emit_const_expr(c, value, expr);
			return;
		case EXPR_ACCESS:
			tinybackend_emit_access_addr(c, value, expr);
			return;
		case EXPR_BINARY:
			tinybackend_emit_binary_expr(c, value, expr);
			return;
		case EXPR_IDENTIFIER:
			value_set_decl(value, expr->identifier_expr.decl);
			return;
		default:
			TODO
	}
}



static void TBE_VALUE_set_reg(TBEValue *value, TB_Register reg, Type *type)
{
	value->reg = reg;
	value->kind = TBE_ADDRESS;
	value->type = type_lowering(type);
}


static TB_Register tilde_emit_local_decl(TbContext *c, Decl *decl)
{
	// 1. Get the declaration and the LLVM type.
	Type *var_type = type_lowering(type_no_fail(decl->type));

	// 2. In the case we have a static variable,
	//    then we essentially treat this as a global.
	if (decl->var.is_static)
	{
		if (IS_FAILABLE(decl))
		{
			scratch_buffer_clear();
			scratch_buffer_append(decl_get_extname(decl));
			scratch_buffer_append(".f");
			TB_InitializerID initializer = tb_initializer_create(c->module, type_size(type_anyerr), type_alloca_alignment(type_anyerr), 1);
			decl->var.tb_failable_reg = tb_global_create(c->module, scratch_buffer_to_string(), TB_STORAGE_DATA, TB_LINKAGE_PRIVATE);
			tb_global_set_initializer(c->module, decl->var.tb_failable_reg, initializer);
		}
		TB_InitializerID static_initializer = tb_initializer_create(c->module, type_size(var_type), type_alloca_alignment(var_type), 1);
		decl->tb_register = tb_global_create(c->module, "tempglobal", TB_STORAGE_DATA, TB_LINKAGE_PRIVATE);
		tb_global_set_initializer(c->module, decl->tb_register, static_initializer);
		tilde_emit_global_initializer(c, decl);
		return decl->tb_register;
	}
	tilde_emit_local_var_alloca(c, decl);
	TB_Register reg = decl->tb_register;
	Expr *init = decl->var.init_expr;
	if (IS_FAILABLE(decl))
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.tb_failable_reg = tb_inst_local(c->f, type_size(type_anyerr), type_alloca_alignment(type_anyerr));
		// Only clear out the result if the assignment isn't a failable.
	}

	TBEValue value;
	value_set_decl(&value, decl);
	if (init)
	{
		tilde_emit_assign_expr(c, &value, decl->var.init_expr, decl->var.tb_failable_reg);
	}
	else if (!decl->var.no_init)
	{
		if (decl->var.tb_failable_reg)
		{
			tilde_store_self_aligned(c, tilde_get_zero(c, type_anyerr), decl->var.tb_failable_reg, type_anyerr);
		}
		tilde_store_value_zero(c, &value);
	}
	return reg;
}


static void print_callback(void *ptr, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
}


static void tilde_emit_function_body(TbContext *c, Decl *decl)
{
	printf("Function: %s\n", decl_get_extname(decl));

	c->current_func_decl = decl;

	TB_FunctionPrototype *prototype = tilde_get_function_type(c->module, decl->type->func.prototype);
	TB_Function *function = tb_prototype_build(c->module, prototype, decl_get_extname(decl), TB_LINKAGE_PUBLIC);
	c->f = function;

	AstId current = astptr(decl->func_decl.body)->compound_stmt.first_stmt;
	while (current)
	{
		tilde_emit_stmt(c, ast_next(&current));
	}

	// Insert a return (and defer) if needed.
	/*
	if (context->current_block && !LLVMGetBasicBlockTerminator(context->current_block))
	{
		assert(!decl->func_decl.body->compound_stmt.defer_list.end);
		llvm_emit_defer(context, decl->func_decl.body->compound_stmt.defer_list.start, 0);
		llvm_emit_return_implicit(context);
	}

	// erase alloca point
	if (LLVMGetInstructionParent(alloca_point))
	{
		context->alloca_point = NULL;
		LLVMInstructionEraseFromParent(alloca_point);
	}

	LLVMDisposeBuilder(context->builder);
	context->builder = NULL;

	if (llvm_use_debug(context))
	{
		llvm_debug_scope_pop(context);
	}

	context->builder = prev_builder;
	context->function = prev_function;
*/
	tb_function_print(c->f, &print_callback, NULL);

}



static TB_Arch tilde_get_arch(void)
{
	switch (platform_target.arch)
	{
		case ARCH_TYPE_AARCH64:
			return TB_ARCH_AARCH64;
		case ARCH_TYPE_X86_64:
			return TB_ARCH_X86_64;
		default:
			FATAL_ERROR("Unsupported architecture for TB backend.");
	}
}

static TB_System tilde_get_system(void)
{
	switch (platform_target.os)
	{
		case OS_TYPE_LINUX:
			return TB_SYSTEM_LINUX;
		case OS_TYPE_WIN32:
			return TB_SYSTEM_WINDOWS;
		case OS_TYPE_MACOSX:
			// TODO add when support exists.
			// return TB_SYSTEM_MACOS;
			return TB_SYSTEM_LINUX;
		case OS_TYPE_UNKNOWN:
			return TB_SYSTEM_LINUX;
		default:
			FATAL_ERROR("Unsupported OS");
	}
}

// Generate context per module (single threaded)
void *tinybackend_gen(Module *module)
{
	if (!vec_size(module->units)) return NULL;
	TbContext *c = ccalloc(sizeof(TbContext), 1);
	c->code_module = module;

	c->module = tb_module_create(tilde_get_arch(), tilde_get_system(), TB_DEBUGFMT_NONE, &c->features);

	const char *result = module_create_object_file_name(module);

	scratch_buffer_clear();
	scratch_buffer_printf("%s%s", result, get_object_extension());
	c->object_filename = scratch_buffer_copy();

	printf("Module: %.*s\n", module->name->len, module->name->module);
	// Forward decls

	VECEACH(module->units, j)
	{
		CompilationUnit *unit = module->units[j];

		printf("  Functions: %d\n", vec_size(unit->functions));
	}

	VECEACH(module->units, j)
	{
		CompilationUnit *unit = module->units[j];

		VECEACH(unit->functions, i)
		{
			Decl *decl = unit->functions[i];
			if (decl->func_decl.body) tilde_emit_function_body(c, decl);
		}
	}

//	tb_module_compile(c->module);
TODO
	return c;
}

// Compile module (multi threaded)
const char *tinybackend_codegen(void *context)
{
	TbContext *c = (TbContext *)context;

	// Write out the object file
	tb_module_export(c->module, c->object_filename);

	return c->object_filename;
}

#else

const char *tinybackend_codegen(void *context)
{
	FATAL_ERROR("Not enabled");
}
void *tinybackend_gen(Module *module)
{
	FATAL_ERROR("Not enabled");
}
void tinybackend_codegen_setup()
{
	FATAL_ERROR("TB not enabled");
}

#endif

