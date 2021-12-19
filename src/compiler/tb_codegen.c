#include "codegen_internal.h"

#if TB_BACKEND

#include <tb.h>

typedef struct
{
	Module *code_module;
	const char *object_filename;

	TB_Module *module;
	TB_FeatureSet features;

	Decl *current_func_decl;
	TB_Function *function;
} GenContext;

typedef enum
{
	BE_VALUE,
	BE_ADDRESS,
	BE_ADDRESS_FAILABLE,
	BE_BOOLEAN,
} BackendValueKind;

typedef struct
{
	BackendValueKind kind: 5;
	AlignSize alignment;

	Type *type; // Should never be a distinct or canonical type.

	TB_Register value;
	TB_Register failable; // TODO what even is a failable?
} BEValue;

static void tinybackend_emit_expr(GenContext *c, BEValue *value, Expr *expr);

// Per instance i think?
void tinybackend_codegen_setup()
{

}

static inline bool tinybackend_value_is_addr(BEValue *value)
{ return value->kind == BE_ADDRESS || value->kind == BE_ADDRESS_FAILABLE; }

static inline bool tinybackend_value_is_bool(BEValue *value)
{ return value->kind == BE_BOOLEAN; }

static TB_DataType tinybackend_get_type(Type *type)
{
	type = type_lowering(type);
	uint8_t elements = 1;
	if (type->type_kind == TYPE_VECTOR)
	{
		elements = (uint8_t)type->vector.len;
		type = type->vector.base;
	}
	switch (type->type_kind)
	{
		case TYPE_U8:
		case TYPE_I8:
			return TB_TYPE_I8(elements);
		case TYPE_U16:
		case TYPE_I16:
			return TB_TYPE_I16(elements);
		case TYPE_U32:
		case TYPE_I32:
			return TB_TYPE_I32(elements);
		case TYPE_U64:
		case TYPE_I64:
			return TB_TYPE_I64(elements);
		case TYPE_POINTER:
			assert(elements == 1);
			return TB_TYPE_PTR();
		case TYPE_I128:
		case TYPE_U128:
			return TB_TYPE_I128(elements);
		case TYPE_BOOL:
			return TB_TYPE_BOOL(elements);
		case TYPE_F64:
			return TB_TYPE_F64(elements);
		case TYPE_F32:
			return TB_TYPE_F32(elements);
		case TYPE_VOID:
			assert(elements == 1);
			return TB_TYPE_VOID();
		case TYPE_F16:
			TODO
		case TYPE_F128:
			TODO
		case TYPE_VECTOR:
			UNREACHABLE
		default:
			// Structs? Unions?
			TODO
	}
}

static inline TB_Register decl_ref(Decl *decl)
{
	if (decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED) return decl_ref(decl->var.alias);

	return (TB_Register)((uintptr_t)decl->backend_ref);
}

TB_Register tinybackend_emit_load_aligned(GenContext *c, TB_DataType dt, TB_Register pointer, AlignSize alignment)
{
	TB_Register value = tb_inst_load(c->function, dt, pointer, alignment);
	return value;
}

void tinybackend_store_self_aligned(GenContext *c, TB_Register pointer, TB_Register value, Type *type)
{
	tb_inst_store(c->function, tinybackend_get_type(type), pointer, value, type_abi_alignment(type));
}

void tinybackend_store_bevalue(GenContext *c, TB_DataType type, BEValue *destination, BEValue *value)
{
	assert(tinybackend_value_is_addr(destination));
	if (value->kind == BE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->value = tinybackend_emit_load_aligned(c,
		                                             tinybackend_get_type(value->type),
		                                             value->value,
		                                             value->alignment);
		value->kind = BE_VALUE;
	}

	AlignSize alignment = destination->alignment;
	switch (value->kind)
	{
		case BE_BOOLEAN:
			value->value = tb_inst_zxt(c->function, value->value, TB_TYPE_I8(1));
			value->kind = BE_VALUE;
			FALLTHROUGH;
		case BE_VALUE:
			tb_inst_store(c->function,
			              tinybackend_get_type(destination->type),
			              destination->value,
			              value->value,
			              alignment ? alignment : type_abi_alignment(value->type));
			return;
		case BE_ADDRESS:
		{
			// Here we do an optimized(?) memcopy.
			ByteSize size = type_size(value->type);
			TB_Register copy_size = tb_inst_iconst(c->function,
			                                       size <= UINT32_MAX ? TB_TYPE_I32(1) : TB_TYPE_I64(1),
			                                       size);

			TB_Register source = value->value;
			tb_inst_memcpy(c->function,
			               destination->value,
			               value->value, copy_size,
			               value->alignment ? (int)value->alignment : (int)type_abi_alignment(value->type));
			return;
		}
		default:
			UNREACHABLE
	}
}

void tinybackend_value_set_address(BEValue *value, TB_Register tb_value, Type *type)
{
	value->value = tb_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_ADDRESS;
	value->type = type_lowering(type);
}

void tinybackend_value_set_decl_address(BEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	tinybackend_value_set_address(value, decl_ref(decl), decl->type);
	value->alignment = decl->alignment;
}

void tinybackend_value_set(BEValue *value, TB_Register reg, Type *type)
{
	type = type_lowering(type);
	assert(reg || type == type_void);
	value->value = reg;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;
}

static void tinybackend_emit_const_expr(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;

	switch (expr->const_expr.const_kind)
	{
		case CONST_INTEGER:
		{
			TB_Register reg;
			Int128 i = expr->const_expr.ixx.i;

			TB_DataType dt = tinybackend_get_type(type);
			printf("Const int type: %d\n", dt.type);

			switch (expr->const_expr.ixx.type)
			{
				case TYPE_I128:
				case TYPE_U128:
					reg = tb_inst_iconst128(c->function, dt, (TB_Int128){ .lo = i.low, .hi = i.high });
					break;
				default:
					reg = tb_inst_iconst(c->function, dt, i.low);
					break;
			}

			tinybackend_value_set(value, reg, type);
			return;
		}
		default:
			TODO
	}
}

BEValue tinybackend_emit_assign_expr(GenContext *c, BEValue *ref, Expr *expr, TB_Register failable)
{
	assert(ref->kind == BE_ADDRESS || ref->kind == BE_ADDRESS_FAILABLE);

	BEValue value;
	tinybackend_emit_expr(c, &value, expr);
	tinybackend_store_bevalue(c, tinybackend_get_type(ref->type), ref, &value);

	return value;
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

static void tinybackend_emit_member_addr(GenContext *c, BEValue *value, Decl *parent, Decl *member)
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
				TB_Register ref = tb_inst_member_access(c->function, value->value, (int32_t)found->offset);

				tinybackend_value_set_address(value, ref, member->type);
				value->alignment = found->alignment;
			}
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}

void tinybackend_emit_access_addr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	tinybackend_emit_expr(c, be_value, parent);
	Decl *member = expr->access_expr.ref;

	tinybackend_emit_member_addr(c, be_value, type_lowering(parent->type)->decl, member);
}

static inline TB_ArithmaticBehavior tinybackend_get_arith_behavior(Type *type)
{
	if (active_target.feature.trap_on_wrap)
	{
		return type_is_unsigned(type) ? TB_UNSIGNED_TRAP_ON_WRAP : TB_SIGNED_TRAP_ON_WRAP;
	}

	return TB_CAN_WRAP;
}

void tinybackend_value_rvalue(GenContext *c, BEValue *value)
{
	if (value->kind != BE_ADDRESS && value->kind != BE_ADDRESS_FAILABLE)
	{
		if (value->type->type_kind == TYPE_BOOL && value->kind != BE_BOOLEAN)
		{
			//value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
			//value->kind = BE_BOOLEAN;
			TODO
		}
		return;
	}
	//llvm_value_fold_failable(c, value);
	value->value = tinybackend_emit_load_aligned(c,
	                                             tinybackend_get_type(value->type),
	                                             value->value,
	                                             value->alignment ? value->alignment : type_abi_alignment(value->type));

	if (value->type->type_kind == TYPE_BOOL)
	{
		//value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
		//value->kind = BE_BOOLEAN;
		//return;
		TODO
	}
	value->kind = BE_VALUE;
}

void tinybackend_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_loaded, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		//gencontext_emit_logical_and_or(c, be_value, expr, binary_op);
		//return;
		TODO
	}
	BEValue lhs;
	if (lhs_loaded)
	{
		lhs = *lhs_loaded;
	}
	else
	{
		tinybackend_emit_expr(c, &lhs, expr->binary_expr.left);
	}
	tinybackend_value_rvalue(c, &lhs);

	BEValue rhs;
	tinybackend_emit_expr(c, &rhs, expr->binary_expr.right);
	tinybackend_value_rvalue(c, &rhs);

	/*EMIT_LOC(c, expr);
	if (binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_emit_comparison(c, be_value, &lhs, &rhs, binary_op);
		return;
	}*/

	Type *lhs_type = lhs.type;
	Type *rhs_type = rhs.type;
	Type *vector_type = lhs_type->type_kind == TYPE_VECTOR ? lhs_type->vector.base : NULL;
	bool is_float = type_is_float(lhs_type) || (vector_type && type_is_float(vector_type));

	TB_Register val = TB_NULL_REG;
	TB_Register lhs_value = lhs.value;
	TB_Register rhs_value = rhs.value;

	TB_DataType dt = tinybackend_get_type(lhs_type);
	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			if (is_float)
			{
				val = tb_inst_fmul(c->function, dt, lhs_value, rhs_value);
				break;
			}

			// TODO(NeGate): review this later, maybe it shouldn't be NO_WRAP
			val = tb_inst_mul(c->function, dt, lhs_value, rhs_value, TB_ASSUME_NUW);
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
				val = tb_inst_fsub(c->function, dt, lhs_value, rhs_value);
				break;
			}
			val = tb_inst_mul(c->function, dt, lhs_value, rhs_value, tinybackend_get_arith_behavior(lhs_type));
			break;
		case BINARYOP_ADD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				TODO
			}
			if (is_float)
			{
				val = tb_inst_fadd(c->function, dt, lhs_value, rhs_value);
				break;
			}
			val = tb_inst_add(c->function, dt, lhs_value, rhs_value, tinybackend_get_arith_behavior(lhs_type));
			break;
		case BINARYOP_DIV:
			//llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero", TOKLOC(expr->span.loc));
			if (is_float)
			{
				val = tb_inst_fdiv(c->function, dt, lhs_value, rhs_value);
				break;
			}

			val = tb_inst_div(c->function, dt, lhs_value, rhs_value, !type_is_unsigned(lhs_type));
			break;
		case BINARYOP_MOD:
		{
			TODO
		}
		case BINARYOP_SHR:
			if (type_is_unsigned(lhs_type))
			{
				val = tb_inst_shr(c->function, dt, lhs_value, rhs_value);
				return;
			}

			val = tb_inst_sar(c->function, dt, lhs_value, rhs_value);
			break;
		case BINARYOP_SHL:
			val = tb_inst_shl(c->function, dt, lhs_value, rhs_value, TB_ASSUME_NUW);
			break;
		case BINARYOP_BIT_AND:
			val = tb_inst_and(c->function, dt, lhs_value, rhs_value);
			break;
		case BINARYOP_BIT_OR:
			val = tb_inst_or(c->function, dt, lhs_value, rhs_value);
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
	}
	assert(val);
	tinybackend_value_set(be_value, val, expr->type);
}

void tinybackend_value_addr(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS) return;

	TB_Register temp = tb_inst_local(c->function, type_size(value->type), type_alloca_alignment(value->type));
	tinybackend_store_self_aligned(c, temp, value->value, value->type);
	tinybackend_value_set_address(value, temp, value->type);
}

static void tinybackend_emit_binary_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	/*if (binary_op >= BINARYOP_ASSIGN && expr_is_vector_index(expr->binary_expr.left))
	{
		tinybackend_emit_vector_assign_expr(c, be_value, expr);
		return;
	}*/
	printf("A\n");
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		BEValue addr;
		tinybackend_emit_expr(c, &addr, expr->binary_expr.left);
		tinybackend_value_addr(c, &addr);
		tinybackend_emit_binary(c, be_value, expr, &addr, base_op);
		tinybackend_store_bevalue(c, tinybackend_get_type(addr.type), &addr, be_value);
		return;
	}
	printf("B\n");
	if (binary_op == BINARYOP_ASSIGN)
	{
		Expr *left = expr->binary_expr.left;
		tinybackend_emit_expr(c, be_value, expr->binary_expr.left);
		assert(tinybackend_value_is_addr(be_value));

		*be_value = tinybackend_emit_assign_expr(c, be_value, expr->binary_expr.right, TB_NULL_REG /* failable_ref */);
		return;
	}

	tinybackend_emit_binary(c, be_value, expr, NULL, binary_op);
}

static void tinybackend_emit_expr(GenContext *c, BEValue *value, Expr *expr)
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
		case EXPR_CONST_IDENTIFIER:
			tinybackend_value_set_decl_address(value, expr->identifier_expr.decl);
			return;
		default:
			TODO
	}
}

static TB_Register tinybackend_emit_local_decl(GenContext *c, Decl *decl)
{
	if (decl->var.is_static)
	{
		TODO
	}

	TB_DataType dt = tinybackend_get_type(decl->type);
	TypeSize size = type_size(decl->type);
	AlignSize align = decl->alignment;

	TB_Register reg = tb_inst_local(c->function, size, align);
	decl->backend_ref = (void *)((size_t)reg);

	printf("Local declare: %s (size: %ld, align: %u)\n", decl->type->name, (long)size, align);

	if (decl->var.failable_ref)
	{
		TODO
	}

	Expr *init = decl->var.init_expr;
	if (init)
	{
		// If we don't have undef, then make an assign.
		if (init->expr_kind != EXPR_UNDEF)
		{
			BEValue value;
			tinybackend_value_set_decl_address(&value, decl);
			tinybackend_emit_assign_expr(c,
			                             &value,
			                             decl->var.init_expr,
			                             (TB_Register)((uintptr_t)decl->var.failable_ref));
		}
	}
	else
	{
		Type *type = type_lowering(decl->type);

		// Normal case, zero init.
		if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
		{
			tb_inst_store(c->function, dt, reg, tb_inst_iconst(c->function, dt, 0), align);
		}
		else
		{
			TODO
		}
	}

	return reg;
}

static void tinybackend_emit_stmt(GenContext *c, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_DECLARE_STMT:
			tinybackend_emit_local_decl(c, ast->declare_stmt);
			break;
		case AST_RETURN_STMT:
			//gencontext_emit_return(c, ast);
			tb_inst_ret(c->function, TB_TYPE_VOID(), TB_NULL_REG);
			break;
		default:
			break;
	}
}

static void tinybackend_emit_function_body(GenContext *c, Decl *decl)
{
	printf("Function: %s\n", decl->external_name);

	c->current_func_decl = decl;

	// TODO(NeGate): Actually interpret the return value
	c->function = tb_function_create(c->module, decl->external_name, TB_TYPE_VOID());

	VECEACH(decl->func_decl.body->compound_stmt.stmts, i)
	{
		tinybackend_emit_stmt(c, decl->func_decl.body->compound_stmt.stmts[i]);
	}

	tb_function_print(c->function, stdout);
}

static void tinybackend_gen_context(GenContext *c)
{
	scratch_buffer_clear();
	StringSlice slice = strtoslice(c->code_module->name->module);
	while (true)
	{
		StringSlice part = strnexttok(&slice, ':');
		scratch_buffer_append_len(part.ptr, part.len);
		if (!slice.len) break;
		slice.ptr++;
		slice.len--;
		scratch_buffer_append_char('.');
	}
	const char *result = scratch_buffer_to_string();
	// TODO filename should depend on platform.
	c->object_filename = strformat("%s.o", result);
}

// Generate context per module (single threaded)
void *tinybackend_gen(Module *module)
{
	GenContext *c = ccalloc(sizeof(GenContext), 1);
	c->code_module = module;

	// TODO identify target architecture
	c->module = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_LINUX, &c->features, TB_OPT_O0, 1, false);

	tinybackend_gen_context(c);

	printf("Module: %.*s\n", module->name->len, module->name->module);

	// Forward decls
	VECEACH(module->contexts, j)
	{
		Context *context = module->contexts[j];

		printf("  External symbols: %d\n", vec_size(context->external_symbol_list));
		printf("  Functions: %d\n", vec_size(context->functions));
	}

	VECEACH(module->contexts, j)
	{
		Context *context = module->contexts[j];

		VECEACH(context->functions, i)
		{
			Decl *decl = context->functions[i];
			if (decl->func_decl.body) tinybackend_emit_function_body(c, decl);
		}
	}

	tb_module_compile(c->module);
	return c;
}

// Compile module (multi threaded)
const char *tinybackend_codegen(void *context)
{
	GenContext *c = (GenContext *)context;

	// Write out the object file
	FILE *file = fopen(c->object_filename, "wb");
	tb_module_export(c->module, file);
	fclose(file);

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

