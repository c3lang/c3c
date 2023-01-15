#include "tilde_internal.h"



static TB_System os_to_tilde_system(OsType target)
{
	switch (target)
	{
		case OS_TYPE_LINUX:
			return TB_SYSTEM_LINUX;
		case OS_TYPE_MACOSX:
			return TB_SYSTEM_MACOS;
		case OS_TYPE_WIN32:
			return TB_SYSTEM_WINDOWS;
		case OS_TYPE_WASI:
		case OS_TYPE_EMSCRIPTEN:
			return TB_SYSTEM_WEB;
		default:
			error_exit("Unsupported system for TB compilation, use LLVM.");
	}
}
static TB_Arch arch_to_tilde_arch(ArchType target)
{
	switch (target)
	{
		case ARCH_TYPE_AARCH64:
			return TB_ARCH_AARCH64;
		case ARCH_TYPE_X86_64:
			return TB_ARCH_X86_64;
		case ARCH_TYPE_WASM32:
			return TB_ARCH_WASM32;
		default:
			error_exit("Unsupported architecture for TB compilation, use LLVM.");
	}
	UNREACHABLE
}


static void tilde_emit_function_body(TildeContext *c, Decl *decl)
{
	TB_Function *fn = tb_function_create(c->module, decl_get_extname(decl), tilde_linkage_for_decl(decl));
	decl->backend_value = fn;
	tb_function_set_prototype(fn, (TB_FunctionPrototype*)tilde_get_func_prototype(c, decl->type->function.prototype));
	vec_add(c->functions, fn);
	c->curr_func = decl;
	c->cur_func.prototype = decl->type->function.prototype;
	c->cur_func.rtype = decl->type->function.prototype->rtype;
	c->cur_func.name = decl->name;
	c->f = fn;
	if (decl->func_decl.body) tilde_emit_stmt(c, astptr(decl->func_decl.body));
}

static TildeContext *tilde_gen_module(Module *module, TB_FeatureSet *feature_set)
{
	if (!vec_size(module->units)) return NULL;
	TildeContext *context = CALLOCS(TildeContext);
	TB_Module *tb_module = tb_module_create(arch_to_tilde_arch(platform_target.arch),
	                                        os_to_tilde_system(platform_target.os),
	                                        feature_set, false);
	codegen_setup_object_names(module, &context->ir_filename, &context->asm_filename, &context->object_filename);
	context->module = tb_module;

	FOREACH_BEGIN(CompilationUnit *unit, module->units)

		FOREACH_BEGIN(Decl *decl, unit->functions)
			if (decl->func_decl.attr_test && !active_target.testing) continue;
			if (decl->func_decl.body) tilde_emit_function_body(context, decl);
		FOREACH_END();

	FOREACH_END();

	return context;
}

void **tilde_gen(Module **modules, unsigned module_count)
{
	if (!module_count) return NULL;
	TB_FeatureSet feature_set;
	switch (platform_target.arch)
	{
		case ARCH_TYPE_AARCH64:
			feature_set.aarch64.bf16 = false;
		case ARCH_TYPE_X86_64:
			feature_set.x64.avx = platform_target.x64.x86_vector_capability >= X86VECTOR_AVX;
		default:
			break;
	}
	TildeContext **contexts = NULL;
	for (unsigned i = 0; i < module_count; i++)
	{
		TildeContext *c = tilde_gen_module(modules[i], &feature_set);
		if (c) vec_add(contexts, c);
	}
	return (void**)contexts;
}

void tinybackend_codegen_setup()
{
}


static TB_DataType tilde_get_abi_type(AbiType type)
{
	if (abi_type_is_type(type)) return tildetype(type.type);
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
			vec_add(*params_ref, tildetype(type));
			return;
	}

}

static void callback(void* user_data, const char* fmt, ...)
{
	va_list list;
	va_start(list, fmt);
	vprintf(fmt, list);
	va_end(list);
}

// Compile module (multi threaded)
const char *tilde_codegen(void *context)
{
	TildeContext *c = (TildeContext *)context;
	bool is_win32 = platform_target.os == OS_TYPE_WIN32;
	TB_DebugFormat debug_format = is_win32 ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_DWARF;
	if (active_target.debug_info == DEBUG_INFO_NONE) debug_format = TB_DEBUGFMT_NONE;

	FOREACH_BEGIN(TB_Function *function, c->functions)
		if (!tb_module_compile_function(c->module, function, TB_ISEL_FAST))
		{
			error_exit("Failed to compile function.");
		}
		tb_function_print(function, &callback, NULL, true);
	FOREACH_END();

	const char *object_name = NULL;
	if (active_target.emit_object_files)
	{
		if (!tb_exporter_write_files(c->module, TB_FLAVOR_OBJECT, debug_format, 1, &c->object_filename))
		{
			error_exit("Failed to create object file %s.", c->object_filename);
		}
		object_name = c->object_filename;
	}

	if (active_target.emit_asm && false)
	{
		if (!tb_exporter_write_files(c->module, TB_FLAVOR_ASSEMBLY, debug_format, 1, &c->asm_filename))
		{
			error_exit("Failed to create asm %s.", c->asm_filename);
		}
	}

	tb_module_destroy(c->module);
	return object_name;


	return c->object_filename;
}

