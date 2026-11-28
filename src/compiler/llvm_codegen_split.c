#include "llvm_codegen_internal.h"

#ifndef SPLIT_MODULE_SHARES
#define SPLIT_MODULE_SHARES 32
#endif
#ifndef SPLIT_MODULE_MIN_COST
#define SPLIT_MODULE_MIN_COST 10000
#endif

static void llvm_split_collect_bodies(Module *module, Decl ***bodies_ref)
{
	vec_resize(*bodies_ref, 0);
	Decl **bodies = *bodies_ref;
	FOREACH(CompilationUnit *, unit, module->units)
	{
		FOREACH(Decl *, func, unit->functions)
		{
			if (func->is_live && func->func_decl.body && !decl_skip_benchtest_function(func)) vec_add(bodies, func);
		}
		FOREACH(Decl *, func, unit->lambdas)
		{
			if (func->is_live) vec_add(bodies, func);
		}
		if (unit->main_function && unit->main_function->is_synthetic) vec_add(bodies, unit->main_function);
		FOREACH(Decl *, func, unit->methods)
		{
			if (func->is_live && func->func_decl.body) vec_add(bodies, func);
		}
	}
	*bodies_ref = bodies;
}

static int64_t llvm_split_calculate_module_cost(Module *module, Decl ***func_ref)
{
	if (!vec_size(module->units)) return 0;
	if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(module)) return 0;
	int64_t cost = 0;
	llvm_split_collect_bodies(module, func_ref);
	FOREACH(Decl *, func, *func_ref) cost += func->func_decl.backend_cost;
	return cost;
}

int64_t llvm_split_calculate_module_shares(Module **modules, int module_count, int64_t **costs_ref)
{
	if (compiler.build.split_modules != SPLIT_MODULES_ON || vec_size(compiler.build.emit_only)) return 0;
	Decl **temp_array = VECNEW(Decl*, 4096);
	int64_t *costs = MALLOC(sizeof(int64_t) * module_count);
	int64_t total = 0;
	for (int i = 0; i < module_count; i++) total += costs[i] = llvm_split_calculate_module_cost(modules[i], &temp_array);
	*costs_ref = costs;
	int64_t share = total / SPLIT_MODULE_SHARES;
	return share < SPLIT_MODULE_MIN_COST ? SPLIT_MODULE_MIN_COST : share;
}


static bool decl_is_local_extname(Decl *decl)
{
	if (decl_is_externally_visible(decl)) return false;
	if (decl->has_extname) return true;
	if (decl->decl_kind != DECL_FUNC || !decl->func_decl.type_parent) return false;
	Type *parent = decl_find_method_target(decl)->type->canonical;
	return type_is_user_defined(parent) && parent->decl->has_extname;
}


static bool llvm_split_has_local_extname(Module *module, Decl **bodies)
{
	FOREACH(Decl *, func, bodies)
	{
		if (decl_is_local_extname(func)) return true;
	}
	FOREACH(CompilationUnit *, unit, module->units)
	{
		FOREACH(Decl *, var, unit->vars)
		{
			if (var->is_live && decl_is_local_extname(var)) return true;
		}
	}
	return false;
}

int llvm_split_module(Module *module, int64_t cost, int64_t share, Decl ***bodies_ref)
{
	if (cost < share + share / 2) return 1;
	llvm_split_collect_bodies(module, bodies_ref);
	if (llvm_split_has_local_extname(module, *bodies_ref)) return 1;
	int64_t parts = (cost + share - 1) / share;
	int64_t part_cost = cost / parts;
	int64_t filled = 0;
	int part = 0;
	FOREACH(Decl *, func, *bodies_ref)
	{
		if (func->func_decl.attr_init || func->func_decl.attr_finalizer)
		{
			func->func_decl.backend_part = 0;
			continue;
		}
		if (part < parts - 1 && filled >= part_cost)
		{
			part++;
			filled = 0;
		}
		filled += func->func_decl.backend_cost;
		func->func_decl.backend_part = part;
	}
	FOREACH(CompilationUnit *, unit, module->units)
	{
		FOREACH(Decl *, func, unit->functions)
		{
			if (!func->is_live || !func->func_decl.body || decl_skip_benchtest_function(func)) func->func_decl.backend_part = 0;
		}
		FOREACH(Decl *, func, unit->methods)
		{
			if (!func->is_live || !func->func_decl.body) func->func_decl.backend_part = 0;
		}
	}
	return part + 1;
}