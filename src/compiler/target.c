#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include "compiler_internal.h"

typedef struct
{
	LLVMTargetRef target;
	LLVMTargetMachineRef machine;
	LLVMTargetDataRef data_layout;
	int alloca_address_space;
} Target;

static Target target = {};

int target_alloca_addr_space()
{
	return target.alloca_address_space;
}

void target_setup()
{
	assert(!target.target);

	LLVMInitializeAllTargetInfos();
	LLVMInitializeAllTargetMCs();
	LLVMInitializeAllTargets();
	LLVMInitializeAllAsmPrinters();
	LLVMInitializeAllAsmParsers();

	target.target = NULL;
	if (!build_options.target)
	{
		build_options.target = LLVMGetDefaultTargetTriple();
	}
	char *err = NULL;

	if (LLVMGetTargetFromTriple(build_options.target, &target.target, &err) != 0)
	{
		error_exit("Could not create target: %s", err);
		// Usually we would dispose of err, but no need to do it due to exit.
	}

	target.alloca_address_space = 0;

	DEBUG_LOG("Target set to %s.", build_options.target);
	// Create a specific target machine
	LLVMCodeGenOptLevel level;
	LLVMRelocMode reloc_mode = LLVMRelocDefault;

	switch (build_options.optimization_level)
	{
		case OPTIMIZATION_NOT_SET:
			UNREACHABLE;
		case OPTIMIZATION_AGGRESSIVE:
			level = LLVMCodeGenLevelAggressive;
			break;
		case OPTIMIZATION_DEFAULT:
			level = LLVMCodeGenLevelDefault;
			break;
		case OPTIMIZATION_LESS:
			level = LLVMCodeGenLevelLess;
			break;
		case OPTIMIZATION_NONE:
			level = LLVMCodeGenLevelNone;
			break;
		default:
			UNREACHABLE;
	}
//	reloc = (opt->pic || opt->library)? LLVMRelocPIC : LLVMRelocDefault;
	if (!build_options.cpu)
	{
		build_options.cpu = "generic";
	}
	/*
	if (!opt->features)
	{
		opt->features = "";
	}*/
	if (!(target.machine = LLVMCreateTargetMachine(target.target, build_options.target, build_options.cpu, "", level, reloc_mode,
	                                               LLVMCodeModelDefault))) {
		error_exit("Failed to create target machine.");
	}

	// The below is broken for the AMDGPU target.
	target.alloca_address_space = 0;
	target.data_layout = LLVMCreateTargetDataLayout(target.machine);
	build_options.pointer_size = (int)LLVMPointerSize(target.data_layout);
	DEBUG_LOG("Deduced pointer size to be %d bits", build_options.pointer_size * 8);
}

void target_destroy()
{
	assert(target.machine);
	LLVMDisposeTargetMachine(target.machine);
}

void *target_machine()
{
	return target.machine;
}
void *target_data_layout()
{
	return target.data_layout;
}