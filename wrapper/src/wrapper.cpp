
// For hacking the C API
#include <llvm/IR/PassManager.h>
#include "c3_llvm.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/ArchiveWriter.h"
#include "llvm/Object/IRObjectFile.h"
#include "llvm/Object/SymbolicFile.h"
#include "llvm/Target/TargetMachine.h"
#include "lld/Common/CommonLinkerContext.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm-c/Transforms/PassBuilder.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Target.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/CBindingWrapping.h"
#include "llvm/Transforms/Instrumentation/MemorySanitizer.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/ThreadSanitizer.h"
#include "llvm/Transforms/Instrumentation/HWAddressSanitizer.h"
#include "llvm/Transforms/Scalar/EarlyCSE.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/JumpThreading.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Analysis/GlobalsModRef.h"
static_assert(LLVM_VERSION_MAJOR >= 17, "Unsupported LLVM version, 17+ is needed.");

#define LINK_SIG \
bool link(llvm::ArrayRef<const char *> args, llvm::raw_ostream &stdoutOS, \
		  llvm::raw_ostream &stderrOS, bool exitEarly, bool disableOutput);
#define CALL_ARGS arg_vector, output, output_err, false, false

namespace lld {

	namespace coff {
		LINK_SIG
	}

	namespace mingw {
		LINK_SIG
	}

	namespace elf {
		LINK_SIG
	}

	namespace mach_o {
		LINK_SIG
	}

	namespace macho {
		LINK_SIG
	}

	namespace wasm {
		LINK_SIG
	}
}

typedef enum
{
	ELF,
	WASM,
	MACHO,
	COFF,
	MINGW
} ObjFormat;

static bool llvm_link(ObjFormat format, const char **args, int arg_count, const char** error_string)
{
	std::vector<const char*> arg_vector = std::vector<const char *>();
	switch (format)
	{
		case ELF:
			arg_vector.push_back("ld.lld");
			break;
		case WASM:
			arg_vector.push_back("wasm-ld");
			break;
		case MACHO:
			arg_vector.push_back("ld64.lld");
			break;
		case COFF:
			arg_vector.push_back("lld-link");
			break;
		case MINGW:
			arg_vector.push_back("ld");
			break;
	}
	for (int i = 0; i < arg_count; i++) arg_vector.push_back(strdup(args[i]));
	std::string output_string {};
	std::string output_err_string {};
	/*
	llvm::raw_string_ostream output { output_string };
	llvm::raw_string_ostream output_err { output_err_string };*/
	llvm::raw_ostream &output = llvm::outs();
	llvm::raw_ostream &output_err = llvm::errs();
	bool success;
	switch (format)
	{
		case ELF:
			success = lld::elf::link(CALL_ARGS);
			break;
		case MACHO:
			success = lld::macho::link(CALL_ARGS);
			break;
		case WASM:
			success = lld::wasm::link(CALL_ARGS);
			break;
		case COFF:
			success = lld::coff::link(CALL_ARGS);
			break;
		case MINGW:
			printf("Mingw not enabled");
			exit(1);
		default:
			printf("Unsupported linker");
			exit(-1);
	}
	if (success)
	{
		lld::CommonLinkerContext::destroy();
	}
	//*error_string = strdup(output_err_string.c_str());
	return success;
}

extern "C" {


bool llvm_run_passes(LLVMModuleRef m, LLVMTargetMachineRef tm,
					 LLVMPasses *passes)
{
	llvm::TargetMachine *Machine = (llvm::TargetMachine *)(tm);
	llvm::Module *Mod = llvm::unwrap(m);
	llvm::PassInstrumentationCallbacks PIC;
	llvm::PipelineTuningOptions PTO{};
	PTO.LoopUnrolling = passes->opt.unroll_loops;
	PTO.LoopInterleaving = passes->opt.interleave_loops;
	PTO.LoopVectorization = passes->opt.vectorize_loops;
	PTO.SLPVectorization = passes->opt.slp_vectorize;
	PTO.MergeFunctions = passes->opt.merge_functions;
	PTO.CallGraphProfile = true; // We always use integrated ASM
#if LLVM_VERSION_MAJOR > 16
	PTO.UnifiedLTO = false;
#endif
	llvm::PassBuilder PB(Machine, PTO, std::nullopt, &PIC);

	llvm::LoopAnalysisManager LAM;
	llvm::FunctionAnalysisManager FAM;
	llvm::CGSCCAnalysisManager CGAM;
	llvm::ModuleAnalysisManager MAM;
	PB.registerLoopAnalyses(LAM);
	PB.registerFunctionAnalyses(FAM);
	PB.registerCGSCCAnalyses(CGAM);
	PB.registerModuleAnalyses(MAM);
	PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

	llvm::StandardInstrumentations SI(Mod->getContext(), passes->should_debug, passes->should_verify);
	SI.registerCallbacks(PIC, &MAM);

	// Assignment tracking pass is not enabled, but could be added
	// Skipping TargetLibraryAnalysis

	llvm::OptimizationLevel level;
	switch (passes->opt_level)
	{
		case LLVM_O0:
			level = llvm::OptimizationLevel::O0;
			break;
		case LLVM_O1:
			level = llvm::OptimizationLevel::O1;
			break;
		case LLVM_O2:
			level = llvm::OptimizationLevel::O2;
			break;
		case LLVM_O3:
			level = llvm::OptimizationLevel::O3;
			break;
		case LLVM_Os:
			level = llvm::OptimizationLevel::Os;
			break;
		case LLVM_Oz:
			level = llvm::OptimizationLevel::Oz;
			break;
		default:
			exit(-1);
	}
#if LLVM_VERSION_MAJOR > 19
	llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(level, llvm::ThinOrFullLTOPhase::None);
#else
	llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(level, false);
#endif
	if (passes->should_verify)
	{
		MPM.addPass(llvm::VerifierPass());
	}
	if (passes->sanitizer.mem_sanitize)
	{
		llvm::MemorySanitizerOptions options(passes->sanitizer.mem_track_origins,
											 passes->sanitizer.recover,
											 passes->is_kernel,
											 passes->sanitizer.mem_retval);

		MPM.addPass(llvm::MemorySanitizerPass(options));
		if (passes->opt_level != LLVM_O0)
		{
			// Add another function pass like Clang does.
			MPM.addPass(llvm::RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>());
			llvm::FunctionPassManager FPM;
			FPM.addPass(llvm::EarlyCSEPass(true /* Enable mem-ssa. */));
			FPM.addPass(llvm::InstCombinePass());
			FPM.addPass(llvm::JumpThreadingPass());
			FPM.addPass(llvm::GVNPass());
			FPM.addPass(llvm::InstCombinePass());
			MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
		}
	}
	if (passes->sanitizer.thread_sanitize)
	{
		MPM.addPass(llvm::ModuleThreadSanitizerPass());
		MPM.addPass(createModuleToFunctionPassAdaptor(llvm::ThreadSanitizerPass()));
	}

	if (passes->sanitizer.address_sanitize)
	{
		llvm::AddressSanitizerOptions Opts;
		bool use_globals_gc = false; // No need to add this yet.
		Opts.CompileKernel = passes->is_kernel;
		Opts.Recover = passes->sanitizer.recover;
		Opts.UseAfterScope = passes->sanitizer.asan_use_after_scope;
		Opts.UseAfterReturn = passes->sanitizer.asan_use_after_return
				? llvm::AsanDetectStackUseAfterReturnMode::Always
				: llvm::AsanDetectStackUseAfterReturnMode::Never;
		bool is_windows = Machine->getTargetTriple().isOSWindows();
		MPM.addPass(llvm::AddressSanitizerPass(Opts,
											   use_globals_gc,
											   !is_windows,
											   passes->sanitizer.asan_use_global_dstor
												? llvm::AsanDtorKind::Global
												: llvm::AsanDtorKind::None));
	}
	if (passes->sanitizer.hwaddress_sanitize)
	{
		MPM.addPass(llvm::HWAddressSanitizerPass({
			passes->is_kernel,
			passes->sanitizer.recover,
			passes->opt_level == LLVM_O0
		}));
	}
	// MPM.addPass(DataFlowSanitizerPass(LangOpts.NoSanitizeFiles));

	MPM.run(*Mod, MAM);
	return true;
}

bool llvm_ar(const char *out_name, const char **args, size_t count, int ArFormat)
{
	llvm::object::Archive::Kind kind;
	switch (ArFormat)
	{
		case AR_BSD:
			kind = llvm::object::Archive::K_BSD;
			break;
		case AR_DARWIN:
			kind = llvm::object::Archive::K_DARWIN;
			break;
		case AR_DARWIN64:
			kind = llvm::object::Archive::K_DARWIN64;
			break;
		case AR_GNU:
			kind = llvm::object::Archive::K_GNU;
			break;
		case AR_GNU64:
			kind = llvm::object::Archive::K_GNU64;
			break;
		case AR_COFF:
			kind = llvm::object::Archive::K_GNU;
			break;
		default:
			assert(false);
	}
	bool is_win = ArFormat == AR_COFF;
	std::vector<llvm::NewArchiveMember> new_members{};
	for (size_t i = 0; i < count; i++)
	{
		auto member = llvm::NewArchiveMember::getFile(std::string(args[i]), false);
		if (!member) return false;
		if (is_win)
		{
			// Needs relative paths.
			const char *rel_name = strrchr(args[i], '/');
			if (rel_name) member->MemberName = rel_name + 1;
		}
		new_members.push_back(std::move(*member));
	}
#if LLVM_VERSION_MAJOR > 17
	return !llvm::writeArchive(std::string(out_name), std::move(new_members), llvm::SymtabWritingMode::NormalSymtab, kind, true, false, nullptr);
#else
	return !llvm::writeArchive(std::string(out_name), std::move(new_members), true, kind, true, false, nullptr);
#endif
}

int llvm_version_major = LLVM_VERSION_MAJOR;

void LLVMSetTargetMachineUseInitArray(LLVMTargetMachineRef ref, bool use_init_array)
{
	auto machine = (llvm::TargetMachine*)ref;
	machine->Options.UseInitArray = use_init_array;
}
void LLVMSetDSOLocal(LLVMValueRef Global, bool value)
{
	llvm::unwrap<llvm::GlobalValue>(Global)->setDSOLocal(value);
}

void LLVMBuilderSetFastMathFlags(LLVMBuilderRef Builder, FastMathOption option)
{
	llvm::FastMathFlags math_flags {};
	switch (option)
	{
		case RELAXED:
			math_flags.setAllowReassoc(true);
			math_flags.setAllowReciprocal(true);
			math_flags.setAllowContract(true);
			break;
		case FAST:
			math_flags.setFast(true);
			break;
		case STRICT:
		default:
			return;
	}
	llvm::unwrap(Builder)->setFastMathFlags(math_flags);
}

LLVMValueRef LLVMConstBswap(LLVMValueRef ConstantVal)
{
	llvm::Constant *Val = llvm::unwrap<llvm::Constant>(ConstantVal);
	const llvm::APInt &i = Val->getUniqueInteger();
	return llvm::wrap(llvm::Constant::getIntegerValue(Val->getType(), i.byteSwap()));
}

bool llvm_link_elf(const char **args, int arg_count, const char** error_string)
{
	return llvm_link(ELF, args, arg_count, error_string);
}

bool llvm_link_macho(const char **args, int arg_count, const char** error_string)
{
	return llvm_link(MACHO, args, arg_count, error_string);
}

bool llvm_link_coff(const char **args, int arg_count, const char** error_string)
{
	return llvm_link(COFF, args, arg_count, error_string);
}

bool llvm_link_wasm(const char **args, int arg_count, const char** error_string)
{
	return llvm_link(WASM, args, arg_count, error_string);
}

bool llvm_link_mingw(const char **args, int arg_count, const char** error_string)
{
	return llvm_link(MINGW, args, arg_count, error_string);
}


}
