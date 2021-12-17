
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/raw_ostream.h>

// For hacking the C API
#include "llvm-c/Core.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <system_error>

namespace lld {
	namespace coff {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
	}

	namespace mingw {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
	}

	namespace elf {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
	}

	namespace mach_o {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
	}

	namespace macho {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
	}

	namespace wasm {
		bool link(llvm::ArrayRef<const char *> args, bool canExitEarly,
		          llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
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
	std::vector<const char*> arg_vector = std::vector<const char *>(arg_count + 1);
	for (int i = 0; i < arg_count; i++) arg_vector.push_back(args[i]);
	std::string output_string {};
	std::string output_err_string {};
	llvm::raw_string_ostream output { output_string };
	llvm::raw_string_ostream output_err { output_err_string };
	switch (format)
	{
		case ELF:
			if (lld::elf::link(arg_vector, false, output, output_err)) return true;
			break;
		case MACHO:
			#if LLVM_VERSION_MAJOR > 13
				if (lld::macho::link(arg_vector, false, output, output_err)) return true;
			#else
				if (lld::mach_o::link(arg_vector, false, output, output_err)) return true;
			#endif
			break;
		case WASM:
			if (lld::wasm::link(arg_vector, false, output, output_err)) return true;
			break;
		case COFF:
			if (lld::coff::link(arg_vector, false, output, output_err)) return true;
			break;
		case MINGW:
			if (lld::mingw::link(arg_vector, false, output, output_err)) return true;
			break;
		default:
			exit(-1);
	}
	*error_string = strdup(output_err_string.c_str());
	return false;
}


extern "C" {

#if LLVM_VERSION_MAJOR < 13
#if _MSC_VER
	__declspec(selectany)
#else
	__attribute__((weak))
#endif
	LLVMAttributeRef LLVMCreateTypeAttribute(LLVMContextRef C, unsigned KindID,
											 LLVMTypeRef type_ref)
	{
		auto &Ctx = *llvm::unwrap(C);
		auto AttrKind = (llvm::Attribute::AttrKind)KindID;
		return wrap(llvm::Attribute::get(Ctx, AttrKind, llvm::unwrap(type_ref)));
	}
#endif
	LLVMValueRef LLVMConstBswap(LLVMValueRef ConstantVal)
	{
		llvm::Constant *Val = llvm::unwrap<llvm::Constant>(ConstantVal);
		const llvm::APInt& i = Val->getUniqueInteger();
		return llvm::wrap(llvm::Constant::getIntegerValue(Val->getType(), i.byteSwap()));
	}
#if LLVM_VERSION_MAJOR < 14
	LLVMValueRef LLVMConstGEP2(LLVMTypeRef Ty, LLVMValueRef ConstantVal,
							   LLVMValueRef *ConstantIndices, unsigned NumIndices) {
		llvm::ArrayRef<llvm::Constant *> IdxList(llvm::unwrap<llvm::Constant>(ConstantIndices, NumIndices),
												 NumIndices);
		llvm::Constant *Val = llvm::unwrap<llvm::Constant>(ConstantVal);
		return wrap(llvm::ConstantExpr::getGetElementPtr(llvm::unwrap(Ty), Val, IdxList));
	}

	LLVMValueRef LLVMConstInBoundsGEP2(LLVMTypeRef Ty,
									   LLVMValueRef ConstantVal,
									  LLVMValueRef *ConstantIndices,
									  unsigned NumIndices) {
		llvm::ArrayRef<llvm::Constant *> IdxList(llvm::unwrap<llvm::Constant>(ConstantIndices, NumIndices),
												 NumIndices);
		llvm::Constant *Val = llvm::unwrap<llvm::Constant>(ConstantVal);
		return wrap(llvm::ConstantExpr::getInBoundsGetElementPtr(llvm::unwrap(Ty), Val, IdxList));
	}
#endif

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
