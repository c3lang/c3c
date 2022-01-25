
// For hacking the C API
#include "llvm/IR/IRBuilder.h"

#if LLVM_VERSION_MAJOR > 13
#define LINK_SIG \
bool link(llvm::ArrayRef<const char *> args, llvm::raw_ostream &stdoutOS, \
		  llvm::raw_ostream &stderrOS, bool exitEarly, bool disableOutput);
#define CALL_ARGS arg_vector, output, output_err, false, false
#else
#define LINK_SIG \
bool link(llvm::ArrayRef<const char *> args, bool canExitEarly, \
llvm::raw_ostream &stdoutOS, llvm::raw_ostream &stderrOS);
#define CALL_ARGS arg_vector, false, output, output_err
#endif

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
	std::vector<const char*> arg_vector = std::vector<const char *>(arg_count + 1);
	for (int i = 0; i < arg_count; i++) arg_vector.push_back(args[i]);
	std::string output_string {};
	std::string output_err_string {};
	llvm::raw_string_ostream output { output_string };
	llvm::raw_string_ostream output_err { output_err_string };
	switch (format)
	{
		case ELF:
			if (lld::elf::link(CALL_ARGS)) return true;
			break;
		case MACHO:
			#if LLVM_VERSION_MAJOR > 13
				if (lld::macho::link(CALL_ARGS)) return true;
			#else
				if (lld::mach_o::link(CALL_ARGS)) return true;
			#endif
			break;
		case WASM:
			if (lld::wasm::link(CALL_ARGS)) return true;
			break;
		case COFF:
			if (lld::coff::link(CALL_ARGS)) return true;
			break;
		case MINGW:
			if (lld::mingw::link(CALL_ARGS)) return true;
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
