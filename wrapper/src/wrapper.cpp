
// For hacking the C API
#include "llvm/IR/IRBuilder.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/ArchiveWriter.h"
#include "llvm/Object/IRObjectFile.h"
#include "llvm/Object/SymbolicFile.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/Target/TargetMachine.h"

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

typedef enum
{
	AR_GNU,
	AR_DARWIN,
	AR_DARWIN64,
	AR_BSD,
	AR_GNU64,
	AR_COFF,
} ArFormat;



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
	switch (format)
	{
		case ELF:
			if (lld::elf::link(CALL_ARGS)) return true;
			break;
		case MACHO:
			if (lld::macho::link(CALL_ARGS)) return true;
			break;
		case WASM:
			if (lld::wasm::link(CALL_ARGS)) return true;
			break;
		case COFF:
			if (lld::coff::link(CALL_ARGS)) return true;
			break;
		case MINGW:
			exit(-1);
			if (lld::mingw::link(CALL_ARGS)) return true;
			break;
		default:
			exit(-1);
	}
	//*error_string = strdup(output_err_string.c_str());
	return false;
}


extern "C" {

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
		std::vector<llvm::NewArchiveMember> new_members {};
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
		return !llvm::writeArchive(std::string(out_name), std::move(new_members), true, kind, true, false, nullptr);
	}

	int llvm_version_major = LLVM_VERSION_MAJOR;

	void LLVMSetTargetMachineUseInitArray(LLVMTargetMachineRef ref, bool use_init_array)
	{
		auto machine = reinterpret_cast<llvm::TargetMachine *>(ref);
		machine->Options.UseInitArray = use_init_array;
	}

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
