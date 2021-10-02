
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/raw_ostream.h>

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
