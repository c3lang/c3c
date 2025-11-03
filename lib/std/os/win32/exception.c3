module std::os::win32 @if(env::WIN32);

import std::io;

const EXCEPTION_MAXIMUM_PARAMETERS = 15;

struct ExceptionRecord
{
	Win32_DWORD exception_code;
	Win32_DWORD exception_flags;
	ExceptionRecord* exception_record;
	void* exception_address;
	Win32_DWORD number_parameters;
	Win32_ULONG_PTR[EXCEPTION_MAXIMUM_PARAMETERS] exception_information;
}

struct ExceptionContext @align(16)
{
	Win32_DWORD64 p1_home;
	Win32_DWORD64 p2_home;
	Win32_DWORD64 p3_home;
	Win32_DWORD64 p4_home;
	Win32_DWORD64 p5_home;
	Win32_DWORD64 p6_home;

	Win32_DWORD context_flags;
	Win32_DWORD mx_csr;

	Win32_WORD seg_cs;
	Win32_WORD seg_ds;
	Win32_WORD seg_es;
	Win32_WORD seg_fs;
	Win32_WORD seg_gs;
	Win32_WORD seg_ss;
	Win32_DWORD eflags;

	Win32_DWORD64 dr0;
	Win32_DWORD64 dr1;
	Win32_DWORD64 dr2;
	Win32_DWORD64 dr3;
	Win32_DWORD64 dr6;
	Win32_DWORD64 dr7;

	Win32_DWORD64 rax;
	Win32_DWORD64 rcx;
	Win32_DWORD64 rdx;
	Win32_DWORD64 rbx;
	Win32_DWORD64 rsp;
	Win32_DWORD64 rbp;
	Win32_DWORD64 rsi;
	Win32_DWORD64 rdi;
	Win32_DWORD64 r8;
	Win32_DWORD64 r9;
	Win32_DWORD64 r10;
	Win32_DWORD64 r11;
	Win32_DWORD64 r12;
	Win32_DWORD64 r13;
	Win32_DWORD64 r14;
	Win32_DWORD64 r15;

	Win32_DWORD64 rip;

	union
	{
		struct flt_save @align(16)
		{
			Win32_WORD control_word;
			Win32_WORD status_word;
			Win32_BYTE tag_word;
			Win32_BYTE reserved1;
			Win32_WORD error_opcode;
			Win32_DWORD error_offset;
			Win32_WORD error_selector;
			Win32_WORD reserved2;
			Win32_DWORD data_offset;
			Win32_WORD data_selector;
			Win32_WORD reserved3;
			Win32_DWORD mx_csr;
			Win32_DWORD mx_csr_mask;

			Win32_M128A[16] xmm_registers;
			Win32_BYTE[96] reserved4;
		}
		struct
		{
			Win32_M128A[2] header;
			Win32_M128A[8] legacy;
			Win32_M128A xmm0;
			Win32_M128A xmm1;
			Win32_M128A xmm2;
			Win32_M128A xmm3;
			Win32_M128A xmm4;
			Win32_M128A xmm5;
			Win32_M128A xmm6;
			Win32_M128A xmm7;
			Win32_M128A xmm8;
			Win32_M128A xmm9;
			Win32_M128A xmm10;
			Win32_M128A xmm11;
			Win32_M128A xmm12;
			Win32_M128A xmm13;
			Win32_M128A xmm14;
			Win32_M128A xmm15;
		}
	}

	Win32_M128A[26] vector_registers;
	Win32_DWORD64 vector_control;

	Win32_DWORD64 debug_control;
	Win32_DWORD64 last_branch_to_rip;
	Win32_DWORD64 last_branch_from_rip;
	Win32_DWORD64 last_exception_to_rip;
	Win32_DWORD64 last_exception_from_rip;
}

struct ExceptionPointers
{
	ExceptionRecord* exception_record;
	ExceptionContext* context_record;
}

alias UnhandledExceptionFilter = fn Win32_LONG (ExceptionPointers* exception_info);
const EXCEPTION_EXECUTE_HANDLER = 1;

extern fn void debugBreak() @extern("DebugBreak") @if(env::WIN32);
extern fn bool isDebuggerPresent() @extern("IsDebuggerPresent") @if(env::WIN32);
extern fn UnhandledExceptionFilter setUnhandledExceptionFilter(UnhandledExceptionFilter filter) @extern("SetUnhandledExceptionFilter") @if(env::WIN32);

UnhandledExceptionFilter previous_filter;
PanicFn previous_panic;
bool has_panicked;

fn Win32_LONG exception_handler(ExceptionPointers* exception_info)
{
	if (!has_panicked)
	{
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.init(allocator: allocator);
			Win32_DWORD code = exception_info.exception_record.exception_code;
			void* addr = exception_info.exception_record.exception_address;
			switch (code)
			{
			case 0x80000001: s.appendf("Guard page violation at address %p", addr);
			case 0x80000002: s.appendf("Datatype misalignment at address %p", addr);
			case 0xC0000005: s.appendf("Access Violation at address %p", addr);
			case 0xC0000006: s.appendf("In page error at address %p", addr);
			case 0xC000001D: s.appendf("Illegal instruction at address %p", addr);
			case 0xC000008C: s.appendf("Array bounds exceeded at address %p", addr);
			case 0xC000008D: s.appendf("Flt denormal operand at address %p", addr);
			case 0xC000008E: s.appendf("Flt divide by zero at address %p", addr);
			case 0xC0000090: s.appendf("Flt invalid operation at address %p", addr);
			case 0xC0000094: s.appendf("Integer divide by zero at address %p", addr);
			case 0xC00000FD: s.appendf("Stack overflow at address %p", addr);
			case 0xC0000096: s.appendf("Privileged instruction at address %p", addr);
			case 0xC0000374: s.appendf("Heap corruption detected at address %p", addr);
			case 0xC0000409: s.appendf("Stack buffer overflow at address %p", addr);
			case 0xC00004A2: s.appendf("Enclave violation at address %p", addr);
			default:
				s.appendf("Unhandled exception (%X) at %p", code, addr);
			}
			if (!builtin::print_backtrace(s.str_view(), 8))
			{
				io::eprintfn("\nERROR: %s", s.str_view());
			}
		};
	}
	if (previous_filter)
	{
		return previous_filter(exception_info);
	}
	return EXCEPTION_EXECUTE_HANDLER;
}

fn void panic_tracker(String message, String file, String function, uint line)
{
	has_panicked = true;
	previous_panic(message, file, function, line);
}

fn void init_exception_handler() @init
{
	previous_filter = setUnhandledExceptionFilter(&exception_handler);
	previous_panic = builtin::panic;
	builtin::panic = &panic_tracker;
}
