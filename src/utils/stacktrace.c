// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <signal.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <imagehlp.h>
#else
#include <err.h>
#include <execinfo.h>
#endif


static char const * icky_global_program_name;

/* Resolve symbol name and source location given the path to the executable
   and an address */
int addr2line(char const * const program_name, void const * const addr)
{
	char addr2line_cmd[512] = {0};

	/* have addr2line map the address to the relent line in the code */
#ifdef __APPLE__
	sprintf(addr2line_cmd,"atos -o %.256s %p", program_name, addr);
#else
	sprintf(addr2line_cmd,"addr2line -f -p -e %.256s %p", program_name, addr);
#endif
	return system(addr2line_cmd);
}
#ifdef _WIN32
void windows_print_stacktrace(CONTEXT* context)
{
	SymInitialize(GetCurrentProcess(), 0, true);

	STACKFRAME frame = { 0 };

	/* setup initial stack frame */
	frame.AddrPC.Offset         = context->Rip;
	frame.AddrPC.Mode           = AddrModeFlat;
	frame.AddrStack.Offset      = context->Rsp;
	frame.AddrStack.Mode        = AddrModeFlat;
	frame.AddrFrame.Offset      = context->Rbp;
	frame.AddrFrame.Mode        = AddrModeFlat;

	while (StackWalk(
			IMAGE_FILE_MACHINE_I386,
			GetCurrentProcess(),
			GetCurrentThread(),
			&frame,
			context,
			0,
			SymFunctionTableAccess,
			SymGetModuleBase,
			0))
	{
		addr2line(icky_global_program_name, (void *)frame.AddrPC.Offset);
	}
	SymCleanup( GetCurrentProcess() );
}

LONG WINAPI windows_exception_handler(EXCEPTION_POINTERS *ExceptionInfo)
{
	puts("fjefijejfe");
	switch(ExceptionInfo->ExceptionRecord->ExceptionCode)
	{
		case EXCEPTION_ACCESS_VIOLATION:
			fputs("Error: EXCEPTION_ACCESS_VIOLATION\n", stderr);
			break;
	  	case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
			fputs("Error: EXCEPTION_ARRAY_BOUNDS_EXCEEDED\n", stderr);
			break;
	  	case EXCEPTION_BREAKPOINT:
			fputs("Error: EXCEPTION_BREAKPOINT\n", stderr);
			break;
	  	case EXCEPTION_DATATYPE_MISALIGNMENT:
			fputs("Error: EXCEPTION_DATATYPE_MISALIGNMENT\n", stderr);
			break;
	  	case EXCEPTION_FLT_DENORMAL_OPERAND:
			fputs("Error: EXCEPTION_FLT_DENORMAL_OPERAND\n", stderr);
			break;
	  	case EXCEPTION_FLT_DIVIDE_BY_ZERO:
			fputs("Error: EXCEPTION_FLT_DIVIDE_BY_ZERO\n", stderr);
			break;
	  	case EXCEPTION_FLT_INEXACT_RESULT:
			fputs("Error: EXCEPTION_FLT_INEXACT_RESULT\n", stderr);
			break;
	  	case EXCEPTION_FLT_INVALID_OPERATION:
			fputs("Error: EXCEPTION_FLT_INVALID_OPERATION\n", stderr);
			break;
	  	case EXCEPTION_FLT_OVERFLOW:
			fputs("Error: EXCEPTION_FLT_OVERFLOW\n", stderr);
			break;
	  	case EXCEPTION_FLT_STACK_CHECK:
			fputs("Error: EXCEPTION_FLT_STACK_CHECK\n", stderr);
			break;
	  	case EXCEPTION_FLT_UNDERFLOW:
			fputs("Error: EXCEPTION_FLT_UNDERFLOW\n", stderr);
			break;
	  	case EXCEPTION_ILLEGAL_INSTRUCTION:
			fputs("Error: EXCEPTION_ILLEGAL_INSTRUCTION\n", stderr);
			break;
	  	case EXCEPTION_IN_PAGE_ERROR:
			fputs("Error: EXCEPTION_IN_PAGE_ERROR\n", stderr);
			break;
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
			fputs("Error: EXCEPTION_INT_DIVIDE_BY_ZERO\n", stderr);
			break;
	  	case EXCEPTION_INT_OVERFLOW:
			fputs("Error: EXCEPTION_INT_OVERFLOW\n", stderr);
			break;
	  	case EXCEPTION_INVALID_DISPOSITION:
			fputs("Error: EXCEPTION_INVALID_DISPOSITION\n", stderr);
			break;
	  	case EXCEPTION_NONCONTINUABLE_EXCEPTION:
			fputs("Error: EXCEPTION_NONCONTINUABLE_EXCEPTION\n", stderr);
			break;
	  	case EXCEPTION_PRIV_INSTRUCTION:
			fputs("Error: EXCEPTION_PRIV_INSTRUCTION\n", stderr);
			break;
	  	case EXCEPTION_SINGLE_STEP:
			fputs("Error: EXCEPTION_SINGLE_STEP\n", stderr);
			break;
	  	case EXCEPTION_STACK_OVERFLOW:
			fputs("Error: EXCEPTION_STACK_OVERFLOW\n", stderr);
			break;
	  	default:
			fputs("Error: Unrecognized Exception\n", stderr);
			break;
	}
	fflush(stderr);
	/* If this is a stack overflow then we can't walk the stack, so just show
	  where the error happened */
	if (EXCEPTION_STACK_OVERFLOW != ExceptionInfo->ExceptionRecord->ExceptionCode)
	{
		windows_print_stacktrace(ExceptionInfo->ContextRecord);
	}
	else
	{
		addr2line(icky_global_program_name, (void*)ExceptionInfo->ContextRecord->Rip);
	}
	return EXCEPTION_EXECUTE_HANDLER;
}

void set_signal_handler()
{
	SetUnhandledExceptionFilter(windows_exception_handler);
}
#else

#define MAX_STACK_FRAMES 64
static void *stack_traces[MAX_STACK_FRAMES];

void posix_print_stack_trace(void)
{
	int trace_size = backtrace(stack_traces, MAX_STACK_FRAMES);
	char **messages = backtrace_symbols(stack_traces, trace_size);

	for (int i = 3; i < trace_size - 1; i++)
	{
		if (addr2line(icky_global_program_name, stack_traces[i]) != 0)
		{
			printf("  error determining line # for: %s\n", messages[i]);
		}

	}
	if (messages) { free(messages); }
}

void posix_signal_handler(int sig, siginfo_t *siginfo, void *context)
{
	(void)context;
	switch(sig)
	{
		case SIGSEGV:
			fputs("Caught SIGSEGV: Segmentation Fault\n", stderr);
			break;
		case SIGINT:
			fputs("Caught SIGINT: Interactive attention signal, (usually ctrl+c)\n", stderr);
			break;
		case SIGFPE:
			switch(siginfo->si_code)
			{
				case FPE_INTDIV:
					fputs("Caught SIGFPE: (integer divide by zero)\n", stderr);
					break;
				case FPE_INTOVF:
					fputs("Caught SIGFPE: (integer overflow)\n", stderr);
					break;
				case FPE_FLTDIV:
					fputs("Caught SIGFPE: (floating-point divide by zero)\n", stderr);
					break;
				case FPE_FLTOVF:
					fputs("Caught SIGFPE: (floating-point overflow)\n", stderr);
					break;
				case FPE_FLTUND:
					fputs("Caught SIGFPE: (floating-point underflow)\n", stderr);
					break;
				case FPE_FLTRES:
					fputs("Caught SIGFPE: (floating-point inexact result)\n", stderr);
					break;
				case FPE_FLTINV:
					fputs("Caught SIGFPE: (floating-point invalid operation)\n", stderr);
					break;
				case FPE_FLTSUB:
					fputs("Caught SIGFPE: (subscript out of range)\n", stderr);
					break;
				default:
					fputs("Caught SIGFPE: Arithmetic Exception\n", stderr);
					break;
			}
		case SIGILL:
			switch(siginfo->si_code)
			{
				case ILL_ILLOPC:
					fputs("Caught SIGILL: (illegal opcode)\n", stderr);
					break;
				case ILL_ILLOPN:
					fputs("Caught SIGILL: (illegal operand)\n", stderr);
					break;
				case ILL_ILLADR:
					fputs("Caught SIGILL: (illegal addressing mode)\n", stderr);
					break;
				case ILL_ILLTRP:
					fputs("Caught SIGILL: (illegal trap)\n", stderr);
					break;
				case ILL_PRVOPC:
					fputs("Caught SIGILL: (privileged opcode)\n", stderr);
					break;
				case ILL_PRVREG:
					fputs("Caught SIGILL: (privileged register)\n", stderr);
					break;
				case ILL_COPROC:
					fputs("Caught SIGILL: (coprocessor error)\n", stderr);
					break;
				case ILL_BADSTK:
					fputs("Caught SIGILL: (internal stack error)\n", stderr);
					break;
				default:
					fputs("Caught SIGILL: Illegal Instruction\n", stderr);
					break;
			}
			break;
		case SIGTERM:
			fputs("Caught SIGTERM: a termination request was sent to the program\n", stderr);
			break;
		case SIGABRT:
			fputs("Caught SIGABRT: usually caused by an abort() or assert()\n", stderr);
			break;
		default:
			break;
	}
	posix_print_stack_trace();
	_Exit(1);
}

static uint8_t alternate_stack[128 * 1024];
void set_signal_handler()
{
	/* setup alternate stack */
	{
		stack_t ss = {};
		/* malloc is usually used here, I'm not 100% sure my static allocation
		   is valid but it seems to work just fine. */
		ss.ss_sp = (void*)alternate_stack;
		ss.ss_size = SIGSTKSZ;
		ss.ss_flags = 0;

		if (sigaltstack(&ss, NULL) != 0) { err(1, "sigaltstack"); }
	}

	/* register our signal handlers */
	{
		struct sigaction sig_action = {};
		sig_action.sa_sigaction = posix_signal_handler;
		sigemptyset(&sig_action.sa_mask);

#ifdef __APPLE__
		/* for some reason we backtrace() doesn't work on osx
		   when we use an alternate stack */
		sig_action.sa_flags = SA_SIGINFO;
#else
		sig_action.sa_flags = SA_SIGINFO | SA_ONSTACK;
#endif

		if (sigaction(SIGSEGV, &sig_action, NULL) != 0) { err(1, "sigaction"); }
		if (sigaction(SIGFPE,  &sig_action, NULL) != 0) { err(1, "sigaction"); }
		if (sigaction(SIGINT,  &sig_action, NULL) != 0) { err(1, "sigaction"); }
		if (sigaction(SIGILL,  &sig_action, NULL) != 0) { err(1, "sigaction"); }
		if (sigaction(SIGTERM, &sig_action, NULL) != 0) { err(1, "sigaction"); }
		if (sigaction(SIGABRT, &sig_action, NULL) != 0) { err(1, "sigaction"); }
	}
}
#endif


static char const * icky_global_program_name;

void install_stacktrace(const char *program_name)
{
	icky_global_program_name = program_name;
	set_signal_handler();
}


