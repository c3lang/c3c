// Copyright (c) 2021-2026 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import std::os::backtrace, std::io;

<*
 Assert (always) that a condition is true. Otherwise call abort.

 @param #value : "The expression to test"
 @param $fmt : "The formatting string"
 @param ... : "Arguments to the format string"
*>
macro void always_assert(bool #value, String $fmt = "", ...) @builtin @format(1) @safemacro
{
	bool val = #value;
	if (@unlikely(!val))
	{
		$if $fmt == "":
			abort(@sprintf("Violated assert '%s'.", $stringify(#value)));
		$else
			abort(@sprintf("Violated assert '%s': %s", $stringify(#value), $fmt), ...$vaarg);
		$endif
	}
}

<*
 Unconditionally break into an attached debugger when reached.
*>
macro void breakpoint() @builtin
{
	$$breakpoint();
}

fn int panic_print_backtrace_count(BacktraceList backtrace, int backtraces_to_ignore)
{
	bool inside_builtin_panic_skipped = true;
	for (int i = backtraces_to_ignore; i < backtrace.len(); i++)
	{
		String function_name = backtrace[i].function;
		switch
		{
			case function_name == "std.os.backtrace.capture_current":
			case function_name == "std.core.builtin.print_backtrace":
			case function_name == "std.core.builtin.default_panic":
			case function_name == "std.core.builtin.panicf":
			case function_name.starts_with("std.core.runtime.panic_test."):
			case function_name.starts_with("std.core.runtime.test_panic."):
			case function_name == "__GI___sigaction":
				backtraces_to_ignore = i + 1;
			default:
				inside_builtin_panic_skipped = false;
				break;
		}
	}
	return backtraces_to_ignore;
}

fn bool print_backtrace(String message, int backtraces_to_ignore, void *added_backtrace = null) @feat(LINUX, DARWIN, OPENBSD, WIN32) => @stack_mem(0x1100; Allocator smem)
{
	void*[256] buffer;
	void*[] backtraces = backtrace::capture_current(&buffer);
	if (added_backtrace)
	{
		backtraces[++backtraces_to_ignore] = added_backtrace;
	}
	@stack_mem(4096; Allocator mem)
	{
		BacktraceList? backtrace = backtrace::symbolize_backtrace(mem, backtraces);
		if (catch backtrace) return false;

		// Update ignore-count to also ignore the first traces from the panic functions
		backtraces_to_ignore = panic_print_backtrace_count(backtrace, backtraces_to_ignore);
		if (backtrace.len() <= backtraces_to_ignore) return false;

		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintn("'");

		sz repetition_count;
		foreach (i, &trace : backtrace)
		{
			if (i < backtraces_to_ignore) continue;
			if (trace.has_file() && trace.file.ends_with("/lib/std/core/private/main_stub.c3")) {
				break;
			}

			// Repeating backtraces (f.e. for max recursion depth) pretty print
			if (i > 0 && *trace == backtrace[i-1]) {
				if (i + 1 < backtrace.len() && *trace == backtrace[i+1]) {
					repetition_count += 1;
					continue;
				} else if (repetition_count > 0) {
					if (trace.is_unknown()) {
						io::eprintfn("  ... (%s repetitions of ???)", repetition_count, trace.function);
					} else {
						io::eprintfn("  ... (%s repetitions of %s)", repetition_count, trace.function);
					}
				}
			} else {
				repetition_count = 0;
			}

			String inline_suffix = trace.is_inline ? " [inline]" : "";
			switch
			{
				case trace.is_unknown():
					io::eprintfn("  in ???%s", inline_suffix);
				case trace.has_file():
					io::eprintfn("  in %s (%s:%d) [%s]%s", trace.function, trace.file, trace.line, trace.object_file, inline_suffix);
				default:
					io::eprintfn("  in %s (source unavailable) [%s]%s", trace.function, trace.object_file, inline_suffix);
			}
		}
	};
	return true;
}

fn void default_panic(String message, String file, String function, int line) @feat(LINUX, DARWIN, OPENBSD, WIN32)
{
	in_panic = true;
	$if $defined(io::stderr) && env::PANIC_MSG:
		if (!print_backtrace(message, 2))
		{
			io::eprintfn("\nERROR: '%s', in %s (%s:%d)", message, function, file, line);
		}
	$endif
	$$trap();

}

macro void abort(String string = "Unrecoverable error reached", ...) @format(0) @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, ...$vaarg);
	$$trap();
}

bool in_panic @private = false;

fn void default_panic(String message, String file, String function, int line) @weak
{
	$if $defined(io::stderr) && env::PANIC_MSG && !env::EMSCRIPTEN:
		if (in_panic)
		{
			io::eprintn("Panic inside of panic.");
			$$trap();
			return;
		}
		in_panic = true;
		$if $defined(io::stderr):
			io::eprint("\nERROR: '");
			io::eprint(message);
			io::eprintfn("', in %s (%s:%d)", function, file, line);
		$endif
		in_panic = false;
	$endif
	$$trap();
}

alias PanicFn = fn void(String message, String file, String function, int line);

PanicFn panic = &default_panic;

fn void panicf(String fmt, String file, String function, int line, args...)
{
	$if $defined(io::stderr) && env::PANIC_MSG && !env::EMSCRIPTEN:
		if (in_panic)
		{
			io::eprint("Panic inside of panic: ");
			io::eprintn(fmt);
			return;
		}
		in_panic = true;
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.init(allocator);
			s.appendf(fmt, ...args);
			in_panic = false;
			panic(s.str_view(), file, function, line);
		};
	$else
		panic(fmt, file, function, line);
	$endif
}

<*
 Marks the path as unreachable. This will panic in safe mode, and in fast will simply be assumed
 never happens.
 @param [in] string : "The panic message or format string"
*>
macro void unreachable(String string = "Unreachable statement reached.", ...) @builtin @noreturn
{
	$if env::COMPILER_SAFE_MODE:
		panicf(string, $$FILE, $$FUNC, $$LINE, ...$vaarg);
	$else
		$$unreachable();
	$endif
}

<*
 Marks the path as unsupported, this is similar to unreachable.
 @param [in] string : "The error message"
*>
macro void unsupported(String string = "Unsupported function invoked") @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, ...$vaarg);
	$$unreachable();
}

module std::core::builtin @feat(LINUX, ANDROID, DARWIN) @feat(SAFE_MODE) @feat(DEBUG_SYMBOLS);
import libc, std::io, std::os::posix;

fn void sig_panic(String message)
{
	default_panic(message, "???", "???", 0);
}

fn void sig_bus_error(CInt i, void* info, void* context)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal memory access.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: 'Illegal memory access'.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

fn void sig_segmentation_fault(CInt i, void* p1, void* context)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Out of bounds memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Out of bounds memory access.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: Memory error without backtrace, possible stack overflow.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

fn void sig_illegal_instruction(CInt i, void* p1, void* context)
{
	if (in_panic) os::fastexit(128 + i);
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal instruction.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal instruction.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: Illegal instruction.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

char[64 * 1024] sig_stack @local @feat(BACKTRACE) @feat(LINUX);

// Clean this up
fn void install_signal_handlers() @init(101) @local @feat(BACKTRACE)
{
	$if env::LINUX && !$feat(ASAN):
		Stack_t ss = {
			.ss_sp = &sig_stack,
			.ss_size = sig_stack.len
		};
		libc::sigaltstack(&ss, null);
	$endif

	posix::install_signal_handler(libc::SIGBUS, &sig_bus_error);
	posix::install_signal_handler(libc::SIGSEGV, &sig_segmentation_fault);
	posix::install_signal_handler(libc::SIGILL,  &sig_illegal_instruction);
}
