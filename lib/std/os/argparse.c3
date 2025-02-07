<*
 Console arguments parsing module

 Features:
 - --long-names and -s short optional
 - --flags and negative flags via --no-flags to unset
 - Short flags stacking -s -f -v equals to -sfv
 - Support of `--long=value` and `--long value`
 - Support of `--` makes all following options as arguments
 - Generic option value to program value parsing
 - Sub-commands support
 - ZII initialization, with sensible defaults
 - Builtin argument validation for simple types
 - Custom argument parser via user-defined callbacks
 - Automatic help printout
 - Support of arrays of the arguments like -I ./foo -I ./bar (use callbacks!)
 - Support of accumulated values: -vvvv will increase int value by 1 four times
 - Low-level parser via `ArgParse.next()` if you want customized behavior
 - Automatically detects value of option your program expects, and does sanity checks
 - Doesn't require crating special types for arg parse tasks, just use ArgParse instance 
 - Does not allocate memory, it's just a static struct instance

 Example:

```c3

// Simple arguments
import std::os::argparse;
import std::io;

fn int main(String[] args)
{
    int val = 0;
    bool flag = false;
    String s;
    float f = 0.0;
    argparse::ArgParse agp = {
        .description = "My program",
        .usage = "[-a] [--string=<some>] arg1",
        .epilog= "Footer message",
        .options = { 
            argparse::help_opt(),
            argparse::group_opt("Basic options"),
            {.short_name = 'a', .long_name = "all", .value = &val, .help = "This is a sample option"},
            argparse::group_opt("Advanced options"),
            { .long_name = "string", .value = &s, .help = "Simple long_name string"},
            { .short_name = 'f', .value = &flag, .help = "short opt flag"},
        }, 
    };

    if(catch err = agp.parse(args)){
        agp.print_usage()!!;
        return 1;
    }

    io::printf("My arguments: val=%s, s=%s, flag=%s\n", val, s, f);
    return 0;
}
```
*>
// Copyright (c) 2025 Alex Veden <i@alexveden.com>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::os::argparse;
import std::io;
import std::io::path;

struct ArgParse
{
	/// Contains remaining positional arguments, or subcommands if stop_at_non_option flag is true
	String[] arguments;
	/// Program usage line with argument description, it will be auto-generated if not set
	String usage;
	/// Program description in --help printout
	String description;
	/// Program footer in --help printout
	String epilog;
	/// Customized name of the program: default argv[0]
	String program_name;
	/// List of expected options
	ArgOpt[] options;

	struct flags
	{
		/// Stops parsing when first non-option argument found, keeps all remaining arguments in
		/// .arguments field of this struct. It can be used for cascading argparse for subcommands.
		bool stop_at_non_option;
	}

	// Internal state
	struct _ctx
	{
		int argc;
		String[] argv;
		String[] out;
		int cpidx;
		String optvalue;
		bool has_argument;
	}
}

struct ArgOpt
{
	/// short option letter, default: '\0'
	char short_name;

	/// long option string, default: ""
	String long_name;

	/// option value or callback context, must always be non null
	any value;

	/// set true if value is mandatory (default: false)
	bool required;

	/// optional: help string for display in --help output
	String help;

	/// optional: custom parsing logic callback, you can implement custom parsing logic
	ArgParseCallbackFn callback;

	/// Allows passing multiple options of this type.
	/// If value type is `int` it start acting as accumulator, all options considered as flags.
	bool allow_many;

	// Private fields

	/// Set to true if argument passed by the user
	bool _is_present;

	/// Private argument type
	ArgType _arg_type;
}

<*
 Custom argparse option parser callback.

 You can use callback if you need custom parsing logic, or process multiple arguments of
 different types. 

 Example of parsing array of numbers:

```c3

List(<int>) numbers;
numbers.new_init(); 
defer numbers.free();

ArgParse agp = {
    .options = {
        {
            .short_name = 'n',
            .long_name = "num",
            .value = &numbers,
            .allow_many = true,
            .callback = fn void! (ArgOpt* opt, String value) {
                List(<int>) * ctx = anycast(opt.value, List(<int>))!;
                int val = value.to_integer(int)!;
                ctx.push(val);
            }
        },
    }
};
```

 param opt "ArgOpt related to option parsed"
 param value "raw string value from command line argv"
*>
def ArgParseCallbackFn = fn void! (ArgOpt* opt, String value);

<*
 Special template option for --help/-h - prints usage
*>
macro help_opt(String help_prompt = "show this help message and exit")
{
	return ArgOpt {
		._arg_type = ArgType.HELP,
		.short_name = 'h',
		.long_name = "help",
		.help = help_prompt,
		.value = any_make(null,bool.typeid),
	};
}

<*
 Section separator in usage printout
*>
macro group_opt(String group_name)
{
	return ArgOpt {
		._arg_type = ArgType.GROUP,
		.help = group_name,
	};
}

<*
 Low-level argument parser, for fine-grained control of how arguments being parsed

 This method return argv items as is, and after getting option with '--some-opt' or '-s' 
 you can try to do another ArgParse.next() to fetch this argument value.

 Long names `--foo=bar` are split into 2 chunks: "--foo", "bar"

 Example:

```c3
fn void test_argparse_next_all_together()
{
    List(<String>) args;
    args.new_init(); 
    defer args.free();

    String[] argv = { "testprog", "-n", "5", "--num", "2", "--foo=3", "--", "-fex", "-I./foo"};

    ArgParse agp;
    while (String arg = agp.next(argv)!!) {
        args.push(arg);
    }
    io::printfn("%s", args);
    // prints
    // [-n, 5, --num, 2, --foo, 3, --, -fex, -I./foo]
}
```

 Switching over options:

```c3
int! sum_args(String argv[]) 
{ 
    ArgParse agp = {
        .description = "Test number sum program",
        .options = { 
            {.short_name = 'n', .long_name="num", .help = "number to add"},
        }
    };
    // String[] argv = { "testprog", "-n", "5", "--num", "2", "--num=3"};
    int n_sum = 0;  // expected to be 5+2+3
    while (String arg = agp.next(argv)!) {
        switch (arg) {
            case "-n":
            case "--num":
                // fetch next value of "--num", it may fail when no value "--num="
                String value = agp.next(argv)!;
                n_sum += value.to_integer(int)!;
            default:
                // you may use ArgParse options only for usage display as well
                // OR make your own usage printout
                agp.print_usage()!;

                // maybe exit program here ...
        }
    }
    return n_sum;
}
```
*>
fn String! ArgParse.next(&self, String[] argv)
{
	assert(self.arguments.len == 0, "ArgParse already initialized or double parse call");

	if (!self._ctx.has_argument) {
		// initial call
		assert(argv.len > 0, "empty argv");
		self._ctx.argc = argv.len - 1;
		self._ctx.cpidx = 0;
		self._ctx.argv = argv[1..];
		self._ctx.out = argv;
	}

	// protection flag (prevents mixed usage with ArgParse.parse())
	self._ctx.has_argument = true;

	if (self._ctx.argc > 0 && self._ctx.argv.len > 0) {
		String arg = self._ctx.argv[0];
		if (arg.len == 0) {
			io::printf("Error: argument too short `%s`\n", arg);
			return ArgError.INVALID_ARGUMENT?;
		}
		if (self._ctx.cpidx > 0) {
			// we have --opt=foo, return 'foo' part
			arg = arg[self._ctx.cpidx + 1 ..];
			self._ctx.cpidx = 0;
		} else if (arg.starts_with("--")) {
			if (try idx = arg.index_of_char('=')) {
				// handling --opt=foo, returns '--opt' and 'foo'
				self._ctx.cpidx = (int)idx;

				if (idx == arg.len - 1) {
					io::printf("Error: option has no value `%s`\n", arg);
					return ArgError.INVALID_ARGUMENT?;
				}
				arg = arg[..idx - 1];
				// return '--opt' part
				return arg;
			}
		}
		self._ctx.argc--;
		self._ctx.argv = self._ctx.argv[1..];
		return arg;
	}

	return "";
}

<*
 Parses all arguments.

 @param argv "arguments list from system argv or from previous argparse.arguments for subcommands"
 @return! ArgError.INVALID_ARGUMENT
*>
fn void! ArgParse.parse(&self, String[] argv)
{
	assert(self.arguments.len == 0, "ArgParse already initialized or double parse call");
	assert(!self._ctx.has_argument, "ArgParse.next() already used before (pick only one way)?");
	assert(argv.len > 0);

	self._ctx.argc = argv.len - 1;
	self._ctx.cpidx = 0;
	self._ctx.argv = argv[1..];
	self._ctx.out = argv;

	if (!self.program_name) {
		self.program_name = argv[0];
	}
	self.options_check()!;

	for (; self._ctx.argc && self._ctx.argv.len; self._ctx.argc--) {
		String arg = self._ctx.argv[0];
		if (arg.len == 0) {
			io::printf("Error: argument too short `%s`\n", arg);
			return ArgError.INVALID_ARGUMENT?;
		}

		if (arg.len == 1 || arg[0] != '-' || arg[1] == '\0') {
			self._ctx.has_argument = true;

			if (self.flags.stop_at_non_option) {
				self._ctx.argc--;
				self._ctx.argv = self._ctx.argv[1..];
				break;
			} else {
				self._ctx.argv = self._ctx.argv[1..];
			}
			continue;
		}
		// short option
		if (arg[1] != '-') {
			self._ctx.optvalue = arg[1..];
			self._ctx.cpidx++;

			if (self._ctx.has_argument) {
				io::printf("Error: passing options after arguments not allowed at `%s`\n", arg);
				return ArgError.INVALID_ARGUMENT?;
			}

			self.parse_short_arg()!;

			self._ctx.argv = self._ctx.argv[1..];

			// Parsing stacked short args: when '-f -a -b' passed as '-fab'
			while (self._ctx.optvalue) {
				self.parse_short_arg()!;
			}
			continue;
		}
		// if '--' presents
		if (arg.len == 2) {
			self._ctx.argc--;
			self._ctx.argv = self._ctx.argv[1..];
			self._ctx.cpidx++;
			break;
		}

		if (self._ctx.has_argument) {
			io::printf("Error: passing options after arguments not allowed at `%s`\n", arg);
			return ArgError.INVALID_ARGUMENT?;
		}

		self.parse_long_arg()!;
		self._ctx.cpidx++;
		self._ctx.argv = self._ctx.argv[1..];
	}

	self.options_ensure_required()!;

	self.arguments = argv[self._ctx.cpidx + 1 ..];  // excludes 1st argv[0], program_name
}

<*
 Prints program usage based on ArgParse fields composition
*>
fn void! ArgParse.print_usage(&self)
{
	assert(self.options.len > 0, "ArgParse is not properly configured, or using ArgParse.next()");

	io::printf("\nUsage:\n");
	if (self.usage) {
		@pool() {
			// NOTE: prepending program_name to all usage line
			path::Path path = path::temp_new(self.program_name)!;
			foreach (usage : self.usage.tsplit("\n")) {
				usage = usage.trim();
				if (!usage) {
					continue;
				}
				io::printf("%s %s\n", path.basename(), usage);
			}
			io::print("\n");
		};
	} else {
		io::printf("%s [options] [--] [arg1 argN]\n", self.program_name);
	}

	if (self.description) {
		io::printf("%s\n", self.description);
	}

	usz usage_opts_width = 0;
	foreach (opt : self.options) {
		usz len = 0;
		if (opt.short_name) {
			len += 2;
			if (opt.long_name) len += 2;  // extra for separator ', '
		}
		if (opt.long_name) {
			len += opt.long_name.len + 2;
		}

		if (opt.value) {
			switch (opt.value.type) {
				case int.typeid:
				case uint.typeid:
				case long.typeid:
				case ulong.typeid:
					len += 6;  // strlen("=<int>")
				case float.typeid:
				case double.typeid:
					len += 6;  // strlen("=<flt>")
				case String.typeid:
					len += 6;  // strlen("=<str>")
			}
		}

		len = (len + 3) - ((len + 3) & 3);
		if (usage_opts_width < len) {
			usage_opts_width = len;
		}
	}
	usage_opts_width += 4;  // 4 spaces prefix

	foreach (opt : self.options) {
		usz pos = 0;
		usz pad = 0;
		if (opt._arg_type == ArgType.GROUP) {
			io::printf("\n%s\n", opt.help);
			continue;
		}
		pos = io::printf("    ")!;
		if (opt.short_name) {
			pos += io::printf("-%c", opt.short_name)!;
		}
		if (opt.long_name && opt.short_name) {
			pos += io::printf(", ")!;
		}
		if (opt.long_name) {
			pos += io::printf("--%s", opt.long_name)!;
		}
		if (opt.value) {
			switch (opt.value.type) {
				case short.typeid:
				case ushort.typeid:
				case int.typeid:
				case uint.typeid:
				case long.typeid:
				case ulong.typeid:
					pos += io::printf("=<int>")!;
				case float.typeid:
				case double.typeid:
					pos += io::printf("=<flt>")!;
				case String.typeid:
					pos += io::printf("=<str>")!;
			}
		}

		if (pos <= usage_opts_width) {
			pad = usage_opts_width - pos;
		} else {
			io::print("\n");
			pad = usage_opts_width;
		}
		io::printf("%*s%s", (int)pad + 2, "", opt.help);
		if (!opt.required) {
			if (opt.value) io::printf(" [default: %s]", opt.value);
		} else {
			io::printf(" (required)");
		}
		io::print("\n");
	}
	if (self.epilog) {
		io::printf("\n%s\n", self.epilog);
	}
}

/*
*                  PRIVATE IMPLEMENTATION
*/

fault ArgError @private
{
	MISSING_ARGUMENT,
	INVALID_ARGUMENT,
	ARGUMENT_VALUE,
	CONFIGURATION,
	HELP_SHOW,
}

enum ArgType : int @private
{
	USER,
	GROUP,
	HELP,
}

$assert(ArgType.USER.ordinal == 0);  // USER must be first to support ZII behavior

fn void! ArgParse.options_ensure_required(&self) @local
{
	bool has_errors = false;
	foreach (i, opt : self.options) {
		if (opt._arg_type == ArgType.GROUP || opt._arg_type == ArgType.HELP) {
			continue;
		}

		if (opt.required && !opt._is_present) {
			io::printf("Error: missing required option: -%c/--%s\n", opt.short_name, opt.long_name);
			has_errors = true;
		}
	}
	if (has_errors) {
		return ArgError.MISSING_ARGUMENT?;
	}
}

fn void! ArgParse.options_check(&self) @local
{
	assert(self.arguments.len == 0, "already processed?");

	foreach (i, opt : self.options) {
		if (opt._arg_type == ArgType.GROUP || opt._arg_type == ArgType.HELP) {
			continue;
		}

		if (!(opt.short_name || opt.long_name)) {
			unreachable("option[%d] both long/short_name are not set", i);
		}
		if (!opt.value) {
			unreachable("option[%d][-%c/--%s] has no valid .value= pointer set", i, opt.short_name, opt.long_name);
		}

		if (opt.allow_many) {
			if (!opt.callback) {
				if (opt.value.type != int.typeid) {
					unreachable(
						"option[%d][-%c/--%s] has .allow_many=true, without .callback, ""only int type .value supported for accumulator flags",
						i,
						opt.short_name,
						opt.long_name
					);
				}
			}
		}

		switch (opt.value.type) {
			case bool.typeid:
			case uint.typeid:
			case long.typeid:
			case ulong.typeid:
			case float.typeid:
			case double.typeid:
			case String.typeid:
			case int.typeid:
				// resetting presence flag, it will be updated after all argument parsing
				opt._is_present = false;
			default:
				if (!opt.callback) {
					unreachable(
						"option[%d][-%c/--%s] type[%s] is not supported (or pass .callback)",
						i,
						opt.short_name,
						opt.long_name,
						opt.value.type
					);
				}
		}
	}
}

fn String! ArgParse.get_arg(&self) @local
{
	if (self._ctx.optvalue) {
		// --arg=<optvalue> via =
		String result = self._ctx.optvalue;
		self._ctx.optvalue = "";
		return result;
	} else if (self._ctx.argv.len > 1) {
		self._ctx.argv = self._ctx.argv[1..];
		self._ctx.cpidx++;
		String result = self._ctx.argv[0];
		if (!result || result[0] == '-') {
			io::printf("Error: getting another option instead of previous option value at `%s`\n", self._ctx.argv[0]);
			return ArgError.ARGUMENT_VALUE?;
		}
		return result;
	} else {
		io::printf("Error: missing argument value for %s\n", self._ctx.argv[0]);
		return ArgError.ARGUMENT_VALUE?;
	}

}

fn void! ArgParse.parse_arg_val(&self, ArgOpt* opt, bool flag_unset = false) @local
{
	assert(opt);
	if (opt._arg_type == ArgType.HELP) {
		return ArgError.HELP_SHOW?;
	}

	if (opt._is_present && !opt.allow_many) {
		io::printf("Error: duplicated option %s\n", self._ctx.argv[0]);
		return ArgError.INVALID_ARGUMENT?;
	}

	assert(opt.value);

	if (opt.callback) {
		if (opt.value.type == bool.typeid || (opt.allow_many && opt.value.type == int.typeid)) {
			opt.callback(opt, (flag_unset) ? "0" : "1")!;
		} else {
			String aval = self.get_arg()!;
			opt.callback(opt, aval)!;
		}
		opt._is_present = true;
		return;
	}

	assert(opt.value.type == bool.typeid || !flag_unset, "flag_unset only for bool options");

	opt._is_present = true;

	switch (opt.value.type) {
		case bool.typeid:
			bool* pvalue = anycast(opt.value, bool)!;
			*pvalue = (flag_unset) ? false : true;
		case String.typeid:
			*anycast(opt.value, String)! = self.get_arg()!;
		case int.typeid:
			if (opt.allow_many) {
				// if .allow_many=true, .value acts as accumulator
				*anycast(opt.value, int)! += 1;
			} else {
				String aval = self.get_arg()!;
				*anycast(opt.value, int)! = aval.to_integer(int)!;
			}
		case uint.typeid:
			String aval = self.get_arg()!;
			*anycast(opt.value, uint)! = aval.to_integer(uint)!;
		case long.typeid:
			String aval = self.get_arg()!;
			*anycast(opt.value, long)! = aval.to_integer(long)!;
		case ulong.typeid:
			String aval = self.get_arg()!;
			*anycast(opt.value, ulong)! = aval.to_integer(ulong)!;
		case float.typeid:
			String aval = self.get_arg()!;
			*anycast(opt.value, float)! = aval.to_float()!;
		case double.typeid:
			String aval = self.get_arg()!;
			*anycast(opt.value, double)! = aval.to_double()!;
		default:
			unreachable("TODO: implement this typeid: %s", opt.value.type.kindof);
	}
}

fn void! ArgParse.parse_short_arg(&self) @local
{
	foreach (&opt : self.options) {
		if (opt.short_name == self._ctx.optvalue[0]) {
			self._ctx.optvalue = self._ctx.optvalue.len > 1 ? self._ctx.optvalue[1..] : "";
			if (catch err = self.parse_arg_val(opt)) {
				case ArgError.MISSING_ARGUMENT:
				case ArgError.INVALID_ARGUMENT:
				case ArgError.ARGUMENT_VALUE:
				case ArgError.CONFIGURATION:
				case ArgError.HELP_SHOW:
					return err?;
				default:
					io::printf("Error: argument parse error -%s failed with `%s`\n", opt.short_name, err);
					return ArgError.ARGUMENT_VALUE?;
			}
			return;  // ALL OK
		}
	}
	io::printf("Error: invalid argument %s\n", self._ctx.argv[0]);
	return ArgError.INVALID_ARGUMENT?;
}

fn String! prefix_skip(String s, String prefix) @local
{
	if (s.starts_with(prefix)) {
		return s[prefix.len ..];
	} else {
		return ArgError.MISSING_ARGUMENT?;
	}
}

fn void! ArgParse.parse_long_arg(&self) @local
{
	foreach (&opt : self.options) {
		if (!opt.long_name) {
			continue;
		}

		int opt_flags = 0;
		bool flag_unset = false;
		String! arg = prefix_skip(self._ctx.argv[0][2..], opt.long_name);
		if (catch err = arg) {
			switch (err) {
				case ArgError.MISSING_ARGUMENT:
					// NOTE: for boolean flags it's possible to pass unset with '--no-<flag_name>'
					if (
						opt.value.type == bool.typeid &&
						self._ctx.argv[0][2..].starts_with("no-") &&
						self._ctx.argv[0][5..].starts_with(opt.long_name)
					) {
						flag_unset = true;
					} else {
						continue;
					}
				default:
					unreachable();
			}
		} else {
			if (arg && arg[0] == '=') {
				self._ctx.optvalue = arg[1..];
			}
		}

		if (catch err = self.parse_arg_val(opt, flag_unset)) {
			case ArgError.MISSING_ARGUMENT:
			case ArgError.INVALID_ARGUMENT:
			case ArgError.ARGUMENT_VALUE:
			case ArgError.CONFIGURATION:
			case ArgError.HELP_SHOW:
				return err?;
			default:
				io::printf("Error: argument parse error %s failed with `%s`\n", self._ctx.argv[0], err);
				return ArgError.ARGUMENT_VALUE?;
		}
		return;  // ALL OK
	}

	io::printf("Error: invalid argument %s\n", self._ctx.argv[0]);
	return ArgError.INVALID_ARGUMENT?;
}
