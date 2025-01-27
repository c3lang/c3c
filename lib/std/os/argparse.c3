<*
Console arguments parsing module

Features:
- --long-names and -s short optional
- generic option value to program value parsing
- sub-commands support
- ZII initialization, with sensible defaults
- Builtin argument validation
- Custom argument parser via user-defined callbacks
- Automatic help printout

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

struct ArgParse {
    String[] arguments;  // remaining positional arguments after successful parse

    // Settings
    String usage;        // usage text (can be multiline), each line prepended by program_name
    String description;  // a description after usage
    String epilog;       // a description at the end
    String program_name; // program name in usage (by default: argv[0])
    ArgOpt[] options;    // argument parser options

    struct flags {
        bool stop_at_non_option; // allows incremental args parsing (e.g. support of commands)
    }

    struct _ctx {
        // Internal ArgParse state after calling argparse::parse()

        int argc;
        String[] argv;
        String[] out;
        int cpidx;
        String optvalue; 
        bool has_argument;
    }
}

struct ArgOpt {
    char short_name;
    String long_name;
    any value;
    bool required;
    String help;
    ArgParseCallbackFn callback;
    bool _is_present;
    ArgType _arg_type;
}
def ArgParseCallbackFn = fn void!(ArgOpt* opt, String value);

macro help_opt(String help_prompt = "show this help message and exit") {
    return ArgOpt {
        ._arg_type = ArgType.HELP,
        .short_name = 'h',
        .long_name = "help",
        .help = help_prompt,
        .value = any_make(null, bool.typeid),
    };
}

macro group_opt(String group_name) {
    return ArgOpt {
        ._arg_type = ArgType.GROUP,
        .help = group_name,
    };
}

fn void! ArgParse.parse(&self, String[] argv) {
    assert(self.arguments.len == 0, "ArgParse already initialized or double parse call");
    assert(argv.len > 0);

    self._ctx.argc = argv.len - 1;
    self._ctx.cpidx = 0;
    self._ctx.argv = argv[1..];
    self._ctx.out = argv;

    if (!self.program_name){
        self.program_name = argv[0];
    }
    self.options_check()!;

    for (; self._ctx.argc && self._ctx.argv.len; self._ctx.argc--) {
        String arg = self._ctx.argv[0];
        // io::printf("arg: %s\n", arg);
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

            // Parsing stackted short args: when '-f -a -b' passed as '-fab'
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

    self.arguments = argv[self._ctx.cpidx + 1..]; // excludes 1st argv[0], program_name
}

fn void! ArgParse.print_usage(&self) {

    io::printf("\nUsage:\n");
    if (self.usage) {
        @pool() {
            // NOTE: prepending program_name to all usage line

            path::Path path = path::temp_new(self.program_name)!;
            foreach(usage:self.usage.tsplit("\n")) {
                usage = usage.trim();
                if(!usage){
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
    foreach(opt: self.options){
        usz len = 0;
        if(opt.short_name) {
            len += 2; 
            if(opt.long_name) len += 2; // extra for separator ', '
        }
        if(opt.long_name) {
            len += opt.long_name.len + 2;
        }

        if (opt.value){
            switch(opt.value.type) {
                case int.typeid:
                case uint.typeid:
                case long.typeid:
                case ulong.typeid:
                    len += 6; // strlen("=<int>")
                case float.typeid:
                case double.typeid:
                    len += 6; // strlen("=<flt>")
                case String.typeid:
                    len += 6; // strlen("=<str>")
            }
        }

        len = (len + 3) - ((len + 3) & 3);
        if (usage_opts_width < len) {
            usage_opts_width = len;
        }
    }
    usage_opts_width += 4; // 4 spaces prefix

    foreach(opt: self.options){
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
        if (opt.value){
            switch(opt.value.type) {
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
            if(opt.value) io::printf(" [default: %s]", opt.value); 
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

fault ArgError @private { 
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
$assert(ArgType.USER.ordinal == 0); // USER must be first to support ZII behavior


fn void! ArgParse.options_ensure_required(&self) @local
{
    bool has_errors = false;
    foreach(i, opt: self.options){
        if (opt._arg_type == ArgType.GROUP || opt._arg_type == ArgType.HELP) {
            continue;
        }

        if (opt.required && !opt._is_present) {
            io::printf(
                "Error: missing required option: -%c/--%s\n",
                opt.short_name,
                opt.long_name
            );
            has_errors = true;
        }
    }
    if (has_errors){
        return ArgError.MISSING_ARGUMENT?;
    }
}
fn void! ArgParse.options_check(&self) @local
{
    assert(self.arguments.len == 0, "already processed?");

    foreach(i, opt: self.options){
        if (opt._arg_type == ArgType.GROUP || opt._arg_type == ArgType.HELP) {
            continue;
        }

        if (!(opt.short_name || opt.long_name)) {
            unreachable("option[%d] both long/short_name are not set", i);
        }
        if (!opt.value) {
            unreachable("option[%d][-%c/--%s] has no valid .value= pointer set",
                        i, opt.short_name, opt.long_name);
        }

        switch (opt.value.type) {
            case bool.typeid:
            case int.typeid:
            case uint.typeid:
            case long.typeid:
            case ulong.typeid:
            case float.typeid:
            case double.typeid:
            case String.typeid:
                // resetting presence flag, it will be updated after all argument parsing
                opt._is_present = false;
            default:
                if (!opt.callback){
                    unreachable("option[%d][-%c/--%s] type[%s] is not supported (or pass .callback)", 
                                i, opt.short_name, opt.long_name, opt.value.type);
                }
        }
    }
}


fn String! ArgParse.get_arg(&self) @local {
    if(self._ctx.optvalue) {
        // --arg=<optvalue> via =
        String result = self._ctx.optvalue;
        self._ctx.optvalue = "";
        return result;
    } else if(self._ctx.argv.len > 1) {
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

    if (opt._is_present) {
        io::printf("Error: duplicated option %s\n", self._ctx.argv[0]);
        return ArgError.INVALID_ARGUMENT?; 
    }
    assert(opt.value);
    assert(opt.value.type == bool.typeid || !flag_unset, "flag_unset only for bool options");
    opt._is_present = true;

    if(opt.callback) {
        if (opt.value.type == bool.typeid) {
            return opt.callback(opt, (flag_unset) ? "0" : "1");
        } else {
            String aval = self.get_arg()!;
            return opt.callback(opt, aval);
        }
    }

    switch (opt.value.type) {
        case bool.typeid:
            bool* pvalue = anycast(opt.value, bool)!;
            *pvalue = (flag_unset) ? false : true;
        case String.typeid:
            *anycast(opt.value, String)! = self.get_arg()!;
        case int.typeid:
            String aval = self.get_arg()!;
            *anycast(opt.value, int)! = aval.to_integer(int)!;
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
    foreach(&opt: self.options){
        if (opt.short_name == self._ctx.optvalue[0]) {
            self._ctx.optvalue = self._ctx.optvalue.len > 1 ? self._ctx.optvalue[1..] : "";
            if(catch err = self.parse_arg_val(opt)){
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
            return; // ALL OK
        }
    }
    io::printf("Error: invalid argument %s\n", self._ctx.argv[0]);
    return ArgError.INVALID_ARGUMENT?;
}

fn String! prefix_skip(String s, String prefix) @local
{
    if(s.starts_with(prefix)) {
        return s[prefix.len..];
    } else {
        return ArgError.MISSING_ARGUMENT?;
    }
}

fn void! ArgParse.parse_long_arg(&self) @local
{
    foreach(&opt: self.options){
        if (!opt.long_name) {
            continue;
        }

        int opt_flags = 0;
        bool flag_unset = false;
        String! arg = prefix_skip(self._ctx.argv[0][2..], opt.long_name);
        if(catch err = arg){
            switch(err){
                case ArgError.MISSING_ARGUMENT:
                    // NOTE: for boolean flags it's possible to pass unset with '--no-<flag_name>'
                    if (opt.value.type == bool.typeid && 
                        self._ctx.argv[0][2..].starts_with("no-") &&
                        self._ctx.argv[0][5..].starts_with(opt.long_name))
                    {
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

        if(catch err = self.parse_arg_val(opt, flag_unset)){
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
        return; // ALL OK
    }

    io::printf("Error: invalid argument %s\n", self._ctx.argv[0]);
    return ArgError.INVALID_ARGUMENT?;
}

