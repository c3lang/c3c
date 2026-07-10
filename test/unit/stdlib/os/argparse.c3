module argparser_test;

import std::io;
import std::os::argparse;

enum LogLevel { DEBUG, INFO, WARNING, ERROR }

fn void? parse_port(String value)
{
	int? port = value.to_int();
	if (catch err = port)
	{
		io::eprintfn("Validation failed: '%s' is not a valid integer.", value);
		return argparse::INVALID_VALUE~;
	}

	if (port < 1 || port > 65535)
	{
		io::eprintfn("Validation failed: Port %d is out of range.", port);
		return argparse::INVALID_VALUE~;
	}

	io::printfn("Callback verified valid port: %d", port);
	return;
}

fn int main(String[] args)
{
	bool help = false;
	int verbosity = 0;
	float scale = 1.0f;
	String name;
	LogLevel log_level = LogLevel.INFO;
	String[3] input_files = {};
	String[10] raw_args = {};

	// Returns the index of the first argument not handled, or a parsing fault.
	sz? result = argparse::@parse(args,
		"h*", "help*",     "Display this help manual",             &help,
		"v+", "verbose+",  "Increment logging verbosity level",    &verbosity,
		"s",  "scale",     "Specify scale factor (float)",         &scale,
		"n",  "name",      "Override application name",            &name,
		"l",  "level",     "Select logging level (DEBUG, etc.)",   &log_level,
		"f",  "files",     "Target sequential files",              &input_files,
		"p",  "port=env:PORT", "Configure port (or fallback to $PORT)", &parse_port,
		&raw_args
	);

	if (catch err = result)
	{
		if (err == argparse::HELP_REQUESTED) return 0;

		io::eprintfn("Parsing failed with error: %s", err);
		return 1;
	}

	io::printfn("Verbosity level: %d", verbosity);
	io::printfn("Scale count:     %f", scale);
	io::printfn("App name:        %s", name);
	io::printfn("Logger level:    %s", log_level);
	io::printfn("Target files:    %s, %s, %s", input_files[0], input_files[1], input_files[2]);

	io::printn("Parsed Positional Arguments: ");
	for (sz i = 0; i < raw_args.len; ++i)
	{
		if (raw_args[i].len == 0) break;
		io::printf("%s ", raw_args[i]);
	}
	io::printn("\n");

	return 0;
}

//////////////
module argparser_test2;


import std::io;
import std::os::argparse;

fn int main2(String[] args)
{
	bool verbose = false;
	String[5] positionals = {};

	// Passing compile-time `true` after `args` enables strict mode
	sz? result = argparse::@parse(args, true,
		"v", "verbose", "Verbose output", &verbose,
		&positionals
	);

	if (catch err = result)
	{
		if (err == argparse::HELP_REQUESTED) return 0;
		io::eprintfn("Parsing failed: %s", err);
		return 1;
	}

	io::printfn("Verbose: %s", verbose ? "true" : "false");
	io::print("Positional arguments: ");
	for (sz i = 0; i < positionals.len; ++i)
	{
		if (positionals[i].len == 0) break;
		io::printf("'%s' ", positionals[i]);
	}
	io::printn("");

	return 0;
}

/////////////

module test_argparse;

import std::os::argparse;
import std::os::env;
import std::core::test;
import std::io;

enum TestEnum {
	RED,
	GREEN,
	BLUE
}

fn void? validate_non_system_port(String value)
{
	int? port = value.to_int();
	if (catch err = port) return argparse::INVALID_VALUE~;
	if (port < 1024) return argparse::INVALID_VALUE~;
	return;
}

fn void test_basic_options() @test
{
	String[] args = { "program", "-p", "8080", "--host", "127.0.0.1", "-v" };
	int port = 0;
	String host = "";
	bool verbose = false;

	sz? res = argparse::@parse(args,
		"p", "port", "The server port", &port,
		"s", "host", "The server host name", &host,
		"v", "verbose", &verbose
	);

	test::eq(6, res!!);
	test::eq(8080, port);
	test::eq("127.0.0.1", host);
	test::eq(true, verbose);
}

fn void test_optional_arguments() @test
{
	String[] args_omitted = { "program", "--opt" };
	bool opt_val = false;

	sz? res1 = argparse::@parse(args_omitted,
		"o?", "opt?", "An optional configuration value", &opt_val
	);
	test::eq(2, res1!!);
	test::eq(true, opt_val);

	String[] args_specified = { "program", "--opt=false" };
	bool opt_val2 = true;

	sz? res2 = argparse::@parse(args_specified,
		"o?", "opt?", "An optional configuration value", &opt_val2
	);
	test::eq(2, res2!!);
	test::eq(false, opt_val2);
}

fn void test_incremental_counters() @test
{
	String[] args = { "program", "-v", "-v", "--verbose", "-v" };
	int verbosity = 0;

	sz? res = argparse::@parse(args,
		"v+", "verbose+", "Logging verbosity selector", &verbosity
	);

	test::eq(5, res!!);
	test::eq(4, verbosity);
}

fn void test_array_collection() @test
{
	String[] args = { "program", "-f", "file1.txt", "--file", "file2.txt" };
	String[3] files = {};

	sz? res = argparse::@parse(args,
		"f", "file", "Target input files", &files
	);

	test::eq(5, res!!);
	test::eq("file1.txt", files[0]);
	test::eq("file2.txt", files[1]);
	test::eq("", files[2]);
}

fn void test_enum_mapping() @test
{
	String[] args_name = { "program", "-c", "green" };
	TestEnum color_name = TestEnum.RED;

	sz? res1 = argparse::@parse(args_name,
		"c", "color", "Desired display color", &color_name
	);
	test::eq(3, res1!!);
	test::eq(TestEnum.GREEN, color_name);

	String[] args_ord = { "program", "-c", "2" };
	TestEnum color_ord = TestEnum.RED;

	sz? res2 = argparse::@parse(args_ord,
		"c", "color", "Desired display color", &color_ord
	);
	test::eq(3, res2!!);
	test::eq(TestEnum.BLUE, color_ord);
}

fn void test_callbacks() @test
{
	String[] args_valid = { "program", "-p", "8080" };
	sz? res_valid = argparse::@parse(args_valid,
		"p", "port", "Custom validated port", &validate_non_system_port
	);
	test::eq(3, res_valid!!);

	String[] args_invalid = { "program", "-p", "80" };
	test::@error(
		argparse::@parse(args_invalid, "p", "port", "Custom validated port", &validate_non_system_port),
		argparse::INVALID_VALUE
	);
}

fn void test_positional_capture() @test
{
	String[] args = { "program", "input1.txt", "-v", "input2.txt" };
	bool verbose = false;
	String[4] positionals = {};

	sz? res = argparse::@parse(args,
		"v", "verbose", "Enable logging verbosity", &verbose,
		&positionals
	);

	test::eq(4, res!!);
	test::eq(true, verbose);
	test::eq("input1.txt", positionals[0]);
	test::eq("input2.txt", positionals[1]);
	test::eq("", positionals[2]);
}

fn void test_fault_propagation() @test
{
	String[] args_missing = { "program", "-p" };
	int port = 0;
	test::@error(
		argparse::@parse(args_missing, "p", "port", "Port selection", &port),
		argparse::MISSING_ARGUMENT
	);

	String[] args_illegal = { "program", "-x" };
	bool dummy = false;
	test::@error(
		argparse::@parse(args_illegal, "d", "dummy", "Dummy state flag", &dummy),
		argparse::ILLEGAL_OPTION
	);

	String[] args_overflow = { "program", "-f", "1", "-f", "2" };
	String[1] files = {};
	test::@error(
		argparse::@parse(args_overflow, "f", "file", "Target file list", &files),
		argparse::OUT_OF_BOUNDS
	);
}

fn void test_negative_numbers_positional() @test
{
	String[] args = { "program", "-1", "-2" };
	String[3] positionals = {};

	sz? res = argparse::@parse(args,
		&positionals
	);

	test::eq(3, res!!);
	test::eq("-1", positionals[0]);
	test::eq("-2", positionals[1]);
}

fn void test_env_fallback() @test
{
	env::set_var("PORT", "9090");

	String[] args = { "program" };
	int port = 8080;

	sz? res = argparse::@parse(args,
		"p", "port=env:PORT", "Target port", &port
	);

	test::eq(1, res!!);
	test::eq(9090, port);
}

fn void test_strict_mode() @test
{
	String[] args = { "program", "pos1.txt", "-v" };
	bool verbose = false;
	String[2] positionals = {};

	sz? res = argparse::@parse(args, true,
		"v", "verbose", "Verbose mode", &verbose,
		&positionals
	);

	test::eq(3, res!!);
	test::eq(false, verbose);
	test::eq("pos1.txt", positionals[0]);
	test::eq("-v", positionals[1]);
}

fn void test_help_requested() @test
{
	String[] args = { "program", "--help" };
	bool help = false;

	test::@error(
		argparse::@parse(args,
			"h*", "help*", "Show help options", &help
		),
		argparse::HELP_REQUESTED
	);
}

fn void test_inline_long_options() @test
{
	String[] args = { "program", "--port=9000" };
	int port = 0;

	sz? res = argparse::@parse(args,
		"p", "port", "Server port", &port
	);

	test::eq(2, res!!);
	test::eq(9000, port);
}

fn void test_grouped_short_flags_with_value() @test
{
	String[] args = { "program", "-vp", "9000" };
	int verbosity = 0;
	int port = 0;

	sz? res = argparse::@parse(args,
		"v+", "verbose+", "Increase verbosity", &verbosity,
		"p",  "port",     "Server port",      &port
	);

	test::eq(3, res!!);
	test::eq(1, verbosity);
	test::eq(9000, port);
}

fn void test_invalid_enum_matching() @test
{
	String[] args = { "program", "-c", "invalid_color" };
	TestEnum color = TestEnum.RED;

	test::@error(
		argparse::@parse(args,
			"c", "color", "Desired display color", &color
		),
		argparse::INVALID_ENUM_VALUE
	);
}

fn void test_env_fallback_missing() @test
{
	env::clear_var("ENV_DUMMY");

	String[] args = { "program" };
	int target = 42;

	sz? res = argparse::@parse(args,
		"t", "target=env:ENV_DUMMY", "Target value", &target
	);

	test::eq(1, res!!);
	test::eq(42, target);
}

fn void test_end_of_options_marker() @test
{
	String[] args = { "program", "-v", "--", "-p", "8080" };
	bool verbose = false;
	String[3] positionals = {};

	sz? res = argparse::@parse(args,
		"v", "verbose", "Verbose mode", &verbose,
		&positionals
	);

	test::eq(5, res!!);
	test::eq(true, verbose);
	test::eq("-p", positionals[0]);
	test::eq("8080", positionals[1]);
}

fn void? validate_env_port(String value)
{
	int? port = value.to_int();
	if (catch err = port) return argparse::INVALID_VALUE~;
	if (port != 9999) return argparse::INVALID_VALUE~;
	return;
}

fn void test_float_double_parsing() @test
{
	String[] args = { "program", "-f", "3.14", "-d", "2.71828" };
	float fval = 0.0f;
	double dval = 0.0;

	sz? res = argparse::@parse(args,
		"f", "float", &fval,
		"d", "double", &dval
	);

	test::eq(5, res!!);
	test::eq(3.14f, fval);
	test::eq(2.71828, dval);
}

fn void test_positional_overflow() @test
{
	String[] args = { "program", "pos1", "pos2", "pos3" };
	String[2] positionals = {};

	test::@error(
		argparse::@parse(args, &positionals),
		argparse::OUT_OF_BOUNDS
	);
}

fn void test_env_fallback_advanced() @test
{
	env::set_var("FALLBACK_COLOR", "BLUE");
	env::set_var("FALLBACK_PORT", "9999");
	env::set_var("FALLBACK_FLAG", "true");

	String[] args = { "program" };
	TestEnum color = TestEnum.RED;
	bool flag = false;

	sz? res = argparse::@parse(args,
		"c", "color=env:FALLBACK_COLOR", &color,
		"p", "port=env:FALLBACK_PORT", &validate_env_port,
		"f", "flag=env:FALLBACK_FLAG", &flag
	);

	test::eq(1, res!!);
	test::eq(TestEnum.BLUE, color);
	test::eq(true, flag);
}

fn void test_env_fallback_invalid_enum() @test
{
	env::set_var("BAD_COLOR", "YELLOW"); // Not in TestEnum

	String[] args = { "program" };
	TestEnum color = TestEnum.RED;

	test::@error(
		argparse::@parse(args,
			"c", "color=env:BAD_COLOR", &color
		),
		argparse::INVALID_ENUM_VALUE
	);
}

fn void test_large_integer_precision() @test
{
	String[] args = { "program", "-l", "5000000000", "-u", "9000000000000" };
	long lval = 0;
	ulong ulval = 0;

	sz? res = argparse::@parse(args,
		"l", "long_val", &lval,
		"u", "ulong_val", &ulval
	);

	test::eq(5, res!!);
	test::eq((long)5000000000, lval);
	test::eq((ulong)9000000000000, ulval);
}