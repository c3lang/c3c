module test::std::os::argparse @test;
import std::os::argparse @public;
import std::io;

fn void test_simple_arg_init() {

	int val = 0;
	String s;
	float f = 7.0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.epilog= "ArgParse epilog",
		.usage = `option -sdf -[sdf]`,
		.options = { 
			argparse::help_opt(),
			argparse::group_opt("Basic options"),
			{.short_name = 'a', .long_name = "all", .value = &val,  .help = "This is a sample option"},
			{.short_name = 's', .long_name = "string", .value = &s,  .help = "Simple string"},
			{.short_name = 'f', .long_name = "flt", .value = &f, .help = "float arg"},
		}
	};

    String[] args = {"testprog"}; 
	agp.parse(args)!!;

	test::equal(agp.usage, "option -sdf -[sdf]");
	test::equal(agp.description, "test ArgParse program");
	test::equal(agp.epilog, "ArgParse epilog");
	test::equal(agp.options.len, 5);
	test::equal(agp.program_name, "testprog");
}

fn void test_help_show() {

	int val = 0;
	String s;
	float f = 7.0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			argparse::help_opt(),
		}
	};

    String[] args = {"testprog", "-h"}; 
	test::@error(agp.parse(args), ArgError.HELP_SHOW);
}

fn void test_help_show_long() {

	int val = 0;
	String s;
	float f = 7.0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			argparse::help_opt(),
		}
	};

    String[] args = {"testprog", "--help"}; 
	test::@error(agp.parse(args), ArgError.HELP_SHOW);
}

fn void test_bool_flag() {

	bool val = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "-f"}; 
    test::equal(val, false);
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, true);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_bool_flag_default() {

	bool val = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog"}; 
    test::equal(val, true);
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, true);
    test::equal(agp.options[0]._is_present, false);
}

fn void test_bool_flag_unset() {

	bool val = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--no-flt"}; 
    test::equal(val, true);
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, false);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_str_default() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog"}; 
    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);
}

fn void test_str_set_equal() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--flt=bar"}; 
    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, "bar");
    test::equal(agp.options[0]._is_present, true);
}

fn void test_str_set_next_arg() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--flt", "bar"}; 
    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, "bar");
    test::equal(agp.options[0]._is_present, true);
}


fn void test_str_set_short_arg() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "-f", "bar"}; 
    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, "bar");
    test::equal(agp.options[0]._is_present, true);
}

fn void test_invalid_arg_short() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "-z"}; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_invalid_arg_long() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--zooo"}; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_missing_require_argument() {

	bool val = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .required = true},
		}
	};

    String[] args = {"testprog"}; 
	test::@error(agp.parse(args), ArgError.MISSING_ARGUMENT);
}


fn void test_missing_require_arguments_many() {

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .required = true},
			{.short_name = 'z', .long_name = "zzz", .value = &val, .required = true},
		}
	};

    String[] args = {"testprog"}; 
	test::@error(agp.parse(args), ArgError.MISSING_ARGUMENT);
}

fn void test_passing_short_option_after_argument() {

	bool val = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "foo", "-f"}; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_passing_long_option_after_argument() {

	bool val = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "foo", "--flt"}; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_multiple_short_args() {

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
			{.short_name = 'z', .long_name = "zzz", .value = &val2},
		}
	};

    String[] args = {"testprog", "-f", "-z"}; 
    agp.parse(args)!!;

    test::equal(val, true);
    test::equal(val2, true);
    test::equal(agp.options[0]._is_present, true);
    test::equal(agp.options[1]._is_present, true);
}

fn void test_str_set_next_arg_missing_value_with_eqchar() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--flt="}; 
	test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_str_set_next_arg_missing_value() {

	String val = "foo";
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
		}
	};

    String[] args = {"testprog", "--flt"}; 
	test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_str_set_next_arg_is_another_option() {

	String val = "foo";
	bool val2 = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
			{.short_name = 'a',  .value = &val},
		}
	};

    String[] args = {"testprog", "--flt", "-a" }; 
	test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_str_set_next_arg_is_another_option_long() {

	String val = "foo";
	bool val2 = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val},
			{.long_name = "all",  .value = &val},
		}
	};

    String[] args = {"testprog", "--flt", "--all" }; 
	test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_duplicated_flag(){

	bool val2 = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "all",  .value = &val2},
		}
	};

    String[] args = {"testprog", "--all", "--all" }; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_integer_option_i32(){

	int val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "--int=3"}; 
    agp.parse(args)!!;

    test::equal(val, 3);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_integer_option_parsing_error(){

	int val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'i', .long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "--int=a"}; 
    test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_integer_option_parsing_error_short(){

	int val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'i', .long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "-i", "a"}; 
    test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_integer_option_u32(){

	uint val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "--int=3"}; 
    agp.parse(args)!!;

    test::equal(val, 3);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_integer_option_i64(){

	long val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "--int=3"}; 
    agp.parse(args)!!;

    test::equal(val, 3);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_integer_option_u64(){

	ulong val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "int",  .value = &val},
		}
	};

    String[] args = {"testprog", "--int=3"}; 
    agp.parse(args)!!;

    test::equal(val, 3);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_float_option_f32(){

	float val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "float",  .value = &val},
		}
	};

    String[] args = {"testprog", "--float=3.23"}; 
    agp.parse(args)!!;

    test::almost_equal(val, 3.23);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_float_option_f64(){

	double val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "float",  .value = &val},
		}
	};

    String[] args = {"testprog", "--float=3.23"}; 
    agp.parse(args)!!;

    test::almost_equal(val, 3.23);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_float_option_f64_nan(){

	double val = 0;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "float",  .value = &val},
		}
	};

    String[] args = {"testprog", "--float=nan"}; 
    agp.parse(args)!!;

    test::almost_equal(val, double.nan);
    test::equal(agp.options[0]._is_present, true);
}

fn void test_multi_short_flags(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		}
	};

    String[] args = {"testprog", "-ab"}; 
	agp.parse(args)!!;

	test::equal(val, true);
	test::equal(val2, true);
}

fn void test_empty_argument(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		}
	};

    String[] args = {"testprog", ""}; 
	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_multi_arguments_double_dash(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		}
	};

    String[] args = {"testprog", "-ab", "--", "--some-opt=4323", "arg1", "arg2"}; 
	agp.parse(args)!!;

	test::equal(val, true);
	test::equal(val2, true);
	test::equal(agp.arguments.len, 3);
	test::equal(agp.arguments[0], "--some-opt=4323");
	test::equal(agp.arguments[1], "arg1");
	test::equal(agp.arguments[2], "arg2");
}

fn void test_multi_arguments(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		}
	};

    String[] args = {"testprog", "-ab", "arg1", "arg2"}; 
	agp.parse(args)!!;

	test::equal(val, true);
	test::equal(val2, true);
	test::equal(agp.arguments.len, 2);
	test::equal(agp.arguments[0], "arg1");
	test::equal(agp.arguments[1], "arg2");
}

fn void test_multi_arguments_stop_at_non_option(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		},
		.flags = {
			.stop_at_non_option = true,
		}
		
	};

    String[] args = {"testprog", "command", "-ab", "arg1", "arg2"}; 
	agp.parse(args)!!;

	test::equal(val, false);
	test::equal(val2, false);
	test::equal(agp.arguments.len, 4);
	test::equal(agp.arguments[0], "command");
	test::equal(agp.arguments[1], "-ab");
	test::equal(agp.arguments[2], "arg1");
	test::equal(agp.arguments[3], "arg2");
}

fn void test_subcommands(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		},
		.flags = {
			.stop_at_non_option = true, // NOTE: this is crucial for sub commands 
		}
		
	};

	int cmd_int = 0;
	argparse::ArgParse agp_cmd = {
		.description = "test sub command",
		.options = { 
			{.long_name = "int",  .value = &cmd_int},
		},
	};

    String[] args = {"testprog", "-ab", "command", "--int=4", "arg1", "arg2"}; 
	agp.parse(args)!!;


	// passed "-ab" flags to higher level
	test::equal(val, true);
	test::equal(val2, true);
	test::equal(agp.arguments.len, 4);
	test::equal(agp.arguments[0], "command");
	test::equal(agp.arguments[1], "--int=4");
	test::equal(agp.arguments[2], "arg1");
	test::equal(agp.arguments[3], "arg2");

	// NOTE: passing agr.arguments to sub command
	agp_cmd.parse(agp.arguments)!!;
	test::equal(cmd_int, 4);
	test::equal(agp_cmd.arguments.len, 2);
	test::equal(agp_cmd.arguments[0], "arg1");
	test::equal(agp_cmd.arguments[1], "arg2");
}

fn void test_custom_type_callback_unknown_type() {

	char val = '\0';
	ArgParseCallbackFn cbf = fn void! (ArgOpt* opt, String value) {
		test::equal(value, "bar");
        *anycast(opt.value, char)! = value[0];
	};

	argparse::ArgParse agp = {
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .callback = cbf},
		}
	};

    String[] args = {"testprog", "--flt=bar"}; 
    test::equal(val, '\0');
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, 'b');
    test::equal(agp.options[0]._is_present, true);
}

fn void test_custom_type_callback_str_type() {

	String val = "foo";
	ArgParseCallbackFn cbf = fn void! (ArgOpt* opt, String value) {
        *anycast(opt.value, String)! = value[0..1];
	};

	argparse::ArgParse agp = {
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .callback = cbf},
		}
	};

    String[] args = {"testprog", "--flt=bar"}; 
    test::equal(val, "foo");
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, "ba");
    test::equal(agp.options[0]._is_present, true);
}

fn void test_custom_type_callback_str_type_bad_arg() {

	String val = "foo";
	ArgParseCallbackFn cbf = fn void! (ArgOpt* opt, String value) {
        *anycast(opt.value, String)! = value[0..1];
	};

	argparse::ArgParse agp = {
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .callback = cbf},
		}
	};

    String[] args = {"testprog", "--flt="}; 
	test::@error(agp.parse(args), ArgError.ARGUMENT_VALUE);
}

fn void test_custom_type_callback_bool() {

	bool val = false;
	ArgParseCallbackFn cbf = fn void! (ArgOpt* opt, String value) {
		test::equal(value, "1");
		if (value == "1"){
        	*anycast(opt.value, bool)! = true;
		} 
		else if (value == "1"){
        	*anycast(opt.value, bool)! = false;
		} else {
			unreachable();
		}
	};

	argparse::ArgParse agp = {
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .callback = cbf},
		}
	};

    String[] args = {"testprog", "--flt"}; 
	agp.parse(args)!!;
	test::equal(val, true);
}

fn void test_custom_type_callback_bool_unset() {

	bool val = true;
	ArgParseCallbackFn cbf = fn void! (ArgOpt* opt, String value) {
		test::equal(value, "0");
		if (value == "1"){
        	*anycast(opt.value, bool)! = true;
		} 
		else if (value == "0"){
        	*anycast(opt.value, bool)! = false;
		} else {
			unreachable();
		}
	};

	argparse::ArgParse agp = {
		.options = { 
			{.short_name = 'f', .long_name = "flt", .value = &val, .callback = cbf},
		}
	};

    String[] args = {"testprog", "--no-flt"}; 
	agp.parse(args)!!;
	test::equal(val, false);
}

fn void test_bool_flag_unset_no_short() {

	bool val = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "color", .value = &val,
				.help = "Test output with colors, --no-color to disable"},
		}
	};

    String[] args = {"testprog", "--no-flt"}; 
    test::equal(val, true);
    test::equal(agp.options[0]._is_present, false);

	test::@error(agp.parse(args), ArgError.INVALID_ARGUMENT);
}

fn void test_bool_flag_unset_has_arg() {

	bool val = true;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.long_name = "color", .value = &val,
				.help = "Test output with colors, --no-color to disable"},
		}
	};

    String[] args = {"testprog", "--no-color"}; 
    test::equal(val, true);
    test::equal(agp.options[0]._is_present, false);

	agp.parse(args)!!;

    test::equal(val, false);
    test::equal(agp.options[0]._is_present, true);
}
fn void test_short_arguments(){

	bool val = false;
	bool val2 = false;
	argparse::ArgParse agp = {
		.description = "test ArgParse program",
		.options = { 
			{.short_name = 'a',  .value = &val},
			{.short_name = 'b',  .value = &val2},
		}
	};

    String[] args = {"testprog", ".", "-"}; 
	agp.parse(args)!!;

	test::equal(val, false);
	test::equal(val2, false);
	test::equal(agp.arguments.len, 2);
	test::equal(agp.arguments[0], ".");
	test::equal(agp.arguments[1], "-");
}
