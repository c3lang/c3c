module param_identity;

fn void foo(int a, double b) { }
fn void bar(int x, double y) { }
fn void same_sig(int a, double b) { }
fn void defaulted(int a, int b = 5) { }
fn void typed_va(int x, double... y) { }
fn void untyped_va(int x, y...) { }
extern fn void c_va(int x, ...);
extern fn void unnamed(int, double);
fn void declared_later(int q) { }

struct Data { int v; }
fn void Data.method(&self, int a) { }

interface Iface
{
	fn void ifn(int a, double b);
}

alias FooParams = $Typefrom($Typeof(foo)::param_struct);
alias BarParams = $Typefrom($Typeof(bar)::param_struct);
alias DefaultedParams = $Typefrom($Typeof(defaulted)::param_struct);
alias TypedVaParams = $Typefrom($Typeof(typed_va)::param_struct);
alias UntypedVaParams = $Typefrom($Typeof(untyped_va)::param_struct);
alias CVaParams = $Typefrom($Typeof(c_va)::param_struct);
alias UnnamedParams = $Typefrom($Typeof(unnamed)::param_struct);
alias LaterParams = $Typefrom($Typeof(declared_later)::param_struct);
alias IfaceParams = $Typefrom($Typeof(Iface.ifn)::param_struct);
alias MethodParams = $Typefrom($Typeof(Data.method)::param_struct);

fn void member_names_and_types_follow_the_parameters()
{
	$assert FooParams::members.len == 2;
	$assert FooParams::members[0].name == "a";
	$assert FooParams::members[1].name == "b";
	$assert FooParams::members[0].type == int;
	$assert FooParams::members[1].type == double;
}

fn void the_struct_is_cached_per_function()
{
	$assert $Typeof(foo)::param_struct == $Typeof(foo)::param_struct;
	$assert $reflect(foo).param_struct == $Typeof(foo)::param_struct;
}

fn void distinct_functions_get_distinct_structs()
{
	$assert $Typeof(foo)::param_struct != $Typeof(bar)::param_struct;
	$assert BarParams::members[0].name == "x";
	$assert BarParams::members[1].name == "y";

	$assert $Typeof(foo)::param_struct != $Typeof(same_sig)::param_struct;
}

fn void defaulted_parameters_are_included()
{
	$assert DefaultedParams::members.len == 2;
	$assert DefaultedParams::members[1].name == "b";
}

fn void varargs_are_reflected_as_slices()
{
	$assert TypedVaParams::members[1].type == double[];
	$assert UntypedVaParams::members[1].type == any[];
	// C-style varargs contribute no member.
	$assert CVaParams::members.len == 1;
}

fn void unnamed_parameters_get_positional_names()
{
	$assert UnnamedParams::members[0].name == "__anon_0";
	$assert UnnamedParams::members[1].name == "__anon_1";
}

fn void functions_declared_after_use_resolve()
{
	$assert LaterParams::members[0].name == "q";
}

fn void methods_include_the_receiver()
{
	$assert MethodParams::members.len == 2;
	$assert MethodParams::members[0].name == "self";
	$assert MethodParams::members[1].name == "a";
	$assert MethodParams::members[0].type == Data*;

	$assert IfaceParams::members.len == 3;
	$assert IfaceParams::members[1].name == "a";

	$assert $Typeof(Data.method)::param_struct == $Typeof(Data.method)::param_struct;
}

fn void nullary() { }

fn void functions_without_parameters_reflect_as_void()
{
	$assert $Typeof(nullary)::param_struct == void;
	$assert $Typeof(foo)::param_struct != void;
}
