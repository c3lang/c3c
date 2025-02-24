module test::module_a;
import std::io;

macro line() => $$LINE;
macro func() => $$FUNC;
macro mod() => $$MODULE;

macro line_indirect() => line();
macro func_indirect() => func();
macro mod_indirect() => mod();

module test @test;

fn void test_builtins()
{
	assert(module_a::line() == module_a::line_indirect());
	assert(module_a::line() == $$LINE);
	assert(module_a::func() == module_a::func_indirect());
	assert(module_a::func() == $$FUNC);
	assert(module_a::mod() == module_a::mod_indirect());
	assert(module_a::mod() == $$MODULE);
}