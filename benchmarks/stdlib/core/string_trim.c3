module string_trim_wars;

const String WHITESPACE_TARGET = "    \n\t\r\f\va \tbcde\v\f\r\t\n    ";
const String WHITESPACE_NUMERIC_TARGET = " 25290   0969  99a \tbcde12332 34 43          0000";

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(64);
	set_benchmark_max_iterations(1 << 24);
}

macro void trim_bench($trim_str, String $target = WHITESPACE_TARGET) => @pool()
{
	String s1;
	String s2 = $target.tcopy();

	runtime::@start_benchmark();

	$switch:
	$case $typeof($trim_str) == String:
		s1 = s2.trim($trim_str);
	$case $typeof($trim_str) == AsciiCharset:
		s1 = s2.trim_charset($trim_str);
	$default: $error "Unable to determine the right String `trim` operation to use.";
	$endswitch

	@volatile_load(s1);

	runtime::@end_benchmark();
}


module string_trim_wars @benchmark;

fn void trim_control() => trim_bench(" ");   // only spaces

fn void trim_whitespace_default() => trim_bench("\t\n\r ");   // default set
fn void trim_whitespace_default_ordered() => trim_bench(" \n\t\r");   // default \w set, but ordered by expected freq

fn void trim_whitespace_bad() => trim_bench("\f\v\n\t\r ");   // bad-perf ordering, all \w

fn void trim_whitespace_ordered_extended() => trim_bench(" \n\t\r\f\v");   // proposed ordering, all \w
fn void trim_charset_whitespace() => trim_bench(ascii::WHITESPACE_SET);   // use charset, all \w

fn void trim_many() => trim_bench(" \n\t\r\f\v0123456789", WHITESPACE_NUMERIC_TARGET);   // ordered, all \w + num
fn void trim_charset_many() => trim_bench(ascii::WHITESPACE_SET | ascii::NUMBER_SET, WHITESPACE_NUMERIC_TARGET);   // set, all \w + num
