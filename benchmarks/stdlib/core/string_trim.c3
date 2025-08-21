module string_trim_wars;

const String WHITESPACE_TARGET = "    \n \t \ra \tbcde\r\n \t  \v";
const String WHITESPACE_NUMERIC_TARGET = " 25290   09\n69 \t 99\ra \tbcde123\r\n32 3\t4 43 \v0000\t";

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(64);
	set_benchmark_max_iterations(1 << 24);
}

macro String trim_bench($trim_str, String $target = WHITESPACE_TARGET)
{
	String s1;

	$switch:
	$case @typeis($trim_str, String):
		s1 = $target.trim($trim_str);
	$case @typeis($trim_str, AsciiCharset):
		s1 = $target.trim_charset($trim_str);
	$default: $error "Unable to determine the right String `trim` operation to use.";
	$endswitch

	return s1;
}


module string_trim_wars @benchmark;

fn void trim_control() => (void)trim_bench("");   // no trim

fn void trim_whitespace_default() => (void)trim_bench("\t\n\r ");   // default set
fn void trim_whitespace_default_ordered() => (void)trim_bench(" \n\t\r");   // default \w set, but ordered by expected freq

fn void trim_whitespace_bad() => (void)trim_bench("\f\v\n\t\r ");   // bad-perf ordering, all \w

fn void trim_whitespace_ordered_extended() => (void)trim_bench(" \n\t\r\f\v");   // proposed ordering, all \w
fn void trim_charset_whitespace() => (void)trim_bench(ascii::WHITESPACE_SET);   // use charset, all \w

fn void trim_many() => (void)trim_bench(" \n\t\r\f\v0123456789", WHITESPACE_NUMERIC_TARGET);   // ordered, all \w + num
fn void trim_charset_many() => (void)trim_bench(ascii::WHITESPACE_SET | ascii::NUMBER_SET, WHITESPACE_NUMERIC_TARGET);   // set, all \w + num
