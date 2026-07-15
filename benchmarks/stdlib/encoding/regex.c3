module regex_benchmarks;
import std;
import std::encoding::regex;

const String PATTERN_SIMPLE = "hello";
const String PATTERN_MEDIUM = "(\\w+)@(\\w+\\.\\w+)";
const String PATTERN_COMPLEX = "(a|b|c|d|e|f|g|h|i|j)*x";
const String PATTERN_UNICODE = "(Привет|Мир|Язык|Программирование)*x";
const String PATTERN_CAPTURES = "(\\w+)-(\\w+)-(\\w+)-(\\w+)-(\\w+)";

Regex re_simple;
Regex re_medium;
Regex re_complex;
Regex re_unicode;
Regex re_captures;

RegexT glibc_re_simple;
RegexT glibc_re_medium;
RegexT glibc_re_complex;
RegexT glibc_re_unicode;
RegexT glibc_re_captures;

String input_small;
String input_medium;
String input_large;
String input_email;
String input_unicode;
String input_captures = "one-two-three-four-five";

struct RegexT @align(16)
{
	void*[16] __space;
}

extern fn int regcomp(RegexT* preg, char* regex, CompileFlags cflags = {});
extern fn int regexec(RegexT* preg, char* string, usz nmatch, RegMatch* pmatch, ExecFlags eflags = {});
extern fn void regfree(RegexT* preg);

const int REG_NOMATCH = 1;


fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(2_000_000);

	set_benchmark_func_iterations($reflect(compile_pattern_simple_c3).qname,    115_000);
	set_benchmark_func_iterations($reflect(compile_pattern_simple_glibc).qname, 350_000);
	set_benchmark_func_iterations($reflect(compile_pattern_medium_c3).qname,     49_000);
	set_benchmark_func_iterations($reflect(compile_pattern_medium_glibc).qname,  55_000);
	set_benchmark_func_iterations($reflect(compile_pattern_complex_c3).qname,    18_000);
	set_benchmark_func_iterations($reflect(compile_pattern_complex_glibc).qname, 43_000);

	set_benchmark_func_iterations($reflect(match_simple_c3).qname,             440_000);
	set_benchmark_func_iterations($reflect(match_simple_glibc).qname,        1_800_000);

	set_benchmark_func_iterations($reflect(match_medium_c3).qname,              52_000);
	set_benchmark_func_iterations($reflect(match_medium_glibc).qname,             75_000);

	set_benchmark_func_iterations($reflect(match_captures_c3).qname,            66_000);
	set_benchmark_func_iterations($reflect(match_captures_glibc).qname,           81_000);

	set_benchmark_func_iterations($reflect(match_complex_small_c3).qname,        8_300);
	set_benchmark_func_iterations($reflect(match_complex_small_glibc).qname,     8_200);

	set_benchmark_func_iterations($reflect(match_complex_medium_c3).qname,         820);
	set_benchmark_func_iterations($reflect(match_complex_medium_glibc).qname,        85);

	set_benchmark_func_iterations($reflect(match_complex_large_c3).qname,           83);
	set_benchmark_func_iterations($reflect(match_complex_large_glibc).qname,          1);

	set_benchmark_func_iterations($reflect(match_unicode_c3).qname,              2_300);
	set_benchmark_func_iterations($reflect(match_unicode_glibc).qname,             180);

	set_benchmark_func_iterations($reflect(match_and_compile_simple_c3).qname,    82_000);
	set_benchmark_func_iterations($reflect(match_and_compile_simple_glibc).qname, 300_000);
	set_benchmark_func_iterations($reflect(match_and_compile_medium_c3).qname,    25_000);
	set_benchmark_func_iterations($reflect(match_and_compile_medium_glibc).qname, 32_000);
	set_benchmark_func_iterations($reflect(match_and_compile_complex_c3).qname,       81);
	set_benchmark_func_iterations($reflect(match_and_compile_complex_glibc).qname,      1);

	set_benchmark_func_iterations($reflect(stress_backtracking_c3).qname,               35_000);
	set_benchmark_func_iterations($reflect(stress_backtracking_glibc).qname,            35_000);
	set_benchmark_func_iterations($reflect(stress_overlapping_alternation_c3).qname,    20_000);
	set_benchmark_func_iterations($reflect(stress_overlapping_alternation_glibc).qname, 20_000);
	set_benchmark_func_iterations($reflect(stress_massive_alternation_c3).qname,         2_000);
	set_benchmark_func_iterations($reflect(stress_massive_alternation_glibc).qname,      4_000);
	set_benchmark_func_iterations($reflect(stress_multi_wildcard_c3).qname,                100);
	set_benchmark_func_iterations($reflect(stress_multi_wildcard_glibc).qname,           4_000);

	input_small   = repeat_str("a", 100);
	input_medium  = repeat_str("a", 1000);
	input_large   = repeat_str("a", 10000);
	input_unicode = repeat_str("Привет", 200);

	input_email = "contact: somemail@c3lang.org today, please send to akjashkd@c3lang.org too.";

	re_simple  = regex::compile(mem, PATTERN_SIMPLE)!!;
	re_medium  = regex::compile(mem, PATTERN_MEDIUM, EXTENDED)!!;
	re_complex = regex::compile(mem, PATTERN_COMPLEX, EXTENDED)!!;
	re_unicode = regex::compile(mem, PATTERN_UNICODE, EXTENDED)!!;
	re_captures = regex::compile(mem, PATTERN_CAPTURES, EXTENDED)!!;

	int rc;
	rc = regcomp(&glibc_re_simple, PATTERN_SIMPLE);
	assert(rc == 0);
	rc = regcomp(&glibc_re_medium, PATTERN_MEDIUM, regex::CompileFlags.EXTENDED);
	assert(rc == 0);
	rc = regcomp(&glibc_re_complex, PATTERN_COMPLEX, regex::CompileFlags.EXTENDED);
	assert(rc == 0);
	rc = regcomp(&glibc_re_unicode, PATTERN_UNICODE, regex::CompileFlags.EXTENDED);
	assert(rc == 0);
	rc = regcomp(&glibc_re_captures, PATTERN_CAPTURES, regex::CompileFlags.EXTENDED);
	assert(rc == 0);

	assert(@ok(re_simple.texec("say hello world")));
	regexec(&glibc_re_simple, "say hello world", 0, null);

	assert(@ok(re_medium.texec(input_email, 3)));
	RegMatch[3] pm_med;
	regexec(&glibc_re_medium, input_email, 3, &pm_med);

	assert(@ok(re_captures.texec(input_captures, 6)));
	RegMatch[6] pm_cap;
	regexec(&glibc_re_captures, input_captures, 6, &pm_cap);

	if (catch err = re_complex.texec(input_small))
	{
		assert(err == regex::NOMATCH);
	}
	int rx = regexec(&glibc_re_complex, input_small, 0, null);
	assert(rx == REG_NOMATCH);

	if (catch err = re_unicode.texec(input_unicode))
	{
		assert(err == regex::NOMATCH);
	}
	rx = regexec(&glibc_re_unicode, input_unicode, 0, null);
	assert(rx == REG_NOMATCH);
}

fn String repeat_str(String s, sz count)
{
	char[] arr = alloc::alloc_array(mem, char, s.len * count + 1);
	for (sz i = 0; i < count; i++)
	{
		mem::copy(&arr[i * s.len], s, s.len);
	}
	arr[s.len * count] = 0;
	return (String)arr[0 : s.len * count];
}

// =========================================================================
// ======================================================================================
// =======================================================================================
module regex_benchmarks @benchmark;

import std::encoding::regex;

fn void compile_pattern_simple_c3()
{
	Regex re = regex::compile(mem, PATTERN_SIMPLE)!!;
	re.free();
}

fn void compile_pattern_simple_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_SIMPLE);
	regfree(&re);
}

fn void compile_pattern_medium_c3()
{
	Regex re = regex::compile(mem, PATTERN_MEDIUM, EXTENDED)!!;
	re.free();
}

fn void compile_pattern_medium_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_MEDIUM, regex::CompileFlags.EXTENDED);
	regfree(&re);
}

fn void compile_pattern_complex_c3()
{
	Regex re = regex::compile(mem, PATTERN_COMPLEX, EXTENDED)!!;
	re.free();
}

fn void compile_pattern_complex_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_COMPLEX, regex::CompileFlags.EXTENDED);
	regfree(&re);
}

// Compile-and-Match Benchmarks

fn void match_and_compile_simple_c3()
{
	Regex re = regex::compile(mem, PATTERN_SIMPLE)!!;
	(void)re.texec("say hello world", 0);
	re.free();
}

fn void match_and_compile_simple_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_SIMPLE);
	regexec(&re, "say hello world", 0, null);
	regfree(&re);
}

fn void match_and_compile_medium_c3()
{
	Regex re = regex::compile(mem, PATTERN_MEDIUM, EXTENDED)!!;
	(void)re.texec(input_email, 3);
	re.free();
}

fn void match_and_compile_medium_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_MEDIUM, regex::CompileFlags.EXTENDED);
	RegMatch* pm = mem::talloc_array(RegMatch, 3);
	regexec(&re, input_email, 3, pm);
	Match[] res = mem::talloc_array(Match, 3);
	for (sz i = 0; i < 3; i++)
	{
		res[i] = { .str = input_email, .match.rm_so = pm[i].rm_so, .match.rm_eo = pm[i].rm_eo };
	}
	regfree(&re);
}

fn void match_and_compile_complex_c3()
{
	Regex re = regex::compile(mem, PATTERN_COMPLEX, EXTENDED)!!;
	(void)re.texec(input_large, 0);
	re.free();
}

fn void match_and_compile_complex_glibc()
{
	RegexT re;
	regcomp(&re, PATTERN_COMPLEX, regex::CompileFlags.EXTENDED);
	regexec(&re, input_large, 0, null);
	regfree(&re);
}

// Match Benchmarks

fn void match_simple_c3() => (void)re_simple.texec("say hello world", 0);
fn void match_simple_glibc() => regexec(&glibc_re_simple, "say hello world", 0, null);
fn void match_medium_c3() => (void)re_medium.texec(input_email, 3);

fn void match_medium_glibc()
{
	RegMatch* pm = mem::talloc_array(RegMatch, 3);
	regexec(&glibc_re_medium, input_email, 3, pm);
	Match[] res = mem::talloc_array(Match, 3);
	for (sz i = 0; i < 3; i++)
	{
		res[i] = { .str = input_email, .match.rm_so = pm[i].rm_so, .match.rm_eo = pm[i].rm_eo };
	}
}

fn void match_captures_c3() => (void)re_captures.texec(input_captures, 6);

fn void match_captures_glibc()
{
	RegMatch* pm = mem::talloc_array(RegMatch, 6);
	regexec(&glibc_re_captures, input_captures, 6, pm);
	Match[] res = mem::talloc_array(Match, 6);
	for (sz i = 0; i < 6; i++)
	{
		res[i] = { .str = input_captures, .match.rm_so = pm[i].rm_so, .match.rm_eo = pm[i].rm_eo };
	}
}

fn void match_complex_small_c3() => (void)re_complex.texec(input_small, 0);
fn void match_complex_small_glibc() => regexec(&glibc_re_complex, input_small, 0, null);
fn void match_complex_medium_c3() => (void)re_complex.texec(input_medium, 0);
fn void match_complex_medium_glibc() => regexec(&glibc_re_complex, input_medium, 0, null);
fn void match_complex_large_c3() => (void)re_complex.texec(input_large, 0);
fn void match_complex_large_glibc() => regexec(&glibc_re_complex, input_large, 0, null);
fn void match_unicode_c3() => (void)re_unicode.texec(input_unicode, 0);
fn void match_unicode_glibc() => regexec(&glibc_re_unicode, input_unicode, 0, null);

fn void stress_backtracking_c3()
{
	Regex re = regex::compile(mem, "(a|a+)+b", EXTENDED)!!;
	(void)re.texec("aaaaaaaaaaaaaaaaaaaaaaaa!", 0);
	re.free();
}

fn void stress_backtracking_glibc()
{
	RegexT re;
	regcomp(&re, "(a|a+)+b", regex::CompileFlags.EXTENDED);
	regexec(&re, "aaaaaaaaaaaaaaaaaaaaaaaa!", 0, null);
	regfree(&re);
}

fn void stress_overlapping_alternation_c3()
{
	Regex re = regex::compile(mem, "([a-zA-Z0-9]+-)*[a-zA-Z0-9]+\\.com", EXTENDED)!!;
	(void)re.texec("a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-org", 0);
	re.free();
}

fn void stress_overlapping_alternation_glibc()
{
	RegexT re;
	regcomp(&re, "([a-zA-Z0-9]+-)*[a-zA-Z0-9]+\\.com", regex::CompileFlags.EXTENDED);
	regexec(&re, "a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-org", 0, null);
	regfree(&re);
}

fn void stress_massive_alternation_c3()
{
	Regex re = regex::compile(mem, "^(apple|banana|orange|grape|strawberry|melon|peach|cherry|blueberry|raspberry|blackberry|plum|apricot|fig|date|kiwi|mango|papaya|pineapple|pomegranate)*$", EXTENDED)!!;
	(void)re.texec("applebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaz", 0);
	re.free();
}

fn void stress_massive_alternation_glibc()
{
	RegexT re;
	regcomp(&re, "^(apple|banana|orange|grape|strawberry|melon|peach|cherry|blueberry|raspberry|blackberry|plum|apricot|fig|date|kiwi|mango|papaya|pineapple|pomegranate)*$", regex::CompileFlags.EXTENDED);
	regexec(&re, "applebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaapplebananaorangegrapekiwimangopapayaz", 0, null);
	regfree(&re);
}

fn void stress_multi_wildcard_c3()
{
	Regex re = regex::compile(mem, "a*a*a*a*a*b", EXTENDED)!!;
	(void)re.texec(input_large, 0);
	re.free();
}

fn void stress_multi_wildcard_glibc()
{
	RegexT re;
	regcomp(&re, "a*a*a*a*a*b", regex::CompileFlags.EXTENDED);
	regexec(&re, input_large, 0, null);
	regfree(&re);
}