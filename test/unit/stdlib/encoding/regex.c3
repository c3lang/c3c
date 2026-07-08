module regex_test;
import std, std::encoding::regex;

// API

fn void test_idiomatic_api() @test
{
	// --- compile: invalid pattern throws a fault ---------------------
	if (catch err = regex::compile("(unclosed", regex::CompileFlags.EXTENDED))
	{
		test::eq(err, regex::EPAREN);
		test::eq(regex::error_string(err), "Missing ')'");
	}
	else
	{
		unreachable("compile should have failed with fault regex::EPAREN");
	}

	// compile: simple literal 
	Regex re = regex::compile("hello")!!;
	defer re.free();

	// exec: successful match 
	String input1 = "say hello world";
	Match[] m = re.exec(input1)!!;
	// Since Match implements Printable, we can print it directly:
	io::print(m[0]);  // prints: hello

	test::eq(string::tformat("%s", m[0]), "hello");

	// exec: no match returns NOMATCH fault
	if (catch err = re.exec("goodbye"))
	{
		test::eq(err, regex::NOMATCH);
	}
	else
	{
		unreachable("exec should have returned NOMATCH");
	}

	// compile + exec: capture groups
	Regex re2 = regex::compile("(\\w+)@(\\w+\\.\\w+)", regex::CompileFlags.EXTENDED)!!;
	defer re2.free();

	// nmatch=3: slot 0=whole match, 1=user, 2=domain
	String input2 = "contact: user@example.com today";
	Match[] em = re2.exec(input2, 3)!!;
	// io::print(em[0]);  // prints: user@example.com
	// io::print(em[1]);  // prints: user
	// io::print(em[2]);  // prints: example.com
	test::eq(em[0], "user@example.com");
	test::eq(em[1], "user");
	test::eq(em[2], "example.com");
}

// ============================================================
//  Some more tests

// Verify compile() doesn't read past pattern.len when the string is not null-terminated.
// C3 slice operations produce exactly such strings (e.g. "ab"[0:1] has 'b' at ptr[1]).
fn void test_non_null_terminated_pattern() @test
{
	// Slice of a literal: 'b' follows 'a' in memory, so without a guard pattern is "ab".
	{
		String pattern = "ab"[0:1];
		Regex re = regex::compile(pattern)!!;
		defer re.free();
		assert(@ok(re.exec("a")));
		test::eq(re.exec("ab")!![0].str(), "a");
	}

	// Trailing byte '[' would make the pattern syntactically invalid (EBRACK).
	{
		String pattern = "a["[0:1];
		Regex? r = regex::compile(pattern, EXTENDED);
		assert(@ok(r));
		if (@ok(r)) r!!.free();
	}

	// Stack buffer: '+' immediately after the pattern would turn "a" into "a+".
	{
		char[5] buf = { 'a', '+', 'b', 0, 0 };
		Regex re = regex::compile((String)buf[0:1], EXTENDED)!!;
		defer re.free();
		test::eq(re.exec("aaa")!![0].str(), "a");
	}
}

fn void test_bracket_negation_max_codepoint() @test
{
	Regex re = regex::compile("[^a]", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	// In UTF-8, the max codepoint 0x10ffff is exactly the 4 bytes f4, 8f, bf, bf.
	String input = "\xf4\x8f\xbf\xbf";

	// This character is not 'a', so a negated bracket [^a] must match it.
	test::@check(!@catch(re.exec(input)));
}

fn void test_negated_class_sentinel_corruption() @test
{
	Regex re1 = regex::compile("[^[:digit:]]", regex::CompileFlags.EXTENDED)!!;
	defer re1.free();

	Regex re2 = regex::compile("a*b*c*d*e*f*g*h*i*j*k*l*", regex::CompileFlags.EXTENDED)!!;
	defer re2.free();

	test::@check(!@catch(re1.exec("a")));
}

//  GAP: Escape sequences \b, \B, \w, \W, \s, \S, \d, \D
fn void test_escape_word_boundary() @test
{
	Regex re = regex::compile("\\bfoo\\b", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("foo")));
	test::eq(@catch(re.exec("foobar")), regex::NOMATCH);
	test::@check(!@catch(re.exec(" foo ")));
}

fn void test_escape_w() @test
{
	Regex re = regex::compile("\\w+", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("hello_world")));
	test::eq(@catch(re.exec("---")), regex::NOMATCH);
}

fn void test_escape_W() @test
{
	Regex re = regex::compile("\\W", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec(" ")));
	test::@check(!@catch(re.exec("!")));
	test::eq(@catch(re.exec("a")), regex::NOMATCH);
}

fn void test_escape_s() @test
{
	Regex re = regex::compile("\\s+", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("   ")));
	test::@check(!@catch(re.exec("\t\n")));
	test::eq(@catch(re.exec("abc")), regex::NOMATCH);
}

fn void test_escape_S() @test
{
	Regex re = regex::compile("\\S+", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("abc")));
	test::eq(@catch(re.exec("   ")), regex::NOMATCH);
}

fn void test_escape_d() @test
{
	Regex re = regex::compile("\\d+", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("123")));
	test::eq(@catch(re.exec("abc")), regex::NOMATCH);
}

fn void test_escape_D() @test
{
	Regex re = regex::compile("\\D+", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	test::@check(!@catch(re.exec("abc")));
	test::eq(@catch(re.exec("123")), regex::NOMATCH);
}

//  GAP: BRE backreferences \1
fn void test_backref() @test
{
	Regex re = regex::compile("^\\(a*\\)\\1$")!!;
	defer re.free();

	test::@check(!@catch(re.exec("aa")));
	test::eq(@catch(re.exec("aaa")), regex::NOMATCH); // "aaa" is odd-length, so it doesn't match ^\(a*\)\1$
}

//  GAP: tre_fill_pmatch, parent-bounds reset
fn void test_submatch_parent_bounds() @test
{
	// When outer group does not participate, inner group must return -1/-1
	// Pattern: (a|(b))c  matched against "ac"
	// group 1 = "a", group 2 = -1 (not used)
	Regex re = regex::compile("(a|(b))c", regex::CompileFlags.EXTENDED)!!;
	defer re.free();

	Match[] m = re.exec("ac", 3)!!;
	test::eq(m[0].match.rm_so, 0);
	test::eq(m[0].match.rm_eo, 2); // whole match
	test::eq(m[1].match.rm_so, 0);
	test::eq(m[1].match.rm_eo, 1); // group 1 = "a"
	test::eq(m[2].match.rm_so, -1);
	test::eq(m[2].match.rm_eo, -1); // group 2 unused
}

//  GAP: Alternation in BRE with \|
fn void test_bre_alternation() @test
{
	Regex re = regex::compile("cat\\|dog")!!;
	defer re.free();

	test::@check(!@catch(re.exec("cat")));
	test::@check(!@catch(re.exec("dog")));
	test::eq(@catch(re.exec("bird")), regex::NOMATCH);
}

//  GAP: eflags REG_NOTBOL / REG_NOTEOL
fn void test_eflags_notbol() @test
{
	Regex re = regex::compile("^abc")!!;
	defer re.free();

	// Without flag: matches at start
	test::@check(!@catch(re.exec("abc")));
	// With REG_NOTBOL: ^ should not match at position 0
	test::eq(@catch(re.exec("abc", 1, regex::ExecFlags.NOTBOL)), regex::NOMATCH);
}

fn void test_eflags_noteol() @test
{
	Regex re = regex::compile("abc$")!!;
	defer re.free();

	// Without flag: matches at end
	test::@check(!@catch(re.exec("abc")));
	// With REG_NOTEOL: $ should not match at end
	test::eq(@catch(re.exec("abc", 1, regex::ExecFlags.NOTEOL)), regex::NOMATCH);
}

//  GAP: \x hex escape sequences
fn void test_hex_escape() @test
{
	{
		// \x41 = 'A' - exercises the digit branch (4, 1) of hexval
		Regex re = regex::compile("\\x41", regex::CompileFlags.EXTENDED)!!;
		defer re.free();
		test::@check(!@catch(re.exec("A")));
		test::eq(@catch(re.exec("B")), regex::NOMATCH);
	}

	{
		// \x6e = 'n' (0x6e=110) - '6' digit branch + 'e' letter branch of hexval
		Regex re = regex::compile("\\x6e", regex::CompileFlags.EXTENDED)!!;
		defer re.free();
		test::@check(!@catch(re.exec("n")));
		test::eq(@catch(re.exec("m")), regex::NOMATCH);
	}

	{
		// \x4b = 'K' (0x4b=75) - '4' digit branch + 'b' letter branch of hexval
		// Also tests uppercase hex input: \x4B (hexval does c|=32 to fold case)
		Regex re = regex::compile("\\x4B", regex::CompileFlags.EXTENDED)!!;
		defer re.free();
		test::@check(!@catch(re.exec("K")));
		test::eq(@catch(re.exec("k")), regex::NOMATCH);
	}
}

/* ==========================================================
    taken from: (https://go.dev/src/regexp/testdata/README)
   https://go.dev/src/regexp/testdata/basic.dat?m=text
   https://go.dev/src/regexp/testdata/repetition.dat?m=text
   https://go.dev/src/regexp/testdata/nullsubexpr.dat?m=text
  ===========================================================  */

faultdef NO_ERR; // sentinel: compile_error_code returns this when no error is expected

fn void test_posix_compliance_go_testdata_333_cases() @test
{
	foreach (tc : RegexTestCase::values)
	{
		if (tc.expected_fault != NO_ERR && tc.expected_fault != regex::NOMATCH)
		{
			test::@error(regex::compile(tc.pattern.tcopy(), tc.cflags), tc.expected_fault);
			continue;
		}

		Regex re = regex::compile(tc.pattern.tcopy(), tc.cflags)!!;
		defer re.free();

		if (tc.expected_fault == regex::NOMATCH)
		{
			test::@error(re.exec(decode_input_escapes(tc.input).tcopy(), 32), regex::NOMATCH);
			continue;
		}

		Match[] pm = re.exec(decode_input_escapes(tc.input).tcopy(), 32)!!;
		for (sz s = 0; s < tc.expected_spans.len; s++)
		{
			int got_so = (s < pm.len) ? pm[s].match.rm_so : -1;
			int got_eo = (s < pm.len) ? pm[s].match.rm_eo : -1;
			test::eq(tc.expected_spans[s].start, got_so);
			test::eq(tc.expected_spans[s].end, got_eo);
		}
	}
}

<*
 Decodes literal escape sequences in the input string to actual control bytes.
 Patterns are passed through directly as the regex compiler parses escapes natively.
*>
fn String decode_input_escapes(String input) {
    if (!input.contains_char('\\')) return input;
    return input.treplace(`\n`, "\n")
                .treplace(`\t`, "\t")
                .treplace(`\r`, "\r")
                .treplace(`\a`, "\a")
                .treplace(`\b`, "\b")
                .treplace(`\f`, "\f")
                .treplace(`\v`, "\v")
                .treplace(`\\`, `\`);
}

// Data

struct ExpectedSpan {
    int start;
    int end;
}

enum RegexTestCase : int (
    CompileFlags cflags,
    String pattern,
    String input,
    fault expected_fault,
    ExpectedSpan[] expected_spans
)
{
	CASE_0 { {}, "abracadabra$", "abracadabracadabra", NO_ERR, {{ 7, 18 }} },
	CASE_1 { {}, "a...b", "abababbb", NO_ERR, {{ 2, 7 }} },
	CASE_2 { {}, "XXXXXX", "..XXXXXX", NO_ERR, {{ 2, 8 }} },
	CASE_3 { EXTENDED, "\\)", "()", NO_ERR, {{ 1, 2 }} },
	CASE_4 { {}, "a]", "a]a", NO_ERR, {{ 0, 2 }} },
	CASE_5 { {}, "}", "}", NO_ERR, {{ 0, 1 }} },
	CASE_6 { EXTENDED, "\\}", "}", NO_ERR, {{ 0, 1 }} },
	CASE_7 { {}, "\\]", "]", NO_ERR, {{ 0, 1 }} },
	CASE_8 { {}, "]", "]", NO_ERR, {{ 0, 1 }} },
	CASE_9 { EXTENDED, "]", "]", NO_ERR, {{ 0, 1 }} },
	CASE_10 { {}, "{", "{", NO_ERR, {{ 0, 1 }} },
	CASE_11 { {}, "}", "}", NO_ERR, {{ 0, 1 }} },
	CASE_12 { {}, "^a", "ax", NO_ERR, {{ 0, 1 }} },
	CASE_13 { {}, "\\^a", "a^a", NO_ERR, {{ 1, 3 }} },
	CASE_14 { {}, "a\\^", "a^", NO_ERR, {{ 0, 2 }} },
	CASE_15 { {}, "a$", "aa", NO_ERR, {{ 1, 2 }} },
	CASE_16 { {}, "a\\$", "a$", NO_ERR, {{ 0, 2 }} },
	CASE_17 { {}, "^$", "", NO_ERR, {{ 0, 0 }} },
	CASE_18 { EXTENDED, "$^", "", NO_ERR, {{ 0, 0 }} },
	CASE_19 { EXTENDED, "a($)", "aa", NO_ERR, {{ 1, 2 }, { 2, 2 }} },
	CASE_20 { EXTENDED, "a*(^a)", "aa", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_21 { EXTENDED, "(..)*(...)*", "a", NO_ERR, {{ 0, 0 }} },
	CASE_22 { EXTENDED, "(..)*(...)*", "abcd", NO_ERR, {{ 0, 4 }, { 2, 4 }} },
	CASE_23 { EXTENDED, "(ab|a)(bc|c)", "abc", NO_ERR, {{ 0, 3 }, { 0, 2 }, { 2, 3 }} },
	CASE_24 { EXTENDED, "(ab)c|abc", "abc", NO_ERR, {{ 0, 3 }, { 0, 2 }} },
	CASE_25 { EXTENDED, "a{0}b", "ab", NO_ERR, {{ 1, 2 }} },
	CASE_26 { EXTENDED, "(a*)(b?)(b+)b{3}", "aaabbbbbbb", NO_ERR, {{ 0, 10 }, { 0, 3 }, { 3, 4 }, { 4, 7 }} },
	CASE_27 { EXTENDED, "(a*)(b{0,1})(b{1,})b{3}", "aaabbbbbbb", NO_ERR, {{ 0, 10 }, { 0, 3 }, { 3, 4 }, { 4, 7 }} },
	CASE_28 { EXTENDED, "a{9876543210}", "", regex::BADBR, {} },
	CASE_29 { EXTENDED, "((a|a)|a)", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }, { 0, 1 }} },
	CASE_30 { EXTENDED, "(a*)(a|aa)", "aaaa", NO_ERR, {{ 0, 4 }, { 0, 3 }, { 3, 4 }} },
	CASE_31 { EXTENDED, "a*(a.|aa)", "aaaa", NO_ERR, {{ 0, 4 }, { 2, 4 }} },
	CASE_32 { EXTENDED, "a(b)|c(d)|a(e)f", "aef", NO_ERR, {{ 0, 3 }, { -1, -1 }, { -1, -1 }, { 1, 2 }} },
	CASE_33 { EXTENDED, "(a|b)?.*", "b", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_34 { EXTENDED, "(a|b)c|a(b|c)", "ac", NO_ERR, {{ 0, 2 }, { 0, 1 }} },
	CASE_35 { EXTENDED, "(a|b)c|a(b|c)", "ab", NO_ERR, {{ 0, 2 }, { -1, -1 }, { 1, 2 }} },
	CASE_36 { EXTENDED, "(a|b)*c|(a|ab)*c", "abc", NO_ERR, {{ 0, 3 }, { 1, 2 }} },
	CASE_37 { EXTENDED, "(a|b)*c|(a|ab)*c", "xc", NO_ERR, {{ 1, 2 }} },
	CASE_38 { EXTENDED, "(.a|.b).*|.*(.a|.b)", "xa", NO_ERR, {{ 0, 2 }, { 0, 2 }} },
	CASE_39 { EXTENDED, "a?(ab|ba)ab", "abab", NO_ERR, {{ 0, 4 }, { 0, 2 }} },
	CASE_40 { EXTENDED, "a?(ac{0}b|ba)ab", "abab", NO_ERR, {{ 0, 4 }, { 0, 2 }} },
	CASE_41 { EXTENDED, "ab|abab", "abbabab", NO_ERR, {{ 0, 2 }} },
	CASE_42 { EXTENDED, "aba|bab|bba", "baaabbbaba", NO_ERR, {{ 5, 8 }} },
	CASE_43 { EXTENDED, "aba|bab", "baaabbbaba", NO_ERR, {{ 6, 9 }} },
	CASE_44 { EXTENDED, "(aa|aaa)*|(a|aaaaa)", "aa", NO_ERR, {{ 0, 2 }, { 0, 2 }} },
	CASE_45 { EXTENDED, "(a.|.a.)*|(a|.a...)", "aa", NO_ERR, {{ 0, 2 }, { 0, 2 }} },
	CASE_46 { EXTENDED, "ab|a", "xabc", NO_ERR, {{ 1, 3 }} },
	CASE_47 { EXTENDED, "ab|a", "xxabc", NO_ERR, {{ 2, 4 }} },
	CASE_48 { EXTENDED | ICASE, "(Ab|cD)*", "aBcD", NO_ERR, {{ 0, 4 }, { 2, 4 }} },
	CASE_49 { {}, "[^-]", "--a", NO_ERR, {{ 2, 3 }} },
	CASE_50 { {}, "[a-]*", "--a", NO_ERR, {{ 0, 3 }} },
	CASE_51 { {}, "[a-m-]*", "--amoma--", NO_ERR, {{ 0, 4 }} },
	CASE_52 { EXTENDED, ":::1:::0:|:::1:1:0:", ":::0:::1:::1:::0:", NO_ERR, {{ 8, 17 }} },
	CASE_53 { EXTENDED, ":::1:::0:|:::1:1:1:", ":::0:::1:::1:::0:", NO_ERR, {{ 8, 17 }} },
	CASE_54 { EXTENDED, "[[:lower:]]+", "`az{", NO_ERR, {{ 1, 3 }} },
	CASE_55 { EXTENDED, "[[:upper:]]+", "@AZ[", NO_ERR, {{ 1, 3 }} },
	CASE_56 { {}, "\\n", "\\n", NO_ERR, {{ 0, 1 }} },
	CASE_57 { NEWLINE, "\\n", "\\n", NO_ERR, {{ 0, 1 }} },
	CASE_58 { {}, "[^a]", "\\n", NO_ERR, {{ 0, 1 }} },
	CASE_59 { {}, "\\na", "\\na", NO_ERR, {{ 0, 2 }} },
	CASE_60 { EXTENDED, "(a)(b)(c)", "abc", NO_ERR, {{ 0, 3 }, { 0, 1 }, { 1, 2 }, { 2, 3 }} },
	CASE_61 { {}, "xxx", "xxx", NO_ERR, {{ 0, 3 }} },
	CASE_62 { EXTENDED, "(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\\* */?)0*[6-7]))([^0-9]|$)", "feb 6,", NO_ERR, {{ 0, 6 }} },
	CASE_63 { EXTENDED, "(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\\* */?)0*[6-7]))([^0-9]|$)", "2/7", NO_ERR, {{ 0, 3 }} },
	CASE_64 { EXTENDED, "(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\\* */?)0*[6-7]))([^0-9]|$)", "feb 1,Feb 6", NO_ERR, {{ 5, 11 }} },
	CASE_65 { EXTENDED, "((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))", "x", NO_ERR, {{ 0, 1 }, { 0, 1 }, { 0, 1 }} },
	CASE_66 { EXTENDED, "((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))*", "xx", NO_ERR, {{ 0, 2 }, { 1, 2 }, { 1, 2 }} },
	CASE_67 { EXTENDED, "a?(ab|ba)*", "ababababababababababababababababababababababababababababababababababababababababa", NO_ERR, {{ 0, 81 }, { 79, 81 }} },
	CASE_68 { EXTENDED, "abaa|abbaa|abbbaa|abbbbaa", "ababbabbbabbbabbbbabbbbaa", NO_ERR, {{ 18, 25 }} },
	CASE_69 { EXTENDED, "abaa|abbaa|abbbaa|abbbbaa", "ababbabbbabbbabbbbabaa", NO_ERR, {{ 18, 22 }} },
	CASE_70 { EXTENDED, "aaac|aabc|abac|abbc|baac|babc|bbac|bbbc", "baaabbbabac", NO_ERR, {{ 7, 11 }} },
	CASE_71 { EXTENDED, "aaaa|bbbb|cccc|ddddd|eeeeee|fffffff|gggg|hhhh|iiiii|jjjjj|kkkkk|llll", "XaaaXbbbXcccXdddXeeeXfffXgggXhhhXiiiXjjjXkkkXlllXcbaXaaaa", NO_ERR, {{ 53, 57 }} },
	CASE_72 { EXTENDED, "a*a*a*a*a*b", "aaaaaaaaab", NO_ERR, {{ 0, 10 }} },
	CASE_73 { {}, "^", "", NO_ERR, {{ 0, 0 }} },
	CASE_74 { {}, "$", "", NO_ERR, {{ 0, 0 }} },
	CASE_75 { {}, "^$", "", NO_ERR, {{ 0, 0 }} },
	CASE_76 { {}, "^a$", "a", NO_ERR, {{ 0, 1 }} },
	CASE_77 { {}, "abc", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_78 { {}, "abc", "xabcy", NO_ERR, {{ 1, 4 }} },
	CASE_79 { {}, "abc", "ababc", NO_ERR, {{ 2, 5 }} },
	CASE_80 { {}, "ab*c", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_81 { {}, "ab*bc", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_82 { {}, "ab*bc", "abbc", NO_ERR, {{ 0, 4 }} },
	CASE_83 { {}, "ab*bc", "abbbbc", NO_ERR, {{ 0, 6 }} },
	CASE_84 { EXTENDED, "ab+bc", "abbc", NO_ERR, {{ 0, 4 }} },
	CASE_85 { EXTENDED, "ab+bc", "abbbbc", NO_ERR, {{ 0, 6 }} },
	CASE_86 { EXTENDED, "ab?bc", "abbc", NO_ERR, {{ 0, 4 }} },
	CASE_87 { EXTENDED, "ab?bc", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_88 { EXTENDED, "ab?c", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_89 { {}, "^abc$", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_90 { {}, "^abc", "abcc", NO_ERR, {{ 0, 3 }} },
	CASE_91 { {}, "abc$", "aabc", NO_ERR, {{ 1, 4 }} },
	CASE_92 { {}, "^", "abc", NO_ERR, {{ 0, 0 }} },
	CASE_93 { {}, "$", "abc", NO_ERR, {{ 3, 3 }} },
	CASE_94 { {}, "a.c", "abc", NO_ERR, {{ 0, 3 }} },
	CASE_95 { {}, "a.c", "axc", NO_ERR, {{ 0, 3 }} },
	CASE_96 { {}, "a.*c", "axyzc", NO_ERR, {{ 0, 5 }} },
	CASE_97 { {}, "a[bc]d", "abd", NO_ERR, {{ 0, 3 }} },
	CASE_98 { {}, "a[b-d]e", "ace", NO_ERR, {{ 0, 3 }} },
	CASE_99 { {}, "a[b-d]", "aac", NO_ERR, {{ 1, 3 }} },
	CASE_100 { {}, "a[-b]", "a-", NO_ERR, {{ 0, 2 }} },
	CASE_101 { {}, "a[b-]", "a-", NO_ERR, {{ 0, 2 }} },
	CASE_102 { {}, "a]", "a]", NO_ERR, {{ 0, 2 }} },
	CASE_103 { {}, "a[]]b", "a]b", NO_ERR, {{ 0, 3 }} },
	CASE_104 { {}, "a[^bc]d", "aed", NO_ERR, {{ 0, 3 }} },
	CASE_105 { {}, "a[^-b]c", "adc", NO_ERR, {{ 0, 3 }} },
	CASE_106 { {}, "a[^]b]c", "adc", NO_ERR, {{ 0, 3 }} },
	CASE_107 { EXTENDED, "ab|cd", "abc", NO_ERR, {{ 0, 2 }} },
	CASE_108 { EXTENDED, "ab|cd", "abcd", NO_ERR, {{ 0, 2 }} },
	CASE_109 { EXTENDED, "a\\(b", "a(b", NO_ERR, {{ 0, 3 }} },
	CASE_110 { EXTENDED, "a\\(*b", "ab", NO_ERR, {{ 0, 2 }} },
	CASE_111 { EXTENDED, "a\\(*b", "a((b", NO_ERR, {{ 0, 4 }} },
	CASE_112 { EXTENDED, "((a))", "abc", NO_ERR, {{ 0, 1 }, { 0, 1 }, { 0, 1 }} },
	CASE_113 { EXTENDED, "(a)b(c)", "abc", NO_ERR, {{ 0, 3 }, { 0, 1 }, { 2, 3 }} },
	CASE_114 { EXTENDED, "a+b+c", "aabbabc", NO_ERR, {{ 4, 7 }} },
	CASE_115 { EXTENDED, "a*", "aaa", NO_ERR, {{ 0, 3 }} },
	CASE_116 { EXTENDED, "(a*)*", "-", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_117 { EXTENDED, "(a*)+", "-", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_118 { EXTENDED, "(a*|b)*", "-", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_119 { EXTENDED, "(a+|b)*", "ab", NO_ERR, {{ 0, 2 }, { 1, 2 }} },
	CASE_120 { EXTENDED, "(a+|b)+", "ab", NO_ERR, {{ 0, 2 }, { 1, 2 }} },
	CASE_121 { EXTENDED, "(a+|b)?", "ab", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_122 { {}, "[^ab]*", "cde", NO_ERR, {{ 0, 3 }} },
	CASE_123 { EXTENDED, "(^)*", "-", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_124 { {}, "a*", "", NO_ERR, {{ 0, 0 }} },
	CASE_125 { EXTENDED, "([abc])*d", "abbbcd", NO_ERR, {{ 0, 6 }, { 4, 5 }} },
	CASE_126 { EXTENDED, "([abc])*bcd", "abcd", NO_ERR, {{ 0, 4 }, { 0, 1 }} },
	CASE_127 { EXTENDED, "a|b|c|d|e", "e", NO_ERR, {{ 0, 1 }} },
	CASE_128 { EXTENDED, "(a|b|c|d|e)f", "ef", NO_ERR, {{ 0, 2 }, { 0, 1 }} },
	CASE_129 { EXTENDED, "((a*|b))*", "-", NO_ERR, {{ 0, 0 }, { 0, 0 }, { 0, 0 }} },
	CASE_130 { {}, "abcd*efg", "abcdefg", NO_ERR, {{ 0, 7 }} },
	CASE_131 { {}, "ab*", "xabyabbbz", NO_ERR, {{ 1, 3 }} },
	CASE_132 { {}, "ab*", "xayabbbz", NO_ERR, {{ 1, 2 }} },
	CASE_133 { EXTENDED, "(ab|cd)e", "abcde", NO_ERR, {{ 2, 5 }, { 2, 4 }} },
	CASE_134 { {}, "[abhgefdc]ij", "hij", NO_ERR, {{ 0, 3 }} },
	CASE_135 { EXTENDED, "(a|b)c*d", "abcd", NO_ERR, {{ 1, 4 }, { 1, 2 }} },
	CASE_136 { EXTENDED, "(ab|ab*)bc", "abc", NO_ERR, {{ 0, 3 }, { 0, 1 }} },
	CASE_137 { EXTENDED, "a([bc]*)c*", "abc", NO_ERR, {{ 0, 3 }, { 1, 3 }} },
	CASE_138 { EXTENDED, "a([bc]*)(c*d)", "abcd", NO_ERR, {{ 0, 4 }, { 1, 3 }, { 3, 4 }} },
	CASE_139 { EXTENDED, "a([bc]+)(c*d)", "abcd", NO_ERR, {{ 0, 4 }, { 1, 3 }, { 3, 4 }} },
	CASE_140 { EXTENDED, "a([bc]*)(c+d)", "abcd", NO_ERR, {{ 0, 4 }, { 1, 2 }, { 2, 4 }} },
	CASE_141 { EXTENDED, "a[bcd]*dcdcde", "adcdcde", NO_ERR, {{ 0, 7 }} },
	CASE_142 { EXTENDED, "(ab|a)b*c", "abc", NO_ERR, {{ 0, 3 }, { 0, 2 }} },
	CASE_143 { EXTENDED, "((a)(b)c)(d)", "abcd", NO_ERR, {{ 0, 4 }, { 0, 3 }, { 0, 1 }, { 1, 2 }, { 3, 4 }} },
	CASE_144 { {}, "[A-Za-z_][A-Za-z0-9_]*", "alpha", NO_ERR, {{ 0, 5 }} },
	CASE_145 { EXTENDED, "^a(bc+|b[eh])g|.h$", "abh", NO_ERR, {{ 1, 3 }} },
	CASE_146 { EXTENDED, "(bc+d$|ef*g.|h?i(j|k))", "effgz", NO_ERR, {{ 0, 5 }, { 0, 5 }} },
	CASE_147 { EXTENDED, "(bc+d$|ef*g.|h?i(j|k))", "ij", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 1, 2 }} },
	CASE_148 { EXTENDED, "(bc+d$|ef*g.|h?i(j|k))", "reffgz", NO_ERR, {{ 1, 6 }, { 1, 6 }} },
	CASE_149 { EXTENDED, "(((((((((a)))))))))", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }, { 0, 1 }} },
	CASE_150 { {}, "multiple words", "multiple words yeah", NO_ERR, {{ 0, 14 }} },
	CASE_151 { EXTENDED, "(.*)c(.*)", "abcde", NO_ERR, {{ 0, 5 }, { 0, 2 }, { 3, 5 }} },
	CASE_152 { {}, "abcd", "abcd", NO_ERR, {{ 0, 4 }} },
	CASE_153 { EXTENDED, "a(bc)d", "abcd", NO_ERR, {{ 0, 4 }, { 1, 3 }} },
	CASE_154 { EXTENDED, "a[-]?c", "ac", NO_ERR, {{ 0, 3 }} },
	CASE_155 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Qaddafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 10, 12 }} },
	CASE_156 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Mo'ammar Gadhafi", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_157 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Kaddafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 10, 12 }} },
	CASE_158 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Qadhafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 10, 12 }} },
	CASE_159 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Gadafi", NO_ERR, {{ 0, 14 }, { -1, -1 }, { 10, 11 }} },
	CASE_160 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Mu'ammar Qadafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 11, 12 }} },
	CASE_161 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Moamar Gaddafi", NO_ERR, {{ 0, 14 }, { -1, -1 }, { 9, 11 }} },
	CASE_162 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Mu'ammar Qadhdhafi", NO_ERR, {{ 0, 18 }, { -1, -1 }, { 13, 15 }} },
	CASE_163 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Khaddafi", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_164 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Ghaddafy", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_165 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Ghadafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 11, 12 }} },
	CASE_166 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Ghaddafi", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_167 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muamar Kaddafi", NO_ERR, {{ 0, 14 }, { -1, -1 }, { 9, 11 }} },
	CASE_168 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Quathafi", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_169 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Muammar Gheddafi", NO_ERR, {{ 0, 16 }, { -1, -1 }, { 11, 13 }} },
	CASE_170 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Moammar Khadafy", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 11, 12 }} },
	CASE_171 { EXTENDED, "M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]", "Moammar Qudhafi", NO_ERR, {{ 0, 15 }, { -1, -1 }, { 10, 12 }} },
	CASE_172 { EXTENDED, "a+(b|c)*d+", "aabcdd", NO_ERR, {{ 0, 6 }, { 3, 4 }} },
	CASE_173 { EXTENDED, "^.+$", "vivi", NO_ERR, {{ 0, 4 }} },
	CASE_174 { EXTENDED, "^(.+)$", "vivi", NO_ERR, {{ 0, 4 }, { 0, 4 }} },
	CASE_175 { EXTENDED, "^([^!.]+).att.com!(.+)$", "gryphon.att.com!eby", NO_ERR, {{ 0, 19 }, { 0, 7 }, { 16, 19 }} },
	CASE_176 { EXTENDED, "^([^!]+!)?([^!]+)$", "bas", NO_ERR, {{ 0, 3 }, { -1, -1 }, { 0, 3 }} },
	CASE_177 { EXTENDED, "^([^!]+!)?([^!]+)$", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_178 { EXTENDED, "^([^!]+!)?([^!]+)$", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_179 { EXTENDED, "^.+!([^!]+!)([^!]+)$", "foo!bar!bas", NO_ERR, {{ 0, 11 }, { 4, 8 }, { 8, 11 }} },
	CASE_180 { EXTENDED, "((foo)|(bar))!bas", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }, { -1, -1 }, { 0, 3 }} },
	CASE_181 { EXTENDED, "((foo)|(bar))!bas", "foo!bar!bas", NO_ERR, {{ 4, 11 }, { 4, 7 }, { -1, -1 }, { 4, 7 }} },
	CASE_182 { EXTENDED, "((foo)|(bar))!bas", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }, { 0, 3 }} },
	CASE_183 { EXTENDED, "((foo)|bar)!bas", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }} },
	CASE_184 { EXTENDED, "((foo)|bar)!bas", "foo!bar!bas", NO_ERR, {{ 4, 11 }, { 4, 7 }} },
	CASE_185 { EXTENDED, "((foo)|bar)!bas", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }, { 0, 3 }} },
	CASE_186 { EXTENDED, "(foo|(bar))!bas", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }, { 0, 3 }} },
	CASE_187 { EXTENDED, "(foo|(bar))!bas", "foo!bar!bas", NO_ERR, {{ 4, 11 }, { 4, 7 }, { 4, 7 }} },
	CASE_188 { EXTENDED, "(foo|(bar))!bas", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }} },
	CASE_189 { EXTENDED, "(foo|bar)!bas", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }} },
	CASE_190 { EXTENDED, "(foo|bar)!bas", "foo!bar!bas", NO_ERR, {{ 4, 11 }, { 4, 7 }} },
	CASE_191 { EXTENDED, "(foo|bar)!bas", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 3 }} },
	CASE_192 { EXTENDED, "^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$", "foo!bar!bas", NO_ERR, {{ 0, 11 }, { 0, 11 }, { -1, -1 }, { -1, -1 }, { 4, 8 }, { 8, 11 }} },
	CASE_193 { EXTENDED, "^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$", "bas", NO_ERR, {{ 0, 3 }, { -1, -1 }, { 0, 3 }} },
	CASE_194 { EXTENDED, "^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_195 { EXTENDED, "^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$", "foo!bar!bas", NO_ERR, {{ 0, 11 }, { -1, -1 }, { -1, -1 }, { 4, 8 }, { 8, 11 }} },
	CASE_196 { EXTENDED, "^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_197 { EXTENDED, "^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$", "bas", NO_ERR, {{ 0, 3 }, { 0, 3 }, { -1, -1 }, { 0, 3 }} },
	CASE_198 { EXTENDED, "^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$", "bar!bas", NO_ERR, {{ 0, 7 }, { 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_199 { EXTENDED, "^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$", "foo!bar!bas", NO_ERR, {{ 0, 11 }, { 0, 11 }, { -1, -1 }, { -1, -1 }, { 4, 8 }, { 8, 11 }} },
	CASE_200 { EXTENDED, "^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$", "foo!bas", NO_ERR, {{ 0, 7 }, { 0, 7 }, { 0, 4 }, { 4, 7 }} },
	CASE_201 { EXTENDED, ".*(/XXX).*", "/XXX", NO_ERR, {{ 0, 4 }, { 0, 4 }} },
	CASE_202 { EXTENDED, ".*(\\\\XXX).*", "\\XXX", NO_ERR, {{ 0, 4 }, { 0, 4 }} },
	CASE_203 { EXTENDED, "\\\\XXX", "\\XXX", NO_ERR, {{ 0, 4 }} },
	CASE_204 { EXTENDED, ".*(/000).*", "/000", NO_ERR, {{ 0, 4 }, { 0, 4 }} },
	CASE_205 { EXTENDED, ".*(\\\\000).*", "\\000", NO_ERR, {{ 0, 4 }, { 0, 4 }} },
	CASE_206 { EXTENDED, "\\\\000", "\\000", NO_ERR, {{ 0, 4 }} },
	CASE_207 { EXTENDED, "((..)|(.))", "", regex::NOMATCH, {} },
	CASE_208 { EXTENDED, "((..)|(.))((..)|(.))", "", regex::NOMATCH, {} },
	CASE_209 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "", regex::NOMATCH, {} },
	CASE_210 { EXTENDED, "((..)|(.)){1}", "", regex::NOMATCH, {} },
	CASE_211 { EXTENDED, "((..)|(.)){2}", "", regex::NOMATCH, {} },
	CASE_212 { EXTENDED, "((..)|(.)){3}", "", regex::NOMATCH, {} },
	CASE_213 { EXTENDED, "((..)|(.))*", "", NO_ERR, {{ 0, 0 }} },
	CASE_214 { EXTENDED, "((..)|(.))", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }, { -1, -1 }, { 0, 1 }} },
	CASE_215 { EXTENDED, "((..)|(.))((..)|(.))", "a", regex::NOMATCH, {} },
	CASE_216 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "a", regex::NOMATCH, {} },
	CASE_217 { EXTENDED, "((..)|(.)){1}", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }, { -1, -1 }, { 0, 1 }} },
	CASE_218 { EXTENDED, "((..)|(.)){2}", "a", regex::NOMATCH, {} },
	CASE_219 { EXTENDED, "((..)|(.)){3}", "a", regex::NOMATCH, {} },
	CASE_220 { EXTENDED, "((..)|(.))*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }, { -1, -1 }, { 0, 1 }} },
	CASE_221 { EXTENDED, "((..)|(.))", "aa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_222 { EXTENDED, "((..)|(.))((..)|(.))", "aa", NO_ERR, {{ 0, 2 }, { 0, 1 }, { -1, -1 }, { 0, 1 }, { 1, 2 }, { -1, -1 }, { 1, 2 }} },
	CASE_223 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "aa", regex::NOMATCH, {} },
	CASE_224 { EXTENDED, "((..)|(.)){1}", "aa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_225 { EXTENDED, "((..)|(.)){3}", "aa", regex::NOMATCH, {} },
	CASE_226 { EXTENDED, "((..)|(.))*", "aa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_227 { EXTENDED, "((..)|(.))", "aaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_228 { EXTENDED, "((..)|(.))((..)|(.))", "aaa", NO_ERR, {{ 0, 3 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 3 }, { -1, -1 }, { 2, 3 }} },
	CASE_229 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "aaa", NO_ERR, {{ 0, 3 }, { 0, 1 }, { -1, -1 }, { 0, 1 }, { 1, 2 }, { -1, -1 }, { 1, 2 }, { 2, 3 }, { -1, -1 }, { 2, 3 }} },
	CASE_230 { EXTENDED, "((..)|(.)){1}", "aaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_231 { EXTENDED, "((..)|(.)){3}", "aaa", NO_ERR, {{ 0, 3 }, { 2, 3 }, { -1, -1 }, { 2, 3 }} },
	CASE_232 { EXTENDED, "((..)|(.))", "aaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_233 { EXTENDED, "((..)|(.))((..)|(.))", "aaaa", NO_ERR, {{ 0, 4 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_234 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "aaaa", NO_ERR, {{ 0, 4 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 3 }, { -1, -1 }, { 2, 3 }, { 3, 4 }, { -1, -1 }, { 3, 4 }} },
	CASE_235 { EXTENDED, "((..)|(.)){1}", "aaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_236 { EXTENDED, "((..)|(.)){2}", "aaaa", NO_ERR, {{ 0, 4 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_237 { EXTENDED, "((..)|(.))", "aaaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_238 { EXTENDED, "((..)|(.))((..)|(.))", "aaaaa", NO_ERR, {{ 0, 4 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_239 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "aaaaa", NO_ERR, {{ 0, 5 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 4 }, { 2, 4 }, { -1, -1 }, { 4, 5 }, { -1, -1 }, { 4, 5 }} },
	CASE_240 { EXTENDED, "((..)|(.)){1}", "aaaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_241 { EXTENDED, "((..)|(.)){2}", "aaaaa", NO_ERR, {{ 0, 4 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_242 { EXTENDED, "((..)|(.))", "aaaaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_243 { EXTENDED, "((..)|(.))((..)|(.))", "aaaaaa", NO_ERR, {{ 0, 4 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_244 { EXTENDED, "((..)|(.))((..)|(.))((..)|(.))", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 2 }, { 0, 2 }, { -1, -1 }, { 2, 4 }, { 2, 4 }, { -1, -1 }, { 4, 6 }, { 4, 6 }, { -1, -1 }} },
	CASE_245 { EXTENDED, "((..)|(.)){1}", "aaaaaa", NO_ERR, {{ 0, 2 }, { 0, 2 }, { 0, 2 }, { -1, -1 }} },
	CASE_246 { EXTENDED, "((..)|(.)){2}", "aaaaaa", NO_ERR, {{ 0, 4 }, { 2, 4 }, { 2, 4 }, { -1, -1 }} },
	CASE_247 { EXTENDED, "((..)|(.)){3}", "aaaaaa", NO_ERR, {{ 0, 6 }, { 4, 6 }, { 4, 6 }, { -1, -1 }} },
	CASE_248 { EXTENDED, "((..)|(.))*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 4, 6 }, { 4, 6 }, { -1, -1 }} },
	CASE_249 { EXTENDED, "X(.?){0,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_250 { EXTENDED, "X(.?){1,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_251 { EXTENDED, "X(.?){2,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_252 { EXTENDED, "X(.?){3,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_253 { EXTENDED, "X(.?){4,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_254 { EXTENDED, "X(.?){5,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_255 { EXTENDED, "X(.?){6,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_256 { EXTENDED, "X(.?){8,}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 8, 8 }} },
	CASE_257 { EXTENDED, "X(.?){0,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_258 { EXTENDED, "X(.?){1,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_259 { EXTENDED, "X(.?){2,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_260 { EXTENDED, "X(.?){3,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_261 { EXTENDED, "X(.?){4,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_262 { EXTENDED, "X(.?){5,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_263 { EXTENDED, "X(.?){6,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_264 { EXTENDED, "X(.?){7,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 7, 8 }} },
	CASE_265 { EXTENDED, "X(.?){8,8}Y", "X1234567Y", NO_ERR, {{ 0, 9 }, { 8, 8 }} },
	CASE_266 { EXTENDED, "(a|ab|c|bcd){4,}(d*)", "ababcd", regex::NOMATCH, {} },
	CASE_267 { EXTENDED, "(a|ab|c|bcd){4,10}(d*)", "ababcd", regex::NOMATCH, {} },
	CASE_268 { EXTENDED, "(ab|a|c|bcd){0,}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_269 { EXTENDED, "(ab|a|c|bcd){1,}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_270 { EXTENDED, "(ab|a|c|bcd){2,}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_271 { EXTENDED, "(ab|a|c|bcd){3,}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_272 { EXTENDED, "(ab|a|c|bcd){4,}(d*)", "ababcd", regex::NOMATCH, {} },
	CASE_273 { EXTENDED, "(ab|a|c|bcd){0,10}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_274 { EXTENDED, "(ab|a|c|bcd){1,10}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_275 { EXTENDED, "(ab|a|c|bcd){2,10}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_276 { EXTENDED, "(ab|a|c|bcd){3,10}(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_277 { EXTENDED, "(ab|a|c|bcd){4,10}(d*)", "ababcd", regex::NOMATCH, {} },
	CASE_278 { EXTENDED, "(ab|a|c|bcd)*(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_279 { EXTENDED, "(ab|a|c|bcd)+(d*)", "ababcd", NO_ERR, {{ 0, 6 }, { 3, 6 }, { 6, 6 }} },
	CASE_280 { EXTENDED, "(a*)*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_281 { EXTENDED, "(a*)*", "x", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_282 { EXTENDED, "(a*)*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_283 { EXTENDED, "(a*)*", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_284 { EXTENDED, "(a*)+", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_285 { EXTENDED, "(a*)+", "x", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_286 { EXTENDED, "(a*)+", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_287 { EXTENDED, "(a*)+", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_288 { EXTENDED, "(a+)*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_289 { EXTENDED, "(a+)*", "x", NO_ERR, {{ 0, 0 }} },
	CASE_290 { EXTENDED, "(a+)*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_291 { EXTENDED, "(a+)*", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_292 { EXTENDED, "(a+)+", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_293 { EXTENDED, "(a+)+", "x", regex::NOMATCH, {} },
	CASE_294 { EXTENDED, "(a+)+", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_295 { EXTENDED, "(a+)+", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_296 { EXTENDED, "([a]*)*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_297 { EXTENDED, "([a]*)*", "x", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_298 { EXTENDED, "([a]*)*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_299 { EXTENDED, "([a]*)*", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_300 { EXTENDED, "([a]*)+", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_301 { EXTENDED, "([a]*)+", "x", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_302 { EXTENDED, "([a]*)+", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_303 { EXTENDED, "([a]*)+", "aaaaaax", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_304 { EXTENDED, "([^b]*)*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_305 { EXTENDED, "([^b]*)*", "b", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_306 { EXTENDED, "([^b]*)*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_307 { EXTENDED, "([^b]*)*", "aaaaaab", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_308 { EXTENDED, "([ab]*)*", "a", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_309 { EXTENDED, "([ab]*)*", "aaaaaa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_310 { EXTENDED, "([ab]*)*", "ababab", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_311 { EXTENDED, "([ab]*)*", "bababa", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_312 { EXTENDED, "([ab]*)*", "b", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_313 { EXTENDED, "([ab]*)*", "bbbbbb", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_314 { EXTENDED, "([ab]*)*", "aaaabcde", NO_ERR, {{ 0, 5 }, { 0, 5 }} },
	CASE_315 { EXTENDED, "([^a]*)*", "b", NO_ERR, {{ 0, 1 }, { 0, 1 }} },
	CASE_316 { EXTENDED, "([^a]*)*", "bbbbbb", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_317 { EXTENDED, "([^a]*)*", "aaaaaa", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_318 { EXTENDED, "([^ab]*)*", "ccccxx", NO_ERR, {{ 0, 6 }, { 0, 6 }} },
	CASE_319 { EXTENDED, "([^ab]*)*", "ababab", NO_ERR, {{ 0, 0 }, { 0, 0 }} },
	CASE_320 { EXTENDED, "((z)+|a)*", "zabcde", NO_ERR, {{ 0, 2 }, { 1, 2 }} },
	CASE_321 { {}, "\\(a*\\)*\\(x\\)", "x", NO_ERR, {{ 0, 1 }, { 0, 0 }, { 0, 1 }} },
	CASE_322 { {}, "\\(a*\\)*\\(x\\)", "ax", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_323 { {}, "\\(a*\\)*\\(x\\)", "axa", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_324 { EXTENDED, "(a*)*(x)", "x", NO_ERR, {{ 0, 1 }, { 0, 0 }, { 0, 1 }} },
	CASE_325 { EXTENDED, "(a*)*(x)", "ax", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_326 { EXTENDED, "(a*)*(x)", "axa", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_327 { EXTENDED, "(a*)+(x)", "x", NO_ERR, {{ 0, 1 }, { 0, 0 }, { 0, 1 }} },
	CASE_328 { EXTENDED, "(a*)+(x)", "ax", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_329 { EXTENDED, "(a*)+(x)", "axa", NO_ERR, {{ 0, 2 }, { 0, 1 }, { 1, 2 }} },
	CASE_330 { EXTENDED, "(a*){2}(x)", "x", NO_ERR, {{ 0, 1 }, { 0, 0 }, { 0, 1 }} },
	CASE_331 { EXTENDED, "(a*){2}(x)", "ax", NO_ERR, {{ 0, 2 }, { 1, 1 }, { 1, 2 }} },
	CASE_332 { EXTENDED, "(a*){2}(x)", "axa", NO_ERR, {{ 0, 2 }, { 1, 1 }, { 1, 2 }} },
}

