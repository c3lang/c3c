module regex_benchmarks;

import std::regex;
import std::io;


const DEFAULT_ITERATIONS = 50_000;

// A moderately long input string used across several benchmarks.
const String HAYSTACK = "The quick brown fox jumps over the lazy dog. "
    "Pack my box with five dozen liquor jugs. "
    "How vexingly quick daft zebras jump! "
    "The five boxing wizards jump quickly. "
    "Sphinx of black quartz, judge my vow.";
const String EMAIL_INPUT = "Contact us at support@example.com for help.";
const String BACKREF_HIT = "hello hello";
const String BACKREF_MISS = "hello world";
const String BACKREF_FIND_INPUT = "IDs: 42-42 and 71-19";
const String CONDITIONAL_HIT = "3xdigit";
const String CONDITIONAL_MISS = "3xother";
const String GPOS_INPUT = "12345678x";

Regex* re_literal;
Regex* re_charclass;
Regex* re_quantifiers;
Regex* re_alternation;
Regex* re_groups;
Regex* re_anchors;
Regex* re_complex;
Regex* re_bt_backref;
Regex* re_bt_atomic;
Regex* re_bt_conditional;
Regex* re_bt_find_backref;
Regex* re_bt_gpos;

fn void bench_setup() @init
{
    set_benchmark_warmup_iterations(3);
    set_benchmark_max_iterations(DEFAULT_ITERATIONS);

    set_benchmark_func_iterations($qnameof(compile_literal),       200_000);
    set_benchmark_func_iterations($qnameof(compile_charclass),     100_000);
    set_benchmark_func_iterations($qnameof(compile_quantifiers),   100_000);
    set_benchmark_func_iterations($qnameof(compile_alternation),    80_000);
    set_benchmark_func_iterations($qnameof(compile_groups),         80_000);
    set_benchmark_func_iterations($qnameof(compile_complex),        20_000);
    set_benchmark_func_iterations($qnameof(compile_bt_backref),     80_000);
    set_benchmark_func_iterations($qnameof(compile_bt_atomic),      80_000);
    set_benchmark_func_iterations($qnameof(compile_bt_conditional), 60_000);

    set_benchmark_func_iterations($qnameof(is_match_literal_hit),  200_000);
    set_benchmark_func_iterations($qnameof(is_match_literal_miss), 200_000);
    set_benchmark_func_iterations($qnameof(is_match_charclass),    100_000);
    set_benchmark_func_iterations($qnameof(is_match_quantifiers),  100_000);
    set_benchmark_func_iterations($qnameof(is_match_alternation),  100_000);
    set_benchmark_func_iterations($qnameof(is_match_groups),       100_000);
    set_benchmark_func_iterations($qnameof(is_match_anchors),      100_000);
    set_benchmark_func_iterations($qnameof(is_match_complex),       20_000);
    set_benchmark_func_iterations($qnameof(is_match_bt_backref_hit),  80_000);
    set_benchmark_func_iterations($qnameof(is_match_bt_backref_miss), 80_000);
    set_benchmark_func_iterations($qnameof(is_match_bt_atomic),       80_000);
    set_benchmark_func_iterations($qnameof(is_match_bt_conditional),  60_000);

    set_benchmark_func_iterations($qnameof(find_short_input),      100_000);
    set_benchmark_func_iterations($qnameof(find_long_input),        50_000);
    set_benchmark_func_iterations($qnameof(find_all_words),         20_000);
    set_benchmark_func_iterations($qnameof(find_bt_backref),        80_000);
    set_benchmark_func_iterations($qnameof(find_all_bt_gpos),       40_000);

    set_benchmark_func_iterations($qnameof(replace_literal),        50_000);
    set_benchmark_func_iterations($qnameof(replace_all_words),      10_000);

    re_literal     = regex::compile(mem, "fox")!!;
    re_charclass   = regex::compile(mem, "[a-z]+")!!;
    re_quantifiers = regex::compile(mem, "\\w{3,8}")!!;
    re_alternation = regex::compile(mem, "fox|dog|cat|rat|owl")!!;
    re_groups      = regex::compile(mem, "(quick)(\\s+)(brown)")!!;
    re_anchors     = regex::compile(mem, "^The")!!;
    re_complex     = regex::compile(mem, `[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}`)!!;
    re_bt_backref     = regex::compile(mem, "(\\w+) \\1")!!;
    re_bt_atomic      = regex::compile(mem, "(?>a|ab)c")!!;
    re_bt_conditional = regex::compile(mem, "^(\\d)?x(?(1)digit|other)$")!!;
    re_bt_find_backref = regex::compile(mem, "(\\d+)-\\1")!!;
    re_bt_gpos        = regex::compile(mem, "\\G\\d{2}")!!;
}

fn void bench_teardown() @finalizer
{
    re_literal.free();
    re_charclass.free();
    re_quantifiers.free();
    re_alternation.free();
    re_groups.free();
    re_anchors.free();
    re_complex.free();
    re_bt_backref.free();
    re_bt_atomic.free();
    re_bt_conditional.free();
    re_bt_find_backref.free();
    re_bt_gpos.free();
}


// ==============================================================================================
module regex_benchmarks @benchmark;

import std::regex;


// ----- Compilation benchmarks ---------------------------------------------------------------
// These measure parse + NFA-build cost for patterns of increasing complexity.

fn void compile_literal() => @pool()
{
    Regex* re = regex::tcompile("hello");
    re.free();
}

fn void compile_charclass() => @pool()
{
    Regex* re = regex::tcompile("[a-zA-Z0-9_]+");
    re.free();
}

fn void compile_quantifiers() => @pool()
{
    Regex* re = regex::tcompile("a{2,5}b*c+d?");
    re.free();
}

fn void compile_alternation() => @pool()
{
    Regex* re = regex::tcompile("foo|bar|baz|qux|quux");
    re.free();
}

fn void compile_groups() => @pool()
{
    Regex* re = regex::tcompile("(foo)(bar)(baz)");
    re.free();
}

fn void compile_complex() => @pool()
{
    // Simulate a realistic pattern: simple email-ish validator
    Regex* re = regex::tcompile(`[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}`);
    re.free();
}

fn void compile_bt_backref() => @pool()
{
    Regex* re = regex::tcompile("(\\w+) \\1");
    re.free();
}

fn void compile_bt_atomic() => @pool()
{
    Regex* re = regex::tcompile("(?>a|ab)c");
    re.free();
}

fn void compile_bt_conditional() => @pool()
{
    Regex* re = regex::tcompile("^(\\d)?x(?(1)digit|other)$");
    re.free();
}


// ----- is_match benchmarks ------------------------------------------------------------------
// Pre-compiled regexes (set up in bench_setup); only measure the matching engine.

fn void is_match_literal_hit()
{
    assert(re_literal.is_match("The quick brown fox jumps over the lazy dog."));
}

fn void is_match_literal_miss()
{
    assert(!re_literal.is_match("No match here at all."));
}

fn void is_match_charclass()
{
    assert(re_charclass.is_match(HAYSTACK));
}

fn void is_match_quantifiers()
{
    assert(re_quantifiers.is_match(HAYSTACK));
}

fn void is_match_alternation()
{
    assert(re_alternation.is_match(HAYSTACK));
}

fn void is_match_groups()
{
    assert(re_groups.is_match(HAYSTACK));
}

fn void is_match_anchors()
{
    assert(re_anchors.is_match(HAYSTACK));
}

fn void is_match_complex()
{
    assert(!re_complex.is_match(HAYSTACK));
    assert(re_complex.is_match(EMAIL_INPUT));
}


// ----- Backtracking-only is_match benchmarks -------------------------------------------------
// These patterns force the dedicated backtracking engine via backrefs, atomics, or conditionals.

fn void is_match_bt_backref_hit()
{
    assert(re_bt_backref.is_match(BACKREF_HIT));
}

fn void is_match_bt_backref_miss()
{
    assert(!re_bt_backref.is_match(BACKREF_MISS));
}

fn void is_match_bt_atomic()
{
    assert(!re_bt_atomic.is_match("abc"));
    assert(re_bt_atomic.is_match("ac"));
}

fn void is_match_bt_conditional()
{
    assert(re_bt_conditional.is_match(CONDITIONAL_HIT));
    assert(!re_bt_conditional.is_match(CONDITIONAL_MISS));
}


// ----- find / find_all benchmarks -----------------------------------------------------------

fn void find_short_input() => @pool()
{
    Regex* re = regex::tcompile("\\b\\w{5}\\b");
    Match m = re.find(tmem, "The quick brown fox")!!;
    defer m.free();
}

fn void find_long_input() => @pool()
{
    Match m = re_alternation.find(tmem, HAYSTACK)!!;
    defer m.free();
    assert(m.full() == "fox" || m.full() == "dog");
}

fn void find_all_words() => @pool()
{
    Regex* re = regex::tcompile("\\b[A-Z][a-z]+\\b");
    MatchIter it = re.find_all(tmem, HAYSTACK);
    usz count;
    while (try m = it.next())
    {
        defer m.free();
        count++;
    }
    assert(count > 0);
}


// ----- Backtracking-only find benchmarks -----------------------------------------------------

fn void find_bt_backref() => @pool()
{
    Match m = re_bt_find_backref.find(tmem, BACKREF_FIND_INPUT)!!;
    defer m.free();
    assert(m.full() == "42-42");
}

fn void find_all_bt_gpos() => @pool()
{
    MatchIter it = re_bt_gpos.find_all(tmem, GPOS_INPUT);
    usz count;
    while (try m = it.next())
    {
        defer m.free();
        count++;
    }
    assert(count == 4);
}


// ----- replace benchmarks -------------------------------------------------------------------

fn void replace_literal() => @pool()
{
    Regex* re = regex::tcompile("fox");
    String result = re.replace(tmem, "The quick brown fox", "cat")!!;
    assert(result == "The quick brown cat");
}

fn void replace_all_words() => @pool()
{
    Regex* re = regex::tcompile("\\b(fox|dog)\\b");
    String result = re.replace_all(tmem, HAYSTACK, "animal")!!;
    assert(result.len > 0);
}
