module std::core::test::string::test @test;

struct EscapeTest
{
    String input;
    String expected_escaped;
    String expected_content_escaped;
}

EscapeTest[] escape_tests = {
    // Basic strings
    { "hello", `"hello"`, "hello" },
    { "", `""`, "" },

    // Special characters that need escaping
    { "hello\"world", `"hello\"world"`, `hello\"world` },
    { "path\\to\\file", `"path\\to\\file"`, `path\\to\\file` },
    { "line1\nline2", `"line1\nline2"`, `line1\nline2` },
    { "tab\there", `"tab\there"`, `tab\there` },
    { "carriage\rreturn", `"carriage\rreturn"`, `carriage\rreturn` },
    { "backspace\bchar", `"backspace\bchar"`, `backspace\bchar` },
    { "form\ffeed", `"form\ffeed"`, `form\ffeed` },
    { "vertical\vtab", `"vertical\vtab"`, `vertical\vtab` },
    { "null\0char", `"null\0char"`, `null\0char` },

    // Non-printable characters (should use hex escapes)
    { "\x01\x1f\x7f", `"\x01\x1f\x7f"`, `\x01\x1f\x7f` },

    // Mixed content
    { "Hello\nWorld\t!", `"Hello\nWorld\t!"`, `Hello\nWorld\t!` },
    { "Quote: \"Hello\"", `"Quote: \"Hello\""`, `Quote: \"Hello\"` },
};

struct UnescapeTest
{
    String input;
    String expected;
    fault expected_error;
}

UnescapeTest[] unescape_tests = {
    // Valid cases
    { `"hello"`, "hello", {} },
    { `""`, "", {} },
    { `"hello\"world"`, "hello\"world", {} },
    { `"path\\to\\file"`, "path\\to\\file", {} },
    { `"line1\nline2"`, "line1\nline2", {} },
    { `"tab\there"`, "tab\there", {} },
    { `"carriage\rreturn"`, "carriage\rreturn", {} },
    { `"backspace\bchar"`, "backspace\bchar", {} },
    { `"form\ffeed"`, "form\ffeed", {} },
    { `"vertical\vtab"`, "vertical\vtab", {} },
    { `"null\0char"`, "null\0char", {} },
    { `"slash\/works"`, "slash/works", {} },

    // Hex escapes
    { `"\x41\x42\x43"`, "ABC", {} },
    { `"\x00\x1f\x7f"`, "\x00\x1f\x7f", {} },

    // Unicode escapes
    { `"\u0041\u0042\u0043"`, "ABC", {} },
    { `"\u2603"`, "☃", {} }, // Snowman
    { `"\U0001F600"`, "😀", {} }, // Grinning face emoji

    // Error cases
    { `"unterminated`, "", string::UNTERMINATED_STRING },
    { `unterminated"`, "", string::UNTERMINATED_STRING },
    { `"invalid\q"`, "", string::INVALID_ESCAPE_SEQUENCE },
    { `"incomplete\"`, "", string::INVALID_ESCAPE_SEQUENCE },
    { `"bad\x"`, "", string::INVALID_HEX_ESCAPE },
    { `"bad\xG1"`, "", string::INVALID_HEX_ESCAPE },
    { `"bad\u"`, "", string::INVALID_UNICODE_ESCAPE },
    { `"bad\uGGGG"`, "", string::INVALID_UNICODE_ESCAPE },
    { `"bad\U"`, "", string::INVALID_UNICODE_ESCAPE },
    { `"bad\UGGGGGGGG"`, "", string::INVALID_UNICODE_ESCAPE },
};

fn void test_escape()
{
    foreach (test : escape_tests)
    {
        String result = test.input.tescape();
        assert(result == test.expected_escaped,
               "escape(%s) = %s, expected %s",
               test.input, result, test.expected_escaped);
    }
}

fn void test_escape_content()
{
    foreach (test : escape_tests)
    {
        String result = test.input.tescape(strip_quotes: true);
        assert(result == test.expected_content_escaped,
               "escape_content(%s) = %s, expected %s",
               test.input, result, test.expected_content_escaped);
    }
}

fn void test_unescape()
{
    foreach (test : unescape_tests)
    {
        String? result = test.input.tunescape();

        if (test.expected_error)
        {
            // Expecting an error
            if (catch err = result)
            {
                assert(err == test.expected_error,
                       "unescape(%s) failed with %s, expected %s",
                       test.input, err, test.expected_error);
            }
            else
            {
                assert(false, "unescape(%s) should have failed with %s",
                       test.input, test.expected_error);
            }
        }
        else
        {
            // Expecting success
            if (try actual = result)
            {
                assert(actual == test.expected,
                       "unescape(%s) = %s, expected %s",
                       test.input, actual, test.expected);
            }
            else
            {
                assert(false, "unescape(%s) failed unexpectedly", test.input);
            }
        }
    }
}

fn void test_roundtrip()
{
    String[] test_strings = {
        "hello world",
        "special chars: \n\t\r\"\\",
        "unicode: ☃ 😀",
        "mixed: Hello\nWorld\t!",
        "",
        "\x00\x01\x1f\x7f",
    };

    foreach (original : test_strings)
    {
        String escaped = original.tescape();
        String? unescaped = escaped.tunescape();

        if (try actual = unescaped)
        {
            assert(actual == original,
                   "roundtrip failed for %s: got %s",
                   original, actual);
        }
        else
        {
            assert(false, "roundtrip failed for %s: couldn't unescape %s",
                   original, escaped);
        }
    }
}

fn void test_needs_escape()
{
    // Characters that need escaping
    assert(string::needs_escape('"'));
    assert(string::needs_escape('\\'));
    assert(string::needs_escape('\n'));
    assert(string::needs_escape('\t'));
    assert(string::needs_escape('\r'));
    assert(string::needs_escape('\b'));
    assert(string::needs_escape('\f'));
    assert(string::needs_escape('\v'));
    assert(string::needs_escape('\0'));
    assert(string::needs_escape('\x01'));
    assert(string::needs_escape('\x1f'));
    assert(string::needs_escape('\x7f'));

    // Characters that don't need escaping
    assert(!string::needs_escape('a'));
    assert(!string::needs_escape('Z'));
    assert(!string::needs_escape('0'));
    assert(!string::needs_escape('9'));
    assert(!string::needs_escape(' '));
    assert(!string::needs_escape('!'));
    assert(!string::needs_escape('~'));
}

fn void test_escape_len()
{
    foreach (test : escape_tests)
    {
        usz calculated_len = string::escape_len(test.input);
        usz actual_len = test.expected_escaped.len;
        assert(calculated_len == actual_len,
               "escape_len(%s) = %d, but actual escaped length is %d",
               test.input, calculated_len, actual_len);
    }
}
