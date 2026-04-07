# How to contribute to C3

The C3 project consists of

1. The C3 language itself.
2. The C3 compiler, called c3c.
3. The C3 standard library
4. Various tools, such as the editor plugins

## 1. How to contribute to the C3 language

The C3 language is essentially the language specification. You can contribute to the language by:

1. Filing enhancement requests for changes to the language.
2. Offering feedback on existing features, on Discord or by filing issues.
3. Help working on the language specification.
4. Help working on the grammar.

## 2. How to contribute to the C3 compiler

The C3 compiler consists for the compiler itself + test suites for testing the compiler. 
You can contribute by:

1. File bugs (by far the most important thing).
2. Suggest improved diagnostics / error messages.
3. Refactoring existing code (needs deep understanding of the compiler).
4. Add support for more architectures.
5. Add support for more backends.

## 3. How to contribute to the standard library

The standard library is the library itself + test suites for testing the standard library.
You can contribute by:

1. Filing bugs on the standard library.
2. Write additional unit tests.
3. Suggest new functionality by filing an issue.
4. Work on stdlib additions.
5. Fix bugs in the stdlib
6. Maintain a section of the standard library

### How to work on small stdlib additions

If there is just a matter of adding a function or two to an existing module, a pull request
is sufficient. However, please make sure that:

1. It follows the guidelines for the code to ensure a uniform experience (naming standard, indentation, braces etc).
2. Add a line in the release notes about the change.
3. Make sure it has unit tests.

### How to work on non-trivial additions to the stdlib

Regardless whether an addition is approved for inclusion or not, it needs to incubate:

1. First implement it standalone, showing that it’s working well and has a solid design. This has the advantage of people being able to contribute or even create competing implementations
2. Once it is considered finished it can be proposed for inclusion.

This will greatly help improving the quality of additions.

Note that any new addition needs a full set of unit tests before being included into the standard library.

### Maintain a part of the standard library

A single maintainer is insufficient for a standard library, instead we need one or more maintainer
for each module. The maintainer(s) will review pull requests and actively work on making the module
pristine with the highest possible quality.

## 4. How to contribute to various tools

In general, file a pull request. Depending on who maintains it, rules may differ.

## 5. AI usage in contributions

AI tools do not change contributor responsibilities.

**Pull Requests**

- You must fully understand and be able to explain all submitted code.
- Code must meet normal standards: style, clarity and tests.
- **Respect maintainer time:** Do not use PR reviews as an AI feedback loop.
- If review effort exceeds implementation effort, the PR may be rejected.
- Be transparent if a significant portion of the code is AI-generated.
- **Copyright risks:** The legal status of AI-generated code is uncertain and may become copyright encumbered in the future. Direct copy-pasting of AI-generated code cannot be accepted.

**Issues and Reports**

- Verify all claims, errors, and reproducible steps before submitting.
- Do not paste raw AI output. Write concise reports in your own words.

Use AI as a tool, not a substitute for understanding or effort.