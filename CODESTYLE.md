# C3C Coding style for contributors

This is a rough guide to the coding style used for the C source code in c3c.

## Visual conventions

### Naming Conventions

- Constants use upper snake case, e.g. `THIS_IS_A_CONSTANT`
- Macros use upper snake case, e.g. `MY_MACRO(123)`
- User defined types use capitalized camel case. e.g. `MyLittleType`
- Function names use snake case, e.g. `my_little_function(123)`
- Variables use snake case, e.g. `context`  

In addition, function names that act on a particular object, should
be prefixed with the type name in snake case, with the name of
the function following e.g. `type_info_new` (and not "new_type_info").

### Brace placement

The source Allman style:

    if (foo) 
    {
        ... code here ...
    }

### Case and label indentation

Goto labels have no indentation:

    int foo()
    {
        x = 0;
    LABEL1:
        if (foo) 
        {
            ... code ...
        }
    }

Case statements have one tab indentation:

    switch foo()
    {
        case 1:
            do_something();
            break;
        case 2:
            do_something_else();
            FALLTHROUGH;
        default:
            do_default();
    }

### Spacing

Spaces between control statements and `()`:

    if (foo) ...
    while (foo) ...

Space after `,` and between expressions:

    if (foo && bar) return (Baz) { 1, 2, 3 };

Space around assignment:

    a += b;

No space inside parenthesis:

    int x = c * (foo(b) + b);

### Tab vs spaces

Recommendation: tabs, 4 spaces wide. No CRLF in the source.

### If, braces and new lines

Any `if` statement with `else` should use braces.

    if (foo)
    {
        ...
    }
    else
    {
        ...
    }

Single line `if` statements are allowed without braces.

    if (this_is_fine()) return true;

Otherwise use braces.

## General principles

General principles for coding.

### Keep big headers

Keep fairly large headers that can be used across multiple files, e.g. `sema_internal.h`
instead of fragmenting into one header file per c file.

### Keep the code simple and to the point

Don't add things "just because", because this requires more code to maintain.

### If it isn't tested it doesn't work

Code that works without testing are rare things. Test your code and preferably add
more tests as you go along.

### Don't bring in dependencies

External libraries has maintainability issues. Try to depend on as few libraries
as possible. Currently c3c only depends on LLVM and libc.

Do use rewrites of subsets of other libraries to bring in functionality, but avoid
copying in libraries that needs to be updated separately.

## Internals

Some notes about the internals.

### The arena allocator

The compiler uses an arena allocator that isn't released until the compiler closes.
In addition to this general allocator, there are allocators for certain types,
such as `Decl` `Ast` etc. These are discarded before code generation. Consequently
you need to make sure that none of those are used in the code generation phase.

### Dynamic array

Dynamic arrays are used throughout the compiler. They use the arena allocator and
thus will use up memory until the compiler ends. To create a dynamic array, just
declare a null pointer to the type and use `vec_add` to add elements:

    Foo *foos = NULL;
    vec_add(foos, (Foo) { 1, 2 });

Iterating over the elements are done using `VECEACH`.

### Scratch buffer for strings.

There is a scratch buffer for strings in the `global_context` prefer using that
one with related functions when working on temporary strings.