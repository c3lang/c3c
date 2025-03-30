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

Use tabs for indentation, no CRLF in the source.

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
as possible. Currently, c3c only depends on LLVM and libc with an optional 
dependency on libcurl.

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

# C3 Standard library style guide.

When contributing to the standard librairy please to your best to follow the following style requirements 
as to ensure a consistent style in the stdlib and also make accepting PRs more quickly.

### Braces are placed on the next line

**NO:**
```c
fn void foo(String bar) {
  @pool() {
    ...
  };
} 
```

**YES:**
```c
fn void foo(String bar)
{
  @pool() 
  {
    ...
  };
}
```

### Indentation with tabs

Use tab for indentation, not spaces, no CRLF in the sources 

### Type names

Use `PascalCase` not `Ada_Case` for type names.

**YES:**
```c
enum MyEnum
{
  ABC,
  DEF
}  
```

**NO:**
```c
enum My_Enum
{
  ABC,
  DEF
}  
```

### Type names when binding to OS libraries

When doing bindings (for instance, adding declarations referring to Win32 APIs),
try to retain the original name when possible. If it isn't possible use (consistently)
one of two options:

1. Prefix: `HANDLE` -> `Win32_HANDLE`
2. Change the first letter to upper case: `mode_t` -> `Mode_t`

### Variables, function, methods and globals

Use `snake_case`, not `camelCase`.

**YES:**
```c
int some_global = 1;

fn void open_file(String special_file)
{
  ...
}
```

**NO:**
```c
int someGlobal = 1;

fn void openFile(String specialFile)
{
  ...
}
```

### Variables, function, methods and globals when binding to OS libraries

When doing bindings (for instance, adding declarations referring to Win32 APIs),
try to retain the original name when possible. If it isn't possible use (consistently)
one of two options:

1. Prefix: `win32_GetWindowLongPtrW`. However, this is usually only recommended if it is builtin.
2. Change first character to lower case: `GetWindowLongPtrW` -> `getWindowLongPtrW`

### Use `self` as the first method argument

Unless there is a strong reason not to, use `self` for the first parameter in a method.

### The allocator argument

Prefer always calling the allocator parameter `allocator`, and make it the first regular
argument.

## Add tests to your changes

If you add or fix things, then there should always be tests in `test/unit/stdlib` to verify
the functionality.